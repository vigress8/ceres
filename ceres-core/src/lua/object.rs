use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use mlua::prelude::*;

use anyhow::anyhow;
use atoi::atoi;
use ceres_formats::{ObjectId, ObjectKind};
use ceres_formats::metadata::FieldDesc;
use ceres_formats::object::{Object, Value};
use ceres_formats::objectstore::ObjectStore;
use ceres_formats::parser::w3obj;

use crate::error::StringError;
use crate::lua::util::*;

fn get_field_for<C>(object: &Object, field_getter: C) -> Option<&Value>
where
    C: Fn(&Object) -> Option<&Value>,
{
    field_getter(object).or_else(|| {
        w3data::data()
            .object_prototype(object)
            .and_then(field_getter)
    })
}

struct StaticMethodKeys {
    obj_getfield:       LuaRegistryKey,
    obj_setfield:       LuaRegistryKey,
    obj_clone:          LuaRegistryKey,
    objstore_read:      LuaRegistryKey,
    objstore_write:     LuaRegistryKey,
    objstore_setobject: LuaRegistryKey,
    objstore_getobject: LuaRegistryKey,
}

thread_local! {
    static OBJECT_METHODS: RefCell<Option<StaticMethodKeys >> = const { RefCell::new(None) };
}

struct StaticMethods {
    obj_getfield:       LuaFunction,
    obj_setfield:       LuaFunction,
    obj_clone:          LuaFunction,
    objstore_read:      LuaFunction,
    objstore_write:     LuaFunction,
    objstore_setobject: LuaFunction,
    objstore_getobject: LuaFunction,
}

impl<'lua> StaticMethods {
    fn new(ctx: &Lua, keys: &StaticMethodKeys) -> StaticMethods {
        StaticMethods {
            obj_getfield:       ctx.registry_value(&keys.obj_getfield).unwrap(),
            obj_setfield:       ctx.registry_value(&keys.obj_setfield).unwrap(),
            obj_clone:          ctx.registry_value(&keys.obj_clone).unwrap(),
            objstore_read:      ctx.registry_value(&keys.objstore_read).unwrap(),
            objstore_write:     ctx.registry_value(&keys.objstore_write).unwrap(),
            objstore_getobject: ctx.registry_value(&keys.objstore_getobject).unwrap(),
            objstore_setobject: ctx.registry_value(&keys.objstore_setobject).unwrap(),
        }
    }

    fn with<C, R>(ctx: &Lua, callback: C) -> R
    where
        C: FnOnce(&Lua, StaticMethods) -> R,
    {
        OBJECT_METHODS.with(|keys| {
            if keys.borrow().is_none() {
                let objstore_read = ctx
                    .create_registry_value(
                        ctx.create_function(LuaObjectStoreWrapper::read).unwrap(),
                    )
                    .unwrap();

                let objstore_write = ctx
                    .create_registry_value(
                        ctx.create_function(LuaObjectStoreWrapper::write).unwrap(),
                    )
                    .unwrap();

                let objstore_getobject = ctx
                    .create_registry_value(
                        ctx.create_function(|ctx, (object, key): (LuaAnyUserData, LuaValue)| {
                            let mut object = object.borrow_mut::<LuaObjectStoreWrapper>()?;
                            LuaObjectStoreWrapper::get_object(ctx, (&mut object, key))
                        })
                        .unwrap(),
                    )
                    .unwrap();

                let objstore_setobject = ctx
                    .create_registry_value(
                        ctx.create_function(
                            |ctx, (object, key, value): (LuaAnyUserData, LuaValue, LuaValue)| {
                                let mut object = object.borrow_mut::<LuaObjectStoreWrapper>()?;
                                LuaObjectStoreWrapper::set_object(ctx, (&mut object, key, value))
                            },
                        )
                        .unwrap(),
                    )
                    .unwrap();

                let obj_clone = ctx
                    .create_registry_value(ctx.create_function(LuaObjectWrapper::clone).unwrap())
                    .unwrap();

                let obj_setfield = ctx
                    .create_registry_value(
                        ctx.create_function(
                            |ctx, (object, key, value): (LuaAnyUserData, LuaValue, LuaValue)| {
                                let object = object.borrow::<LuaObjectWrapper>()?;
                                LuaObjectWrapper::set_field(ctx, (&object, key, value))
                            },
                        )
                        .unwrap(),
                    )
                    .unwrap();

                let obj_getfield = ctx
                    .create_registry_value(
                        ctx.create_function(|ctx, (object, key): (LuaAnyUserData, LuaValue)| {
                            let object = object.borrow::<LuaObjectWrapper>()?;
                            LuaObjectWrapper::get_field(ctx, (&object, key))
                        })
                        .unwrap(),
                    )
                    .unwrap();

                *keys.borrow_mut() = Some(StaticMethodKeys {
                    obj_setfield,
                    obj_getfield,
                    obj_clone,
                    objstore_read,
                    objstore_write,
                    objstore_getobject,
                    objstore_setobject,
                })
            }

            let keys_ref = keys.borrow();
            let keys = keys_ref.as_ref().unwrap();
            let methods = StaticMethods::new(ctx, keys);

            callback(ctx, methods)
        })
    }

    fn obj_getfield_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.obj_getfield)
        })
    }

    fn obj_setfield_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.obj_setfield)
        })
    }

    fn obj_clone_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| LuaValue::Function(methods.obj_clone))
    }

    fn objstore_read_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_read)
        })
    }

    fn objstore_write_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_write)
        })
    }

    fn objstore_getobject_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_getobject)
        })
    }

    fn objstore_setobject_fn(ctx: &Lua) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_setobject)
        })
    }
}

struct LuaObjectWrapper {
    inner: Rc<RefCell<Object>>,
}

impl LuaObjectWrapper {
    fn clone(
        _ctx: &Lua,
        object: LuaAnyUserData,
    ) -> Result<impl IntoLuaMulti, LuaError> {
        let object = object.borrow::<LuaObjectWrapper>()?;

        let object = object.inner.borrow();
        let mut new_object = Object::new(object.id(), object.kind());

        new_object.set_parent_id(object.parent_id());
        new_object.add_from(&object);

        Ok(LuaObjectWrapper {
            inner: Rc::new(RefCell::new(new_object)),
        })
    }

    fn fields(ctx: &Lua, object: &Object) -> Result<LuaValue, LuaError> {
        let fields: Vec<_> = w3data::metadata()
            .query_all_object_fields(object)
            .map(|field_desc| field_desc.id)
            .collect();

        fields.into_lua(ctx)
    }

    fn translate_field_name(
        ctx: &Lua,
        key: LuaValue,
        object: &Object,
    ) -> Result<Option<(&'static FieldDesc, Option<u32>)>, LuaError> {
        if let Ok(id) = LuaInteger::from_lua(key.clone(), ctx) {
            let field_desc =
                w3data::metadata().query_object_field(ObjectId::new(id as u32), object);

            return Ok(field_desc.map(|d| (d, None)));
        }

        let key = LuaString::from_lua(key, ctx)?;
        let field_bytes = key.as_bytes();

        // check if the field is in the form of 'XXXX' or 'XXXX+Y'
        if (field_bytes.len() == 4) || (field_bytes.len() > 5 && field_bytes[4] == b'+') {
            let object_id = ObjectId::from_bytes(&field_bytes[0..4]).unwrap();

            if let Some(field_desc) = w3data::metadata().query_object_field(object_id, object) {
                let level = if field_bytes.len() > 5 {
                    atoi::<u32>(&field_bytes[5..])
                } else {
                    None
                };

                if (level.is_some() && field_desc.variant.is_leveled())
                    || (level.is_none() && !field_desc.variant.is_leveled())
                {
                    return Ok(Some((field_desc, level)));
                }
            }
        }

        let result = w3data::metadata().query_lua_field(object, &key.to_str()?);

        Ok(result)
    }

    fn get_field(
        ctx: &Lua,
        (object, key): (&LuaObjectWrapper, LuaValue),
    ) -> Result<impl IntoLua, LuaError> {
        let object = object.inner.borrow();

        if let Some((field_desc, level)) = Self::translate_field_name(ctx, key, &object)? {
            let field = if let Some(level) = level {
                get_field_for(&object, |o| o.leveled_field(field_desc.id, level))
            } else {
                get_field_for(&object, |o| o.simple_field(field_desc.id))
            };

            if let Some(field) = field {
                return Ok(value_to_lvalue(ctx, field));
            }
        }

        Ok(LuaValue::Nil)
    }

    fn set_field(
        ctx: &Lua,
        (object, key, value): (&LuaObjectWrapper, LuaValue, LuaValue),
    ) -> Result<impl IntoLuaMulti, LuaError> {
        let object = &mut object.inner.borrow_mut();

        if let Some((field_desc, level)) = Self::translate_field_name(ctx, key, object)? {
            if let LuaValue::Nil = value {
                if let Some(level) = level {
                    object.unset_leveled_field(field_desc.id, level)
                } else {
                    object.unset_simple_field(field_desc.id)
                }
            } else {
                let value = lvalue_to_value(ctx, value, field_desc)?;
                if let Some(level) = level {
                    object.set_leveled_field(field_desc.id, level, value)
                } else {
                    object.set_simple_field(field_desc.id, value)
                }
            }

            return Ok(LuaValue::Nil);
        }

        Err(LuaError::external(anyhow!(
            "cannot set unknown field on object"
        )))
    }

    fn index(
        ctx: &Lua,
        object: &mut LuaObjectWrapper,
        key: LuaValue,
    ) -> Result<impl IntoLua, LuaError> {
        let object_inner = &object.inner.borrow();

        if let Ok(key) = LuaString::from_lua(key.clone(), ctx) {
            let key = key.as_bytes();

            match key.as_ref() {
                b"all" => return Self::fields(ctx, object_inner),
                b"clone" => return Ok(StaticMethods::obj_clone_fn(ctx)),
                b"setField" => return Ok(StaticMethods::obj_setfield_fn(ctx)),
                b"getField" => return Ok(StaticMethods::obj_getfield_fn(ctx)),
                b"id" => return object_inner.id().into_lua(ctx),
                b"parentId" => return object_inner.parent_id().into_lua(ctx),
                b"type" => return object_inner.kind().to_typestr().into_lua(ctx),
                _ => {}
            }
        }

        Self::get_field(ctx, (object, key))?.into_lua(ctx)
    }

    fn newindex(
        ctx: &Lua,
        object: &mut LuaObjectWrapper,
        (key, value): (LuaValue, LuaValue),
    ) -> Result<impl IntoLuaMulti, LuaError> {
        Self::set_field(ctx, (object, key, value))
    }
}

struct LuaObjectStoreWrapper {
    inner: ObjectStore,
    kind:  ObjectKind,
}

impl LuaObjectStoreWrapper {
    fn read(
        ctx: &Lua,
        (data, value): (LuaAnyUserData, LuaValue),
    ) -> Result<LuaValue, LuaError> {
        let mut data = data.borrow_mut::<LuaObjectStoreWrapper>()?;
        let kind = data.kind;
        let data = &mut data.inner;
        let value = LuaString::from_lua(value, ctx)?;

        w3obj::read::read_object_file(&value.as_bytes(), data, kind).map_err(LuaError::external)?;
        data.reset_dirty();

        Ok(LuaValue::Nil)
    }

    fn write(
        ctx: &Lua,
        data: LuaAnyUserData,
    ) -> Result<LuaValue, LuaError> {
        let data = data.borrow::<LuaObjectStoreWrapper>()?;
        let kind = data.kind;
        let data = &data.inner;

        let mut buf = Vec::new();
        w3obj::write::write_object_file(&mut buf, w3data::metadata(), data, kind)
            .map_err(LuaError::external)?;

        Ok(LuaValue::String(ctx.create_string(&buf)?))
    }

    fn object_or_new(
        data: &mut ObjectStore,
        kind: ObjectKind,
        id: ObjectId,
    ) -> Option<Rc<RefCell<Object>>> {
        data.object(id).map(Rc::clone).or_else(|| {
            w3data::data()
                .object(id)
                .filter(|object| kind.contains(object.kind()))
                .map(|object| {
                    let object = Object::new(object.id(), object.kind());
                    data.insert_object(object);

                    Rc::clone(data.object(id).unwrap())
                })
        })
    }

    fn objects(
        ctx: &Lua,
        data: &mut ObjectStore,
        kind: ObjectKind,
    ) -> Result<LuaValue, LuaError> {
        let table = ctx.create_table()?;
        let mut set: HashSet<ObjectId> = HashSet::new();

        set.extend(
            w3data::data()
                .objects()
                .filter(|object| kind.contains(object.kind()))
                .map(|o| o.id()),
        );

        set.extend(data.objects().map(|o| o.borrow().id()));

        for id in set {
            let object = Self::object_or_new(data, kind, id).unwrap();
            table.set(id, LuaObjectWrapper { inner: object })?;
        }

        Ok(LuaValue::Table(table))
    }

    fn get_object(
        ctx: &Lua,
        (data, key): (&mut LuaObjectStoreWrapper, LuaValue),
    ) -> Result<LuaValue, LuaError> {
        let kind = data.kind;
        let data = &mut data.inner;

        if let Ok(id) = ObjectId::from_lua(key, ctx) {
            return Self::object_or_new(data, kind, id)
                .map(|object| LuaObjectWrapper { inner: object })
                .into_lua(ctx);
        }

        Ok(LuaValue::Nil)
    }

    fn set_object(
        ctx: &Lua,
        (data, key, value): (&mut LuaObjectStoreWrapper, LuaValue, LuaValue),
    ) -> Result<LuaValue, LuaError> {
        let data = &mut data.inner;

        if let Ok(id) = ObjectId::from_lua(key.clone(), ctx) {
            if let LuaValue::Nil = value {
                data.remove_object(id)
            } else {
                let value = LuaAnyUserData::from_lua(value, ctx)?;
                let object = value.borrow::<LuaObjectWrapper>()?;
                let object = object.inner.borrow();

                if w3data::data().object(id).is_some() {
                    if id == object.id() {
                        let object_clone = object.clone();
                        data.insert_object(object_clone);
                    } else {
                        return Err(LuaError::external(anyhow!(
                            "stock objects can only be assigned to the same stock object"
                        )));
                    }
                } else {
                    let mut object_clone = object.clone();

                    if object_clone.parent_id().is_none() {
                        object_clone.set_parent_id(Some(object_clone.id()));
                    }

                    object_clone.set_id(id);
                    data.insert_object(object_clone);
                }
            }

            Ok(LuaValue::Nil)
        } else {
            Err(LuaError::external(anyhow!(
                "cannot assign invalid field {:?} on object store",
                key
            )))
        }
    }

    fn index(
        ctx: &Lua,
        data: &mut LuaObjectStoreWrapper,
        key: LuaValue,
    ) -> Result<LuaValue, LuaError> {
        let kind = data.kind;
        let data_inner = &mut data.inner;

        if let Ok(key) = LuaString::from_lua(key.clone(), ctx) {
            let key = key.as_bytes();

            match key.as_ref() {
                b"all" => return Self::objects(ctx, data_inner, kind),
                b"readFromString" => return Ok(StaticMethods::objstore_read_fn(ctx)),
                b"writeToString" => return Ok(StaticMethods::objstore_write_fn(ctx)),
                b"getObject" => return Ok(StaticMethods::objstore_getobject_fn(ctx)),
                b"setObject" => return Ok(StaticMethods::objstore_setobject_fn(ctx)),
                b"ext" => return kind.to_ext().into_lua(ctx),
                b"typestr" => return kind.to_typestr().into_lua(ctx),
                b"isDirty" => return data_inner.is_dirty().into_lua(ctx),
                _ => {}
            }
        }

        Self::get_object(ctx, (data, key))
    }

    fn newindex(
        ctx: &Lua,
        data: &mut LuaObjectStoreWrapper,
        (key, value): (LuaValue, LuaValue),
    ) -> Result<LuaValue, LuaError> {
        Self::set_object(ctx, (data, key, value))
    }
}

impl LuaUserData for LuaObjectWrapper {
    fn add_methods<T>(methods: &mut T)
    where
        T: LuaUserDataMethods<Self>,
    {
        methods.add_meta_method_mut(LuaMetaMethod::Index, LuaObjectWrapper::index);
        methods.add_meta_method_mut(LuaMetaMethod::NewIndex, LuaObjectWrapper::newindex);
    }
}

impl LuaUserData for LuaObjectStoreWrapper {
    fn add_methods<T>(methods: &mut T)
    where
        T: LuaUserDataMethods<Self>,
    {
        methods.add_meta_method_mut(LuaMetaMethod::Index, LuaObjectStoreWrapper::index);
        methods.add_meta_method_mut(LuaMetaMethod::NewIndex, LuaObjectStoreWrapper::newindex);
    }
}

// standalone functions

fn open_store_from_str(
    source: &[u8],
    kind: ObjectKind,
) -> Result<LuaObjectStoreWrapper, anyhow::Error> {
    let mut data = ObjectStore::default();
    w3obj::read::read_object_file(source, &mut data, kind)?;
    data.reset_dirty();

    Ok(LuaObjectStoreWrapper { inner: data, kind })
}

fn get_open_store_from_str_luafn(ctx: &Lua) -> LuaFunction {
    ctx.create_function(|_ctx: &Lua, (data, ext): (LuaString, LuaString)| {
        let data = data.as_bytes();
        let kind = ObjectKind::from_ext(&ext.to_str()?);

        if kind == ObjectKind::empty() {
            return Err(StringError::new(format!(
                "{} is not a valid format",
                ext.to_str().unwrap()
            ))
            .into());
        }

        let result = open_store_from_str(&data, kind).map_err(LuaError::external)?;

        Ok(result)
    })
    .unwrap()
}

fn get_open_store_blank_luafn(ctx: &Lua) -> LuaFunction {
    ctx.create_function(|_ctx: &Lua, ext: LuaString| {
        let kind = ObjectKind::from_ext(&ext.to_str()?);

        if kind == ObjectKind::empty() {
            return Err(StringError::new(format!(
                "{} is not a valid format",
                ext.to_str().unwrap()
            ))
            .into());
        }

        Ok(LuaObjectStoreWrapper {
            inner: ObjectStore::default(),
            kind,
        })
    })
    .unwrap()
}

pub fn get_object_module(ctx: &Lua) -> LuaTable {
    let table = ctx.create_table().unwrap();

    table
        .set("openStore", get_open_store_from_str_luafn(ctx))
        .unwrap();

    table
        .set("newStore", get_open_store_blank_luafn(ctx))
        .unwrap();

    table
}
