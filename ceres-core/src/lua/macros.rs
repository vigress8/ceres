use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::thread_local;

use mlua::prelude::*;

use compiler::MacroProvider;

use crate::compiler;
use crate::error::*;
use crate::lua::util::evaluate_macro_args;

pub struct LuaMacroProvider {
    registered_macros: RefCell<HashMap<String, LuaRegistryKey>>,
}

impl LuaMacroProvider {
    fn register_macro(&self, ctx: &Lua, id: &str, func: LuaFunction) {
        let registry_key = ctx.create_registry_value(func).unwrap();

        self.registered_macros
            .borrow_mut()
            .insert(id.into(), registry_key);
    }
}

impl MacroProvider for LuaMacroProvider {
    fn is_macro_id(&self, id: &str) -> bool {
        self.registered_macros.borrow().contains_key(id)
    }

    fn handle_macro(
        &self,
        ctx: &Lua,
        id: &str,
        compilation_data: &mut compiler::CompilationData,
        macro_invocation: compiler::MacroInvocation,
    ) -> Result<(), MacroInvocationError> {
        let args = evaluate_macro_args(ctx, macro_invocation.args).unwrap();
        let callback: LuaFunction = {
            let registered_macros = self.registered_macros.borrow();

            let registry_key = registered_macros.get(id).unwrap();
            ctx.registry_value(registry_key).unwrap()
        };

        let value = callback.call(args).unwrap();

        if let LuaValue::String(value) = value {
            compilation_data.src += &value.to_str().unwrap();
        }

        Ok(())
    }
}

impl MacroProvider for Rc<LuaMacroProvider> {
    fn is_macro_id(&self, id: &str) -> bool {
        (self.deref()).is_macro_id(id)
    }

    fn handle_macro(
        &self,
        ctx: &Lua,
        id: &str,
        compilation_data: &mut compiler::CompilationData,
        macro_invocation: compiler::MacroInvocation,
    ) -> Result<(), MacroInvocationError> {
        (self.deref()).handle_macro(ctx, id, compilation_data, macro_invocation)
    }
}

thread_local! {
    static LUA_MACRO_PROVIDER: RefCell<Option<Rc<LuaMacroProvider>>> = RefCell::new(None);
}

pub fn get_threadlocal_macro_provider() -> Rc<LuaMacroProvider> {
    LUA_MACRO_PROVIDER.with(|macro_provider| {
        let mut macro_provider = macro_provider.borrow_mut();

        if macro_provider.is_none() {
            let macro_provider_new = LuaMacroProvider {
                registered_macros: Default::default(),
            };

            macro_provider.replace(Rc::new(macro_provider_new));
        }

        Rc::clone(macro_provider.as_ref().unwrap())
    })
}

pub fn get_register_luafn(ctx: &Lua) -> LuaFunction {
    ctx.create_function(|ctx, (id, callback): (String, LuaFunction)| {
        let lua_macro_provider = get_threadlocal_macro_provider();

        lua_macro_provider.register_macro(ctx, &id, callback);

        Ok(())
    })
    .unwrap()
}
