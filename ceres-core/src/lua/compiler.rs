use std::path::PathBuf;

use mlua::prelude::*;

use crate::compiler;
use crate::lua::macros;
use crate::lua::util::wrap_result;

pub fn get_compile_script_luafn(ctx: &Lua) -> LuaFunction {
    ctx.create_function(|ctx, args: LuaTable| {
        let result = compile_script(ctx, args);

        Ok(wrap_result(ctx, result))
    })
    .unwrap()
}

fn compile_script(ctx: &Lua, args: LuaTable) -> Result<String, anyhow::Error> {
    let src_directories: Vec<LuaString> = args.get("srcDirectories")?;
    let map_script: LuaString = args.get("mapScript")?;

    let src_directories: Vec<PathBuf> = src_directories
        .iter()
        .map(|s| s.to_str().unwrap().as_ref().into())
        .collect();

    let mut module_provider = compiler::ProjectModuleProvider::new(&src_directories);
    module_provider.scan();
    let macro_provider = macros::get_threadlocal_macro_provider();
    let mut compiler = compiler::ScriptCompiler::new(ctx, module_provider, macro_provider);

    compiler.set_map_script(map_script.to_str()?.as_ref().into());
    compiler.add_module("main", false)?;
    compiler.add_module("config", true)?;
    compiler.add_module("init", true)?;

    Ok(compiler.emit_script())
}
