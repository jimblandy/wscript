#![allow(dead_code)]
use anyhow::{Context, Result};
use argh::FromArgs;

use std::{fs, path};

mod ast;
mod error;
mod lex;
mod parse;

#[derive(FromArgs)]
/// Execute a `wgpu` script.
struct WScriptArgs {
    #[argh(positional)]
    filename: path::PathBuf,
}

fn main() -> Result<()> {
    let args: WScriptArgs = argh::from_env();
    let mut cache = error::Cache::default();

    let script = fs::read_to_string(&args.filename)
        .with_context(|| format!("error reading script from {}", args.filename.display()))?;
    let source_id = cache.insert(args.filename, &script);

    let _program = match parse::parse(&script, source_id) {
        Ok(program) => program,
        Err(parse_error) => {
            parse_error.report().eprint(cache).unwrap();
            std::process::exit(1);
        }
    };

    Ok(())
}
