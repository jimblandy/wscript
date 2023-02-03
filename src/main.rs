#![doc = include_str!("../README.md")]
#![allow(dead_code)]

use wscript::error::AriadneReport as _;
use wscript::{error, parse, plan, run};

use anyhow::{Context, Result};
use argh::FromArgs;

use std::{fs, path};

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

    let program = match parse::parse(&script, source_id) {
        Ok(program) => program,
        Err(parse_error) => {
            parse_error.write(std::io::stderr(), &mut cache)?;
            std::process::exit(1);
        }
    };

    let plan = match plan::plan(&program, source_id) {
        Ok(plan) => plan,
        Err(plan_error) => {
            plan_error.write(std::io::stderr(), &mut cache)?;
            std::process::exit(1);
        }
    };

    let mut context = run::Context::create("wscript initial device")?;
    if let Err(run_error) = plan(&mut context) {
        run_error.write(std::io::stderr(), &mut cache)?;
        std::process::exit(1);
    }

    Ok(())
}
