/*! Try running wscripts.

If `$top` is the directory containing `wscript`'s `Cargo.toml` file,
then `$top/run` is a directory containing wscript files that should
run and exit successfully.

*/

use wscript::error::AriadneReport as _;
use wscript::{error, parse, plan, run};

use anyhow::Context;
use datatest_stable::{harness, Result};

use std::{fs, path};

/// Run a given wscript, and check that it exited without error.
fn run(path: &path::Path) -> Result<()> {
    // Read the input script.
    let script = fs::read_to_string(path)
        .with_context(|| format!("error reading script from {}", path.display()))?;

    // Prepare the Ariadne source cache.
    let mut cache = error::Cache::default();
    // Don't include the directory name in the error message.
    let file_name: path::PathBuf = path.file_name().unwrap().to_owned().into();
    let source_id = cache.insert(file_name, &script);

    let program = match parse::parse(&script, source_id) {
        Ok(program) => program,
        Err(parse_error) => {
            parse_error.write(std::io::stderr(), &mut cache)?;
            std::process::exit(1);
        }
    };

    let (plan, summary) = match plan::plan(&program) {
        Ok(plan) => plan,
        Err(plan_error) => {
            plan_error.write(std::io::stderr(), &mut cache)?;
            std::process::exit(1);
        }
    };

    let mut context = run::Context::create(summary, "wscript initial device")?;
    if let Err(run_error) = plan(&mut context) {
        run_error.write(std::io::stderr(), &mut cache)?;
        std::process::exit(1);
    }

    Ok(())
}

harness!(run, "run", r".*\.ws$");
