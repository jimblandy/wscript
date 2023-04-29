/*! Check runtime error message generation.

These tests pass the given file through the parser and planner,
expecting success, and then check that an expected error occurs when
the script is run.

If `$top` is the directory containing `wscript`'s `Cargo.toml` file,
then `$top/runtime-errors` is a directory containing wscript files that
should elicit errors when executed. The expected output appears in a
file with the same name as the script, with `.rs` changed to
`.ariadne`.

*/

use wscript::error::AriadneReport as _;
use wscript::{error, parse, plan, run};

use anyhow::Context;
use datatest_stable::{harness, Result};

use std::{fs, path};

/// Parse a wscript file `FOO.ws`, expect an error, and check the
/// `[ariadne`]-formatted error against the corresponding
/// `FOO.ariadne` file.
///
/// [`ariadne`]: https://docs.rs/ariadne/latest/ariadne/
fn runtime_errors(path: &path::Path) -> Result<()> {
    // Read the input script.
    let script = fs::read_to_string(path)
        .with_context(|| format!("error reading script from {}", path.display()))?;

    // Read expected error message.
    let mut ariadne_path = path.to_owned();
    ariadne_path.set_extension("ariadne");
    let expected_ariadne = fs::read_to_string(&ariadne_path).with_context(|| {
        format!(
            "error reading expected ariadne output from {}",
            ariadne_path.display(),
        )
    })?;

    // Prepare the Ariadne source cache.
    let mut cache = error::Cache::default();
    // Don't include the directory name in the error message.
    let file_name: path::PathBuf = path.file_name().unwrap().to_owned().into();
    let source_id = cache.insert(file_name, &script);

    // Parse and plan the script. This is supposed to succeed.
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

    // Run the program. This is expected to fail.
    let mut context = run::Context::create(summary, "wscript initial device")?;
    match plan(&mut context) {
        Err(error) => {
            let config = ariadne::Config::default().with_color(false);

            let mut buffer = Vec::new();
            error
                .write_with_config(&mut buffer, &mut cache, config)
                .unwrap();
            let error_message = String::from_utf8(buffer).unwrap();

            if error_message != expected_ariadne {
                println!(
                    "error message for `{}` doesn't match expectation",
                    path.display()
                );
                let diff = similar::TextDiff::from_lines(&expected_ariadne, &error_message);
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        similar::ChangeTag::Equal => ' ',
                        similar::ChangeTag::Delete => '-',
                        similar::ChangeTag::Insert => '+',
                    };
                    print!("{}{}", sign, change);
                }
                panic!(
                    "error message doesn't match expectation: {}",
                    path.display()
                );
            }
            
        }
        Ok(()) => panic!(
            "Unexpected pass: parsing and planning `{}` did not produce an error",
            path.display()
        ),
    }

    Ok(())
}

harness!(runtime_errors, "runtime-errors", r".*\.ws$");
