/*! Check static error message generation.

These tests simply pass the given files to the parser, check that an
error occurs, and check that it matches an expected message.

If `$top` is the directory containing `wscript`'s `Cargo.toml` file,
then `$top/static-errors` is a directory containing wscript files that
should elicit errors when parsed. The expected output appears in a
file with the same name as the script, with `.rs` changed to
`.ariadne`.

*/

use wscript::error::AriadneReport as _;
use wscript::{error, parse, plan};

use anyhow::Context;
use datatest_stable::{harness, Result};

use std::{fs, path};

/// Parse a wscript file `FOO.ws`, expect an error, and check the
/// `[ariadne`]-formatted error against the corresponding
/// `FOO.ariadne` file.
///
/// [`ariadne`]: https://docs.rs/ariadne/latest/ariadne/
fn ariadne(path: &path::Path) -> Result<()> {
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

    // Parse and plan the script. One of these steps should produce an error.
    let error_message = loop {
        let program = match parse::parse(&script, source_id) {
            Ok(program) => program,
            Err(parse_error) => {
                let config = ariadne::Config::default().with_color(false);

                let mut buffer = Vec::new();
                parse_error
                    .write_with_config(&mut buffer, &mut cache, config)
                    .unwrap();
                break Some(String::from_utf8(buffer).unwrap());
            }
        };

        let (_plan, _summary) = match plan::plan(&program) {
            Ok(success) => success,
            Err(plan_error) => {
                let config = ariadne::Config::default().with_color(false);

                let mut buffer = Vec::new();
                plan_error
                    .write_with_config(&mut buffer, &mut cache, config)
                    .unwrap();
                break Some(String::from_utf8(buffer).unwrap());
            }
        };

        break None;
    };

    match error_message {
        None => panic!(
            "Unexpected pass: parsing and planning `{}` did not produce an error",
            path.display()
        ),
        Some(error_message) => {
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
    }

    Ok(())
}

harness!(ariadne, "static-errors", r".*\.ws$");
