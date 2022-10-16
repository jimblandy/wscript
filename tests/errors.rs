/*! Check error message generation.

If `$top` is the directory containing `wscript`'s `Cargo.toml` file,
then `$top/errors` is a directory containing wscript files that should
elicit errors when parsed. The files contain specially formatted
comments containing the expected output. These tests simply pass the
given files to the parser, check that an error occurs, and check that
it matches the expectation.

*/

use anyhow::Context;
use datatest_stable::{harness, Result};

use std::{fs, path};

/// Parse a wscript file `FOO.ws`, expect an error, and check the
/// `[ariadne`]-formatted error against the corresponding
/// `FOO.ariadne` file.
///
/// [`ariadne`]: https://docs.rs/ariadne/latest/ariadne/
fn ariadne(path: &path::Path) -> Result<()> {
    let mut cache = wscript::error::Cache::default();

    let script = fs::read_to_string(path)
        .with_context(|| format!("error reading script from {}", path.display()))?;

    let mut ariadne_path = path.to_owned();
    ariadne_path.set_extension("ariadne");
    let expected_ariadne = fs::read_to_string(&ariadne_path).with_context(|| {
        format!(
            "error reading expected ariadne output from {}",
            ariadne_path.display(),
        )
    })?;

    // Don't include the directory name in the error message.
    let file_name: path::PathBuf = path.file_name().unwrap().to_owned().into();
    let source_id = cache.insert(file_name, &script);

    match wscript::parse::parse(&script, source_id) {
        Ok(_) => panic!(
            "Unexpected pass: parsing `{}` did not produce an error",
            path.display()
        ),
        Err(parse_error) => {
            let config = ariadne::Config::default().with_color(false);

            let mut buffer = Vec::new();
            parse_error
                .report_with_config(config)
                .write(cache, &mut buffer)
                .unwrap();
            let report = String::from_utf8(buffer).unwrap();

            if report != expected_ariadne {
                println!(
                    "error message for `{}` doesn't match expectation",
                    path.display()
                );
                let diff = similar::TextDiff::from_lines(&expected_ariadne, &report);
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

harness!(ariadne, "errors", r".*\.ws$");
