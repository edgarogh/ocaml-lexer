use ocaml_lexer::parse_tokens;
use rayon::prelude::*;
use std::ffi::OsStr;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use walkdir::WalkDir;

fn main() {
    let mut total_tokens = AtomicUsize::new(0);
    let mut total_files = AtomicUsize::new(0);
    let mut total_successes = AtomicUsize::new(0);

    let entries = WalkDir::new(".")
        .into_iter()
        .map(Result::unwrap)
        .collect::<Vec<_>>();

    entries
        .into_par_iter()
        .filter(|entry| entry.file_type().is_file())
        .filter_map(|entry| {
            let path: &Path = entry.path().as_ref();
            if path.extension() == Some(OsStr::new("ml")) {
                Some((path.to_owned(), std::fs::read_to_string(path).unwrap()))
            } else {
                None
            }
        })
        .map(|(path, source)| {
            total_files.fetch_add(1, Ordering::SeqCst);

            let result = check_source(&source);
            match result {
                Ok(tokens) => {
                    total_tokens.fetch_add(tokens, Ordering::SeqCst);
                    total_successes.fetch_add(1, Ordering::SeqCst);
                    (path, "[OK]".into())
                }
                Err(nom::Err::Error(err)) => {
                    let input = err.input;
                    if input.len() > 20 {
                        (path, format!("[Err: {:?}…]", &err.input[..20]))
                    } else {
                        (path, format!("[Err: {:?}]", input))
                    }
                }
                Err(err) => (path, format!("[Failure: {}]", err)),
            }
        })
        .for_each(|(path, message)| {
            eprintln!("parsing: {} {}", path.display(), message);
        });

    eprintln!(
        "done. Successes: {}/{}. Total parsed tokens: {}",
        total_successes.get_mut(),
        total_files.get_mut(),
        total_tokens.get_mut(),
    );
}

fn check_source(source: &str) -> Result<usize, nom::Err<nom::error::Error<&str>>> {
    let (_, tokens) = parse_tokens(source)?;
    Ok(tokens.len())
}
