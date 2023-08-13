use std::path::PathBuf;
use clap::Parser;
use crate::compiler::{Compiler, CompileResult};
use crate::error::Message;

mod source;
mod error;
mod util;
mod emit;
mod parsing;
mod lowering;
mod compiler;

#[derive(Parser)]
struct Args {
    #[arg(required=true)]
    file: Vec<PathBuf>,
    #[arg(short, long, required=true)]
    out: PathBuf,
    #[arg(long)]
    clang: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let mut compiler = Compiler::new(args.out, args.clang);
    for file in args.file {
        compiler.add_root(&file).unwrap();
    }
    let res = compiler.compile();
    match res {
        CompileResult::CouldNotParse(errs) => {
            for err in &errs {
                err.render_to_stderr();
            }
        }
        CompileResult::CouldNotTypeCheck(errs) => {
            for err in &errs {
                err.render_to_stderr();
            }
        }
        CompileResult::FileError(e) => eprintln!("{}", e),
        CompileResult::ArgumentError(err) => eprintln!("{}", err),
        CompileResult::ClangError => { /* then clang will write the error to stdout/stderr */ }
        CompileResult::Success => ()
    };
}


#[cfg(test)]
mod test {
    use std::path::{Path, PathBuf};
    use std::process::Command;
    use crate::{Compiler, CompileResult};

    fn run_file(file: &Path) -> std::process::Output {
        let dir = tempfile::tempdir().unwrap();
        let output_path = dir.path().join("output");
        let mut compiler = Compiler::debug(output_path.clone(), PathBuf::from("lib/rt.c"), None);
        compiler.add_root(file).unwrap();
        let res = compiler.compile();
        assert!(matches!(res, CompileResult::Success), "{:?}", res);

        let output = Command::new(&output_path).output().unwrap();
        dir.close().unwrap();
        output
    }

    #[test]
    fn test_simple() {
        let output = run_file(Path::new("test/test_files/test_simple.lmn"));
        assert_eq!(output.status.code(), Some(0));
    }

    #[test]
    fn test_struct() {
        let output = run_file(Path::new("test/test_files/test_struct.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }

    #[test]
    fn test_if_else() {
        let output = run_file(Path::new("test/test_files/test_if_else.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }

    #[test]
    fn test_generic_function() {
        let output = run_file(Path::new("test/test_files/test_generic_function.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }

    #[test]
    fn test_closures() {
        let output = run_file(Path::new("test/test_files/test_closures.lmn"));
        assert_eq!(output.status.code(), Some(20));
    }

    #[test]
    fn test_generic_struct() {
        let output = run_file(Path::new("test/test_files/test_generic_struct.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }
}