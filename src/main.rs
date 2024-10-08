#![forbid(unused_must_use)]

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
    file: PathBuf,
    #[arg(short, long, required=true)]
    out: PathBuf,
    #[arg(long)]
    clang: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let mut compiler = Compiler::new(args.out, args.clang);
    compiler.set_root(args.file);
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
        let mut compiler = Compiler::debug(output_path.clone(), dunce::canonicalize(PathBuf::from("lib")).unwrap(), None);
        compiler.set_root(file.to_owned());
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
    fn test_divergent_if_else() {
        let output = run_file(Path::new("test/test_files/test_divergent_if_else.lmn"));
        assert_eq!(output.status.code(), Some(1));
    }

    #[test]
    fn test_generic_function() {
        let output = run_file(Path::new("test/test_files/test_generic_function.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }

    #[test]
    fn test_functions() {
        let output = run_file(Path::new("test/test_files/test_functions.lmn"));
        assert_eq!(output.status.code(), Some(13));
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

    #[test]
    fn test_new() {
        let output = run_file(Path::new("test/test_files/test_new.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }

    #[test]
    fn test_inherent_impl() {
        let output = run_file(Path::new("test/test_files/test_inherent_impl.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }

    #[test]
    fn test_trait_impl() {
        let output = run_file(Path::new("test/test_files/test_trait_impl.lmn"));
        assert_eq!(output.status.code(), Some(24));
    }

    #[test]
    fn test_foreign_impl() {
        let output = run_file(Path::new("test/test_files/test_foreign_impl.lmn"));
        assert_eq!(output.status.code(), Some(4));
    }

    #[test]
    fn test_impl_generic_variant() {
        let output = run_file(Path::new("test/test_files/test_impl_generic_variant.lmn"));
        assert_eq!(output.status.code(), Some(12));
    }

    #[test]
    fn test_impl_generic() {
        let output = run_file(Path::new("test/test_files/test_impl_generic.lmn"));
        assert_eq!(output.status.code(), Some(12));
    }

    #[test]
    fn test_nonlocal_infer_function() {
        let output = run_file(Path::new("test/test_files/test_nonlocal_infer_function.lmn"));
        assert_eq!(output.status.code(), Some(4));
    }
    
    #[test]
    fn test_nonlocal_infer_function_explicit() {
        let output = run_file(Path::new("test/test_files/test_nonlocal_infer_function_explicit.lmn"));
        assert_eq!(output.status.code(), Some(256));
    }

    #[test]
    fn test_mod() {
        let output = run_file(Path::new("test/test_files/test_mod.lmn"));
        assert_eq!(output.status.code(), Some(32));
    }
}