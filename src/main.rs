use std::fs::File;
use std::io;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use clap::Parser;
use semver::{Version, VersionReq};
use typed_arena::Arena;
use crate::codegen::generate_llvm;
use crate::error::Message;
use crate::lower::lower;
use crate::parser::{parse_file, ParserError};
use crate::source::Source;
use crate::type_check::{resolve_types, TypeCheckError};

mod lexer;
mod source;
mod ast;
mod parser;
mod hir;
mod type_check;
mod llvm;
mod codegen;
mod error;
mod lir;
mod lower;
mod common;

#[derive(Parser)]
struct Args {
    #[arg(required=true)]
    file: Vec<PathBuf>,
    #[arg(short, long, required=true)]
    out: PathBuf,
    #[arg(long)]
    clang: Option<PathBuf>,
}

#[derive(Debug)]
enum CompileResult<'a> {
    CouldNotParse(Vec<ParserError<'a>>),
    CouldNotTypeCheck(Vec<TypeCheckError<'a>>),
    FileError(io::Error),
    ArgumentError(String),
    ClangError,
    Success
}

struct Compiler {
    sources: Arena<Source>,
    output: PathBuf,
    clang: PathBuf,
    rt_path: PathBuf,
}

impl Compiler {
    fn new(output: PathBuf, clang: Option<PathBuf>) -> Compiler {
        let lib_path = std::env::current_exe().unwrap().with_file_name("lib");
        let rt_path = lib_path.join("rt.c");
        Compiler {
            sources: Arena::new(),
            output,
            clang: clang.unwrap_or(PathBuf::from("clang")),
            rt_path
        }
    }

    fn add_root(&self, path: &Path) -> io::Result<&Source> {
        let source = Source::from_file(path)?;
        Ok(self.sources.alloc(source))
    }

    fn compile(&mut self) -> CompileResult {
        let mut files = Vec::new();
        let mut errors = Vec::new();
        for source in self.sources.iter_mut() {
            let s = parse_file(source);
            match s {
                Ok(f) => files.push(f),
                Err(e) => errors.extend(e)
            }
        }
        if !errors.is_empty() {
            return CompileResult::CouldNotParse(errors);
        }
        let ast = ast::AST::from_files("test".into(), files);

        let resolved = match resolve_types(ast) {
            Ok(ir) => ir,
            Err(errs) => {
                return CompileResult::CouldNotTypeCheck(errs)
            }
        };

        let lowered = lower(resolved);

        let llvm = generate_llvm(lowered);

        let Some(name) = self.output.file_name() else {
            return CompileResult::ArgumentError("Output file path is not valid".into());
        };

        let build_dir = self.output.with_file_name("build");
        match std::fs::create_dir_all(&build_dir) {
            Ok(_) => (),
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        let ll_file = build_dir.join(name).with_extension("ll");
        let mut ll_file_handle = match File::create(&ll_file) {
            Ok(f) => f,
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        match ll_file_handle.write_all(llvm.emit().as_bytes()) {
            Ok(()) => (),
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        drop(ll_file_handle);

        let output = String::from_utf8(match Command::new(&self.clang).arg("--version").output() {
            Ok(o) => o,
            Err(e) => {
                return CompileResult::FileError(e);
            }
        }.stdout).unwrap();
        let output: Vec<&str> = output.lines().collect();
        let Some(first) = output.get(0) else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        let Some(version) = first.split(' ').skip(2).next() else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        let req = VersionReq::parse(">=15.0.0").unwrap();
        let Ok(actual) = Version::parse(version) else {
            return CompileResult::ArgumentError(format!("Could not parse clang version information (found {}).", version));
        };
        if !req.matches(&actual) {
            return CompileResult::ArgumentError(format!("Clang version does not meet requirements (found version {}, required {}).", actual, req));
        }

        let output = match
            Command::new(&self.clang)
                .arg(&ll_file)
                .arg(&self.rt_path)
                .arg("-o")
                .arg(&self.output)
                .arg("-Wno-override-module")
                .status() {
            Ok(output) => output,
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        if !output.success() {
            return CompileResult::ClangError;
        }

        CompileResult::Success
    }
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
                err.render();
            }
        }
        CompileResult::CouldNotTypeCheck(errs) => {
            for err in &errs {
                err.render();
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
        let mut compiler = Compiler::debug(output_path.clone(), PathBuf::from("lib/rt.c"));
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

}