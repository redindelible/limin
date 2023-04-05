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

#[derive(Parser)]
struct Args {
    #[arg(required=true)]
    file: Vec<PathBuf>,
    #[arg(short, long, required=true)]
    out: PathBuf
}

enum CompileResult<'a> {
    CouldNotParse(Vec<ParserError<'a>>),
    CouldNotTypeCheck(Vec<TypeCheckError<'a>>),
    FileError(Option<io::Error>),
    ArgumentError(String),
    ClangError,
    Success
}

struct Compiler {
    sources: Arena<Source>,
    output: PathBuf
}

impl Compiler {
    fn new(output: PathBuf) -> Compiler {
        Compiler {
            sources: Arena::new(),
            output
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

        let llvm = generate_llvm(resolved);

        let Some(name) = self.output.file_name() else {
            return CompileResult::ArgumentError("Output file path is not valid".into());
        };

        let build_dir = self.output.with_file_name("build");
        match std::fs::create_dir_all(&build_dir) {
            Ok(_) => (),
            Err(e) => {
                return CompileResult::FileError(Some(e));
            }
        };
        let ll_file = build_dir.join(name).with_extension("ll");
        let mut ll_file_handle = match File::create(&ll_file) {
            Ok(f) => f,
            Err(e) => {
                return CompileResult::FileError(Some(e));
            }
        };
        match ll_file_handle.write_all(llvm.emit().as_bytes()) {
            Ok(()) => (),
            Err(e) => {
                return CompileResult::FileError(Some(e));
            }
        };
        drop(ll_file_handle);

        let output = String::from_utf8(match Command::new("clang").arg("--version").output() {
            Ok(o) => o,
            Err(e) => {
                return CompileResult::FileError(Some(e));
            }
        }.stdout).unwrap();
        let output: Vec<&str> = output.lines().collect();
        let Some(first) = output.get(0) else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        let Some(version) = first.splitn(3, ' ').last() else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        let req = VersionReq::parse(">=15.0.0").unwrap();
        let Ok(actual) = Version::parse(version) else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        if !req.matches(&actual) {
            return CompileResult::ArgumentError(format!("Clang version does not meet requirements (found version {}, required {}).", actual, req));
        }

        let output = match Command::new("clang").arg(&ll_file).arg("-o").arg(&self.output).status() {
            Ok(output) => output,
            Err(e) => {
                return CompileResult::FileError(Some(e));
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
    let mut compiler = Compiler::new(args.out);
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
        CompileResult::FileError(e) => {
            eprintln!("{:?}", e);
        }
        CompileResult::ArgumentError(err) => {
            eprintln!("{}", err);
        }
        CompileResult::ClangError => {

        }
        CompileResult::Success => ()
    };
}