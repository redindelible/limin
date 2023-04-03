use std::io;
use std::path::{Path, PathBuf};
use clap::Parser;
use typed_arena::Arena;
use crate::error::Message;
use crate::parser::{parse_file, ParserError};
use crate::source::Source;

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
    file: Vec<PathBuf>
}

enum CompileResult<'a> {
    CouldNotParse(Vec<ParserError<'a>>),
    Success
}

struct Compiler {
    sources: Arena<Source>
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            sources: Arena::new()
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

        CompileResult::Success
    }
}

fn main() {
    let args = Args::parse();
    let mut compiler = Compiler::new();
    for file in args.file {
        compiler.add_root(&file).unwrap();
    }
    let res = compiler.compile();
    match res {
        CompileResult::CouldNotParse(errs) => {
            for err in &errs {
                err.render();
            }
        },
        CompileResult::Success => ()
    };
}