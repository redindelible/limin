use std::io;
use std::path::{Path, PathBuf};
use typed_arena::Arena;
use crate::source::Source;

mod lexer;
mod source;
mod ast;
mod parser;
mod hir;
mod type_check;
mod llvm;
mod codegen;

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
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let path = PathBuf::from(&args[1]);

    let mut compiler = Compiler::new();
    compiler.add_root(&path).unwrap();
}