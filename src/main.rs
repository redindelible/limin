mod lexer;
mod source;
mod ast;
mod parser;
mod error;
// mod infer;
mod key_table;
mod compiler;

use std::env;
use std::path::PathBuf;
use crate::ast::AnnotatedAST;
use crate::error::CompilerMessage;
use crate::parser::LocationData;
use crate::source::Source;
// use crate::infer::{Builtins, InferenceEngine, Program};


struct Arguments {
    file: PathBuf
}

impl Arguments {
    fn parse(args: Vec<String>) -> Result<Arguments, &'static str> {
        let file_name = args.get(1).ok_or("Expected a file argument")?;
        let file = PathBuf::from(file_name);

        Ok(Arguments { file })
    }
}

fn main() {
    let args = match Arguments::parse(env::args().collect()) {
        Ok(a) => a,
        Err(msg) => {
            eprintln!("Error: {}", msg);
            return;
        }
    };

    let source = match Source::from_file(&args.file) {
        Some(s) => s,
        None => {
            eprintln!("Error: Could not open file {}", args.file.display());
            return;
        }
    };
    let mut ast = ast::AST::new();
    let file = match parser::Parser::parse(&mut ast, &source) {
        Ok(file) => file,
        Err(errs) => {
            CompilerMessage::show_messages(errs);
            return;
        }
    };

    let a: AnnotatedAST<LocationData, ast::AST> = AnnotatedAST::wrap(ast);

    // let mut program = Program::new();
    // let mut engine = InferenceEngine::new(&mut program);
    // engine.add_file(&mut file);
}


#[cfg(test)]
mod test_parser {
    use crate::{ast, parser};
    use crate::source::Source;

    #[test]
    fn test_basic() {
        let source = Source::from_text("fn a() { }");
        let mut ast = ast::AST::new();
        let parsed = parser::Parser::parse(&mut ast, &source);
        parsed.unwrap();
    }
}
