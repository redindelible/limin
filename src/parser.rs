use crate::ast::Expr;
use crate::source::Source;
use crate::lexer::{Lexer, Token};


enum ParserError {
    // UnexpectedToken(Token)
}

type ParseResult<T> = Result<T, ParserError>;

struct Parser<'a> {
    source: &'a Source,
    tokens: Box<[Token<'a>]>,
    idx: usize
}

impl<'a> Parser<'a> {
    fn new(source: &'a Source) -> Parser<'a> {
        let tokens = Lexer::lex(source);
        Parser { source, tokens, idx: 0 }
    }

    // fn parse_expr(&mut self) -> Expr<'a> {
    //
    // }
}