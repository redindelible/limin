use std::ops::{ControlFlow, FromResidual, Try};
use crate::ast::{Expr, ExprNameData};
use crate::source::Source;
use crate::lexer::{Lexer, Token, TokenType};


enum ParserError<'a> {
    UnexpectedToken(Token<'a>, TokenType)
}

type ParseResult<T> = Result<T, usize>;

struct Parser<'a> {
    source: &'a Source,
    tokens: Box<[Token<'a>]>,
    idx: usize,
    errors: Vec<ParserError<'a>>
}

impl<'a> Parser<'a> {
    fn parse(source: &'a Source) -> Result<Expr<'a>, Vec<ParserError<'a>>> {
        let mut parser = Self::new(source);
        let expr = parser.parse_expr();
        match expr {
            Ok(expr) if parser.errors.is_empty() => {
                Ok(expr)
            },
            _ => {
                Err(parser.errors)
            }
        }
    }

    fn new(source: &'a Source) -> Parser<'a> {
        let tokens = Lexer::lex(source);
        Parser { source, tokens, idx: 0, errors: Vec::new() }
    }

    fn curr(&self) -> Token<'a> {
        self.tokens[self.idx]
    }

    fn advance(&mut self) -> Token<'a> {
        let ret = self.curr();
        if self.idx + 1 < self.tokens.len() {
            self.idx += 1;
        }
        ret
    }

    fn parse_expr(&mut self) -> ParseResult<Expr<'a>> {
        Ok(self.parse_terminal()?)
    }

    fn parse_terminal(&mut self) -> ParseResult<Expr<'a>> {
        match self.curr().typ {
            TokenType::IDENTIFIER => {
                let tok = self.advance();
                Ok(Expr::Name(ExprNameData { name: tok.text.to_owned(), loc: tok.loc }))
            },
            _ => {
                todo!()
            }
        }
    }
}