use crate::ast::{BinOp, Expr, Parameter, TopLevel, Type};
use crate::source::Source;
use crate::lexer::{Lexer, Token, TokenType};


#[derive(Debug)]
enum ParserError<'a> {
    UnexpectedToken(Token<'a>, Vec<TokenType>),
    ExpectedSymbol(Token<'a>, &'static str),
    CouldNotParseNumber(Token<'a>, &'static str),
    UnusedTokens(Token<'a>)
}

type ParseResult<T> = Result<T, usize>;

struct Parser<'a> {
    source: &'a Source,
    tokens: Box<[Token<'a>]>,
    idx: usize,
    errors: Vec<ParserError<'a>>
}

struct Point { idx: usize, err_len: usize }

impl<'a> Parser<'a> {
    fn parse(source: &'a Source) -> Result<Expr<'a>, Vec<ParserError<'a>>> {
        let mut parser = Self::new(source);
        let expr = parser.parse_expr();
        if parser.curr().typ != TokenType::EOF {
            parser.errors.push(ParserError::UnusedTokens(parser.curr()));
        }
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

    fn store(&self) -> Point {
        Point { idx: self.idx, err_len: self.errors.len() }
    }

    fn revert(&mut self, point: Point) -> Vec<ParserError<'a>> {
        let errors = self.errors.drain(point.err_len..).collect();
        self.idx = point.idx;
        errors
    }

    fn curr(&self) -> Token<'a> {
        self.tokens[self.idx]
    }

    fn peek(&self) -> Token<'a> {
        if self.idx + 1 >= self.idx {
            self.tokens[self.idx]
        } else {
            self.tokens[self.idx+1]
        }
    }

    fn advance(&mut self) -> Token<'a> {
        let ret = self.curr();
        if self.idx + 1 < self.tokens.len() {
            self.idx += 1;
        }
        ret
    }

    fn expect(&mut self, ttype: TokenType) -> ParseResult<Token<'a>> {
        if self.curr().typ == ttype {
            Ok(self.advance())
        } else {
            self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![ttype]));
            Err(0)
        }
    }

    fn expect_symbol(&mut self, first: TokenType, second: TokenType, name: &'static str) -> ParseResult<()> {
        if self.curr().typ == first && !self.curr().leading_ws && self.peek().typ == second {
            Ok(())
        } else {
            self.errors.push(ParserError::ExpectedSymbol(self.curr(), name));
            Err(0)
        }
    }

    fn matches_symbol(&mut self, first: TokenType, second: TokenType) -> bool {
        self.curr().typ == first && !self.curr().leading_ws && self.peek().typ == second
    }

    fn delimited_parse<T>(&mut self, left: TokenType, right: TokenType, mut each: impl FnMut(&mut Self) -> ParseResult<T>) -> ParseResult<Vec<Box<T>>> {
        self.expect(left)?;
        let mut items = Vec::new();
        while self.curr().typ != TokenType::RightParenthesis {
            let argument = each(self)?;
            items.push(Box::new(argument));
            if self.curr().typ == TokenType::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(right)?;
        Ok(items)
    }

    fn parse_function(&mut self) -> ParseResult<TopLevel<'a>> {
        self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?;
        let parameters = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, Self::parse_parameter)?;
        let return_type = if self.matches_symbol(TokenType::Minus, TokenType::GreaterThan) {
            self.expect_symbol(TokenType::Minus, TokenType::GreaterThan, "->")?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        let body = Box::new(self.parse_expr()?);
        Ok(TopLevel::Function { name: name.text.to_owned(), parameters, return_type, body })
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter<'a>> {
        let name = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let typ = Box::new(self.parse_type()?);
        Ok(Parameter { name: name.text.to_owned(), typ })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr<'a>> {
        Ok(self.parse_comparison()?)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_call()?;
        loop {
            match self.curr().typ {
                TokenType::LessThan => {
                    self.advance();
                    let right = self.parse_call()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::LessThan, right: Box::new(right) };
                },
                TokenType::GreaterThan => {
                    self.advance();
                    let right = self.parse_call()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::GreaterThan, right: Box::new(right) };
                }
                _ => { break; }
            }
        }
        Ok(left)
    }

    fn parse_call(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_terminal()?;
        loop {
            match self.curr().typ {
                TokenType::LeftParenthesis => {
                    let arguments = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, Self::parse_expr)?;
                    left = Expr::Call { callee: Box::new(left), arguments };
                }
                TokenType::LessThan => {
                    let state = self.store();
                    let result: Result<_, usize> = (|| {
                        let type_arguments = self.delimited_parse(TokenType::LessThan, TokenType::GreaterThan, Self::parse_type)?;
                        let arguments = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, Self::parse_expr)?;
                        Ok((type_arguments, arguments))
                    })();
                    match result {
                        Ok((generic_arguments, arguments)) => {
                            left = Expr::GenericCall { callee: Box::new(left), generic_arguments, arguments };
                        },
                        Err(_) => {
                            self.revert(state);
                            break;
                        }
                    }
                }
                _ => { break; }
            }
        }
        Ok(left)
    }

    fn parse_terminal(&mut self) -> ParseResult<Expr<'a>> {
        match self.curr().typ {
            TokenType::Identifier => {
                let tok = self.advance();
                Ok(Expr::Name { name: tok.text.to_owned(), loc: tok.loc })
            },
            TokenType::Integer => {
                let tok = self.advance();
                let Ok(num) = tok.text.parse::<u64>() else {
                    self.errors.push(ParserError::CouldNotParseNumber(tok, "64-bit integer"));
                    return Err(0);
                };
                Ok(Expr::Integer { number: num, loc: tok.loc })
            }
            TokenType::LeftParenthesis => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenType::RightParenthesis)?;
                Ok(expr)
            },
            _ => {
                self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![TokenType::Identifier, TokenType::LeftParenthesis]));
                Err(0)
            }
        }
    }

    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        Ok(self.parse_type_terminal()?)
    }

    fn parse_type_terminal(&mut self) -> ParseResult<Type<'a>> {
        match self.curr().typ {
            TokenType::Identifier => {
                let name = self.advance();
                Ok(Type::Name { name: name.text.to_owned(), loc: name.loc })
            },
            _ => {
                self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![TokenType::Identifier]));
                Err(0)
            }
        }
    }
}


#[cfg(test)]
mod test {
    use crate::ast::{BinOp, Expr, TopLevel, Type};
    use crate::parser::Parser;
    use crate::source::Source;

    #[test]
    fn test_expr_single_ident() {
        let s = Source::from_text("test", "hello");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        assert!(matches!(e, Expr::Name { name, ..} if name == "hello"));
    }

    #[test]
    fn test_expr_call() {
        let s = Source::from_text("test", "hello(ad)");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap_or_else(|_| panic!("{:?}", p.errors));

        let Expr::Call { callee, arguments } = e else { panic!() };
        let Expr::Name { name, ..} = callee.as_ref() else { panic!() };
        assert_eq!(name, "hello");
        assert_eq!(arguments.len(), 1);
        let Expr::Name { name, .. } = arguments[0].as_ref() else { panic!() };
        assert_eq!(name, "ad");
    }

    #[test]
    fn test_expr_less_than() {
        let s = Source::from_text("test", "hello < ad");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::BinOp { left, op: BinOp::LessThan, right } = e else { panic!() };
        let Expr::Name { name, .. } = left.as_ref() else { panic!() };
        assert_eq!(name, "hello");
        let Expr::Name { name, .. } = right.as_ref() else { panic!() };
        assert_eq!(name, "ad");
    }

    #[test]
    fn test_expr_generic() {
        let s = Source::from_text("test", "hello < ad >(a, b)");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::GenericCall { callee, generic_arguments, arguments } = e else { panic!() };
        let Expr::Name { name, ..} = callee.as_ref() else { panic!() };
        assert_eq!(name, "hello");
        assert_eq!(generic_arguments.len(), 1);
        let Type::Name { name, ..} = generic_arguments[0].as_ref() else { panic!() };
        assert_eq!(name, "ad");

        assert_eq!(arguments.len(), 2);
        let Expr::Name { name, .. } = arguments[0].as_ref() else { panic!() };
        assert_eq!(name, "a");
        let Expr::Name { name, .. } = arguments[1].as_ref() else { panic!() };
        assert_eq!(name, "b");
    }

    #[test]
    fn test_expr_generic_false() {
        let s = Source::from_text("test", "hello < ad > a");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::BinOp { left, op: BinOp::GreaterThan, right } = e else { panic!("{:?}", e) };
    }

    #[test]
    fn test_expr_generic_hard() {
        let s = Source::from_text("test", "hello(a<b, c>(d))");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::Call { callee, arguments } = e else { panic!() };
        assert_eq!(arguments.len(), 1);
    }

    #[test]
    fn test_function() {
        let s = Source::from_text("test", r"
            fn main() 0
        ");
        let mut p = Parser::new(&s);
        let top = p.parse_function().unwrap();

        let TopLevel::Function { name, parameters, return_type, body } = top else {
            panic!("{:?}", top);
        };
        assert_eq!(name, "main");
        assert_eq!(parameters, vec![]);
        assert!(return_type.is_none());
        assert!(matches!(body.as_ref(), Expr::Integer { number: 0, .. }));
    }
}