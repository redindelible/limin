use std::fmt;
use std::path::PathBuf;
use crate::parsing::ast::*;
use crate::error::Message;
use crate::source::{Source, HasLoc, Location};
use crate::parsing::lexer::{Lexer, Token, TokenType};
use crate::util::map_join;


pub fn parse_file(source: &Source) -> Result<File, Vec<ParserError>> {
    Parser::parse_file(source)
}

#[derive(Debug)]
pub enum ParserError<'a> {
    UnexpectedToken(Token<'a>, Vec<TokenType>),
    ExpectedSymbol(Token<'a>, &'static str),
    CouldNotParseNumber(Token<'a>, &'static str)
}

impl<'a> Message for ParserError<'a> {
    fn write_into<W: fmt::Write>(&self, to: &mut W) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(token, expected) => {
                if expected.len() == 1 {
                    writeln!(to, "Error: Unexpected token. Got {}, but expected {}.", token.typ.name(), expected[0].name())?;
                } else if expected.len() == 2 {
                    writeln!(to, "Error: Unexpected token. Got {}, but expected {} or {}.", token.typ.name(), expected[0].name(), expected[1].name())?;
                } else {
                    writeln!(to, "Error: Unexpected token. Got {}, but expected any of {}.", token.typ.name(), map_join(expected, |e| e.name()))?;
                }
                Self::show_location(&token.loc, to)
            },
            ParserError::ExpectedSymbol(token, expected) => {
                writeln!(to, "Error: Unexpected token. Got {}, but expected the symbol {}.", token.typ.name(), expected)?;
                Self::show_location(&token.loc, to)
            },
            ParserError::CouldNotParseNumber(token, as_a) => {
                writeln!(to, "Error: Could not parse integer literal into a {}.", as_a)?;
                Self::show_location(&token.loc, to)
            }
        }
    }
}

type ParseResult<T> = Result<T, usize>;

struct Parser<'a> {
    // source: &'a Source,
    tokens: Box<[Token<'a>]>,
    idx: usize,
    errors: Vec<ParserError<'a>>
}

struct Point { idx: usize, err_len: usize }

impl<'a> Parser<'a> {
    fn parse_file(source: &'a Source) -> Result<File<'a>, Vec<ParserError<'a>>> {
        let mut parser = Self::new(source);

        let mut top_levels = Vec::new();
        while parser.curr().typ != TokenType::EOF {
            let top_level = match parser.parse_top_level() {
                Ok(n) => n,
                Err(_) => return Err(parser.errors)
            };
            top_levels.push(top_level);
        };
        let file = File { path: PathBuf::from(source.path.clone()),  top_levels };
        Ok(file)
    }

    fn new(source: &'a Source) -> Parser<'a> {
        let tokens = Lexer::lex(source);
        Parser { tokens, idx: 0, errors: Vec::new() }
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
        if self.idx + 1 >= self.tokens.len() {
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
        if self.curr().typ == first && !self.peek().leading_ws && self.peek().typ == second {
            self.advance(); self.advance();
            Ok(())
        } else {
            self.errors.push(ParserError::ExpectedSymbol(self.curr(), name));
            Err(0)
        }
    }

    fn matches_symbol(&mut self, first: TokenType, second: TokenType) -> bool {
        self.curr().typ == first && !self.peek().leading_ws && self.peek().typ == second
    }

    fn delimited_parse<T>(&mut self, left: TokenType, right: TokenType, mut each: impl FnMut(&mut Self) -> ParseResult<T>) -> ParseResult<(Vec<T>, Location<'a>)> {
        let start = self.expect(left)?;
        let mut items = Vec::new();
        while self.curr().typ != right {
            let argument = each(self)?;
            items.push(argument);
            if self.curr().typ == TokenType::Comma {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.expect(right)?;
        Ok((items, start.loc + end.loc))
    }

    fn parse_top_level(&mut self) -> ParseResult<TopLevel<'a>> {
        match self.curr().typ {
            TokenType::Struct => self.parse_struct(),
            TokenType::Fn => self.parse_function(),
            TokenType::Impl => self.parse_impl(),
            TokenType::Trait => self.parse_trait(),
            _ => {
                self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![TokenType::Struct, TokenType::Fn]));
                Err(0)
            }
        }
    }

    fn parse_trait(&mut self) -> ParseResult<TopLevel<'a>> {
        let start = self.expect(TokenType::Trait)?;
        let name = self.expect(TokenType::Identifier)?;

        let type_params = if self.curr().typ == TokenType::LeftAngle {
            let (type_parameters, _) = self.delimited_parse(TokenType::LeftAngle, TokenType::RightAngle, Self::parse_type_parameter)?;
            type_parameters
        } else {
            vec![]
        };

        let mut methods = Vec::new();
        self.expect(TokenType::LeftBrace)?;
        while self.curr().typ != TokenType::RightBrace {
            let item = self.parse_method_prototype()?;
            methods.push(item);
        }
        self.expect(TokenType::RightBrace)?;

        Ok(TopLevel::Trait(Trait {
            name: name.text.to_owned(),
            type_parameters: type_params,
            method_prototypes: methods,
            loc: start.loc + name.loc
        }))
    }

    fn parse_method_prototype(&mut self) -> ParseResult<MethodPrototype<'a>> {
        let start = self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?;

        self.expect(TokenType::LeftParenthesis)?;
        let mut maybe_self: Option<(String, Location<'a>)> = None;
        let mut parameters: Vec<Parameter<'a>> = Vec::new();
        while self.curr().typ != TokenType::RightParenthesis {
            let name = self.expect(TokenType::Identifier)?;
            if self.curr().typ != TokenType::Colon && parameters.len() == 0 && maybe_self.is_none() {
                maybe_self = Some((name.text.to_owned(), name.loc));
            } else {
                self.expect(TokenType::Colon)?;
                let typ = Box::new(self.parse_type()?);
                parameters.push(Parameter { name: name.text.to_owned(), loc: name.loc + typ.loc(), typ });
            }
            if self.curr().typ == TokenType::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(TokenType::RightParenthesis)?;

        let return_type = if self.matches_symbol(TokenType::Minus, TokenType::RightAngle) {
            self.expect_symbol(TokenType::Minus, TokenType::RightAngle, "->")?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        self.expect(TokenType::Semicolon)?;

        Ok(MethodPrototype {
            name: name.text.to_owned(),
            has_self: maybe_self.is_some(),
            parameters,
            return_type,
            loc: start.loc + name.loc
        })
    }

    fn parse_struct(&mut self) -> ParseResult<TopLevel<'a>> {
        let start = self.expect(TokenType::Struct)?;
        let name = self.expect(TokenType::Identifier)?;

        let type_params = if self.curr().typ == TokenType::LeftAngle {
            let (type_parameters, _) = self.delimited_parse(TokenType::LeftAngle, TokenType::RightAngle, Self::parse_type_parameter)?;
            type_parameters
        } else {
            vec![]
        };

        let super_struct: Option<(String, Location)> = if self.curr().typ == TokenType::LeftParenthesis {
            let start = self.expect(TokenType::LeftParenthesis)?;
            let name = self.expect(TokenType::Identifier)?.text.to_owned();
            let end = self.expect(TokenType::RightParenthesis)?;
            Some((name, start.loc + end.loc))
        } else {
            None
        };

        let mut items = Vec::new();
        self.expect(TokenType::LeftBrace)?;
        while self.curr().typ != TokenType::RightBrace {
            let item = self.parse_struct_item()?;
            items.push(item);
        }
        self.expect(TokenType::RightBrace)?;

        let loc = name.loc + start.loc;

        Ok(TopLevel::Struct(Struct { name: name.text.to_owned(), type_params, super_struct, items, loc }))
    }

    fn parse_struct_item(&mut self) -> ParseResult<StructItem<'a>> {
        if self.curr().typ == TokenType::Identifier {
            self.parse_struct_field()
        } else {
            self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![TokenType::Identifier, TokenType::Impl]));
            Err(0)
        }
    }

    fn parse_struct_field(&mut self) -> ParseResult<StructItem<'a>> {
        let name = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let typ = Box::new(self.parse_type()?);
        let end = self.expect(TokenType::Semicolon)?;
        Ok(StructItem::Field { name: name.text.to_owned(), typ, loc: name.loc + end.loc })
    }

    fn parse_impl(&mut self) -> ParseResult<TopLevel<'a>> {
        let start = self.expect(TokenType::Impl)?;

        let mut type_parameters = Vec::new();

        let trait_ = if self.curr().typ == TokenType::Identifier {
            let trait_name = self.expect(TokenType::Identifier)?;
            Some(trait_name.text.to_owned())
        } else {
            None
        };

        self.expect(TokenType::For)?;
        let for_type = self.parse_type()?;
        self.expect(TokenType::LeftBrace)?;
        let mut methods: Vec<Method<'a>> = Vec::new();
        while self.curr().typ != TokenType::RightBrace {
            methods.push(self.parse_method()?);
        }
        let end = self.expect(TokenType::RightBrace)?;
        let loc = start.loc + end.loc;
        Ok(TopLevel::Impl(Impl { type_parameters, trait_, for_type, methods, loc }))
    }

    fn parse_method(&mut self) -> ParseResult<Method<'a>> {
        self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?;
        let type_parameters = if self.curr().typ == TokenType::LeftAngle {
            let (type_parameters, _) = self.delimited_parse(TokenType::LeftAngle, TokenType::RightAngle, Self::parse_type_parameter)?;
            type_parameters
        } else {
            vec![]
        };

        let start = self.expect(TokenType::LeftParenthesis)?;
        let mut maybe_self: Option<(String, Location<'a>)> = None;
        let mut parameters: Vec<Parameter<'a>> = Vec::new();
        while self.curr().typ != TokenType::RightParenthesis {
            let name = self.expect(TokenType::Identifier)?;
            if self.curr().typ != TokenType::Colon && parameters.len() == 0 && maybe_self.is_none() {
                maybe_self = Some((name.text.to_owned(), name.loc));
            } else {
                self.expect(TokenType::Colon)?;
                let typ = Box::new(self.parse_type()?);
                parameters.push(Parameter { name: name.text.to_owned(), loc: name.loc + typ.loc(), typ });
            }
            if self.curr().typ == TokenType::Comma {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.expect(TokenType::RightParenthesis)?;
        let param_loc = start.loc + end.loc;

        let return_type = if self.matches_symbol(TokenType::Minus, TokenType::RightAngle) {
            self.expect_symbol(TokenType::Minus, TokenType::RightAngle, "->")?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        let body = self.parse_block()?;
        let loc = start.loc + return_type.as_ref().map_or(param_loc, |t| t.loc());
        Ok(Method { name: name.text.to_owned(), type_parameters, maybe_self, parameters, return_type, body, loc })
    }

    fn parse_function(&mut self) -> ParseResult<TopLevel<'a>> {
        let start = self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?;
        let type_parameters = if self.curr().typ == TokenType::LeftAngle {
            let (type_parameters, _) = self.delimited_parse(TokenType::LeftAngle, TokenType::RightAngle, Self::parse_type_parameter)?;
            type_parameters
        } else {
            vec![]
        };
        let (parameters, param_loc) = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, Self::parse_parameter)?;
        let return_type = if self.matches_symbol(TokenType::Minus, TokenType::RightAngle) {
            self.expect_symbol(TokenType::Minus, TokenType::RightAngle, "->")?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        let body = self.parse_block()?;
        let loc = start.loc + return_type.as_ref().map_or(param_loc, |t| t.loc());
        Ok(TopLevel::Function( Function { name: name.text.to_owned(), type_parameters, parameters, return_type, body, loc }))
    }

    fn parse_type_parameter(&mut self) -> ParseResult<TypeParameter<'a>> {
        let name = self.expect(TokenType::Identifier)?;
        let bound = if self.curr().typ == TokenType::Colon {
            self.expect(TokenType::Colon)?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        Ok(TypeParameter { name: name.text.to_owned(), bound, loc: name.loc })
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter<'a>> {
        let name = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let typ = Box::new(self.parse_type()?);
        Ok(Parameter { name: name.text.to_owned(), loc: name.loc + typ.loc(), typ })
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        match self.curr().typ {
            TokenType::Return => self.parse_return(),
            TokenType::Let => self.parse_decl(),
            _ => self.parse_expr_stmt()
        }
    }

    fn parse_return(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.expect(TokenType::Return)?;
        let value = Box::new(self.parse_expr()?);
        let loc = start.loc + value.loc();
        Ok(Stmt::Return { value, loc })
    }

    fn parse_decl(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.expect(TokenType::Let)?;
        let name = self.expect(TokenType::Identifier)?.text.to_owned();
        let typ = if self.curr().typ == TokenType::Colon {
            self.expect(TokenType::Colon)?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        self.expect(TokenType::Equal)?;
        let value = Box::new(self.parse_expr()?);
        let loc = start.loc + value.loc();
        Ok(Stmt::Decl { name, typ, value, loc })
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let expr = Box::new(self.parse_expr()?);
        let loc = expr.loc();
        Ok(Stmt::Expr { expr, loc })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr<'a>> {
        self.parse_top_expr()
    }

    fn parse_top_expr(&mut self) -> ParseResult<Expr<'a>> {
        if self.curr().typ == TokenType::If {
            let start = self.advance();
            let cond = Box::new(self.parse_expr()?);
            let then_do = Box::new(self.parse_expr()?);
            self.expect(TokenType::Else)?;
            let else_do = Box::new(self.parse_expr()?);
            let loc = start.loc + else_do.loc();
            Ok(Expr::IfElse { cond, then_do, else_do, loc })
        } else if self.curr().typ == TokenType::Bar {
            let (parameters, start) = self.delimited_parse(TokenType::Bar, TokenType::Bar, Self::parse_closure_parameter)?;
            let body = Box::new(self.parse_expr()?);
            let loc = start + body.loc();
            Ok(Expr::Closure { parameters, body, loc})
        } else {
            self.parse_comparison()
        }
    }

    fn parse_closure_parameter(&mut self) -> ParseResult<ClosureParameter<'a>> {
        let name = self.expect(TokenType::Identifier)?;
        let (typ, loc) = if self.curr().typ == TokenType::Colon {
            self.advance();
            let typ = self.parse_type()?;
            let loc = name.loc + typ.loc();
            (Some(typ), loc)
        } else {
            (None, name.loc)
        };
        Ok(ClosureParameter { name: name.text.into(), typ, loc })
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_new()?;
        loop {
            match self.curr().typ {
                TokenType::LeftAngle => {
                    self.advance();
                    let right = self.parse_new()?;
                    let loc = left.loc() + right.loc();
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::LessThan, right: Box::new(right), loc };
                },
                TokenType::RightAngle => {
                    self.advance();
                    let right = self.parse_new()?;
                    let loc = left.loc() + right.loc();
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::GreaterThan, right: Box::new(right), loc };
                }
                _ => { break; }
            }
        }
        Ok(left)
    }

    fn parse_new(&mut self) -> ParseResult<Expr<'a>> {
        if self.curr().typ == TokenType::New {
            let start = self.expect(TokenType::New)?;
            let value = Box::new(self.parse_new()?);
            Ok(Expr::New { loc: start.loc + value.loc(), value })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_terminal()?;
        loop {
            match self.curr().typ {
                TokenType::Period => {
                    self.expect(TokenType::Period)?;
                    let attr = self.expect(TokenType::Identifier)?;
                    let loc = attr.loc + left.loc();
                    left = Expr::GetAttr { obj: Box::new(left), attr: attr.text.into(), loc };
                }
                TokenType::LeftParenthesis => {
                    let (arguments, args_loc) = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, Self::parse_expr)?;
                    let loc = left.loc() + args_loc;
                    if let Expr::GetAttr { obj, attr, .. } = left {
                        left = Expr::MethodCall { object: obj, method: attr, arguments, loc };
                    } else {
                        left = Expr::Call { callee: Box::new(left), arguments, loc };
                    }
                }
                TokenType::LeftAngle => {
                    let state = self.store();
                    let result: Result<_, usize> = (|| {
                        let type_arguments = self.delimited_parse(TokenType::LeftAngle, TokenType::RightAngle, Self::parse_type)?;
                        let arguments = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, Self::parse_expr)?;
                        Ok((type_arguments, arguments))
                    })();
                    match result {
                        Ok(((generic_arguments, _), (arguments, args_loc))) => {
                            let loc = left.loc() + args_loc;
                            left = Expr::GenericCall { callee: Box::new(left), generic_arguments, arguments, loc};
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

    fn parse_block(&mut self) -> ParseResult<Block<'a>> {
        let start = self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        let mut trailing_expr = None;
        while self.curr().typ != TokenType::RightBrace {
            let stmt = Box::new(self.parse_stmt()?);
            if self.curr().typ == TokenType::Semicolon {
                stmts.push(stmt);
                self.advance();
            } else {
                if let Stmt::Expr { expr, .. } = *stmt {
                    trailing_expr = Some(expr);
                } else {
                    self.expect(TokenType::Semicolon)?;  // will error
                }
                break;
            }
        }
        let end = self.expect(TokenType::RightBrace)?;
        Ok(Block { stmts, trailing_expr, loc: start.loc + end.loc })
    }

    fn parse_terminal(&mut self) -> ParseResult<Expr<'a>> {
        match self.curr().typ {
            TokenType::Identifier => {
                let tok = self.advance();

                if self.curr().typ == TokenType::LeftBrace {
                    let (fields, loc) = self.delimited_parse(TokenType::LeftBrace, TokenType::RightBrace, |this| {
                        let name = this.expect(TokenType::Identifier)?;
                        this.expect(TokenType::Colon)?;
                        let argument = this.parse_expr()?;
                        let loc = name.loc + argument.loc();
                        Ok(StructArgument { name: name.text.to_owned(), argument: Box::new(argument), name_loc: loc })
                    })?;

                    Ok(Expr::CreateStruct {
                        struct_: tok.text.to_owned(),
                        type_args: None,
                        arguments: fields,
                        loc: tok.loc + loc
                    })
                } else {
                    Ok(Expr::Name { name: tok.text.to_owned(), loc: tok.loc })
                }
            },
            TokenType::Integer => {
                let tok = self.advance();
                let Ok(num) = tok.text.parse::<u64>() else {
                    self.errors.push(ParserError::CouldNotParseNumber(tok, "64-bit integer"));
                    return Err(0);
                };
                Ok(Expr::Integer { number: num, loc: tok.loc })
            },
            TokenType::True => {
                let tok = self.advance();
                Ok(Expr::Bool { value: true, loc: tok.loc })
            }
            TokenType::False => {
                let tok = self.advance();
                Ok(Expr::Bool { value: false, loc: tok.loc })
            }
            TokenType::LeftParenthesis => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenType::RightParenthesis)?;
                Ok(expr)
            },
            TokenType::LeftBrace => {
                Ok(Expr::Block(self.parse_block()?))
            }
            _ => {
                self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![TokenType::Identifier, TokenType::LeftParenthesis]));
                Err(0)
            }
        }
    }

    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        if self.curr().typ == TokenType::Star {
            let start = self.expect(TokenType::Star)?;
            let right = Box::new(self.parse_type()?);
            Ok(Type::Gc { loc: start.loc + right.loc(), ty: right })
        } else {
            self.parse_type_terminal()
        }
    }

    fn parse_type_terminal(&mut self) -> ParseResult<Type<'a>> {
        match self.curr().typ {
            TokenType::Identifier => {
                let name = self.advance();
                if self.curr().typ == TokenType::LeftAngle {
                    let (type_args, loc) = self.delimited_parse(TokenType::LeftAngle, TokenType::RightAngle, Self::parse_type)?;
                    let loc = loc + name.loc;
                    Ok(Type::Generic { name: name.text.to_owned(), type_args, loc })
                } else {
                    Ok(Type::Name { name: name.text.to_owned(), loc: name.loc })
                }
            },
            TokenType::LeftParenthesis => {
                let (parameters, start) = self.delimited_parse(TokenType::LeftParenthesis, TokenType::RightParenthesis, |this| this.parse_type())?;
                self.expect_symbol(TokenType::Minus, TokenType::RightAngle, "'->'")?;
                let ret = Box::new(self.parse_type()?);
                let loc = start + ret.loc();
                Ok(Type::Function { parameters, ret, loc })
            }
            _ => {
                self.errors.push(ParserError::UnexpectedToken(self.curr(), vec![TokenType::Identifier]));
                Err(0)
            }
        }
    }
}


#[cfg(test)]
mod test {
    use crate::error::Message;
    use crate::parsing::ast::{BinOp, Expr, Function, TopLevel, Type};
    use crate::parsing::parser::{parse_file, Parser};
    use crate::source::Source;

    fn source(name: &str, text: &str) -> Source {
        Source::from_text(name, text.to_owned())
    }

    #[test]
    fn test_expr_single_ident() {
        let s = source("test", "hello");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        assert!(matches!(e, Expr::Name { name, ..} if name == "hello"));
    }

    #[test]
    fn test_expr_call() {
        let s = source("test", "hello(ad)");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap_or_else(|_| panic!("{:?}", p.errors));

        let Expr::Call { callee, arguments, .. } = e else { panic!() };
        let Expr::Name { name, ..} = callee.as_ref() else { panic!() };
        assert_eq!(name, "hello");
        assert_eq!(arguments.len(), 1);
        let Expr::Name { name, .. } = &arguments[0] else { panic!() };
        assert_eq!(name, "ad");
    }

    #[test]
    fn test_expr_less_than() {
        let s = source("test", "hello < ad");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::BinOp { left, op: BinOp::LessThan, right, .. } = e else { panic!() };
        let Expr::Name { name, .. } = left.as_ref() else { panic!() };
        assert_eq!(name, "hello");
        let Expr::Name { name, .. } = right.as_ref() else { panic!() };
        assert_eq!(name, "ad");
    }

    #[test]
    fn test_expr_generic() {
        let s = source("test", "hello < ad >(a, b)");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::GenericCall { callee, generic_arguments, arguments, .. } = e else { panic!() };
        let Expr::Name { name, ..} = callee.as_ref() else { panic!() };
        assert_eq!(name, "hello");
        assert_eq!(generic_arguments.len(), 1);
        let Type::Name { name, ..} = &generic_arguments[0] else { panic!() };
        assert_eq!(name, "ad");

        assert_eq!(arguments.len(), 2);
        let Expr::Name { name, .. } = &arguments[0] else { panic!() };
        assert_eq!(name, "a");
        let Expr::Name { name, .. } = &arguments[1] else { panic!() };
        assert_eq!(name, "b");
    }

    #[test]
    fn test_expr_generic_false() {
        let s = source("test", "hello < ad > a");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::BinOp { op: BinOp::GreaterThan, .. } = e else { panic!("{:?}", e) };
    }

    #[test]
    fn test_expr_generic_hard() {
        let s = source("test", "hello(a<b, c>(d))");
        let mut p = Parser::new(&s);
        let e = p.parse_expr().unwrap();

        let Expr::Call { arguments, .. } = e else { panic!() };
        assert_eq!(arguments.len(), 1);
    }

    #[test]
    fn test_function() {
        let s = source("test", r"
            fn main() {
                let a: int = 0;
                return 0;
            }
        ");
        let mut p = Parser::new(&s);
        let top = p.parse_function().unwrap_or_else(|_| {
            panic!("{:?}", p.errors)
        });

        let TopLevel::Function(Function { name, parameters, return_type, .. }) = top else {
            panic!("{:?}", top);
        };
        assert_eq!(name, "main");
        assert_eq!(parameters, vec![]);
        assert!(return_type.is_none());
    }

    #[test]
    fn test_function_return() {
        let s = source("test", r"
            fn main() -> int {
                let a: int = 0;
                return 0;
            }
        ");
        let mut p = Parser::new(&s);
        let top = p.parse_function().unwrap();

        let TopLevel::Function(Function { name, parameters, return_type, .. }) = top else { panic!("{top:?}") };
        assert_eq!(name, "main");
        assert_eq!(parameters, vec![]);
        assert!(return_type.is_some());
    }

    #[test]
    fn test_trailing_comma() {
        let s = source("test", r"
            fn main() {
                let a = foo(a,);
                return 0;
            }
        ");

        parse_file(&s).unwrap();
    }

    #[test]
    fn test_struct() {
        let s = source("test", r"
            struct thing {
                first: int;
            }
        ");
        let mut p = Parser::new(&s);
        let _ = p.parse_struct().unwrap();
    }

    #[test]
    fn test_file() {
        let s = source("test", r"
            fn main() {
                let a: int = 0;
                return 0;
            }

            struct thing {
                first: int;
            }
        ");

        parse_file(&s).unwrap();
    }

    #[test]
    fn test_parse_complicated() {
        let s = source("test", r"
            fn main() -> i32 {
                let a: int = 0;
                let b = new Foo { };
                let c = new Bar {
                    first: 400,
                    second: { return 0; },
                    third: || { },
                    fourth: |a: Some| |b| true
                };
                return identity<i32, () -> bool>(identity(c).first);
            }

            struct Foo<A, B, C> { }

            struct Bar<T>(Something) {
                first: i32;
                second: List<i32>;
                third: () -> i32;
                fourth: (Some) -> (None) -> Maybe;
            }

            fn identity2<A, B>(thing: A, thing2: B) -> A {
                (if thing {
                    thing
                } else {
                    new List<i32> { something: false }
                })
            }

            struct Nil {
                nada: Foo<a, b, c>;
            }
        ");

        parse_file(&s).unwrap();
    }

    #[test]
    fn test_parsing_error_1() {
        let s = source("test", r"
            fn main() {
                let a: int = 0;
                return 0
            }
        ");

        parse_file(&s).unwrap_err().render_to_string();
    }

    #[test]
    fn test_parsing_error_2() {
        let s = source("test", r"
            fn main() {
                let a := 0;
                return 0;
            }
        ");

        parse_file(&s).unwrap_err().render_to_string();
    }

    #[test]
    fn test_parsing_error_3() {
        let s = source("test", r"
            struct Foo { a: i32 }
        ");

        parse_file(&s).unwrap_err().render_to_string();
    }
}