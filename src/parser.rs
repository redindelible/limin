use std::collections::HashSet;
use crate::lexer::{Lexer, Token, TokenType};
use crate::source::{Loc, Source};
use crate::ast::*;
use crate::error::{CompilerMessage, MessageLevel};

pub struct Parser<'par> {
    ast: &'par mut AST,
    loc: &'par mut ASTTable<LocationData<'par>>,

    tokens: Vec<Token<'par>>,
    source: &'par Source,
    idx: usize,

    curr: Token<'par>,

    handlers: Vec<(HashSet<TokenType>, i32)>,
    msgs: Vec<CompilerMessage>
}

type ParseResult<T> = Result<T, i32>;

pub struct LocationData<'com> {
    phantom: std::marker::PhantomData<&'com ()>
}

impl<'com> ASTData for LocationData<'com> {
    type FileData = ();
    type FunctionData = Loc<'com>;
    type FunctionParameterData = Loc<'com>;
    type ExprData = Loc<'com>;
    type StmtData = Loc<'com>;
    type TypeData = Loc<'com>;
    type NamespaceData = Loc<'com>;
}


impl<'par> Parser<'par> {
    pub fn parse(ast: &'par mut AST, source: &'par Source) -> Result<Key<File>, Vec<CompilerMessage>> {
        let mut loc_data = ASTTable::new();
        let mut parser = Parser::new(ast, &mut loc_data, source);
        parser.parse_file().map_err(|_| parser.msgs)
    }

    fn new(ast: &'par mut AST, loc_data: &'par mut ASTTable<LocationData<'par>>, source: &'par Source) -> Self {
        let tokens: Vec<Token<'par>> = Lexer::from_source(source).collect();
        let idx = 0;
        let curr = if tokens.is_empty() {
            Token::new_eof(source)
        } else {
            tokens[0]
        };
        let handlers = Vec::new();
        let msgs = Vec::new();
        Parser { ast, loc: loc_data, tokens, source, idx, curr, handlers, msgs }
    }

    // region helpers
    fn on_error<T>(&mut self, msg: &str) -> ParseResult<T> {
        self.msgs.push(CompilerMessage::new_located(
            MessageLevel::Error, msg.to_owned(), self.curr.loc
        ));
        loop {
            for (can_handle, id) in self.handlers.iter().rev() {
                if can_handle.contains(&self.curr.typ) {
                    return Err(*id)
                }
            }
            if self.is_done() {
                break;
            }
            self.advance()
        }
        Err(-1)
    }

    fn advance(&mut self) {
        if self.is_done() {
            return;
        } else {
            self.idx += 1;
            self.curr = self.tokens.get(self.idx).cloned().unwrap_or(Token::new_eof(self.source));
        }
    }

    fn get_at(&self, idx: usize) -> Token<'par> {
        self.tokens.get(self.idx + idx).cloned().unwrap_or(Token::new_eof(self.source))
    }

    fn is_done(&self) -> bool {
        return self.idx == self.tokens.len()
    }

    fn expect(&mut self, typ: TokenType) -> ParseResult<Token<'par>> {
        if self.curr.typ == typ {
            let ret = self.curr;
            self.advance();
            Ok(ret)
        } else {
            self.on_error(&format!("Expected a {}, but got {}", typ, self.curr.typ))
        }
    }

    fn expect_symbol(&mut self, typs: &[TokenType], name: &str) -> ParseResult<()> {
        let idx = self.idx;
        let curr = self.curr;
        for typ in typs {
            if self.curr.typ != *typ {
                self.idx = idx;
                self.curr = curr;
                return self.on_error(&format!("Expected {}", name));
            }
            self.advance()
        }
        Ok(())
    }

    fn matches_symbol(& self, typs: &[TokenType]) -> bool {
        for (i, typ) in typs.iter().enumerate() {
            if self.get_at(i).typ != *typ {
                return false;
            }
        }
        return true;
    }

    fn parse_list<T, F>(&mut self, left: TokenType, mut item: F, right: TokenType) -> ParseResult<(Vec<T>, Loc<'par>)>
        where F: FnMut(&mut Self) -> ParseResult<T> {
        let start = self.expect(left)?;
        let mut items = Vec::new();
        while self.curr.typ != right {
            items.push(item(self)?);
            if self.curr.typ == TokenType::Comma {
                self.advance()
            } else {
                break;
            }
        }
        let _ = self.expect(right)?;
        Ok((items, start.loc))
    }

    fn backtrack_point<T, F>(&mut self, mut f: F) -> Option<T>
        where F: FnMut(&mut Self) -> ParseResult<T> {
        let idx = self.idx;
        let curr = self.curr;
        let msgs_len = self.msgs.len();

        match f(self) {
            Ok(res) => Some(res),
            Err(_) => {
                self.idx = idx;
                self.curr = curr;
                self.msgs.drain(msgs_len..);
                None
            }
        }
    }

    fn resume_point<T, F>(&mut self, tokens: &[TokenType], mut f: F) -> ParseResult<()>
        where F: FnMut(&mut Self) -> ParseResult<T> {
        let id = self.handlers.len() as i32;
        self.handlers.push((HashSet::from_iter(tokens.to_owned()), id));

        let res = match f(self) {
            Ok(_) => Ok(()),
            Err(res_id) if res_id == id => Ok(()),
            Err(res_id) => Err(res_id)
        };
        self.handlers.pop();
        res
    }
    // endregion

    fn parse_file(&mut self) -> ParseResult<Key<File>> {
        let mut file = File {
            functions: Vec::new(),
        };
        while !self.is_done() {
            match self.curr.typ {
                TokenType::Fn => {
                    let func = self.parse_fn()?;
                    file.functions.push(func);
                },
                _ => {
                    self.on_error("Expected a top level (a function).")?
                }
            }
        }
        Ok(self.ast.add_file(file))
    }

    fn parse_fn(&mut self) -> ParseResult<Key<Function>> {
        let start = self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?;
        let (parameters, _) = self.parse_list(TokenType::LeftParenthesis, Parser::parse_fn_param, TokenType::RightParenthesis)?;
        let ret = if self.matches_symbol(&[TokenType::Minus, TokenType::RightAngle]) {
            self.advance(); self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_expr()?;
        let func = Function {
            name: name.text.to_owned(),
            parameters,
            ret,
            body
        };
        let end = *body.get(&self.loc);
        Ok(self.ast.add_function(func)
            .set(&mut self.loc, start.loc + end))
    }

    fn parse_fn_param(&mut self) -> ParseResult<Key<FunctionParameter>> {
        let name = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let typ = self.parse_type()?;
        let start = name.loc;
        let end = *typ.get(&self.loc);
        Ok(self.ast.add_function_parameter(FunctionParameter { name: name.text.to_owned(), typ})
            .set(&mut self.loc, start + end))
    }

    fn parse_stmt(&mut self) -> ParseResult<Key<Stmt>> {
        match self.curr.typ {
            TokenType::Return => {
                let start = self.expect(TokenType::Return)?;
                let ret = self.parse_expr()?;
                let end = self.expect(TokenType::Semicolon)?;
                Ok(self.ast.add_stmt(Stmt::Return { expr: ret }).set(&mut self.loc, start.loc + end.loc))
            },
            TokenType::Let => {
                let start = self.expect(TokenType::Let)?;
                let name = self.expect(TokenType::Identifier)?;
                let typ = if self.curr.typ == TokenType::Colon {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                self.expect(TokenType::Equal)?;
                let value = self.parse_expr()?;
                let end = self.expect(TokenType::Semicolon)?;
                Ok(self.ast.add_stmt(Stmt::Let { name: name.text.to_owned(), typ, value }).set(&mut self.loc, start.loc + end.loc))
            }
            _ => {
                let expr = self.parse_expr()?;
                let end = match self.ast[expr] {
                    Expr::Block { .. } => { *expr.get(&self.loc) }
                    _ => {
                        let end = self.expect(TokenType::Semicolon)?;
                        end.loc
                    }
                };
                let start = *expr.get(&self.loc);
                Ok(self.ast.add_stmt(Stmt::Expr { expr }).set(&mut self.loc, start + end))
            }
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Key<Expr>> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> ParseResult<Key<Expr>> {
        self.parse_addition()
    }

    fn parse_addition(&mut self) -> ParseResult<Key<Expr>> {
        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<Key<Expr>> {
        let mut left = self.parse_terminal()?;
        loop {
            match self.curr.typ {
                TokenType::LeftAngle => {
                    match self.backtrack_point(|parser| {
                        let (type_args, _) = parser.parse_list(TokenType::LeftAngle, Parser::parse_type, TokenType::RightAngle)?;
                        let (args, end) = parser.parse_list(TokenType::LeftParenthesis, Parser::parse_expr, TokenType::RightParenthesis)?;
                        Ok((type_args, args, end))
                    }) {
                        Some((generic, args, end)) => {
                            let start = *left.get(&self.loc);
                            left = self.ast.add_expr(Expr::Call { callee: left, generic, args }).set(&mut self.loc, start + end);
                        }
                        None => {
                            break;
                        }
                    }
                },
                TokenType::LeftParenthesis => {
                    let (args, end) = self.parse_list(TokenType::LeftParenthesis, Parser::parse_expr, TokenType::RightParenthesis)?;
                    let start = *left.get(&self.loc);
                    left = self.ast.add_expr(Expr::Call { callee: left, generic: Vec::new(), args }).set(&mut self.loc, start + end);
                }
                TokenType::Period => {
                    let _ = self.expect(TokenType::Period)?;
                    let field = self.expect(TokenType::Identifier)?;
                    let start = *left.get(&self.loc);
                    left = self.ast.add_expr(Expr::Field { obj: left, field: field.text.to_owned() }).set(&mut self.loc, start + field.loc);
                }
                _ => {
                    break;
                }
            }
        }
        Ok(left)
    }

    fn parse_terminal(&mut self) -> ParseResult<Key<Expr>> {
        match self.curr.typ {
            TokenType::Number => {
                let num: u64 = self.curr.text.parse().unwrap();
                let loc = self.curr.loc;
                self.advance();
                Ok(self.ast.add_expr(Expr::Integer { num }).set(&mut self.loc, loc))
            },
            TokenType::Identifier => {
                let name = self.expect(TokenType::Identifier)?;
                let mut left = self.ast.add_expr(Expr::Name { ns: None, name: name.text.to_owned()}).set(&mut self.loc, name.loc);

                while self.matches_symbol(&[TokenType::Colon, TokenType::Colon]) || self.curr.typ == TokenType::LeftAngle {
                    let ns = if self.curr.typ == TokenType::LeftAngle {
                        let (items, loc) = self.parse_list(TokenType::LeftAngle, Parser::parse_type, TokenType::RightAngle)?;
                        let ns = self.ast.add_namespace(Namespace::from_expr(&self.ast[left], items));
                        let start = *left.get(&self.loc);
                        ns.set(&mut self.loc, start + loc);
                        ns
                    } else {
                        let ns = self.ast.add_namespace(Namespace::from_expr(&self.ast[left], Vec::new()));
                        let start = *left.get(&self.loc);
                        ns.set(&mut self.loc, start);
                        ns
                    };

                    self.expect_symbol(&[TokenType::Colon, TokenType::Colon], "'::'")?;
                    let name = self.expect(TokenType::Identifier)?;
                    let left_loc = *left.get(&self.loc);
                    left = self.ast.add_expr(Expr::Name { ns: Some(ns), name: name.text.to_owned() }).set(&mut self.loc, left_loc + name.loc);
                }

                Ok(left)
            },
            TokenType::LeftBrace => self.parse_block(),
            _ => {
                self.on_error("Expected an expression")
            }
        }
    }

    fn parse_block(&mut self) -> ParseResult<Key<Expr>> {
        let start = self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        while self.curr.typ != TokenType::RightBrace {
            stmts.push(self.parse_stmt()?);
        }
        let end = self.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_expr(Expr::Block { stmts }).set(&mut self.loc, start.loc + end.loc))
    }

    fn parse_type(&mut self) -> ParseResult<Key<Type>> {
        match self.curr.typ {
            TokenType::Identifier => {
                let name = self.expect(TokenType::Identifier)?;
                let mut left = if self.curr.typ == TokenType::LeftAngle {
                    let (items, loc) = self.parse_list(TokenType::LeftAngle, Parser::parse_type, TokenType::RightAngle)?;
                    self.ast.add_type(Type::Name { ns: None, name: name.text.to_owned(), generic: items}).set(&mut self.loc, name.loc + loc)
                } else {
                    self.ast.add_type(Type::Name { ns: None, name: name.text.to_owned(), generic: Vec::new()}).set(&mut self.loc, name.loc)
                };

                while self.matches_symbol(&[TokenType::Colon, TokenType::Colon]) {
                    let ns = self.ast.add_namespace(Namespace::from_type(&self.ast[left]));
                    self.expect_symbol(&[TokenType::Colon, TokenType::Colon], "'::'")?;
                    let name = self.expect(TokenType::Identifier)?;

                    let left_loc = *left.get(&self.loc);
                    left = if self.curr.typ == TokenType::LeftAngle {
                        let (items, loc) = self.parse_list(TokenType::LeftAngle, Parser::parse_type, TokenType::RightAngle)?;
                        self.ast.add_type(Type::Name { ns: Some(ns), name: name.text.to_owned(), generic: items}).set(&mut self.loc, name.loc + loc + left_loc)
                    } else {
                        self.ast.add_type(Type::Name { ns: Some(ns), name: name.text.to_owned(), generic: Vec::new()}).set(&mut self.loc, name.loc + left_loc)
                    };
                }

                Ok(left)
            },
            _ => {
                self.on_error("Expected a type")
            }
        }
    }
}