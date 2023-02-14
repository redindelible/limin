use std::fmt::{Debug, Formatter};
use unicode_ident::{is_xid_continue, is_xid_start};
use crate::source::{Location, Source};

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum TokenType {
    IDENTIFIER,
    EOF
}

#[derive(Copy, Clone)]
pub struct Token<'a> {
    pub typ: TokenType,
    pub text: &'a str,
    pub leading_ws: bool,
    pub loc: Location<'a>
}

impl<'a> Token<'a> {
    pub fn eof(source: &'a Source) -> Self {
        Token { typ: TokenType::EOF, text: "", leading_ws: false, loc: source.eof() }
    }
}

impl PartialEq<Self> for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ && self.text == other.text && self.leading_ws == other.leading_ws
    }
}

impl Eq for Token<'_> { }

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token({:?}, {:?})", self.typ, self.text)
    }
}

pub struct Lexer<'a> {
    source: &'a Source,
    chars: Box<[(usize, char)]>,
    _idx: usize
}

impl<'a> Lexer<'a> {
    pub fn lex(source: &'a Source) -> Box<[Token<'a>]> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        while let Some(tok) = lexer.lex_token() {
            tokens.push(tok);
        }
        tokens.into_boxed_slice()
    }

    fn new(source: &'a Source) -> Self {
        let chars = source.text.char_indices().collect();
        Self {
            source, chars, _idx: 0
        }
    }

    fn curr(&self) -> char {
        self.chars.get(self._idx).map_or('\0', |p| p.1)
    }

    fn idx(&self) -> usize {
        self.chars.get(self._idx).map_or(self.source.text.len(), |p| p.0)
    }

    fn advance(&mut self) {
        self._idx += 1;
    }

    fn is_done(&self) -> bool {
        self._idx >= self.chars.len()
    }

    fn create_token(&self, typ: TokenType, start: usize, end: usize, leading_ws: bool) -> Token<'a> {
        Token {
            typ,
            text: &self.source.text[start..end],
            leading_ws,
            loc: Location { source: self.source, start, len: end - start }
        }
    }

    fn lex_token(&mut self) -> Option<Token<'a>> {
        let mut leading_ws = false;
        while !self.is_done() {
            match self.curr() {
                c if c.is_ascii_whitespace() => {
                    leading_ws = true;
                    self.advance();
                }
                c if is_xid_start(c) => {
                    let start = self.idx();
                    while is_xid_continue(self.curr()) {
                        self.advance();
                    }
                    let end = self.idx();
                    return Some(self.create_token(TokenType::IDENTIFIER, start, end, leading_ws));
                }
                _ => {
                    self.advance();
                }
            }
        };
        None
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Lexer, Token, TokenType};
    use crate::source::{Location, Source};
    use TokenType::*;

    fn token<'a>(source: &'a Source, typ: TokenType, text: &'a str, leading_ws: bool) -> Token<'a> {
        Token { typ, text, leading_ws, loc: Location { source, start: 0, len: 0} }
    }

    #[test]
    fn lex_empty() {
        let s = Source::from_text("<test>", "");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![].into());
    }

    #[test]
    fn lex_one() {
        let s = Source::from_text("<test>", "alpha");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, IDENTIFIER, "alpha", false)].into());
    }

    #[test]
    fn lex_one_ws() {
        let s = Source::from_text("<test>", "  alpha ");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, IDENTIFIER, "alpha", true)].into());
    }

    #[test]
    fn lex_two_ws() {
        let s = Source::from_text("<test>", "alpha   beta");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, IDENTIFIER, "alpha", false), token(&s, IDENTIFIER, "beta", true)].into());
    }
}