use std::fmt::{Debug, Formatter};
use phf::phf_map;
use unicode_ident::{is_xid_continue, is_xid_start};
use crate::source::{Location, Source};

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum TokenType {
    Identifier,
    Integer,
    LessThan,
    GreaterThan,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Equal,
    Comma,
    Colon,
    Semicolon,
    Minus,
    Fn,
    Let,
    Return,
    Struct,
    EOF
}

const BASIC_TOKENS: phf::Map<char, TokenType> = phf_map! {
    '<' => TokenType::LessThan,
    '>' => TokenType::GreaterThan,
    '(' => TokenType::LeftParenthesis,
    ')' => TokenType::RightParenthesis,
    '{' => TokenType::LeftBrace,
    '}' => TokenType::RightBrace,
    '=' => TokenType::Equal,
    ':' => TokenType::Colon,
    ';' => TokenType::Semicolon,
    '-' => TokenType::Minus,
    ',' => TokenType::Comma
};

const KEYWORDS: phf::Map<&str, TokenType> = phf_map! {
    "fn" => TokenType::Fn,
    "let" => TokenType::Let,
    "return" => TokenType::Return,
    "struct" => TokenType::Struct
};

impl TokenType {
    pub fn name(&self) -> &'static str {
        match self {
            TokenType::Identifier => "an identifier",
            TokenType::Integer => "an integer",
            TokenType::LessThan => "'<'",
            TokenType::GreaterThan => "'>'",
            TokenType::LeftParenthesis => "'('",
            TokenType::RightParenthesis => "')'",
            TokenType::LeftBrace => "'{'",
            TokenType::RightBrace => "'}'",
            TokenType::Equal => "'='",
            TokenType::Comma => "','",
            TokenType::Colon => "':'",
            TokenType::Semicolon => "';'",
            TokenType::Minus => "'-'",
            TokenType::Fn => "'fn'",
            TokenType::Let => "'let'",
            TokenType::Return => "'return'",
            TokenType::Struct => "'struct'",
            TokenType::EOF => "the end of the file"
        }
    }
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
        tokens.push(Token { typ: TokenType::EOF, text: "", leading_ws: false, loc: source.eof()});
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
                c if BASIC_TOKENS.contains_key(&c) => {
                    let start = self.idx();
                    let ttype = BASIC_TOKENS[&c];
                    self.advance();
                    let end = self.idx();
                    return Some(self.create_token(ttype, start, end, leading_ws));
                },
                c if c.is_ascii_whitespace() => {
                    leading_ws = true;
                    self.advance();
                },
                c if c.is_ascii_digit() => {
                    let start = self.idx();
                    while self.curr().is_ascii_digit() {
                        self.advance();
                    }
                    let end = self.idx();
                    return Some(self.create_token(TokenType::Integer, start, end, leading_ws));
                },
                c if is_xid_start(c) => {
                    let start = self.idx();
                    while is_xid_continue(self.curr()) {
                        self.advance();
                    }
                    let end = self.idx();

                    let token = self.create_token(TokenType::Identifier, start, end, leading_ws);

                    return if KEYWORDS.contains_key(token.text) {
                        Some(self.create_token(KEYWORDS[token.text], start, end, leading_ws))
                    } else {
                        Some(token)
                    }
                },
                _ => {
                    todo!()
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

    fn token_eof(source: &Source) -> Token {
        Token { typ: EOF, text: "", leading_ws: false, loc: source.eof() }
    }

    #[test]
    fn lex_empty() {
        let s = Source::from_text("<test>", "");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token_eof(&s)].into());
    }

    #[test]
    fn lex_one() {
        let s = Source::from_text("<test>", "alpha");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, Identifier, "alpha", false), token_eof(&s)].into());
    }

    #[test]
    fn lex_one_ws() {
        let s = Source::from_text("<test>", "  alpha ");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, Identifier, "alpha", true), token_eof(&s)].into());
    }

    #[test]
    fn lex_two_ws() {
        let s = Source::from_text("<test>", "alpha   beta");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, Identifier, "alpha", false), token(&s, Identifier, "beta", true), token_eof(&s)].into());
    }
}