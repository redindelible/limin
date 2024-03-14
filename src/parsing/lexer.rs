use std::fmt::{Debug, Formatter};
use phf::phf_map;
use unicode_ident::{is_xid_continue, is_xid_start};
use crate::source::{Location, Source};

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum TokenType {
    Identifier,
    Integer,
    LeftAngle,
    RightAngle,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Bar,
    Equal,
    Comma,
    Period,
    Colon,
    Semicolon,
    Minus,
    Star,
    Fn,
    Let,
    For,
    Return,
    Struct,
    Trait,
    Impl,
    Use,
    Mod,
    If,
    Else,
    New,
    True,
    False,
    Unrecognized,
    EOF
}

const BASIC_TOKENS: phf::Map<char, TokenType> = phf_map! {
    '<' => TokenType::LeftAngle,
    '>' => TokenType::RightAngle,
    '(' => TokenType::LeftParenthesis,
    ')' => TokenType::RightParenthesis,
    '{' => TokenType::LeftBrace,
    '}' => TokenType::RightBrace,
    '|' => TokenType::Bar,
    '=' => TokenType::Equal,
    ':' => TokenType::Colon,
    ';' => TokenType::Semicolon,
    '-' => TokenType::Minus,
    ',' => TokenType::Comma,
    '.' => TokenType::Period,
    '*' => TokenType::Star
};

const KEYWORDS: phf::Map<&str, TokenType> = phf_map! {
    "fn" => TokenType::Fn,
    "let" => TokenType::Let,
    "for" => TokenType::For,
    "use" => TokenType::Use,
    "mod" => TokenType::Mod,
    "return" => TokenType::Return,
    "struct" => TokenType::Struct,
    "trait" => TokenType::Trait,
    "new" => TokenType::New,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "impl" => TokenType::Impl,
};

impl TokenType {
    pub fn name(&self) -> &'static str {
        match self {
            TokenType::Identifier => "an identifier",
            TokenType::Integer => "an integer",
            TokenType::LeftAngle => "'<'",
            TokenType::RightAngle => "'>'",
            TokenType::LeftParenthesis => "'('",
            TokenType::RightParenthesis => "')'",
            TokenType::LeftBrace => "'{'",
            TokenType::RightBrace => "'}'",
            TokenType::Bar => "'|'",
            TokenType::Equal => "'='",
            TokenType::Comma => "','",
            TokenType::Period => "'.'",
            TokenType::Colon => "':'",
            TokenType::Semicolon => "';'",
            TokenType::Minus => "'-'",
            TokenType::Star => "'*'",
            TokenType::Fn => "'fn'",
            TokenType::Let => "'let'",
            TokenType::For => "'for'",
            TokenType::Return => "'return'",
            TokenType::Struct => "'struct'",
            TokenType::Trait => "'trait'",
            TokenType::Impl => "'impl'",
            TokenType::Use => "'use'",
            TokenType::Mod => "'mod'",
            TokenType::EOF => "the end of the file",
            TokenType::New => "'new'",
            TokenType::True => "'true'",
            TokenType::False => "'false'",
            TokenType::If => "'if'",
            TokenType::Else => "'else'",
            TokenType::Unrecognized => "unrecognized characters"
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

    fn is_unrecognized(c: char) -> bool {
        match c {
            c if BASIC_TOKENS.contains_key(&c) => false,
            c if c.is_ascii_whitespace() => false,
            c if c.is_ascii_digit() => false,
            _ => true
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
                    let start = self.idx();
                    while Lexer::is_unrecognized(self.curr()) && self.curr() != '\0' {
                        self.advance();
                    }
                    let end = self.idx();

                    return Some(self.create_token(TokenType::Unrecognized, start, end, leading_ws));
                }
            }
        };
        None
    }
}

#[cfg(test)]
mod test {
    use crate::parsing::lexer::{Lexer, Token, TokenType};
    use crate::source::{Location, Source};
    use TokenType::*;

    fn token<'a>(source: &'a Source, typ: TokenType, text: &'a str, leading_ws: bool) -> Token<'a> {
        Token { typ, text, leading_ws, loc: Location { source, start: 0, len: 0} }
    }

    fn token_eof(source: &Source) -> Token {
        Token { typ: EOF, text: "", leading_ws: false, loc: source.eof() }
    }

    fn source(name: &str, text: &str) -> Source {
        Source::from_text(name,  text.to_owned())
    }

    #[test]
    fn lex_empty() {
        let s = source("<test>", "");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token_eof(&s)].into());
    }

    #[test]
    fn lex_one() {
        let s = source("<test>", "alpha");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, Identifier, "alpha", false), token_eof(&s)].into());
    }

    #[test]
    fn lex_one_ws() {
        let s = source("<test>", "  alpha ");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, Identifier, "alpha", true), token_eof(&s)].into());
    }

    #[test]
    fn lex_two_ws() {
        let s = source("<test>", "alpha   beta");
        let toks = Lexer::lex(&s);
        assert_eq!(toks, vec![token(&s, Identifier, "alpha", false), token(&s, Identifier, "beta", true), token_eof(&s)].into());
    }

    #[test]
    fn lex_keyword() {
        let s = source("<test>", "return");
        let toks = Lexer::lex(&s);
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].typ, Return);
    }

    #[test]
    fn lex_maximal_munch() {
        let s = source("<test>", "returntwo");
        let toks = Lexer::lex(&s);
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].typ, Identifier);
    }

    #[test]
    fn lex_number() {
        let s = source("<test>", "2");
        let toks = Lexer::lex(&s);
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].typ, Integer);
    }

    #[test]
    fn lex_symbols() {
        let s = source("<test>", "+-/");
        let toks = Lexer::lex(&s);
        assert_eq!(toks.len(), 4);
    }

    #[test]
    fn lex_error() {
        let s = source("<test>", "````");
        let toks = Lexer::lex(&s);
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].typ, Unrecognized);
    }
}