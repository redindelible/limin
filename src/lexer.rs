use std::fmt::{Display, Formatter};
use unicode_ident::{is_xid_continue, is_xid_start};
use phf::phf_map;

use std::ops::Range;
use std::str::CharIndices;
use crate::source::{Loc, Source};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum TokenType {
    Fn,
    Return,
    Let,
    Identifier,
    Number,
    Plus,
    Minus,
    Equal,
    LeftAngle,
    RightAngle,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    Period,
    Semicolon,
    EndOfFile,
    Error
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match *self {
            TokenType::Fn => "'fn'",
            TokenType::Return => "'return'",
            TokenType::Let => "'let'",
            TokenType::Identifier => "an identifier",
            TokenType::Number => "a number",
            TokenType::Plus => "'+'",
            TokenType::Minus => "'-'",
            TokenType::Equal => "'='",
            TokenType::LeftAngle => "'<'",
            TokenType::RightAngle => "'>'",
            TokenType::LeftParenthesis => "'('",
            TokenType::RightParenthesis => "')'",
            TokenType::LeftBrace => "'{'",
            TokenType::RightBrace => "'}'",
            TokenType::Comma => "','",
            TokenType::Colon => "':'",
            TokenType::Period => "'.'",
            TokenType::Semicolon => "';'",
            TokenType::EndOfFile => "<eof>",
            TokenType::Error => "<error>",
        })
    }
}

static BASIC_TOKENS: phf::Map<char, TokenType> = phf_map! {
    '<' => TokenType::LeftAngle,
    '>' => TokenType::RightAngle,
    '(' => TokenType::LeftParenthesis,
    ')' => TokenType::RightParenthesis,
    // '[' => TokenType::LeftBracket,
    // ']' => TokenType::RightBracket,
    '{' => TokenType::LeftBrace,
    '}' => TokenType::RightBrace,
    '+' => TokenType::Plus,
    '-' => TokenType::Minus,
    // '*' => TokenType::Star,
    // '/' => TokenType::Slash,
    // '%' => TokenType::Percent,
    '=' => TokenType::Equal,
    // '~' => TokenType::Tilde,
    // '&' => TokenType::Ampersand,
    // '|' => TokenType::VerticalBar,
    // '!' => TokenType::Exclamation,
    // '?' => TokenType::Question,
    '.' => TokenType::Period,
    ',' => TokenType::Comma,
    ';' => TokenType::Semicolon,
    ':' => TokenType::Colon,
};

static KEYWORDS: phf::Map<&str, TokenType> = phf_map! {
    "return" => TokenType::Return,
    "let" => TokenType::Let,
    // "if" => TokenType::If,
    // "for" => TokenType::For,
    // "is" => TokenType::Is,
    // "while" => TokenType::While,
    // "struct" => TokenType::Struct,
    // "import" => TokenType::Import,
    "fn" => TokenType::Fn,
    // "trait" => TokenType::Trait,
};

#[derive(Copy, Clone)]
pub struct Token<'par> {
    pub loc: Loc<'par>,
    pub typ: TokenType,
    pub text: &'par str
}

impl<'par> Token<'par> {
    pub fn new_range(typ: TokenType, source: &'par Source, range: Range<usize>) -> Token<'par> {
        Token { loc: Loc::new_inline(source, &range), typ, text: &source.text[range] }
    }

    pub fn new_eof(source: &'par Source) -> Token<'par> {
        Token {
            loc: Loc::new_eof(source),
            typ: TokenType::EndOfFile,
            text: ""
        }
    }
}


pub struct Lexer<'par> {
    source: &'par Source,
    chars: CharIndices<'par>,

    curr: Option<(usize, char)>,
    leading_ws: bool
}

impl<'par> Lexer<'par> {
    pub fn from_source(source: &'par Source) -> Lexer<'par> {
        let mut chars = source.text.char_indices();
        let curr = chars.next();
        Lexer { source, chars, curr, leading_ws: false }
    }

    fn advance(&mut self) {
        self.curr = self.chars.next()
    }

    fn add_token(&self, typ: TokenType, range: Range<usize>) -> Token<'par> {
        Token::new_range(typ, self.source, range)
    }

    fn lex_token(&mut self) -> Option<Token<'par>> {
        while let Some((i, chr)) = self.curr {
            match chr {
                '\n' | ' ' | '\t' | '\r' => {
                    self.leading_ws = true;
                    self.advance();
                },
                '0'..='9' => {
                    let start = i;
                    let mut range = start..self.source.text.len();
                    while let Some((i, chr)) = self.curr {
                        if !matches!(chr, '0'..='9') {
                            range = start..i;
                            break;
                        } else {
                            self.advance();
                        }
                    }
                    let tok = self.add_token(TokenType::Number, range);
                    self.leading_ws = false;
                    return Some(tok);
                },
                c if is_xid_start(c) => {
                    let start = i;
                    let mut range = start..self.source.text.len();
                    while let Some((i, chr)) = self.curr {
                        if is_xid_continue(chr) {
                            self.advance();
                        } else {
                            range = start..i;
                            break;
                        }
                    }
                    let tok = if KEYWORDS.contains_key(&self.source.text[range.clone()]) {
                        self.add_token(KEYWORDS[&self.source.text[range.clone()]], range)
                    } else {
                        self.add_token(TokenType::Identifier, range)
                    };
                    self.leading_ws = false;
                    return Some(tok);
                },
                c if BASIC_TOKENS.contains_key(&c) => {
                    let range = i..i+1;
                    self.advance();
                    let tok = self.add_token(BASIC_TOKENS[&c], range);
                    self.leading_ws = false;
                    return Some(tok);
                }
                c => {
                    let tok = self.add_token(TokenType::Error, i..i+c.len_utf8());
                    self.advance();
                    self.leading_ws = c.is_whitespace();
                    return Some(tok);
                }
            }
        }
        return None;
    }
}

impl<'par> Iterator for Lexer<'par> {
    type Item = Token<'par>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}