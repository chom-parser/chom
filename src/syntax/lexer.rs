use std::iter::Peekable;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::io;
use std::fmt;
use source_span::{Position, Span};
use crate::charset;
use crate::charset::CharSet;
use super::Located;

pub enum Error {
    IO(std::io::Error),
    WrongCloser(Delimiter, char),
    MissingCloser(Delimiter),
    InvalidChar,
    IncompleteString,
    IncompleteCharSet,
    Unexpected(char)
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match self {
            IO(e) => write!(f, "I/O: {}", e),
            WrongCloser(_, _) => write!(f, "wrong delimiter"),
            MissingCloser(_) => write!(f, "missing delimiter"),
            InvalidChar => write!(f, "incomplete char"),
            IncompleteString => write!(f, "incomplete string"),
            IncompleteCharSet => write!(f, "incomplete character set"),
            Unexpected(c) => write!(f, "unexpected character `{}`", c)
        }
    }
}

pub type Result<T> = std::result::Result<T, Located<Error>>;

#[derive(Clone, Copy)]
pub enum Keyword {
    Extern,
    RegExp,
    Type,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Keyword::*;
        match self {
            Extern => write!(f, "@extern"),
            RegExp => write!(f, "@regexp"),
            Type => write!(f, "@type"),
        }
    }
}

#[derive(Clone)]
pub enum Token {
    Keyword(Keyword),
    Punct(char),
    Ident(String),
    Group(Delimiter, Vec<Located<Token>>),
    String(String, bool),
    CharSet(CharSet, bool)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match self {
            Keyword(k) => k.fmt(f),
            Punct(p) => p.fmt(f),
            Ident(s) => s.fmt(f),
            Group(d, tokens) => {
                d.begin().fmt(f)?;
                for token in tokens {
                    write!(f, "{} ", token)?
                }
                d.end().fmt(f)
            },
            String(s, case_sensitive) => {
                if *case_sensitive {
                    write!(f, "'{}'", crate::charset::DisplayString(s))
                } else {
                    write!(f, "\"{}\"", crate::charset::DisplayString(s))
                }
            },
            CharSet(set, negate) => {
                if *negate {
                    write!(f, "[^{}]", set)
                } else {
                    write!(f, "[{}]", set)
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Angle
}

impl Delimiter {
    pub fn begin(&self) -> char {
        use self::Delimiter::*;
        match self {
            Parenthesis => '(',
            Brace => '{',
            Angle => '<'
        }
    }

    pub fn end(&self) -> char {
        use self::Delimiter::*;
        match self {
            Parenthesis => ')',
            Brace => '}',
            Angle => '>'
        }
    }
}

pub struct Lexer<I: Iterator<Item = io::Result<char>>> {
    input: Peekable<I>,
    pos: Position
}

impl<I: Iterator<Item = io::Result<char>>> Lexer<I> {
    pub fn new(input: I) -> Lexer<I> {
        Lexer {
            input: input.peekable(),
            pos: Position::default()
        }
    }
}

fn peek<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, span: &Span) -> Result<Option<char>> {
    match it.peek() {
        Some(Ok(c)) => {
            Ok(Some(*c))
        },
        Some(Err(_)) => {
            let mut dummy_span = *span;
            Ok(consume(it, &mut dummy_span)?) // always return an error.
        },
        None => Ok(None)
    }
}

fn consume<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, span: &mut Span) -> Result<Option<char>> {
    match it.next() {
        Some(Ok(c)) => {
            span.push(c);
            Ok(Some(c))
        },
        Some(Err(e)) => Err(Located::new(Error::IO(e), span.end().into())),
        None => Ok(None)
    }
}

fn parse_group<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Located<Token>> {
    let mut span = pos.into();

    let delimiter = match consume(it, &mut span)?.unwrap() {
        '(' => Delimiter::Parenthesis,
        '{' => Delimiter::Brace,
        '<' => Delimiter::Angle,
        _ => unreachable!()
    };

    let mut tokens = Vec::new();
    skip_whitespaces(it, &mut span)?;

    loop {
        match peek(it, &span)? {
            Some(')') | Some('}') | Some('>') => {
                let c = consume(it, &mut span)?.unwrap();
                if c == delimiter.end() {
                    return Ok(Located::new(Token::Group(delimiter, tokens), span))
                } else {
                    return Err(Located::new(Error::WrongCloser(delimiter, c), span))
                }
            },
            Some(c) => {
                let token = parse_token(it, span.end())?.unwrap();
                span.append(token.span());
                tokens.push(token);

                skip_whitespaces(it, &mut span)?;
            },
            None => return Err(Located::new(Error::MissingCloser(delimiter), span))
        }
    }
}

fn parse_hex_char<I: Iterator<Item = io::Result<char>>>(n: usize, it: &mut Peekable<I>, span: &mut Span) -> Result<char> {
    let mut char_span = span.next();
    let mut codepoint = 0;

    for i in 0..n {
        if let Some(c) = consume(it, span)? {
            if let Some(d) = c.to_digit(16) {
                char_span.push(c);
                codepoint = (codepoint << 4) | d
            } else {
                return Err(Located::new(Error::InvalidChar, char_span))
            }
        } else {
            return Err(Located::new(Error::IO(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected end of stream")), char_span))
        }
    }

    match char::try_from(codepoint) {
        Ok(c) => Ok(c),
        Err(_) => Err(Located::new(Error::InvalidChar, char_span))
    }
}

fn parse_string<I: Iterator<Item = io::Result<char>>>(delimiter: char, it: &mut Peekable<I>, pos: Position) -> Result<Located<Token>> {
    let mut span = pos.into();
    consume(it, &mut span)?;

    let mut string = String::new();

    while let Some(c) = parse_string_char(delimiter, it, &mut span)? {
        string.push(c)
    }

    Ok(Located::new(Token::String(string, delimiter != '"'), span))
}

fn parse_string_char<I: Iterator<Item = io::Result<char>>>(delimiter: char, it: &mut Peekable<I>, span: &mut Span) -> Result<Option<char>> {
    if let Some(c) = consume(it, span)? {
        match c {
            _ if c == delimiter => {
                return Ok(None)
            },
            '\\' => {
                if let Some(c) = consume(it, span)? {
                    match c {
                        'r' => {
                            Ok(Some('\r'))
                        },
                        'n' => {
                            Ok(Some('\n'))
                        },
                        's' => {
                            Ok(Some(' '))
                        },
                        't' => {
                            Ok(Some('\t'))
                        },
                        'x' => {
                            Ok(Some(parse_hex_char(2, it, span)?))
                        },
                        'u' => {
                            Ok(Some(parse_hex_char(4, it, span)?))
                        },
                        'U' => {
                            Ok(Some(parse_hex_char(8, it, span)?))
                        },
                        _ => {
                            Ok(Some(c))
                        }
                    }
                } else {
                    return Err(Located::new(Error::IncompleteCharSet, span.clone()))
                }
            },
            _ => {
                Ok(Some(c))
            }
        }
    } else {
        return Err(Located::new(Error::IncompleteCharSet, span.clone()))
    }
}

fn parse_charset<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Located<Token>> {
    let mut span = pos.into();
    consume(it, &mut span)?;

    let mut escape = false;
    let mut set = CharSet::new();

    let negate = if let Some('^') = peek(it, &span)? {
        consume(it, &mut span)?;
        true
    } else {
        false
    };

    loop {
        if let Some(first) = parse_string_char(']', it, &mut span)? {
            let last = if let Some('-') = peek(it, &span)? {
                consume(it, &mut span)?;
                if let Some(last) = parse_string_char(']', it, &mut span)? {
                    last
                } else {
                    return Err(Located::new(Error::IncompleteCharSet, span))
                }
            } else {
                first
            };

            set.add(charset::Range::new(first, last));
        } else {
            break
        }
    }

    Ok(Located::new(Token::CharSet(set, negate), span))
}

fn is_space(c: char) -> bool {
    c.is_whitespace() || c.is_control() || c == '\n'
}

fn is_punct(c: char) -> bool {
    c == '/' || c == '\\' || c == ',' || c == ';' || c == ':' || c == '.' || c == '*' || c == '+' || c == '?' || c == '!' || c == '=' || c == '|'
}

fn is_separator(c: char) -> bool {
    is_space(c) || is_punct(c) || c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' || c == '<' || c == '>' || c == '"' || c == '\'' || c == '#'
}

fn parse_ident<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, mut span: Span) -> Result<Located<Token>> {
    let mut id = String::new();

    loop {
        if let Some(c) = peek(it, &span)? {
            if is_separator(c) {
                break
            } else {
                consume(it, &mut span)?;
                id.push(c);
            }
        } else {
            break
        }
    }

    match id.as_ref() {
        "" => Err(Located::new(Error::IO(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected end of stream")), span)),
        "@extern" => Ok(Located::new(Token::Keyword(Keyword::Extern), span)),
        "@regexp" => Ok(Located::new(Token::Keyword(Keyword::RegExp), span)),
        "@type" => Ok(Located::new(Token::Keyword(Keyword::Type), span)),
        _ => Ok(Located::new(Token::Ident(id), span))
    }
}

fn skip_whitespaces<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, span: &mut Span) -> Result<()> {
    loop {
        match peek(it, span)? {
            Some(c) if is_space(c) => {
                consume(it, span)?;
            },
            Some('#') => {
                loop {
                    match consume(it, span)? {
                        Some('\n') => break,
                        None => break,
                        _ => ()
                    }
                }
            },
            _ => break
        }
    }

    Ok(())
}

fn parse_token<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Option<Located<Token>>> {
    let mut whitespace_span = pos.into();
    skip_whitespaces(it, &mut whitespace_span);
    loop {
        match peek(it, &whitespace_span)? {
            Some('(') | Some('{') | Some('<') => return Ok(Some(parse_group(it, whitespace_span.end())?)),
            Some(')') | Some('}') | Some('>') => {
                let mut span = whitespace_span.end().into();
                let c = consume(it, &mut span)?.unwrap();
                return Err(Located::new(Error::Unexpected(c), span))
            },
            Some('[') => return Ok(Some(parse_charset(it, whitespace_span.end())?)),
            Some('\'') => return Ok(Some(parse_string('\'', it, whitespace_span.end())?)),
            Some('"') => return Ok(Some(parse_string('"', it, whitespace_span.end())?)),
            Some(c) if is_punct(c) => {
                let mut span = whitespace_span.end().into();
                consume(it, &mut span)?;
                return Ok(Some(Located::new(Token::Punct(c), span)))
            },
            Some(_) => {
                let span = whitespace_span.end().into();
                return Ok(Some(parse_ident(it, span)?))
            },
            None => break
        }
    }

    Ok(None)
}

impl<I: Iterator<Item = io::Result<char>>> Iterator for Lexer<I> {
    type Item = Result<Located<Token>>;

    fn next(&mut self) -> Option<Result<Located<Token>>> {
        match parse_token(&mut self.input, self.pos) {
            Ok(Some(token)) => {
                self.pos = token.span().end();
                Some(Ok(token))
            },
            Ok(None) => None,
            Err(e) => {
                self.pos = e.span().end();
                Some(Err(e))
            }
        }
    }
}
