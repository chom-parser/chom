use crate::charset::CharSet;
use source_span::{Loc, Metrics, Span};
use std::convert::TryFrom;
use std::fmt;
use std::io;
use std::iter::Peekable;

pub enum Error {
	IO(std::io::Error),
	WrongCloser(Delimiter, char),
	MissingCloser(Delimiter),
	InvalidChar,
	IncompleteString,
	IncompleteCharSet,
	Unexpected(char),
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
			Unexpected(c) => write!(f, "unexpected character `{}`", c),
		}
	}
}

pub type Result<T> = std::result::Result<T, Loc<Error>>;

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
			Extern => write!(f, "extern"),
			RegExp => write!(f, "regexp"),
			Type => write!(f, "type"),
		}
	}
}

#[derive(Clone)]
pub enum Token {
	Keyword(Keyword),
	Punct(char),
	Ident(String),
	Group(Delimiter, Vec<Loc<Token>>),
	String(String, bool),
	CharSet(CharSet, bool),
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
			}
			String(s, case_sensitive) => {
				if *case_sensitive {
					write!(f, "'{}'", crate::charset::DisplayString(s))
				} else {
					write!(f, "\"{}\"", crate::charset::DisplayString(s))
				}
			}
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
	Angle,
}

impl Delimiter {
	pub fn begin(&self) -> char {
		use self::Delimiter::*;
		match self {
			Parenthesis => '(',
			Brace => '{',
			Angle => '<',
		}
	}

	pub fn end(&self) -> char {
		use self::Delimiter::*;
		match self {
			Parenthesis => ')',
			Brace => '}',
			Angle => '>',
		}
	}
}

fn is_space(c: char) -> bool {
	c.is_whitespace() || c.is_control() || c == '\n'
}

fn is_punct(c: char) -> bool {
	c == '/'
		|| c == '\\'
		|| c == ','
		|| c == ';'
		|| c == ':'
		|| c == '.'
		|| c == '*'
		|| c == '+'
		|| c == '?'
		|| c == '!'
		|| c == '='
		|| c == '|'
}

fn is_separator(c: char) -> bool {
	is_space(c)
		|| is_punct(c)
		|| c == '('
		|| c == ')'
		|| c == '['
		|| c == ']'
		|| c == '{'
		|| c == '}'
		|| c == '<'
		|| c == '>'
		|| c == '"'
		|| c == '\''
		|| c == '#'
}

pub struct Lexer<I: Iterator<Item = io::Result<char>>, M: Metrics> {
	input: Peekable<I>,
	metrics: M,
	span: Span,
}

impl<I: Iterator<Item = io::Result<char>>, M: Metrics> Lexer<I, M> {
	pub fn new(input: I, metrics: M) -> Lexer<I, M> {
		Lexer {
			input: input.peekable(),
			metrics,
			span: Span::default(),
		}
	}

	fn peek(&mut self) -> Result<Option<char>> {
		match self.input.peek() {
			Some(Ok(c)) => Ok(Some(*c)),
			Some(Err(_)) => self.consume(),
			None => Ok(None),
		}
	}

	fn consume(&mut self) -> Result<Option<char>> {
		match self.input.next() {
			Some(Ok(c)) => {
				self.span.push(c, &self.metrics);
				Ok(Some(c))
			}
			Some(Err(e)) => Err(Loc::new(Error::IO(e), self.span.end().into())),
			None => Ok(None),
		}
	}

	fn parse_group(&mut self) -> Result<Loc<Token>> {
		let delimiter = match self.consume()?.unwrap() {
			'(' => Delimiter::Parenthesis,
			'{' => Delimiter::Brace,
			'<' => Delimiter::Angle,
			_ => unreachable!(),
		};

		let mut span = self.span;
		self.span.clear();

		let mut tokens = Vec::new();

		loop {
			self.skip_whitespaces()?;

			match self.peek()? {
				Some(')') | Some('}') | Some('>') => {
					let c = self.consume()?.unwrap();
					span.append(self.span);

					if c == delimiter.end() {
						return Ok(Loc::new(Token::Group(delimiter, tokens), span));
					} else {
						return Err(Loc::new(Error::WrongCloser(delimiter, c), span));
					}
				}
				Some(_) => {
					let token = self.parse_token()?.unwrap();
					span.append(token.span());
					tokens.push(token);
				}
				None => return Err(Loc::new(Error::MissingCloser(delimiter), span)),
			}
		}
	}

	fn parse_hex_char(&mut self, n: usize) -> Result<char> {
		let mut codepoint = 0;

		for _ in 0..n {
			if let Some(c) = self.consume()? {
				if let Some(d) = c.to_digit(16) {
					codepoint = (codepoint << 4) | d
				} else {
					return Err(Loc::new(Error::InvalidChar, self.span));
				}
			} else {
				return Err(Loc::new(
					Error::IO(io::Error::new(
						io::ErrorKind::UnexpectedEof,
						"unexpected end of stream",
					)),
					self.span,
				));
			}
		}

		match char::try_from(codepoint) {
			Ok(c) => Ok(c),
			Err(_) => Err(Loc::new(Error::InvalidChar, self.span)),
		}
	}

	fn parse_string(&mut self, delimiter: char) -> Result<Loc<Token>> {
		self.consume()?;

		let mut string = String::new();

		while let Some(c) = self.parse_string_char(delimiter)? {
			string.push(c)
		}

		Ok(Loc::new(Token::String(string, delimiter != '"'), self.span))
	}

	fn parse_string_char(&mut self, delimiter: char) -> Result<Option<char>> {
		if let Some(c) = self.consume()? {
			match c {
				_ if c == delimiter => return Ok(None),
				'\\' => {
					if let Some(c) = self.consume()? {
						match c {
							'r' => Ok(Some('\r')),
							'n' => Ok(Some('\n')),
							's' => Ok(Some(' ')),
							't' => Ok(Some('\t')),
							'x' => Ok(Some(self.parse_hex_char(2)?)),
							'u' => Ok(Some(self.parse_hex_char(4)?)),
							'U' => Ok(Some(self.parse_hex_char(8)?)),
							_ => Ok(Some(c)),
						}
					} else {
						return Err(Loc::new(Error::IncompleteCharSet, self.span));
					}
				}
				_ => Ok(Some(c)),
			}
		} else {
			return Err(Loc::new(Error::IncompleteCharSet, self.span));
		}
	}

	fn parse_charset(&mut self) -> Result<Loc<Token>> {
		self.consume()?;

		// let mut escape = false;
		let mut set = CharSet::new();

		let negate = if let Some('^') = self.peek()? {
			self.consume()?;
			true
		} else {
			false
		};

		loop {
			if let Some(first) = self.parse_string_char(']')? {
				let last = if let Some('-') = self.peek()? {
					self.consume()?;
					if let Some(last) = self.parse_string_char(']')? {
						last
					} else {
						return Err(Loc::new(Error::IncompleteCharSet, self.span));
					}
				} else {
					first
				};

				set.insert(first..=last);
			} else {
				break;
			}
		}

		Ok(Loc::new(Token::CharSet(set, negate), self.span))
	}

	fn parse_ident(&mut self) -> Result<Loc<Token>> {
		let mut id = String::new();

		loop {
			if let Some(c) = self.peek()? {
				if is_separator(c) {
					break;
				} else {
					self.consume()?;
					id.push(c);
				}
			} else {
				break;
			}
		}

		match id.as_ref() {
			"" => Err(Loc::new(
				Error::IO(io::Error::new(
					io::ErrorKind::UnexpectedEof,
					"unexpected end of stream",
				)),
				self.span,
			)),
			"extern" => Ok(Loc::new(Token::Keyword(Keyword::Extern), self.span)),
			"regexp" => Ok(Loc::new(Token::Keyword(Keyword::RegExp), self.span)),
			"type" => Ok(Loc::new(Token::Keyword(Keyword::Type), self.span)),
			_ => Ok(Loc::new(Token::Ident(id), self.span)),
		}
	}

	fn skip_whitespaces(&mut self) -> Result<()> {
		loop {
			match self.peek()? {
				Some(c) if is_space(c) => {
					self.consume()?;
				}
				Some('#') => loop {
					match self.consume()? {
						Some('\n') => break,
						None => break,
						_ => (),
					}
				},
				_ => break,
			}
		}

		Ok(())
	}

	fn parse_token(&mut self) -> Result<Option<Loc<Token>>> {
		self.skip_whitespaces()?;
		loop {
			self.span.clear();
			match self.peek()? {
				Some('(') | Some('{') | Some('<') => return Ok(Some(self.parse_group()?)),
				Some(')') | Some('}') | Some('>') => {
					let c = self.consume()?.unwrap();
					return Err(Loc::new(Error::Unexpected(c), self.span));
				}
				Some('[') => return Ok(Some(self.parse_charset()?)),
				Some('\'') => return Ok(Some(self.parse_string('\'')?)),
				Some('"') => return Ok(Some(self.parse_string('"')?)),
				Some(c) if is_punct(c) => {
					self.consume()?;
					return Ok(Some(Loc::new(Token::Punct(c), self.span)));
				}
				Some(_) => return Ok(Some(self.parse_ident()?)),
				None => break,
			}
		}

		Ok(None)
	}
}

impl<I: Iterator<Item = io::Result<char>>, M: Metrics> Iterator for Lexer<I, M> {
	type Item = Result<Loc<Token>>;

	fn next(&mut self) -> Option<Result<Loc<Token>>> {
		self.parse_token().transpose()
	}
}
