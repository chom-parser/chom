use super::lexer;
use source_span::Loc;
use std::fmt;

pub enum Error {
	InvalidIdent(String),
	Lexer(lexer::Error),
	UnexpectedEos,
	UnexpectedToken(lexer::Token),
	EmptyRegExp,
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Error::*;
		match self {
			InvalidIdent(id) => write!(f, "invalid identifier `{}`", id),
			Lexer(e) => write!(f, "{}", e),
			UnexpectedEos => write!(f, "unexpected end of stream"),
			UnexpectedToken(_) => write!(f, "unexpected token"),
			EmptyRegExp => write!(f, "empty regexp"),
		}
	}
}

pub type Result<T> = std::result::Result<T, Loc<Error>>;

impl From<lexer::Error> for Error {
	fn from(e: lexer::Error) -> Error {
		Error::Lexer(e)
	}
}
