use std::fmt;
use super::{lexer, Located};

pub enum Error {
    Lexer(lexer::Error),
    UnexpectedEos,
    UnexpectedToken(lexer::Token),
    EmptyRegExp
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match self {
            Lexer(e) => write!(f, "{}", e),
            UnexpectedEos => write!(f, "unexpected end of stream"),
            UnexpectedToken(_) => write!(f, "unexpected token"),
            EmptyRegExp => write!(f, "empty regexp")
        }
    }
}

pub type Result<T> = std::result::Result<T, Located<Error>>;

impl From<Located<lexer::Error>> for Located<Error> {
    fn from(e: Located<lexer::Error>) -> Located<Error> {
        let span = e.span();
        Located::new(Error::Lexer(e.into_inner()), span)
    }
}
