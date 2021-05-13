use core::{iter::Peekable, marker::PhantomData};
use source_span::{Loc, Metrics, Span};
pub enum Operator {
    Plus,
}
pub enum Token<T: Interface> {
    Integer(T::Int),
    Operator(Operator),
}
pub trait Interface {
    type Error;
    fn unexpected(c: Option<char>) -> Self::Error;
    type Int;
    fn integer(token: &str) -> Result<Self::Int, Self::Error>;
}
pub struct Lexer<I: Iterator, M, T: Interface> {
    #[doc = r" Char stream."]
    chars: Peekable<I>,
    #[doc = r" Token data buffer."]
    buffer: String,
    #[doc = r" Character metrics."]
    metrics: M,
    #[doc = r" Token span."]
    span: Span,
    #[doc = r" Glue."]
    t: PhantomData<T>,
}
impl<E, I: Iterator<Item = Result<char, E>>, M: Metrics, T: Interface> Lexer<I, M, T>
where
    E: Into<T::Error>,
{
    fn consume_char(&mut self) -> Result<Option<char>, Loc<T::Error>> {
        match self.chars.next() {
            Some(Ok(c)) => {
                self.buffer.push(c);
                self.span.push(c, &self.metrics);
                Ok(Some(c))
            }
            Some(Err(e)) => Err(Loc::new(e.into(), self.span.end().into())),
            None => Ok(None),
        }
    }
    fn peek_char(&mut self) -> Result<Option<char>, Loc<T::Error>> {
        match self.chars.peek() {
            Some(Ok(c)) => Ok(Some(*c)),
            Some(Err(_)) => self.consume_char(),
            None => Ok(None),
        }
    }
    pub fn next_token(&mut self) -> Result<Option<Loc<Token<T>>>, Loc<T::Error>> {
        let mut state = 0usize;
        self.buffer.clear();
        self.span.clear();
        loop {
            match state {
                0usize => match self.peek_char()? {
                    Some(c) => match c {
                        '-' => {
                            self.consume_char()?;
                            state = 1usize
                        }
                        '0'..='9' => {
                            self.consume_char()?;
                            state = 2usize
                        }
                        '+' => {
                            self.consume_char()?;
                            state = 3usize
                        }
                        '\t'..='\r' | ' ' => {
                            self.consume_char()?;
                            state = 4usize
                        }
                        _ => {
                            let c = self.consume_char()?;
                            break Err(Loc::new(T::unexpected(c), self.span.last().into()));
                        }
                    },
                    None => break Ok(None),
                },
                1usize => match self.peek_char()? {
                    Some(c) => match c {
                        '0'..='9' => {
                            self.consume_char()?;
                            state = 2usize
                        }
                        _ => {
                            let c = self.consume_char()?;
                            break Err(Loc::new(T::unexpected(c), self.span.last().into()));
                        }
                    },
                    None => {
                        let c = self.consume_char()?;
                        break Err(Loc::new(T::unexpected(c), self.span.last().into()));
                    }
                },
                2usize => match self.peek_char()? {
                    Some(c) => match c {
                        '0'..='9' => {
                            self.consume_char()?;
                            state = 2usize
                        }
                        _ => {
                            break Ok(Some(Loc::new(
                                Token::Integer(
                                    T::integer(self.buffer.as_str())
                                        .map_err(|e| Loc::new(e, self.span))?,
                                ),
                                self.span,
                            )))
                        }
                    },
                    None => {
                        break Ok(Some(Loc::new(
                            Token::Integer(
                                T::integer(self.buffer.as_str())
                                    .map_err(|e| Loc::new(e, self.span))?,
                            ),
                            self.span,
                        )))
                    }
                },
                3usize => match self.peek_char()? {
                    Some(c) => match c {
                        '0'..='9' => {
                            self.consume_char()?;
                            state = 2usize
                        }
                        _ => break Ok(Some(Loc::new(Token::Operator(Operator::Plus), self.span))),
                    },
                    None => break Ok(Some(Loc::new(Token::Operator(Operator::Plus), self.span))),
                },
                4usize => match self.peek_char()? {
                    Some(c) => match c {
                        '\t'..='\r' | ' ' => {
                            self.consume_char()?;
                            state = 4usize
                        }
                        _ => {
                            state = 0usize;
                            self.buffer.clear();
                            self.span.clear();
                        }
                    },
                    None => {
                        state = 0usize;
                        self.buffer.clear();
                        self.span.clear();
                    }
                },
                _ => unreachable!(),
            }
        }
    }
}
impl<E, I: Iterator<Item = Result<char, E>>, M: Metrics, T: Interface> Iterator for Lexer<I, M, T>
where
    E: Into<T::Error>,
{
    type Item = Result<Loc<Token<T>>, Loc<T::Error>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

fn main() {
    // ...
}