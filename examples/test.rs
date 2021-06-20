mod glue {
    pub type Error = ();

    pub type Int = ();

    pub fn integer(str: &str) -> Result<Int, Error> {
        unimplemented!()
    }

    pub fn unexpected(c: Option<char>) -> Error {
        ()
    }
}

enum Operator {
    Plus,
}
enum Token {
    Integer(crate::glue::Int),
    Operator(Operator),
}
struct Lexer<I: ::std::iter::Iterator, M> {
    chars: ::std::iter::Peekable<I>,
    buffer: ::std::string::String,
    metrics: M,
    span: ::source_span::Span,
}
impl<
        E: ::std::convert::Into<crate::glue::Error>,
        I: ::std::iter::Iterator<Item = ::std::result::Result<char, E>>,
        M: ::source_span::Metrics,
    > Lexer<I, M>
{
    fn next_char(
        &mut self,
    ) -> ::std::result::Result<::std::option::Option<char>, ::source_span::Loc<crate::glue::Error>>
    {
        match self.chars.next() {
            Some(Ok(c)) => {
                self.buffer.push(c);
                self.span.push(c, &self.metrics);
                Ok(Some(c))
            }
            Some(Err(e)) => Err(::source_span::Loc::new(e.into(), self.span.end().into())),
            None => Ok(None),
        }
    }
    fn next_token(
        &mut self,
    ) -> ::std::result::Result<
        ::std::option::Option<::source_span::Loc<Token>>,
        ::source_span::Loc<crate::glue::Error>,
    > {
        let mut state = 0usize;
        self.buffer.clear();
        self.span.clear();
        loop {
            match state {
                0usize => {
                    let next_c = self.next_char()?;
                    match next_c {
                        Some(c) => match c {
                            '-' => state = 1usize,
                            '+' => state = 2usize,
                            '0'..='9' => state = 3usize,
                            '\t'..='\r' | ' ' => state = 4usize,
                            _ => {
                                break Err(::source_span::Loc::new(
                                    crate::glue::unexpected(next_c),
                                    self.span.last().into(),
                                ))
                            }
                        },
                        None => break Ok(None),
                    }
                }
                1usize => {
                    let next_c = self.next_char()?;
                    match next_c {
                        Some(c) => match c {
                            '0'..='9' => state = 3usize,
                            _ => {
                                break Err(::source_span::Loc::new(
                                    crate::glue::unexpected(next_c),
                                    self.span.last().into(),
                                ))
                            }
                        },
                        None => {
                            break Err(::source_span::Loc::new(
                                crate::glue::unexpected(next_c),
                                self.span.last().into(),
                            ))
                        }
                    }
                }
                2usize => {
                    let next_c = self.next_char()?;
                    match next_c {
                        Some(c) => match c {
                            '0'..='9' => state = 3usize,
                            _ => {
                                break Ok(Some(::source_span::Loc::new(
                                    Token::Operator(Operator::Plus),
                                    self.span,
                                )))
                            }
                        },
                        None => {
                            break Ok(Some(::source_span::Loc::new(
                                Token::Operator(Operator::Plus),
                                self.span,
                            )))
                        }
                    }
                }
                3usize => {
                    let next_c = self.next_char()?;
                    match next_c {
                        Some(c) => match c {
                            '0'..='9' => state = 3usize,
                            _ => {
                                break Ok(Some(::source_span::Loc::new(
                                    Token::Integer(
                                        crate::glue::integer(self.buffer.as_str())
                                            .map_err(|e| ::source_span::Loc::new(e, self.span))?,
                                    ),
                                    self.span,
                                )))
                            }
                        },
                        None => {
                            break Ok(Some(::source_span::Loc::new(
                                Token::Integer(
                                    crate::glue::integer(self.buffer.as_str())
                                        .map_err(|e| ::source_span::Loc::new(e, self.span))?,
                                ),
                                self.span,
                            )))
                        }
                    }
                }
                4usize => {
                    let next_c = self.next_char()?;
                    match next_c {
                        Some(c) => match c {
                            '\t'..='\r' | ' ' => state = 4usize,
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
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}
impl<
        E: ::std::convert::Into<crate::glue::Error>,
        I: ::std::iter::Iterator<Item = ::std::result::Result<char, E>>,
        M: ::source_span::Metrics,
    > ::std::iter::Iterator for Lexer<I, M>
{
    type Item =
        ::std::result::Result<::source_span::Loc<Token>, ::source_span::Loc<crate::glue::Error>>;
    fn next(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<::source_span::Loc<Token>, ::source_span::Loc<crate::glue::Error>>,
    > {
        self.next_token().transpose()
    }
}

fn main() {
    // ...
}