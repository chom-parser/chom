#[doc = r" Lexer."]
pub struct Lexer<I: ::std::iter::Iterator, M> {
	#[doc = r" Source character stream."]
	source: ::std::iter::Peekable<I>,
	#[doc = r" Token buffer."]
	buffer: ::std::string::String,
	#[doc = r" Character metrics."]
	metrics: M,
	#[doc = r" Token span."]
	span: ::source_span::Span,
}
pub enum Token {
	Operator(crate::test::lexer::Operator),
	Integer(crate::test::glue::Int),
}
pub enum Operator {
	Plus,
}
impl<
		E: ::std::convert::Into<crate::test::glue::Error>,
		I: ::std::iter::Iterator<Item = ::std::result::Result<char, E>>,
		M: ::source_span::Metrics,
	> Lexer<I, M>
{
	fn peek_char(
		&mut self,
	) -> ::std::result::Result<
		::std::option::Option<char>,
		::source_span::Loc<crate::test::glue::Error>,
	> {
		match self.source.peek() {
			Some(Ok(c)) => Ok(Some(*c)),
			Some(Err(_)) => Err(self.consume_char().unwrap_err()),
			None => Ok(None),
		}
	}
	fn consume_char(
		&mut self,
	) -> ::std::result::Result<(), ::source_span::Loc<crate::test::glue::Error>> {
		match self.source.next() {
			Some(Ok(c)) => {
				self.buffer.push(c);
				self.span.push(c, &self.metrics);
				Ok(())
			}
			Some(Err(e)) => Err(::source_span::Loc::new(e.into(), self.span.end().into())),
			None => Ok(()),
		}
	}
	fn next_token(
		&mut self,
	) -> ::std::result::Result<
		::std::option::Option<::source_span::Loc<Token>>,
		::source_span::Loc<crate::test::glue::Error>,
	> {
		self.buffer.clear();
		self.span.clear();
		let mut state = 0u32;
		loop {
			match state {
				0u32 => match self.peek_char()? {
					Some(c @ '-') => {
						self.consume_char()?;
						state = 1u32;
					}
					Some(c @ '+') => {
						self.consume_char()?;
						state = 2u32;
					}
					Some(c @ '0'..='9') => {
						self.consume_char()?;
						state = 3u32;
					}
					Some(c @ ('\t'..='\r' | ' ')) => {
						self.consume_char()?;
						state = 4u32;
					}
					unexpected => {
						break Err(::source_span::Loc::new(
							crate::test::glue::unexpected(unexpected),
							self.span,
						))
					}
				},
				1u32 => match self.peek_char()? {
					Some(c @ '0'..='9') => {
						self.consume_char()?;
						state = 3u32;
					}
					unexpected => {
						break Err(::source_span::Loc::new(
							crate::test::glue::unexpected(unexpected),
							self.span,
						))
					}
				},
				2u32 => match self.peek_char()? {
					Some(c @ '0'..='9') => {
						self.consume_char()?;
						state = 3u32;
					}
					_ => {
						break Ok(Some(::source_span::Loc::new(
							crate::test::lexer::Token::Operator(crate::test::lexer::Operator::Plus),
							self.span,
						)))
					}
				},
				3u32 => match self.peek_char()? {
					Some(c @ '0'..='9') => {
						self.consume_char()?;
						state = 3u32;
					}
					_ => {
						break Ok(Some(::source_span::Loc::new(
							crate::test::lexer::Token::Integer(
								crate::test::glue::integer(self.buffer.as_str())
									.map_err(|e| ::source_span::Loc::new(e, self.span))?,
							),
							self.span,
						)))
					}
				},
				4u32 => match self.peek_char()? {
					Some(c @ ('\t'..='\r' | ' ')) => {
						self.consume_char()?;
						state = 4u32;
					}
					_ => {
						self.buffer.clear();
						self.span.clear();
					}
				},
				_ => {
					unreachable!()
				}
			}
		}
	}
}
