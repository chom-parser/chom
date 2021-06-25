pub enum Keyword {
	Keyword,
}
pub enum Token {
	Ident(crate::test::glue::String),
	Keyword(Keyword),
}
pub struct Lexer<I: ::std::iter::Iterator, M> {
	chars: ::std::iter::Peekable<I>,
	buffer: ::std::string::String,
	metrics: M,
	span: ::source_span::Span,
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
		match self.chars.peek() {
			Some(Ok(c)) => Ok(Some(*c)),
			Some(Err(_)) => Err(self.consume_char().unwrap_err()),
			None => Ok(None),
		}
	}
	fn consume_char(
		&mut self,
	) -> ::std::result::Result<(), ::source_span::Loc<crate::test::glue::Error>> {
		match self.chars.next() {
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
		'next_token: loop {
			self.buffer.clear();
			self.span.clear();
			let mut state = 0usize;
			loop {
				match state {
					0usize => {
						let next_c = self.peek_char()?;
						match next_c {
							Some(c) => match c {
								'A'..='Z' | 'a'..='z' => {
									self.consume_char()?;
									state = 1usize;
									continue;
								}
								'\t'..='\r' | ' ' => {
									self.consume_char()?;
									state = 2usize;
									continue;
								}
								_ => (),
							},
							None => return Ok(None),
						}
						return Err(::source_span::Loc::new(
							crate::test::glue::unexpected(next_c),
							self.span.last().into(),
						));
					}
					1usize => {
						let next_c = self.peek_char()?;
						match next_c {
							Some(c) => match c {
								'0'..='9' | 'A'..='Z' | 'a'..='z' => {
									self.consume_char()?;
									state = 1usize;
									continue;
								}
								_ => (),
							},
							None => (),
						}
						let mut chars = self.buffer.chars();
						let mut state = 0usize;
						loop {
							match state {
								0usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'k' => {
												state = 1usize;
												continue;
											}
											_ => (),
										},
										None => return Ok(None),
									}
									break;
								}
								1usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'e' => {
												state = 2usize;
												continue;
											}
											_ => (),
										},
										None => (),
									}
									break;
								}
								2usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'y' => {
												state = 3usize;
												continue;
											}
											_ => (),
										},
										None => (),
									}
									break;
								}
								3usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'w' => {
												state = 4usize;
												continue;
											}
											_ => (),
										},
										None => (),
									}
									break;
								}
								4usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'o' => {
												state = 5usize;
												continue;
											}
											_ => (),
										},
										None => (),
									}
									break;
								}
								5usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'r' => {
												state = 6usize;
												continue;
											}
											_ => (),
										},
										None => (),
									}
									break;
								}
								6usize => {
									let next_c = chars.next();
									match next_c {
										Some(c) => match c {
											'd' => {
												state = 7usize;
												continue;
											}
											_ => (),
										},
										None => (),
									}
									break;
								}
								_ => unreachable!(),
							}
						}
						return Ok(Some(::source_span::Loc::new(
							Token::Ident(
								crate::test::glue::ident(self.buffer.as_str())
									.map_err(|e| ::source_span::Loc::new(e, self.span))?,
							),
							self.span,
						)));
					}
					2usize => {
						let next_c = self.peek_char()?;
						match next_c {
							Some(c) => match c {
								'\t'..='\r' | ' ' => {
									self.consume_char()?;
									state = 2usize;
									continue;
								}
								_ => (),
							},
							None => (),
						}
						continue 'next_token;
					}
					_ => unreachable!(),
				}
			}
		}
	}
}
impl<
		E: ::std::convert::Into<crate::test::glue::Error>,
		I: ::std::iter::Iterator<Item = ::std::result::Result<char, E>>,
		M: ::source_span::Metrics,
	> ::std::iter::Iterator for Lexer<I, M>
{
	type Item = ::std::result::Result<
		::source_span::Loc<Token>,
		::source_span::Loc<crate::test::glue::Error>,
	>;
	fn next(
		&mut self,
	) -> ::std::option::Option<
		::std::result::Result<
			::source_span::Loc<Token>,
			::source_span::Loc<crate::test::glue::Error>,
		>,
	> {
		self.next_token().transpose()
	}
}
