pub enum Error {
	Lexer(crate::test::glue::Error),
	UnexpectedToken(crate::test::lexer::Token),
	UnexpectedEof,
}
impl ::std::convert::From<crate::test::glue::Error> for Error {
	fn from(e: crate::test::glue::Error) -> Error {
		Self::Lexer(e)
	}
}
pub fn parse_foo<
	L: ::std::iter::Iterator<
		Item = ::std::result::Result<
			::source_span::Loc<crate::test::lexer::Token>,
			::source_span::Loc<Error>,
		>,
	>,
>(
	lexer: &mut L,
) -> ::std::result::Result<::source_span::Loc<crate::test::ast::Foo>, ::source_span::Loc<Error>> {
	#![allow(unreachable_patterns)]
	use crate::test::lexer;
	use ::std::mem::ManuallyDrop;
	enum Node {
		Foo(crate::test::ast::Foo),
	}
	union AnyNode {
		foo: ManuallyDrop<crate::test::ast::Foo>,
	}
	union Data {
		string: ManuallyDrop<crate::test::glue::String>,
	}
	union Item {
		data: ManuallyDrop<Option<Data>>,
		node: ManuallyDrop<AnyNode>,
	}
	let mut stack = Vec::new();
	let mut node = None;
	let mut result_span = ::source_span::Span::default();
	let mut state = 0u32;
	loop {
		state = match state {
			0u32 => match node.take() {
				Some(n) => match n {
					Node::Foo(value) => {
						stack.push((
							Item {
								node: ManuallyDrop::new(AnyNode {
									foo: ManuallyDrop::new(value),
								}),
							},
							state,
						));
						2u32
					}
					_ => unreachable!(),
				},
				None => {
					let (token, span) = ::source_span::Loc::transposed(
						lexer.next().transpose().map_err(|e| e.inner_into())?,
						result_span.end().into(),
					)
					.into_raw_parts();
					result_span.append(span);
					match token {
						Some(lexer::Token::Keyword(lexer::Keyword::Keyword)) => {
							stack.push((
								Item {
									data: ManuallyDrop::new(None),
								},
								state,
							));
							1u32
						}
						unexpected => match unexpected {
							Some(token) => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(token),
									span,
								))
							}
							None => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedEof,
									result_span.end().into(),
								))
							}
						},
					}
				}
			},
			2u32 => match node.take() {
				Some(n) => match n {
					_ => unreachable!(),
				},
				None => {
					let (token, span) = ::source_span::Loc::transposed(
						lexer.next().transpose().map_err(|e| e.inner_into())?,
						result_span.end().into(),
					)
					.into_raw_parts();
					result_span.append(span);
					match token {
						None => 3u32,
						unexpected => match unexpected {
							Some(token) => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(token),
									span,
								))
							}
							None => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedEof,
									result_span.end().into(),
								))
							}
						},
					}
				}
			},
			3u32 => unsafe {
				let (result, _) = stack.pop().unwrap();
				break Ok(::source_span::Loc::new(
					ManuallyDrop::into_inner(ManuallyDrop::into_inner(result.node).foo),
					result_span,
				));
			},
			1u32 => match node.take() {
				Some(n) => match n {
					_ => unreachable!(),
				},
				None => {
					let (token, span) = ::source_span::Loc::transposed(
						lexer.next().transpose().map_err(|e| e.inner_into())?,
						result_span.end().into(),
					)
					.into_raw_parts();
					result_span.append(span);
					match token {
						Some(lexer::Token::Ident(value)) => {
							stack.push((
								Item {
									data: ManuallyDrop::new(Some(Data {
										string: ManuallyDrop::new(value),
									})),
								},
								state,
							));
							4u32
						}
						unexpected => match unexpected {
							Some(token) => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(token),
									span,
								))
							}
							None => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedEof,
									result_span.end().into(),
								))
							}
						},
					}
				}
			},
			4u32 => unsafe {
				let (a0, _) = stack.pop().unwrap();
				let (_, next_state) = stack.pop().unwrap();
				node = Some(Node::Foo(crate::test::ast::Foo::Keyword(
					ManuallyDrop::into_inner(ManuallyDrop::into_inner(a0.data).unwrap().string),
				)));
				next_state
			},
			_ => unreachable!(),
		}
	}
}
