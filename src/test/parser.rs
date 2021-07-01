#[doc = r" Parsing errors."]
pub enum Error {
	#[doc = r" Error comming from the lexer."]
	Lexer(crate::test::glue::Error),
	#[doc = r" Unexpected lexer token."]
	UnexpectedToken(Option<crate::test::lexer::Token>),
	#[doc = r" Unexpected AST node."]
	UnexpectedNode(Node),
}
impl From<crate::test::glue::Error> for Error {
	fn from(e: crate::test::glue::Error) -> Self {
		Self::Lexer(e)
	}
}
pub enum Node {
	Expr(crate::test::ast::Expr),
	Term(crate::test::ast::Term),
}
pub enum Item {
	Token(crate::test::lexer::Token),
	Node(crate::test::parser::Node),
}
fn parse_term<
	L: ::std::iter::Iterator<
		Item = ::std::result::Result<
			::source_span::Loc<crate::test::lexer::Token>,
			::source_span::Loc<Error>,
		>,
	>,
>(
	lexer: &mut L,
) -> ::std::result::Result<::source_span::Loc<crate::test::ast::Term>, ::source_span::Loc<Error>> {
	let mut position = ::source_span::Position::default();
	let mut stack = Vec::new();
	let mut any_node_opt = None;
	let mut state = 1u32;
	loop {
		match state {
			1u32 => {
				let (any_node_opt_spanless, span) =
					::source_span::Loc::transposed(any_node_opt, position.into()).into_raw_parts();
				match any_node_opt_spanless {
					Some(any_node) => match any_node {
						crate::test::parser::Node::Term(node) => {
							stack.push((
								::source_span::Loc::new(
									crate::test::parser::Item::Node(
										crate::test::parser::Node::Term(node),
									),
									span,
								),
								state,
							));
							state = 3u32;
						}
						unexpected => {
							break Err(::source_span::Loc::new(
								Error::UnexpectedNode(unexpected),
								span,
							))
						}
					},
					None => {
						let any_token_opt = lexer.next().transpose().map_err(|e| e.inner_into())?;
						let (any_token_opt_spanless, span) =
							::source_span::Loc::transposed(any_token_opt, position.into())
								.into_raw_parts();
						match any_token_opt_spanless {
							Some(crate::test::lexer::Token::Integer(token)) => {
								stack.push((
									::source_span::Loc::new(
										crate::test::parser::Item::Token(
											crate::test::lexer::Token::Integer(token),
										),
										span,
									),
									state,
								));
								state = 2u32;
							}
							unexpected => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(unexpected),
									span,
								))
							}
						}
					}
				}
			}
			3u32 => {
				let (any_node_opt_spanless, span) =
					::source_span::Loc::transposed(any_node_opt, position.into()).into_raw_parts();
				match any_node_opt_spanless {
					Some(any_node) => match any_node {
						unexpected => {
							break Err(::source_span::Loc::new(
								Error::UnexpectedNode(unexpected),
								span,
							))
						}
					},
					None => {
						let any_token_opt = lexer.next().transpose().map_err(|e| e.inner_into())?;
						let (any_token_opt_spanless, span) =
							::source_span::Loc::transposed(any_token_opt, position.into())
								.into_raw_parts();
						match any_token_opt_spanless {
							None => {
								state = 4u32;
							}
							unexpected => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(unexpected),
									span,
								))
							}
						}
					}
				}
			}
			4u32 => {
				let (any_item0, saved_state) = stack.pop().unwrap();
				let (item0_spanless, item0_span) = any_item0.into_raw_parts();
				if let crate::test::parser::Item::Node(crate::test::parser::Node::Term(result)) =
					item0_spanless
				{
					let span = item0_span;
					break Ok(::source_span::Loc::new(result, span));
				} else {
					unreachable!()
				}
			}
			2u32 => {
				let (any_item0, saved_state) = stack.pop().unwrap();
				let (item0_spanless, item0_span) = any_item0.into_raw_parts();
				if let crate::test::parser::Item::Token(crate::test::lexer::Token::Integer(item0)) =
					item0_spanless
				{
					let span = item0_span;
					any_node_opt = Some(::source_span::Loc::new(
						crate::test::parser::Node::Term(crate::test::ast::Term(
							::source_span::Loc::new(item0, item0_span),
						)),
						span,
					));
					state = saved_state;
				} else {
					unreachable!()
				}
			}
		}
	}
}
fn parse_expr<
	L: ::std::iter::Iterator<
		Item = ::std::result::Result<
			::source_span::Loc<crate::test::lexer::Token>,
			::source_span::Loc<Error>,
		>,
	>,
>(
	lexer: &mut L,
) -> ::std::result::Result<::source_span::Loc<crate::test::ast::Expr>, ::source_span::Loc<Error>> {
	let mut position = ::source_span::Position::default();
	let mut stack = Vec::new();
	let mut any_node_opt = None;
	let mut state = 0u32;
	loop {
		match state {
			0u32 => {
				let (any_node_opt_spanless, span) =
					::source_span::Loc::transposed(any_node_opt, position.into()).into_raw_parts();
				match any_node_opt_spanless {
					Some(any_node) => match any_node {
						crate::test::parser::Node::Expr(node) => {
							stack.push((
								::source_span::Loc::new(
									crate::test::parser::Item::Node(
										crate::test::parser::Node::Expr(node),
									),
									span,
								),
								state,
							));
							state = 5u32;
						}
						crate::test::parser::Node::Term(node) => {
							stack.push((
								::source_span::Loc::new(
									crate::test::parser::Item::Node(
										crate::test::parser::Node::Term(node),
									),
									span,
								),
								state,
							));
							state = 6u32;
						}
					},
					None => {
						let any_token_opt = lexer.next().transpose().map_err(|e| e.inner_into())?;
						let (any_token_opt_spanless, span) =
							::source_span::Loc::transposed(any_token_opt, position.into())
								.into_raw_parts();
						match any_token_opt_spanless {
							Some(crate::test::lexer::Token::Integer(token)) => {
								stack.push((
									::source_span::Loc::new(
										crate::test::parser::Item::Token(
											crate::test::lexer::Token::Integer(token),
										),
										span,
									),
									state,
								));
								state = 2u32;
							}
							unexpected => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(unexpected),
									span,
								))
							}
						}
					}
				}
			}
			6u32 => {
				let (any_item0, saved_state) = stack.pop().unwrap();
				let (item0_spanless, item0_span) = any_item0.into_raw_parts();
				if let crate::test::parser::Item::Node(crate::test::parser::Node::Term(item0)) =
					item0_spanless
				{
					let span = item0_span;
					any_node_opt = Some(::source_span::Loc::new(
						crate::test::parser::Node::Expr(crate::test::ast::Expr::Term(
							::source_span::Loc::new(item0, item0_span),
						)),
						span,
					));
					state = saved_state;
				} else {
					unreachable!()
				}
			}
			5u32 => {
				let (any_node_opt_spanless, span) =
					::source_span::Loc::transposed(any_node_opt, position.into()).into_raw_parts();
				match any_node_opt_spanless {
					Some(any_node) => match any_node {
						unexpected => {
							break Err(::source_span::Loc::new(
								Error::UnexpectedNode(unexpected),
								span,
							))
						}
					},
					None => {
						let any_token_opt = lexer.next().transpose().map_err(|e| e.inner_into())?;
						let (any_token_opt_spanless, span) =
							::source_span::Loc::transposed(any_token_opt, position.into())
								.into_raw_parts();
						match any_token_opt_spanless {
							None => {
								state = 8u32;
							}
							Some(crate::test::lexer::Token::Operator(
								crate::test::lexer::Operator::Plus,
							)) => {
								stack.push((
									::source_span::Loc::new(
										crate::test::parser::Item::Token(
											crate::test::lexer::Token::Operator(
												crate::test::lexer::Operator::Plus,
											),
										),
										span,
									),
									state,
								));
								state = 7u32;
							}
							unexpected => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(unexpected),
									span,
								))
							}
						}
					}
				}
			}
			7u32 => {
				let (any_node_opt_spanless, span) =
					::source_span::Loc::transposed(any_node_opt, position.into()).into_raw_parts();
				match any_node_opt_spanless {
					Some(any_node) => match any_node {
						crate::test::parser::Node::Term(node) => {
							stack.push((
								::source_span::Loc::new(
									crate::test::parser::Item::Node(
										crate::test::parser::Node::Term(node),
									),
									span,
								),
								state,
							));
							state = 9u32;
						}
						unexpected => {
							break Err(::source_span::Loc::new(
								Error::UnexpectedNode(unexpected),
								span,
							))
						}
					},
					None => {
						let any_token_opt = lexer.next().transpose().map_err(|e| e.inner_into())?;
						let (any_token_opt_spanless, span) =
							::source_span::Loc::transposed(any_token_opt, position.into())
								.into_raw_parts();
						match any_token_opt_spanless {
							Some(crate::test::lexer::Token::Integer(token)) => {
								stack.push((
									::source_span::Loc::new(
										crate::test::parser::Item::Token(
											crate::test::lexer::Token::Integer(token),
										),
										span,
									),
									state,
								));
								state = 2u32;
							}
							unexpected => {
								break Err(::source_span::Loc::new(
									Error::UnexpectedToken(unexpected),
									span,
								))
							}
						}
					}
				}
			}
			9u32 => {
				let (any_item2, _) = stack.pop().unwrap();
				stack.pop();
				let (any_item0, saved_state) = stack.pop().unwrap();
				let (item2_spanless, item2_span) = any_item2.into_raw_parts();
				let (item0_spanless, item0_span) = any_item0.into_raw_parts();
				if let crate::test::parser::Item::Node(crate::test::parser::Node::Term(item2)) =
					item2_spanless
				{
					if let crate::test::parser::Item::Node(crate::test::parser::Node::Expr(item0)) =
						item0_spanless
					{
						let span = item0_span.union(item2_span);
						any_node_opt = Some(::source_span::Loc::new(
							crate::test::parser::Node::Expr(crate::test::ast::Expr::Add(
								Box::new(::source_span::Loc::new(item0, item0_span)),
								::source_span::Loc::new(item2, item2_span),
							)),
							span,
						));
						state = saved_state;
					} else {
						unreachable!()
					}
				} else {
					unreachable!()
				}
			}
			2u32 => {
				let (any_item0, saved_state) = stack.pop().unwrap();
				let (item0_spanless, item0_span) = any_item0.into_raw_parts();
				if let crate::test::parser::Item::Token(crate::test::lexer::Token::Integer(item0)) =
					item0_spanless
				{
					let span = item0_span;
					any_node_opt = Some(::source_span::Loc::new(
						crate::test::parser::Node::Term(crate::test::ast::Term(
							::source_span::Loc::new(item0, item0_span),
						)),
						span,
					));
					state = saved_state;
				} else {
					unreachable!()
				}
			}
			8u32 => {
				let (any_item0, saved_state) = stack.pop().unwrap();
				let (item0_spanless, item0_span) = any_item0.into_raw_parts();
				if let crate::test::parser::Item::Node(crate::test::parser::Node::Expr(result)) =
					item0_spanless
				{
					let span = item0_span;
					break Ok(::source_span::Loc::new(result, span));
				} else {
					unreachable!()
				}
			}
		}
	}
}
