use super::super::{id, Context, Expr, Id, Label, Pattern};
use crate::{
	mono,
	parsing::table::{self, lr0::State, Rule},
};
use chom_ir::{
	expr::{Error, MatchCase, SpanExpr, StackExpr, StreamExpr},
	Constant,
};
use std::collections::HashSet;

/// ## Pseudo code
///
/// Operation prefixed by `#` are statically evaluated.
/// ```pseudo
/// let stack = empty_stack in
/// let node = None in
/// let state = initial_state in
/// 'parser tail_recusion lexer stack node state {
/// 	match state { // for each parser state `q` reachable from `initial_state`.
/// 		q => {
/// 			#if reduce {
/// 				let (stack, (a1, next_state), ..., (an, _)) = stack.pop(n) in
/// 				let node = Some(node(a1, ..., an)) in
/// 				let state = next_state in
/// 				recurse 'parser lexer stack node state
/// 			} else { // shift.
/// 				match node {
/// 					Some(node) => { // goto
/// 						match node {
/// 							n => { // for each expected node `n`
/// 								stack.push(n);
/// 								let node = None;
/// 								let state = next_state in
/// 								recurse 'parser lexer stack node state
/// 							},
/// 							unexpected => {
/// 								Err(UnexpectedNode(node))
/// 							}
/// 						}
/// 					},
/// 					None => {
/// 						let (lexer, token) = lexer.next in
/// 						match token { // shift
/// 							Some(token) => match token { // for each expected token.
/// 								let stack = stack.push(token) in
/// 								let state = #next_state in
/// 								recurse 'parser lexer stack node state
/// 							},
/// 							None => { // if end-of-stream is expected.
/// 								let (_, (result, _)) = stack.pop(1) in
/// 								Ok(result)
/// 							},
/// 							unexpected => {
/// 								match unexpected {
/// 									Some(token) => Err(UnexpectedToken(token)),
/// 									None => Err(UnexpectedEOS)
/// 								}
/// 							}
/// 						}
/// 					}
/// 				}
/// 			}
/// 		}
/// 	}
/// }
/// ```
pub fn generate<'a, 'p>(
	context: &Context<'a, 'p>,
	table: &table::LR0,
	initial_state: u32,
) -> Expr<'a, 'p> {
	let mut cases = Vec::new();
	let mut stack = vec![initial_state];
	let mut visited = HashSet::new();
	while let Some(q) = stack.pop() {
		if visited.insert(q) {
			cases.push(MatchCase {
				pattern: Pattern::Literal(Constant::Int(q)),
				expr: generate_state(context, table, &mut stack, q),
			})
		}
	}

	cases.push(MatchCase {
		pattern: Pattern::Any,
		expr: Expr::Unreachable,
	});

	let loop_args = if context.config().locate {
		vec![
			(Id::Parser(id::Parser::Lexer), true),
			(Id::Parser(id::Parser::Position), true),
			(Id::Parser(id::Parser::Stack), true),
			(Id::Parser(id::Parser::AnyNodeOpt), true),
			(Id::Parser(id::Parser::State), true),
		]
	} else {
		vec![
			(Id::Parser(id::Parser::Lexer), true),
			(Id::Parser(id::Parser::Stack), true),
			(Id::Parser(id::Parser::AnyNodeOpt), true),
			(Id::Parser(id::Parser::State), true),
		]
	};

	let expr = Expr::Let(
		Id::Parser(id::Parser::Stack),
		true,
		Box::new(Expr::empty_stack()),
		Box::new(Expr::Let(
			Id::Parser(id::Parser::AnyNodeOpt),
			true,
			Box::new(Expr::none()),
			Box::new(Expr::Let(
				Id::Parser(id::Parser::State),
				true,
				Box::new(Expr::Literal(Constant::Int(initial_state))),
				Box::new(Expr::TailRecursion {
					label: Label::Parser,
					args: loop_args,
					body: Box::new(Expr::Match {
						expr: Box::new(Expr::Get(Id::Parser(id::Parser::State))),
						cases,
					}),
				}),
			)),
		)),
	);

	if context.config().locate {
		Expr::Let(
			Id::Parser(id::Parser::Position),
			true,
			Box::new(Expr::nowhere()),
			Box::new(expr),
		)
	} else {
		expr
	}
}

fn stack_pop<'a, 'p>(
	context: &Context<'a, 'p>,
	patterns: &[Pattern<'a, 'p>],
	next: Expr<'a, 'p>,
	returning: bool,
) -> Expr<'a, 'p> {
	let mut expr = next;
	let last = (patterns.len() - 1) as u32;

	if context.config().locate {
		if !returning {
			expr = Expr::Update(
				Id::Parser(id::Parser::Position),
				Box::new(Expr::Span(SpanExpr::After(Box::new(Expr::Get(
					Id::Parser(id::Parser::Span),
				))))),
				Box::new(expr),
			);
		}

		expr = if last > 0 {
			Expr::Let(
				Id::Parser(id::Parser::Span),
				false,
				Box::new(Expr::Span(SpanExpr::Merge(
					Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(0)))),
					Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(last)))),
				))),
				Box::new(expr),
			)
		} else {
			Expr::Let(
				Id::Parser(id::Parser::Span),
				false,
				Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(0)))),
				Box::new(expr),
			)
		}
	}

	for (i, pattern) in patterns.iter().enumerate() {
		if pattern.is_bound() {
			let i = i as u32;
			expr = Expr::LetMatch(
				pattern.clone(),
				Box::new(Expr::Get(Id::Parser(if context.config().locate {
					id::Parser::AnyItemSpanless(i)
				} else {
					id::Parser::AnyItem(i)
				}))),
				Box::new(expr),
			)
		}
	}

	if context.config().locate {
		for (i, pattern) in patterns.iter().enumerate() {
			let i = i as u32;
			let bound = pattern.is_bound();
			if bound || (i == 0 || i == last) {
				expr = Expr::Span(SpanExpr::Unwrap(
					if bound {
						Some(Id::Parser(id::Parser::AnyItemSpanless(i)))
					} else {
						None
					},
					Some(Id::Parser(id::Parser::AnyItemSpan(i))),
					Box::new(Expr::Get(Id::Parser(id::Parser::AnyItem(i)))),
					Box::new(expr),
				))
			}
		}
	}

	for (i, pattern) in patterns.iter().enumerate() {
		let i = i as u32;
		let value = if pattern.is_bound() || (context.config().locate && (i == 0 || i == last)) {
			Some(Id::Parser(id::Parser::AnyItem(i)))
		} else {
			None
		};

		let state = if i == 0 && !returning {
			Some(Id::Parser(id::Parser::SavedState))
		} else {
			None
		};

		expr = Expr::Stack(
			Id::Parser(id::Parser::Stack),
			StackExpr::Pop(value, state, Box::new(expr)),
		)
	}

	expr
}

fn generate_state<'a, 'p>(
	context: &Context<'a, 'p>,
	table: &table::LR0,
	stack: &mut Vec<u32>,
	q: u32,
) -> Expr<'a, 'p> {
	let recurse_args = if context.config().locate {
		vec![
			Id::Parser(id::Parser::Lexer),
			Id::Parser(id::Parser::Position),
			Id::Parser(id::Parser::Stack),
			Id::Parser(id::Parser::AnyNodeOpt),
			Id::Parser(id::Parser::State),
		]
	} else {
		vec![
			Id::Parser(id::Parser::Lexer),
			Id::Parser(id::Parser::Stack),
			Id::Parser(id::Parser::AnyNodeOpt),
			Id::Parser(id::Parser::State),
		]
	};
	let state = table.state(q).unwrap();

	match state {
		State::Reduce(rule) => match rule {
			Rule::Initial(ty_index) => {
				let pattern = context
					.provided()
					.item_node_pattern(*ty_index, Id::Parser(id::Parser::Result));
				let mut result = Expr::Get(Id::Parser(id::Parser::Result));
				if context.config().locate {
					result = Expr::locate(result, Expr::Get(Id::Parser(id::Parser::Span)))
				}
				stack_pop(context, &[pattern], Expr::Return(vec![Expr::ok(result)]), true)
			}
			Rule::Function(f_index) => {
				let f = context.grammar().function(*f_index).unwrap();
				let patterns: Vec<_> = f
					.arguments()
					.iter()
					.enumerate()
					.map(|(i, a)| {
						let i = i as u32;
						match a {
							mono::ty::Expr::Terminal(index) => context
								.provided()
								.item_token_pattern(*index, || Id::Parser(id::Parser::Item(i))),
							mono::ty::Expr::Type(index) => context
								.provided()
								.item_node_pattern(*index, Id::Parser(id::Parser::Item(i))),
						}
					})
					.collect();

				let mut expr = context.provided().node_expr(
					f.return_ty(),
					context.constructor_expr(*f_index, |i| {
						let mut arg_expr = Expr::Get(Id::Parser(id::Parser::Item(i)));

						if context.config().locate {
							arg_expr = Expr::locate(
								arg_expr,
								Expr::Get(Id::Parser(id::Parser::AnyItemSpan(i))),
							)
						}

						if f.put_argument_on_heap(context.grammar(), f.return_ty(), i) {
							arg_expr = Expr::Heap(Box::new(arg_expr))
						}

						arg_expr
					}),
				);

				if context.config().locate {
					expr = Expr::locate(expr, Expr::Get(Id::Parser(id::Parser::Span)))
				}

				stack_pop(
					context,
					&patterns,
					Expr::Update(
						Id::Parser(id::Parser::AnyNodeOpt),
						Box::new(Expr::some(expr)),
						Box::new(Expr::Update(
							Id::Parser(id::Parser::State),
							Box::new(Expr::Get(Id::Parser(id::Parser::SavedState))),
							Box::new(Expr::Recurse(Label::Parser, recurse_args.clone())),
						)),
					),
					false,
				)
			}
		},
		State::Shift { action, goto } => {
			let mut action_cases: Vec<_> = action
				.iter()
				.map(|(index, next_state)| {
					stack.push(*next_state);
					let next_state_expr = Expr::Update(
						Id::Parser(id::Parser::AnyNodeOpt),
						Box::new(Expr::none()),
						Box::new(Expr::Update(
							Id::Parser(id::Parser::State),
							Box::new(Expr::Literal(Constant::Int(*next_state))),
							Box::new(Expr::Recurse(Label::Parser, recurse_args.clone())),
						)),
					);

					match index {
						Some(index) => {
							let pattern = context
								.provided()
								.token_pattern(*index, || Id::Parser(id::Parser::Token));
							let mut expr = context.provided().item_token_expr(*index, |_| {
								Expr::Get(Id::Parser(id::Parser::Token))
							});

							if context.config().locate {
								expr = Expr::locate(expr, Expr::Get(Id::Parser(id::Parser::Span)))
							}

							MatchCase {
								pattern: Pattern::some(pattern),
								expr: Expr::Stack(
									Id::Parser(id::Parser::Stack),
									StackExpr::Push(
										Box::new(expr),
										Box::new(Expr::Get(Id::Parser(id::Parser::State))),
										Box::new(next_state_expr),
									),
								),
							}
						}
						None => MatchCase {
							pattern: Pattern::none(),
							expr: next_state_expr,
						},
					}
				})
				.collect();

			if action_cases.len() < context.provided().token_count() + 1 {
				let mut err = context.provided().unexpected_token_expr(Expr::Get(Id::Parser(
					id::Parser::Unexpected,
				)));
				if context.config().locate {
					err = Expr::locate(err, Expr::Get(Id::Parser(id::Parser::Span)))
				}
				action_cases.push(MatchCase {
					pattern: Pattern::Bind(Id::Parser(id::Parser::Unexpected)),
					expr: Expr::Return(vec![Expr::err(err)]),
				})
			}

			let mut goto_cases: Vec<_> = goto
				.iter()
				.map(|(index, next_state)| {
					stack.push(*next_state);
					let pattern = context
						.provided()
						.node_pattern(*index, Id::Parser(id::Parser::Node));
					let mut expr = context
						.provided()
						.item_node_expr(*index, Expr::Get(Id::Parser(id::Parser::Node)));

					if context.config().locate {
						expr = Expr::locate(expr, Expr::Get(Id::Parser(id::Parser::Span)))
					}

					MatchCase {
						pattern,
						expr: Expr::Stack(
							Id::Parser(id::Parser::Stack),
							StackExpr::Push(
								Box::new(expr),
								Box::new(Expr::Get(Id::Parser(id::Parser::State))),
								Box::new(Expr::Update(
									Id::Parser(id::Parser::AnyNodeOpt),
									Box::new(Expr::none()),
									Box::new(Expr::Update(
										Id::Parser(id::Parser::State),
										Box::new(Expr::Literal(Constant::Int(*next_state))),
										Box::new(Expr::Recurse(
											Label::Parser,
											recurse_args.clone(),
										)),
									)),
								)),
							),
						),
					}
				})
				.collect();

			if (goto_cases.len() as u32) < context.grammar().type_count() {
				let mut err = context.provided().unexpected_node_expr(Expr::Get(Id::Parser(
					id::Parser::Unexpected,
				)));
				if context.config().locate {
					err = Expr::locate(err, Expr::Get(Id::Parser(id::Parser::Span)))
				}
				goto_cases.push(MatchCase {
					pattern: Pattern::Bind(Id::Parser(id::Parser::Unexpected)),
					expr: Expr::Return(vec![Expr::err(err)]),
				})
			}

			let action_case = if context.config().locate {
				Expr::Span(SpanExpr::Unwrap(
					Some(Id::Parser(id::Parser::AnyTokenOptSpanless)),
					Some(Id::Parser(id::Parser::Span)),
					Box::new(Expr::Span(SpanExpr::Transpose(
						Box::new(Expr::Get(Id::Parser(id::Parser::AnyTokenOpt))),
						Box::new(Expr::Span(SpanExpr::FromPosition(Box::new(Expr::Get(
							Id::Parser(id::Parser::Position),
						))))),
					))),
					Box::new(Expr::Update(
						Id::Parser(id::Parser::Position),
						Box::new(Expr::Span(SpanExpr::After(Box::new(Expr::Get(
							Id::Parser(id::Parser::Span),
						))))),
						Box::new(Expr::Match {
							expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyTokenOptSpanless))),
							cases: action_cases,
						}),
					)),
				))
			} else {
				Expr::Match {
					expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyTokenOpt))),
					cases: action_cases,
				}
			};

			let cases = vec![
				MatchCase {
					pattern: Pattern::some(Pattern::Bind(Id::Parser(id::Parser::AnyNode))),
					expr: Expr::Match {
						expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyNode))),
						cases: goto_cases,
					},
				},
				MatchCase {
					pattern: Pattern::none(),
					expr: Expr::Stream(
						Id::Parser(id::Parser::Lexer),
						StreamExpr::Pull(
							Id::Parser(id::Parser::UnsafeTokenOpt),
							Box::new(Expr::CheckMap(
								Id::Parser(id::Parser::AnyTokenOpt),
								Box::new(Expr::Transpose(Box::new(Expr::Get(Id::Parser(id::Parser::UnsafeTokenOpt))))),
								context.provided().lexer_to_parser_err_function(),
								Vec::new(),
								Box::new(action_case),
							)),
						),
					),
				},
			];

			if context.config().locate {
				Expr::Span(SpanExpr::Unwrap(
					Some(Id::Parser(id::Parser::AnyNodeOptSpanless)),
					Some(Id::Parser(id::Parser::Span)),
					Box::new(Expr::Span(SpanExpr::Transpose(
						Box::new(Expr::Get(Id::Parser(id::Parser::AnyNodeOpt))),
						Box::new(Expr::Span(SpanExpr::FromPosition(Box::new(Expr::Get(
							Id::Parser(id::Parser::Position),
						))))),
					))),
					Box::new(Expr::Match {
						expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyNodeOptSpanless))),
						cases,
					}),
				))
			} else {
				Expr::Match {
					expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyNodeOpt))),
					cases,
				}
			}
		}
	}
}
