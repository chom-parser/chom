use crate::{
	gen::pseudo::{
		expr::{Error, Label, MatchCase, ParserOperation as Operation},
		id, Constant, Context, Expr, Id, Pattern,
	},
	mono,
	parsing::table::{self, lr0::State, Rule},
};
use std::collections::HashSet;

/// ## Pseudo code
///
/// Operation prefixed by `#` are statically evaluated.
/// ```
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
pub fn generate(context: &Context, table: &table::LR0, initial_state: u32) -> Expr {
	let mut cases = Vec::new();
	let mut stack = vec![initial_state];
	let mut visited = HashSet::new();
	while let Some(q) = stack.pop() {
		if visited.insert(q) {
			cases.push(MatchCase {
				pattern: Pattern::Constant(Constant::Int(q)),
				expr: generate_state(context, table, &mut stack, q),
			})
		}
	}

	let expr = Expr::Set(
		Id::Parser(id::Parser::Stack),
		Box::new(Expr::Parser(Operation::StackNew)),
		Box::new(Expr::Set(
			Id::Parser(id::Parser::AnyNodeOpt),
			Box::new(Expr::None),
			Box::new(Expr::Set(
				Id::Parser(id::Parser::State),
				Box::new(Expr::Constant(Constant::Int(initial_state))),
				Box::new(Expr::TailRecursion {
					label: Label::Parser,
					args: vec![
						Id::Parser(id::Parser::Lexer),
						Id::Parser(id::Parser::Stack),
						Id::Parser(id::Parser::AnyNodeOpt),
						Id::Parser(id::Parser::State),
					],
					body: Box::new(Expr::Match {
						expr: Box::new(Expr::Get(Id::Parser(id::Parser::State))),
						cases,
					}),
				}),
			)),
		)),
	);

	if context.config().locate {
		Expr::Set(
			Id::Parser(id::Parser::Position),
			Box::new(Expr::Parser(Operation::PositionNew)),
			Box::new(expr),
		)
	} else {
		expr
	}
}

fn stack_pop(context: &Context, patterns: &[Pattern], next: Expr) -> Expr {
	let mut expr = next;
	let last = (patterns.len() - 1) as u32;

	if context.config().locate {
		expr = if last > 0 {
			Expr::Set(
				Id::Parser(id::Parser::Span),
				Box::new(Expr::Parser(Operation::LocUnion(
					Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(0)))),
					Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(last)))),
				))),
				Box::new(expr),
			)
		} else {
			Expr::Set(
				Id::Parser(id::Parser::Span),
				Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(0)))),
				Box::new(expr),
			)
		}
	}

	for (i, pattern) in patterns.iter().enumerate() {
		if pattern.is_bound() {
			let i = i as u32;
			expr = Expr::Parser(Operation::PatternUnwrap(
				pattern.clone(),
				Box::new(Expr::Get(Id::Parser(if context.config().locate {
					id::Parser::AnyItemSpanless(i)
				} else {
					id::Parser::AnyItem(i)
				}))),
				Box::new(expr),
			))
		}
	}

	if context.config().locate {
		for (i, pattern) in patterns.iter().enumerate() {
			let i = i as u32;
			let bound = pattern.is_bound();
			if bound || (i == 0 || i == last) {
				expr = Expr::Parser(Operation::LocUnwrap(
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

		let state = if i == 0 {
			Some(Id::Parser(id::Parser::SavedState))
		} else {
			None
		};

		expr = Expr::Parser(Operation::StackPop(value, state, Box::new(expr)))
	}

	expr
}

fn generate_state(context: &Context, table: &table::LR0, stack: &mut Vec<u32>, q: u32) -> Expr {
	let recurse_args = vec![
		Id::Parser(id::Parser::Lexer),
		Id::Parser(id::Parser::Stack),
		Id::Parser(id::Parser::AnyNodeOpt),
		Id::Parser(id::Parser::State),
	];
	let state = table.state(q).unwrap();

	match state {
		State::Reduce(rule) => match rule {
			Rule::Initial(ty_index) => {
				let pattern = context
					.built_in
					.item_node_pattern(*ty_index, Id::Parser(id::Parser::Result));
				let mut result = Expr::Get(Id::Parser(id::Parser::Result));
				if context.config().locate {
					result = Expr::Locate(
						Box::new(result),
						Box::new(Expr::Get(Id::Parser(id::Parser::Span))),
					)
				}
				stack_pop(context, &[pattern], Expr::Ok(Box::new(result)))
			}
			Rule::Function(f_index) => {
				let f = context.grammar.function(*f_index).unwrap();
				let patterns: Vec<_> = f
					.arguments()
					.iter()
					.enumerate()
					.map(|(i, a)| {
						let i = i as u32;
						match a {
							mono::ty::Expr::Terminal(index) => context
								.built_in
								.item_token_pattern(*index, || Id::Parser(id::Parser::Item(i))),
							mono::ty::Expr::Type(index) => context
								.built_in
								.item_node_pattern(*index, Id::Parser(id::Parser::Item(i))),
						}
					})
					.collect();

				let mut expr = context.built_in.node_expr(
					f.return_ty(),
					context.constructor_expr(*f_index, |i| {
						let mut arg_expr = Expr::Get(Id::Parser(id::Parser::Item(i)));

						if context.config().locate {
							arg_expr = Expr::Locate(
								Box::new(arg_expr),
								Box::new(Expr::Get(Id::Parser(id::Parser::AnyItemSpan(i)))),
							)
						}

						if f.put_argument_on_heap(context.grammar(), f.return_ty(), i) {
							arg_expr = Expr::Heap(Box::new(arg_expr))
						}

						arg_expr
					}),
				);

				if context.config().locate {
					expr = Expr::Locate(
						Box::new(expr),
						Box::new(Expr::Get(Id::Parser(id::Parser::Span))),
					)
				}

				stack_pop(
					context,
					&patterns,
					Expr::Set(
						Id::Parser(id::Parser::AnyNodeOpt),
						Box::new(Expr::Some(Box::new(expr))),
						Box::new(Expr::Set(
							Id::Parser(id::Parser::State),
							Box::new(Expr::Get(Id::Parser(id::Parser::SavedState))),
							Box::new(Expr::Recurse(Label::Parser, recurse_args.clone())),
						)),
					),
				)
			}
		},
		State::Shift { action, goto } => {
			let mut action_cases: Vec<_> = action
				.iter()
				.map(|(index, next_state)| {
					stack.push(*next_state);
					let next_state_expr = Expr::Set(
						Id::Parser(id::Parser::State),
						Box::new(Expr::Constant(Constant::Int(*next_state))),
						Box::new(Expr::Recurse(Label::Parser, recurse_args.clone())),
					);

					match index {
						Some(index) => {
							let pattern = context
								.built_in
								.token_pattern(*index, || Id::Parser(id::Parser::Token));
							let mut expr = context.built_in.item_token_expr(*index, || {
								Expr::Get(Id::Parser(id::Parser::Token))
							});

							if context.config().locate {
								expr = Expr::Locate(
									Box::new(expr),
									Box::new(Expr::Get(Id::Parser(id::Parser::Span))),
								)
							}

							MatchCase {
								pattern: Pattern::Some(Box::new(pattern)),
								expr: Expr::Parser(Operation::StackPush(
									Box::new(expr),
									Box::new(next_state_expr),
								)),
							}
						}
						None => MatchCase {
							pattern: Pattern::None,
							expr: next_state_expr,
						},
					}
				})
				.collect();

			if action_cases.len() < context.built_in.token_count() + 1 {
				let mut err = Expr::Error(Error::UnexpectedToken(Box::new(Expr::Get(Id::Parser(
					id::Parser::Unexpected,
				)))));
				if context.config().locate {
					err = Expr::Locate(
						Box::new(err),
						Box::new(Expr::Get(Id::Parser(id::Parser::Span))),
					)
				}
				action_cases.push(MatchCase {
					pattern: Pattern::BindAny(Id::Parser(id::Parser::Unexpected)),
					expr: Expr::Err(Box::new(err)),
				})
			}

			let mut goto_cases: Vec<_> = goto
				.iter()
				.map(|(index, next_state)| {
					stack.push(*next_state);
					let pattern = context
						.built_in
						.node_pattern(*index, Id::Parser(id::Parser::Node));
					let mut expr = context
						.built_in
						.item_node_expr(*index, Expr::Get(Id::Parser(id::Parser::Node)));

					if context.config().locate {
						expr = Expr::Locate(
							Box::new(expr),
							Box::new(Expr::Get(Id::Parser(id::Parser::Span))),
						)
					}

					MatchCase {
						pattern,
						expr: Expr::Parser(Operation::StackPush(
							Box::new(expr),
							Box::new(Expr::Set(
								Id::Parser(id::Parser::State),
								Box::new(Expr::Constant(Constant::Int(*next_state))),
								Box::new(Expr::Recurse(Label::Parser, recurse_args.clone())),
							)),
						)),
					}
				})
				.collect();

			if (goto_cases.len() as u32) < context.grammar.type_count() {
				let mut err = Expr::Error(Error::UnexpectedNode(Box::new(Expr::Get(Id::Parser(
					id::Parser::Unexpected,
				)))));
				if context.config().locate {
					err = Expr::Locate(
						Box::new(err),
						Box::new(Expr::Get(Id::Parser(id::Parser::Span))),
					)
				}
				goto_cases.push(MatchCase {
					pattern: Pattern::BindAny(Id::Parser(id::Parser::Unexpected)),
					expr: Expr::Err(Box::new(err)),
				})
			}

			let action_case = if context.config().locate {
				Expr::Parser(Operation::LocUnwrap(
					Some(Id::Parser(id::Parser::AnyTokenOptSpanless)),
					Some(Id::Parser(id::Parser::Span)),
					Box::new(Expr::Parser(Operation::LocOptTranspose(
						Box::new(Expr::Get(Id::Parser(id::Parser::AnyTokenOpt))),
						Box::new(Expr::Parser(Operation::PositionToSpan(Box::new(
							Expr::Get(Id::Parser(id::Parser::Position)),
						)))),
					))),
					Box::new(Expr::Match {
						expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyTokenOptSpanless))),
						cases: action_cases,
					}),
				))
			} else {
				Expr::Match {
					expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyTokenOpt))),
					cases: action_cases,
				}
			};

			let cases = vec![
				MatchCase {
					pattern: Pattern::Some(Box::new(Pattern::BindAny(Id::Parser(
						id::Parser::AnyNode,
					)))),
					expr: Expr::Match {
						expr: Box::new(Expr::Get(Id::Parser(id::Parser::AnyNode))),
						cases: goto_cases,
					},
				},
				MatchCase {
					pattern: Pattern::None,
					expr: Expr::Parser(Operation::LexerNext(Box::new(action_case))),
				},
			];

			if context.config().locate {
				Expr::Parser(Operation::LocUnwrap(
					Some(Id::Parser(id::Parser::AnyNodeOptSpanless)),
					Some(Id::Parser(id::Parser::Span)),
					Box::new(Expr::Parser(Operation::LocOptTranspose(
						Box::new(Expr::Get(Id::Parser(id::Parser::AnyNodeOpt))),
						Box::new(Expr::Parser(Operation::PositionToSpan(Box::new(
							Expr::Get(Id::Parser(id::Parser::Position)),
						)))),
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
