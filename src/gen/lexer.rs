use crate::{
	lexing::{DetState, Table},
	mono::terminal,
};
use super::{
	Context,
	Id,
	id,
	Expr,
	Label,
	Pattern
};
use chom_ir::{
	Constant,
	expr::{
		Var,
		LexerExpr,
		StreamExpr,
		MatchCase
	}
};
use btree_range_map::{util::PartialEnum, AnyRange};
use std::{
	collections::{BTreeMap, BTreeSet, HashMap},
	ops::{Bound, RangeBounds},
};

pub fn generate<'a, 'p>(context: &Context<'a, 'p>, table: &Table) -> Expr<'p> {
	generate_automaton(context, table, 0, false)
}

fn locate<'a, 'p>(context: &Context<'a, 'p>, e: Expr<'p>) -> Expr<'p> {
	if context.config().locate {
		Expr::locate(
			e,
			Expr::Lexer(Var::This, LexerExpr::Span)
		)
	} else {
		e
	}
}

/// ## Pseudo code
///
/// Operation prefixed by `#` are statically evaluated.
/// ```pseudo
/// 'label tail_recursion source state buffer {
/// 	match state { // for each lexer state q.
/// 		q => match source.peek {
/// 			Some(expected) => {
/// 				// consume the char and continue to the next state.
/// 				let buffer = buffer.push expected in
/// 				let source = source.consume in
/// 				let state = next_state in
/// 				recurse 'label source state
/// 			},
/// 			unexpected => {
/// 				#if is_final_state {
///						// found a token.
/// 					#match sub_automaton {
/// 						Some(aut) => {
/// 							let sub_token = #generate_automaton aut true in
/// 							match sub_token {
/// 								Some(sub_token) => {
/// 									#if default_token {
/// 										Some(sub_token)
/// 									} else {
/// 										Ok(sub_token)
/// 									}
/// 								},
/// 								None => {
/// 									#if default_token {
/// 										Some(token(parse_token_data(buffer)))
/// 									} else {
/// 										Ok(token(parse_token_data(buffer)))
/// 									}
/// 								}
/// 							}
/// 						}
/// 						None => {
/// 							#if default_token {
/// 								Some(token(parse_token_data(buffer)))
/// 							} else {
/// 								Ok(token(parse_token_data(buffer)))
/// 							}
/// 						}
/// 					}
/// 				} else {
/// 					#if default_token {
/// 						None
/// 					} else {
/// 						// error
/// 						Err(unexpected_char)
/// 					}
/// 				}
/// 			},
/// 		}
/// 	},
/// 	...
/// }
/// ```
fn generate_automaton<'a, 'p>(context: &Context<'a, 'p>, table: &Table, index: u32, default_token: bool) -> Expr<'p> {
	let automaton = table.automaton(index).unwrap();
	let recurse_args = if default_token {
		vec![
			Var::This,
			Var::Defined(Id::Lexer(id::Lexer::BufferChars)),
			Var::Defined(Id::Lexer(id::Lexer::State)),
		]
	} else {
		vec![
			Var::This,
			Var::Defined(Id::Lexer(id::Lexer::State))
		]
	};

	let mut id_table = HashMap::new();
	fn state_id(table: &mut HashMap<DetState, u32>, q: DetState) -> u32 {
		use std::collections::hash_map::Entry;
		let len = table.len() as u32;
		match table.entry(q) {
			Entry::Occupied(entry) => *entry.get(),
			Entry::Vacant(entry) => {
				entry.insert(len);
				len
			}
		}
	}

	let init_id = state_id(&mut id_table, *automaton.initial_state());
	let mut cases: Vec<_> = automaton
		.states()
		.into_iter()
		.map(|q| {
			// Maps each target state to the set of ranges that leads to it.
			let mut inverse: BTreeMap<DetState, BTreeSet<AnyRange<char>>> = BTreeMap::new();

			for (range, target) in automaton.transitions_from(q) {
				use std::collections::btree_map::Entry;
				match inverse.entry(*target) {
					Entry::Occupied(mut entry) => {
						entry.get_mut().insert(range.clone());
					}
					Entry::Vacant(entry) => {
						entry.insert(BTreeSet::new()).insert(range.clone());
					}
				}
			}

			let id = state_id(&mut id_table, *q);

			let mut state_cases: Vec<_> = inverse
				.into_iter()
				.map(|(target, ranges)| {
					let target_id = state_id(&mut id_table, target);

					let next_state_expr = Expr::Update(
						Id::Lexer(id::Lexer::State),
						Box::new(Expr::Literal(Constant::Int(target_id))),
						Box::new(Expr::Recurse(Label::Lexer(index), recurse_args.clone())),
					);

					MatchCase {
						pattern: Pattern::some(ranges_pattern(&ranges)),
						expr: if default_token {
							next_state_expr
						} else {
							Expr::Lexer(Var::This, LexerExpr::Consume(Box::new(next_state_expr)))
						}
					}
				})
				.collect();

			state_cases.push(match q {
				DetState::Final(terminal_index, _) => {
					let terminal = context.grammar().terminal(*terminal_index).unwrap();
					let expr = match terminal.desc() {
						terminal::Desc::Whitespace(_) => Expr::Lexer(Var::This, LexerExpr::Clear(
							Box::new(Expr::Recurse(
								Label::Lexer(0),
								vec![Var::This, Var::Defined(Id::Lexer(id::Lexer::State))],
							)),
						)),
						terminal::Desc::RegExp(_) => {
							let expr = context.provided().token_expr(*terminal_index, |ir_function| {
								Expr::Call(ir_function, None, vec![Expr::Lexer(Var::This, LexerExpr::Buffer)])
							});
							let result = if default_token {
								Expr::some(expr)
							} else {
								Expr::ok(Expr::some(locate(context, expr)))
							};
							match table.sub_automaton_index(*terminal_index) {
								Some(sub_automaton) => {
									let sub_expr =
										generate_automaton(context, table, sub_automaton, true);

									Expr::Let(
										Id::Lexer(id::Lexer::BufferChars),
										true,
										Box::new(Expr::Lexer(Var::This, LexerExpr::Chars)),
										Box::new(Expr::Let(
											Id::Lexer(id::Lexer::SubToken),
											false,
											Box::new(sub_expr),
											Box::new(Expr::Match {
												expr: Box::new(Expr::Get(Var::Defined(Id::Lexer(
													id::Lexer::SubToken,
												)))),
												cases: vec![
													MatchCase {
														pattern: Pattern::some(
															Pattern::Bind(Id::Lexer(
																id::Lexer::SubToken,
															)),
														),
														expr: if default_token {
															Expr::some(Expr::Get(
																Var::Defined(Id::Lexer(id::Lexer::SubToken)),
															))
														} else {
															Expr::ok(Expr::some(
																locate(
																	context,
																	Expr::Get(Var::Defined(Id::Lexer(
																		id::Lexer::SubToken,
																	))),
																),
															))
														},
													},
													MatchCase {
														pattern: Pattern::none(),
														expr: result,
													},
												],
											}),
										)),
									)
								}
								None => result,
							}
						}
					};

					MatchCase {
						pattern: Pattern::Any,
						expr,
					}
				}
				_ => {
					if default_token {
						MatchCase {
							pattern: Pattern::Any,
							expr: Expr::none(),
						}
					} else {
						let err = Expr::Call(context.lexing_error_function(), None, vec![
							Expr::Get(
								Var::Defined(Id::Lexer(id::Lexer::Unexpected)),
							)
						]);
						MatchCase {
							pattern: Pattern::Bind(Id::Lexer(id::Lexer::Unexpected)),
							expr: Expr::err(locate(context, err)),
						}
					}
				}
			});

			let expr = if default_token {
				Expr::Stream(
					Var::Defined(Id::Lexer(id::Lexer::BufferChars)),
					StreamExpr::Pull(
						Id::Lexer(id::Lexer::CharOpt),
						Box::new(Expr::Match {
							expr: Box::new(Expr::Get(Var::Defined(Id::Lexer(id::Lexer::CharOpt)))),
							cases: state_cases,
						}
					))
				)
			} else {
				Expr::Let(
					Id::Lexer(id::Lexer::UnsafeCharOpt),
					false,
					Box::new(Expr::Lexer(Var::This, LexerExpr::Peek)),
					Box::new(Expr::Check(
						Id::Lexer(id::Lexer::CharOpt),
						Box::new(Expr::Get(Var::Defined(Id::Lexer(id::Lexer::UnsafeCharOpt)))),
						Box::new(Expr::Match {
							expr: Box::new(Expr::Get(Var::Defined(Id::Lexer(id::Lexer::CharOpt)))),
							cases: state_cases,
						})
					))
				)
			};

			MatchCase {
				pattern: Pattern::Literal(Constant::Int(id)),
				expr,
			}
		})
		.collect();

	cases.push(MatchCase {
		pattern: Pattern::Any,
		expr: Expr::Unreachable,
	});

	Expr::Lexer(Var::This, LexerExpr::Clear(
		Box::new(Expr::Let(
			Id::Lexer(id::Lexer::State),
			true,
			Box::new(Expr::Literal(Constant::Int(init_id))),
			Box::new(Expr::TailRecursion {
				label: Label::Lexer(index),
				args: recurse_args,
				body: Box::new(Expr::Match {
					expr: Box::new(Expr::Get(Var::Defined(Id::Lexer(id::Lexer::State)))),
					cases,
				}),
			}),
		)),
	))
}

fn ranges_pattern<'p>(ranges: &BTreeSet<AnyRange<char>>) -> Pattern<'p> {
	if ranges.len() == 1 {
		range_pattern(ranges.iter().next().unwrap())
	} else {
		Pattern::Or(ranges.iter().map(range_pattern).collect())
	}
}

fn range_pattern<'p>(range: &btree_range_map::AnyRange<char>) -> Pattern<'p> {
	let a = included_start_bound(range.start_bound());
	let b = included_end_bound(range.end_bound());

	Pattern::Literal(if a == b {
		Constant::Char(a)
	} else {
		Constant::CharRange(a, b)
	})
}

fn included_start_bound(bound: Bound<&char>) -> char {
	match bound {
		Bound::Included(a) => *a,
		Bound::Excluded(a) => a.succ().unwrap(),
		Bound::Unbounded => PartialEnum::MIN,
	}
}

fn included_end_bound(bound: Bound<&char>) -> char {
	match bound {
		Bound::Included(a) => *a,
		Bound::Excluded(a) => a.pred().unwrap(),
		Bound::Unbounded => PartialEnum::MAX,
	}
}
