use std::{
	collections::{BTreeSet,BTreeMap,HashMap},
	ops::{Bound, RangeBounds}
};
use btree_range_map::{
	AnyRange,
	util::PartialEnum
};
use crate::{
	mono::{
		terminal
	},
	lexing::{
		DetState,
		Table
	},
	gen::pseudo::{
		Context,
		Expr,
		expr::{
			Error,
			Label,
			LexerOperation,
			MatchCase
		},
		Pattern,
		Constant,
		Id
	}
};

pub fn generate(
	context: &Context,
	table: &Table
) -> Expr {
	generate_automaton(context, table, 0, false)
}

/// ## Pseudo code
/// 
/// Operation prefixed by `#` are statically evaluated.
/// ```
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
fn generate_automaton(
	context: &Context,
	table: &Table,
	index: u32,
	default_token: bool
) -> Expr {
	let automaton = table.automaton(index).unwrap();
	let mut recurse_args = vec![Id::Source, Id::State, Id::Buffer];

	if default_token {
		recurse_args.push(Id::BufferChars);
	}

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
	let mut cases: Vec<_> = automaton.states().into_iter().map(|q| {
		// Maps each target state to the set of ranges that leads to it.
		let mut inverse: BTreeMap<
			DetState,
			BTreeSet<AnyRange<char>>,
		> = BTreeMap::new();

		for (range, target) in automaton.transitions_from(q) {
			use std::collections::btree_map::Entry;
			match inverse.entry(*target) {
				Entry::Occupied(mut entry) => {
					entry.get_mut().insert(range.clone());
				},
				Entry::Vacant(entry) => {
					entry.insert(BTreeSet::new()).insert(range.clone());
				}
			}
		}

		let id = state_id(&mut id_table, *q);

		let mut state_cases: Vec<_> = inverse.into_iter().map(|(target, ranges)| {
			let target_id = state_id(&mut id_table, target);

			let next_state_expr = Expr::Set(
				Id::State,
				Box::new(Expr::Constant(Constant::Int(target_id))),
				Box::new(Expr::Recurse(
					Label::Lexer(index),
					recurse_args.clone()
				))
			);

			MatchCase {
				pattern: Pattern::Some(Box::new(ranges_pattern(&ranges))),
				expr: if default_token {
					next_state_expr
				} else {
					Expr::Lexer(LexerOperation::BufferPush(Box::new(
						Expr::Lexer(LexerOperation::SourceConsume(Box::new(next_state_expr)))
					)))
				},
			}
		}).collect();

		state_cases.push(match q {
			DetState::Final(terminal_index, _) => {
				let terminal = context.grammar.terminal(*terminal_index).unwrap();
				let expr = match terminal.desc() {
					terminal::Desc::Whitespace(_) => {
						Expr::Lexer(LexerOperation::BufferClear(Box::new(
							Expr::Recurse(
								Label::Lexer(0),
								vec![Id::Source, Id::State, Id::Buffer]
							)
						)))
					},
					terminal::Desc::RegExp(_) => {
						let expr = context.built_in.token_expr(*terminal_index, || {
							Expr::Lexer(LexerOperation::BufferParse(*terminal_index))
						});
						let result = if default_token {
							Expr::Some(Box::new(expr))
						} else {
							Expr::Ok(Box::new(expr))
						};
						match table.sub_automaton_index(*terminal_index) {
							Some(sub_automaton) => {
								let sub_expr = generate_automaton(context, table, sub_automaton, true);
								
								Expr::Set(
									Id::BufferChars,
									Box::new(Expr::Lexer(LexerOperation::BufferIter)),
									Box::new(Expr::Set(
										Id::SubToken,
										Box::new(sub_expr),
										Box::new(Expr::Match {
											expr: Box::new(Expr::Get(Id::SubToken)),
											cases: vec![
												MatchCase {
													pattern: Pattern::Some(Box::new(Pattern::Bind(Id::SubToken))),
													expr: if default_token {
														Expr::Some(Box::new(Expr::Get(Id::SubToken)))
													} else {
														Expr::Ok(Box::new(Expr::Get(Id::SubToken)))
													}
												},
												MatchCase {
													pattern: Pattern::None,
													expr: result
												}
											]
										})
									))
								)
							},
							None => result
						}
					}
				};

				MatchCase {
					pattern: Pattern::Any,
					expr
				}
			},
			_ => {
				if default_token {
					MatchCase {
						pattern: Pattern::Any,
						expr: Expr::None
					}
				} else {
					MatchCase {
						pattern: Pattern::Bind(Id::Unexpected),
						expr: Expr::Err(Error::UnexpectedChar(
							Box::new(Expr::Get(Id::Unexpected))
						))
					}
				}
			}
		});

		let expr = if default_token {
			Expr::Lexer(LexerOperation::BufferCharsNext(
				Box::new(Expr::Match {
					expr: Box::new(Expr::Get(Id::CharOpt)),
					cases: state_cases
				})
			))
		} else {
			Expr::Match {
				expr: Box::new(Expr::Lexer(LexerOperation::SourcePeek)),
				cases: state_cases
			}
		};

		MatchCase {
			pattern: Pattern::Constant(Constant::Int(id)),
			expr
		}
	}).collect();

	cases.push(MatchCase {
		pattern: Pattern::Any,
		expr: Expr::Unreachable
	});

	Expr::TailRecursion {
		label: Label::Lexer(index),
		args: recurse_args,
		body: Box::new(Expr::Match {
			expr: Box::new(Expr::Get(Id::State)),
			cases
		})
	}
}

fn ranges_pattern(ranges: &BTreeSet<AnyRange<char>>) -> Pattern {
	if ranges.len() == 1 {
		range_pattern(ranges.iter().next().unwrap())
	} else {
		Pattern::Or(ranges.iter().map(range_pattern).collect())
	}
}

fn range_pattern(range: &btree_range_map::AnyRange<char>) -> Pattern {
	let a = included_start_bound(range.start_bound());
	let b = included_end_bound(range.end_bound());

	Pattern::Constant(if a == b {
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