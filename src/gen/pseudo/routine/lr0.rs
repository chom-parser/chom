use std::collections::HashSet;
use crate::{
	mono,
	parsing::table::{
		self,
		lr0::State,
		Rule
	},
	gen::pseudo::{
		Context,
		Expr,
		expr::{
			Error,
			Label,
			ParserOperation as Operation,
			MatchCase
		},
		Pattern,
		Constant,
		Id
	}
};

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
pub fn generate(
	context: &Context,
	table: &table::LR0,
	initial_state: u32
) -> Expr {
	let mut cases = Vec::new();
	let mut stack = vec![initial_state];
	let mut visited = HashSet::new();
	while let Some(q) = stack.pop() {
		if visited.insert(q) {
			cases.push(MatchCase {
				pattern: Pattern::Constant(Constant::Int(q)),
				expr: generate_state(
					context,
					table,
					&mut stack,
					q
				)
			})
		}
	}

	Expr::TailRecursion {
		label: Label::Parser,
		args: vec![Id::Lexer, Id::Stack, Id::AnyNodeOpt, Id::State],
		body: Box::new(Expr::Match {
			expr: Box::new(Expr::Get(Id::State)),
			cases
		})
	}
}

fn generate_state(
	context: &Context,
	table: &table::LR0,
	stack: &mut Vec<u32>,
	q: u32
) -> Expr {
	let recurse_args = vec![Id::Lexer, Id::Stack, Id::AnyNodeOpt, Id::State];
	let state = table.state(q).unwrap();

	match state {
		State::Reduce(rule) => match rule {
			Rule::Initial(ty_index) => {
				let pattern = context.built_in.item_node_pattern(*ty_index, Id::Result);
				let result = Expr::Ok(Box::new(Expr::Get(Id::Result)));
				Expr::Parser(Operation::StackPop(
					vec![pattern],
					Box::new(result)
				))
			},
			Rule::Function(f_index) => {
				let f = context.grammar.function(*f_index).unwrap();
				let patterns = f.arguments().iter().enumerate().map(|(i, a)| {
					let i = i as u32;
					match a {
						mono::ty::Expr::Terminal(index) => {
							context.built_in.item_token_pattern(*index, || Id::Item(i))
						},
						mono::ty::Expr::Type(index) => {
							context.built_in.item_node_pattern(*index, Id::Item(i))
						}
					}
				}).collect();

				let expr = context.constructor_expr(*f_index, |i| Expr::Get(Id::Item(i)));
	
				Expr::Parser(Operation::StackPop(
					patterns,
					Box::new(Expr::Set(
						Id::AnyNodeOpt,
						Box::new(expr),
						Box::new(Expr::Set(
							Id::State,
							Box::new(Expr::Get(Id::SavedState)),
							Box::new(Expr::Recurse(
								Label::Parser,
								recurse_args.clone()
							))
						))
					))
				))
			}
		},
		State::Shift { action, goto } => {
			let mut action_cases: Vec<_> = action.iter().map(|(index, next_state)| {
				let next_state_expr = Expr::Set(
					Id::State,
					Box::new(Expr::Constant(Constant::Int(*next_state))),
					Box::new(Expr::Recurse(
						Label::Parser,
						recurse_args.clone()
					))
				);

				match index {
					Some(index) => {
						let pattern = context.built_in.token_pattern(*index, || Id::Token);
						let expr = context.built_in.token_expr(*index, || Expr::Get(Id::Token));

						MatchCase {
							pattern: Pattern::Some(Box::new(pattern)),
							expr: Expr::Parser(Operation::StackPush(
								Box::new(expr),
								Box::new(next_state_expr)
							))
						}
					},
					None => {
						MatchCase {
							pattern: Pattern::None,
							expr: next_state_expr
						}
					}
				}
			}).collect();

			if action_cases.len() < context.built_in.token_count() + 1 {
				action_cases.push(MatchCase {
					pattern: Pattern::Bind(Id::Unexpected),
					expr: Expr::Err(Error::UnexpectedToken(
						Box::new(Expr::Get(Id::Unexpected))
					))
				})
			}

			let mut goto_cases: Vec<_> = goto.iter().map(|(index, next_state)| {
				MatchCase {
					pattern: context.built_in.node_pattern(*index, Id::Node),
					expr: Expr::Parser(Operation::StackPush(
						Box::new(Expr::Get(Id::Node)),
						Box::new(Expr::Set(
							Id::State,
							Box::new(Expr::Constant(Constant::Int(*next_state))),
							Box::new(Expr::Recurse(
								Label::Parser,
								recurse_args.clone()
							))
						))
					))
				}
			}).collect();

			if (goto_cases.len() as u32) < context.grammar.type_count() {
				goto_cases.push(MatchCase {
					pattern: Pattern::Bind(Id::Unexpected),
					expr: Expr::Err(Error::UnexpectedNode(
						Box::new(Expr::Get(Id::Unexpected))
					))
				})
			}

			Expr::Match {
				expr: Box::new(Expr::Get(Id::AnyNodeOpt)),
				cases: vec![
					MatchCase {
						pattern: Pattern::Some(Box::new(Pattern::Bind(Id::AnyNode))),
						expr: Expr::Match {
							expr: Box::new(Expr::Get(Id::AnyNode)),
							cases: goto_cases
						}
					},
					MatchCase {
						pattern: Pattern::None,
						expr: Expr::Parser(Operation::LexerNext(
							Box::new(Expr::Match {
								expr: Box::new(Expr::Get(Id::AnyTokenOpt)),
								cases: action_cases
							})
						))
					}
				]
			}
		}
	}
}