use std::{
	fmt,
	collections::BTreeMap
};
use source_span::{
	Loc,
	Span,
	Position
};
use yansi::Paint;
use crate::{
	out,
	mono::{
		Index,
		Grammar,
		Function,
		ty
	},
	parsing::FirstAndFollowGraph
};
use super::{
	Item,
	NonDeterministic,
	lr0
};

/// LALR1 ambiguity.
pub enum Ambiguity {
	/// Shift reduce conflict.
	ShiftReduce(Vec<ty::Expr>, u32, Item, Index),
	ReduceReduce(Vec<ty::Expr>, u32, Index, Index)
}

fn format_path(grammar: &Grammar, path: &[ty::Expr]) -> String {
	let mut string = String::new();

	for symbol in path {
		if !string.is_empty() {
			string.push(' ');
		}

		string.push_str(&format!("{}", symbol.instance(grammar)))
	}

	string
}

fn build_ambiguity_example(
	grammar: &Grammar,
	a_rule: &Function,
	a_offset: u32,
	b_rule: &Function,
	b_offset: u32
) -> (String, Span, Span) {
	let metrics = source_span::DEFAULT_METRICS;

	let mut content = String::new();

	let before_len = std::cmp::max(a_offset, b_offset);
	let total_len = before_len + std::cmp::max(a_rule.arity() - a_offset, b_rule.arity() - b_offset);

	let a_start_offset = before_len - a_offset;
	let b_start_offset = before_len - b_offset;

	let mut a_span = Span::default();
	let mut b_span = Span::default();
	let mut pos = Position::default();

	for i in 0..total_len {
		if !content.is_empty() {
			content.push(' ');
			pos = pos.next(' ', &metrics);
			if i < a_start_offset + a_rule.arity() {
				a_span.push(' ', &metrics);
			}
			if i < b_start_offset + b_rule.arity() {
				b_span.push(' ', &metrics);
			}
		}

		let mut symbol = None;

		if i >= a_start_offset {
			let a = i - a_start_offset;
			if a == 0 {
				a_span = pos.into();
			}
			symbol = a_rule.argument(a);
		}

		if i >= b_start_offset {
			let b = i - b_start_offset;
			if b == 0 {
				b_span = pos.into();
			}
			symbol = symbol.or(b_rule.argument(b))
		}

		let instance = symbol.unwrap().instance(grammar);
		for c in instance.chars() {
			content.push(c);
			pos = pos.next(c, &metrics);
			if i < a_start_offset + a_rule.arity() {
				a_span.push(c, &metrics);
			}
			if i < b_start_offset + b_rule.arity() {
				b_span.push(c, &metrics);
			}
		}
	}

	// fmt.add(span, Some(format!("{}", symbol.format(grammar))), source_span::fmt::Style::Help)
	(content, a_span, b_span)
}

impl Ambiguity {
	pub fn title(&self) -> &str {
		match self {
			Self::ShiftReduce(_, _, _, _) => "LL0 shift/reduce conflict",
			Self::ReduceReduce(_, _, _, _) => "LL0 reduce/reduce conflict"
		}
	}

	pub fn self_conflict(&self) -> bool {
		match self {
			Self::ShiftReduce(_, _, shift_item, reduce_rule) => shift_item.function == *reduce_rule,
			Self::ReduceReduce(_, _, rule_a, rule_b) => rule_a == rule_b
		}
	}

	pub fn label(&self) -> Option<String> {
		if self.self_conflict() {
			Some("in conflict with itself".to_string())
		} else {
			None
		}
	}

	pub fn fill_block(&self, grammar: &Grammar, block: &mut out::Block) {
		match self {
			Ambiguity::ShiftReduce(_path, _, shift_item, reduce_rule_index) => {
				let shift_rule = grammar.function(shift_item.function).unwrap();
				let reduce_rule = grammar.function(*reduce_rule_index).unwrap();

				if !self.self_conflict() {
					let (span, message) = match reduce_rule.span() {
						Some(span) => (Some(span), "in conflict with this rule"),
						None => (
							grammar.ty(reduce_rule.return_ty()).unwrap().span(),
							"in conflict with a rule implicitely defined by this symbol"
						)
					};
					
					if let Some(span) = span {
						block.highlights_mut().add(span, Some(message.to_string()), source_span::fmt::Style::Note);
					}
				}

				let shift_target = grammar.ty(shift_rule.return_ty()).unwrap();
				let reduce_target = grammar.ty(reduce_rule.return_ty()).unwrap();

				let (example, shift_span, reduce_span) = build_ambiguity_example(
					grammar,
					&shift_rule,
					shift_item.offset,
					&reduce_rule,
					reduce_rule.arity()
				);

				let formatted_example = out::format_ambiguity_example(
					&example,
					shift_span,
					format!("shift {} `{}`", shift_target.id().name(), shift_rule.id().as_str()),
					reduce_span,
					format!("reduce {} `{}`", reduce_target.id().name(), reduce_rule.id().as_str())
				);

				block.add_note(out::NoteType::Note, format!("the following phrase is ambiguous: {}", Paint::new(example).bold()));
				block.add_note(out::NoteType::Note, format!("it has the following two conflicting decompositions:\n\n{}", formatted_example))
			},
			Ambiguity::ReduceReduce(path, _, _rule_a, rule_b) => {
				if !self.self_conflict() {
					let rule_b = grammar.function(*rule_b).unwrap();
				
					let (span, message) = match rule_b.span() {
						Some(span) => (Some(span), "in conflict with this rule"),
						None => (
							grammar.ty(rule_b.return_ty()).unwrap().span(),
							"in conflict with a rule implicitely defined by this symbol"
						)
					};
					
					if let Some(span) = span {
						block.highlights_mut().add(span, Some(message.to_string()), source_span::fmt::Style::Note);
					}
				}

				block.add_note(out::NoteType::Note, format!("the following phrase is ambiguous: {}", format_path(grammar, path)))
			}
		}
	}
}

impl fmt::Display for Ambiguity {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::ShiftReduce(_, _, _, _) => write!(f, "shift/reduce conflict"),
			Self::ReduceReduce(_, _, _, _) => write!(f, "reduce/reduce conflict")
		}
	}
}

pub enum State {
	LALR1 {
		action: BTreeMap<u32, Action>,
		goto: BTreeMap<Index, u32>
	},
	LR0(lr0::State)
}

pub enum Action {
	Shift(u32),
	Reduce(Index)
}

pub enum ReduceAction {
	Direct(Index),
	Lookahead(BTreeMap<u32, Index>)
}

impl ReduceAction {
	pub fn into_map(self, grammar: &Grammar, lookahead: &FirstAndFollowGraph) -> BTreeMap<u32, Index> {
		match self {
			ReduceAction::Direct(f_index) => {
				let f = grammar.function(f_index).unwrap();
				let target_ty = f.return_ty();
				let mut map = BTreeMap::new();
				for &t in lookahead.follow(target_ty) {
					map.insert(t, f_index);
				}
				map
			},
			ReduceAction::Lookahead(map) => map
		}
	}
}

/// LALR1 parsing table.
pub struct LALR1 {
	states: Vec<State>
}

impl LALR1 {
	pub fn from_non_deterministic(grammar: &Grammar, nd_table: &NonDeterministic) -> Result<Self, Loc<Ambiguity>> {
		let lookahead = FirstAndFollowGraph::new(grammar);

		let mut states = Vec::new();
		
		for (q, state) in nd_table.states().iter().enumerate() {
			let mut reduce: Option<ReduceAction> = None;

			for item in &state.items {
				let f = grammar.function(item.function).unwrap();
				if item.offset >= f.arity() {
					reduce = Some(match reduce {
						Some(other_reduce) => { // potential reduce/reduce conflict
							let mut map = other_reduce.into_map(grammar, &lookahead);

							let target_ty = f.return_ty();
							for &t in lookahead.follow(target_ty) {
								use std::collections::btree_map::Entry;
								match map.entry(t) {
									Entry::Vacant(entry) => {
										entry.insert(item.function);
									},
									Entry::Occupied(entry) => { // confirmed reduce/reduce conflict
										let other_function = *entry.get();

										let path: Vec<_> = nd_table.path_to(q as u32).into_iter().map(|(_, symbol)| symbol).collect();
										let span = f.span().unwrap_or_else(|| {
											grammar.ty(f.return_ty()).unwrap().span().expect("no span")
										});
										
										return Err(Loc::new(Ambiguity::ReduceReduce(path, t, item.function, other_function), span))
									}
								}
							}

							ReduceAction::Lookahead(map)
						},
						None => ReduceAction::Direct(item.function)
					})
				}
			}

			let lalr0_state = match reduce {
				Some(reduce) => {
					let reduce_actions = reduce.into_map(&grammar, &lookahead);

					// search for shift/reduce conflicts.
					for item in &state.items {
						let f = grammar.function(item.function).unwrap();
						if let Some(symbol) = f.argument(item.offset) {
							let firsts = match symbol {
								ty::Expr::Terminal(t) => Firsts::Terminal(Some(t)),
								ty::Expr::Type(ty) => Firsts::Type(lookahead.first(ty).iter())
							};
	
							for t in firsts {
								if let Some(reduce_target) = reduce_actions.get(&t) { // confirmed shift/reduce conflict
									let path: Vec<_> = nd_table.path_to(q as u32).into_iter().map(|(_, symbol)| symbol).collect();
									let span = f.span().unwrap_or_else(|| {
										grammar.ty(f.return_ty()).unwrap().span().expect("no span")
									});
									
									return Err(Loc::new(Ambiguity::ShiftReduce(path, t, *item, *reduce_target), span))
								}
							}
						}
					}

					let mut action = BTreeMap::new();
					let mut goto = BTreeMap::new();

					for (t, reduce_target) in reduce_actions {
						action.insert(t, Action::Reduce(reduce_target));
					}

					for (t, shift_target) in &state.transitions {
						match t {
							ty::Expr::Terminal(t) => { action.insert(*t, Action::Shift(*shift_target)); },
							ty::Expr::Type(ty) => { goto.insert(*ty, *shift_target); }
						}
					}

					State::LALR1 {
						action,
						goto
					}
				},
				None => {
					let mut action = BTreeMap::new();
					let mut goto = BTreeMap::new();

					for (t, shift_target) in &state.transitions {
						match t {
							ty::Expr::Terminal(t) => { action.insert(*t, *shift_target); },
							ty::Expr::Type(ty) => { goto.insert(*ty, *shift_target); }
						}
					}

					State::LR0(lr0::State::Shift {
						action,
						goto
					})
				}
			};

			states.push(lalr0_state)
		}

		Ok(Self {
			states
		})
	}
}

pub enum Firsts<'a> {
	Terminal(Option<u32>),
	Type(std::collections::btree_set::Iter<'a, u32>)
}

impl<'a> Iterator for Firsts<'a> {
	type Item = u32;

	fn next(&mut self) -> Option<u32> {
		match self {
			Self::Terminal(t) => t.take(),
			Self::Type(inner) => inner.next().cloned() 
		}
	}
}