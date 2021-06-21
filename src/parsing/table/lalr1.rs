use super::{lr0, Item, NonDeterministic, Rule, Symbol};
use crate::{
	mono::{ty, Function, Grammar, Index},
	out,
	parsing::FirstAndFollowGraph,
};
use source_span::{Loc, Position, Span};
use std::{collections::BTreeMap, fmt};
use yansi::Paint;

/// LALR1 ambiguity.
pub enum Ambiguity {
	/// Shift reduce conflict.
	ShiftReduce(Vec<Symbol>, Option<u32>, Item, Rule),
	ReduceReduce(Vec<Symbol>, Option<u32>, Rule, Rule),
}

fn format_path(grammar: &Grammar, path: &[Symbol]) -> String {
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
	a_rule: Rule,
	a_offset: u32,
	b_rule: Rule,
	b_offset: u32,
) -> (String, Span, Span) {
	let metrics = source_span::DEFAULT_METRICS;

	let mut content = String::new();

	let before_len = std::cmp::max(a_offset, b_offset);
	let total_len = before_len
		+ std::cmp::max(
			a_rule.len(grammar) - a_offset,
			b_rule.len(grammar) - b_offset,
		);

	let a_start_offset = before_len - a_offset;
	let b_start_offset = before_len - b_offset;

	let mut a_span = Span::default();
	let mut b_span = Span::default();
	let mut pos = Position::default();

	for i in 0..total_len {
		if !content.is_empty() {
			content.push(' ');
			pos = pos.next(' ', &metrics);
			if i < a_start_offset + a_rule.len(grammar) {
				a_span.push(' ', &metrics);
			}
			if i < b_start_offset + b_rule.len(grammar) {
				b_span.push(' ', &metrics);
			}
		}

		let mut symbol = None;

		if i >= a_start_offset {
			let a = i - a_start_offset;
			if a == 0 {
				a_span = pos.into();
			}
			symbol = a_rule.symbol(grammar, a);
		}

		if i >= b_start_offset {
			let b = i - b_start_offset;
			if b == 0 {
				b_span = pos.into();
			}
			symbol = symbol.or(b_rule.symbol(grammar, b))
		}

		let instance = symbol.unwrap().instance(grammar);
		for c in instance.chars() {
			content.push(c);
			pos = pos.next(c, &metrics);
			if i < a_start_offset + a_rule.len(grammar) {
				a_span.push(c, &metrics);
			}
			if i < b_start_offset + b_rule.len(grammar) {
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
			Self::ReduceReduce(_, _, _, _) => "LL0 reduce/reduce conflict",
		}
	}

	pub fn self_conflict(&self) -> bool {
		match self {
			Self::ShiftReduce(_, _, shift_item, reduce_rule) => shift_item.rule == *reduce_rule,
			Self::ReduceReduce(_, _, rule_a, rule_b) => rule_a == rule_b,
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
			Ambiguity::ShiftReduce(_path, _, shift_item, reduce_rule) => {
				if !self.self_conflict() {
					let (span, message) = match reduce_rule.span(grammar) {
						Some(span) => (Some(span), "in conflict with this rule"),
						None => (
							grammar.ty(reduce_rule.return_ty(grammar)).unwrap().span(),
							"in conflict with a rule implicitely defined by this symbol",
						),
					};

					if let Some(span) = span {
						block.highlights_mut().add(
							span,
							Some(message.to_string()),
							source_span::fmt::Style::Note,
						);
					}
				}

				let shift_target = grammar.ty(shift_item.rule.return_ty(grammar)).unwrap();
				let reduce_target = grammar.ty(reduce_rule.return_ty(grammar)).unwrap();

				let (example, shift_span, reduce_span) = build_ambiguity_example(
					grammar,
					shift_item.rule,
					shift_item.offset,
					*reduce_rule,
					reduce_rule.len(grammar),
				);

				let formatted_example = out::format_ambiguity_example(
					&example,
					shift_span,
					format!(
						"shift {} `{}`",
						shift_target.id().name(),
						shift_item.rule.id(grammar).as_str()
					),
					reduce_span,
					format!(
						"reduce {} `{}`",
						reduce_target.id().name(),
						reduce_rule.id(grammar).as_str()
					),
				);

				block.add_note(
					out::NoteType::Note,
					format!(
						"the following phrase is ambiguous: {}",
						Paint::new(example).bold()
					),
				);
				block.add_note(
					out::NoteType::Note,
					format!(
						"it has the following two conflicting decompositions:\n\n{}",
						formatted_example
					),
				)
			}
			Ambiguity::ReduceReduce(path, _, _rule_a, rule_b) => {
				if !self.self_conflict() {
					let (span, message) = match rule_b.span(grammar) {
						Some(span) => (Some(span), "in conflict with this rule"),
						None => (
							grammar.ty(rule_b.return_ty(grammar)).unwrap().span(),
							"in conflict with a rule implicitely defined by this symbol",
						),
					};

					if let Some(span) = span {
						block.highlights_mut().add(
							span,
							Some(message.to_string()),
							source_span::fmt::Style::Note,
						);
					}
				}

				block.add_note(
					out::NoteType::Note,
					format!(
						"the following phrase is ambiguous: {}",
						format_path(grammar, path)
					),
				)
			}
		}
	}
}

impl fmt::Display for Ambiguity {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::ShiftReduce(_, _, _, _) => write!(f, "shift/reduce conflict"),
			Self::ReduceReduce(_, _, _, _) => write!(f, "reduce/reduce conflict"),
		}
	}
}

pub enum State {
	LALR1 {
		action: BTreeMap<Option<u32>, Action>,
		goto: BTreeMap<Index, u32>,
	},
	LR0(lr0::State),
}

pub enum Action {
	Shift(u32),
	Reduce(Rule),
}

pub enum ReduceAction {
	Direct(Rule),
	Lookahead(BTreeMap<Option<u32>, Rule>),
}

impl ReduceAction {
	pub fn into_map(
		self,
		grammar: &Grammar,
		lookahead: &FirstAndFollowGraph,
	) -> BTreeMap<Option<u32>, Rule> {
		match self {
			ReduceAction::Direct(rule) => {
				let target_ty = rule.return_ty(grammar);
				let mut map = BTreeMap::new();
				for &t in lookahead.follow(target_ty) {
					map.insert(t, rule);
				}
				map
			}
			ReduceAction::Lookahead(map) => map,
		}
	}
}

/// LALR1 parsing table.
pub struct LALR1 {
	states: Vec<State>,
}

impl LALR1 {
	pub fn from_non_deterministic(
		grammar: &Grammar,
		nd_table: &NonDeterministic,
	) -> Result<Self, Loc<Ambiguity>> {
		let lookahead = FirstAndFollowGraph::new(grammar);

		let mut states = Vec::new();

		for (q, state) in nd_table.states().iter().enumerate() {
			let mut reduce: Option<ReduceAction> = None;

			for item in &state.items {
				if item.offset >= item.rule.len(grammar) {
					reduce = Some(match reduce {
						Some(other_reduce) => {
							// potential reduce/reduce conflict
							let mut map = other_reduce.into_map(grammar, &lookahead);

							let target_ty = item.rule.return_ty(grammar);
							for &t in lookahead.follow(target_ty) {
								use std::collections::btree_map::Entry;
								match map.entry(t) {
									Entry::Vacant(entry) => {
										entry.insert(item.rule);
									}
									Entry::Occupied(entry) => {
										// confirmed reduce/reduce conflict
										let other_function = *entry.get();

										let path: Vec<_> = nd_table
											.path_to(q as u32)
											.into_iter()
											.map(|(_, symbol)| symbol)
											.collect();
										let span = item.rule.span(grammar).unwrap_or_else(|| {
											grammar.ty(target_ty).unwrap().span().expect("no span")
										});

										return Err(Loc::new(
											Ambiguity::ReduceReduce(
												path,
												t,
												item.rule,
												other_function,
											),
											span,
										));
									}
								}
							}

							ReduceAction::Lookahead(map)
						}
						None => ReduceAction::Direct(item.rule),
					})
				}
			}

			let lalr0_state = match reduce {
				Some(reduce) => {
					let reduce_actions = reduce.into_map(&grammar, &lookahead);

					// search for shift/reduce conflicts.
					for item in &state.items {
						if let Some(symbol) = item.current_symbol(grammar) {
							let firsts = match symbol {
								Symbol::EndOfStream => Firsts::Terminal(Some(None)),
								Symbol::Expr(ty::Expr::Terminal(t)) => {
									Firsts::Terminal(Some(Some(t)))
								}
								Symbol::Expr(ty::Expr::Type(ty)) => {
									Firsts::Type(lookahead.first(ty).iter())
								}
							};

							for t in firsts {
								if let Some(reduce_target) = reduce_actions.get(&t) {
									// confirmed shift/reduce conflict
									let path: Vec<_> = nd_table
										.path_to(q as u32)
										.into_iter()
										.map(|(_, symbol)| symbol)
										.collect();
									let span = item.rule.span(grammar).unwrap_or_else(|| {
										grammar
											.ty(item.rule.return_ty(grammar))
											.unwrap()
											.span()
											.expect("no span")
									});

									return Err(Loc::new(
										Ambiguity::ShiftReduce(path, t, *item, *reduce_target),
										span,
									));
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
							Symbol::EndOfStream => {
								action.insert(None, Action::Shift(*shift_target));
							}
							Symbol::Expr(ty::Expr::Terminal(t)) => {
								action.insert(Some(*t), Action::Shift(*shift_target));
							}
							Symbol::Expr(ty::Expr::Type(ty)) => {
								goto.insert(*ty, *shift_target);
							}
						}
					}

					State::LALR1 { action, goto }
				}
				None => {
					let mut action = BTreeMap::new();
					let mut goto = BTreeMap::new();

					for (t, shift_target) in &state.transitions {
						match t {
							Symbol::EndOfStream => {
								action.insert(None, *shift_target);
							}
							Symbol::Expr(ty::Expr::Terminal(t)) => {
								action.insert(Some(*t), *shift_target);
							}
							Symbol::Expr(ty::Expr::Type(ty)) => {
								goto.insert(*ty, *shift_target);
							}
						}
					}

					State::LR0(lr0::State::Shift { action, goto })
				}
			};

			states.push(lalr0_state)
		}

		Ok(Self { states })
	}
}

pub enum Firsts<'a> {
	Terminal(Option<Option<u32>>),
	Type(std::collections::btree_set::Iter<'a, Option<u32>>),
}

impl<'a> Iterator for Firsts<'a> {
	type Item = Option<u32>;

	fn next(&mut self) -> Option<Option<u32>> {
		match self {
			Self::Terminal(t) => t.take(),
			Self::Type(inner) => inner.next().cloned(),
		}
	}
}
