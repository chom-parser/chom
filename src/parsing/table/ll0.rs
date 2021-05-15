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
	parsing::Error,
	mono::{
		Index,
		Grammar,
		Function,
		ty
	}
};
use super::{
	Item,
	NonDeterministic
};

pub enum Ambiguity {
	/// Shift reduce conflict.
	/// 
	/// The second parameter is the shifting item, the third is the reducing rule. 
	ShiftReduce(Vec<ty::Expr>, Item, Index),
	ReduceReduce(Vec<ty::Expr>, Index, Index)
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

// fn format_rule_instance<R: std::ops::RangeBounds<usize>>(
// 	fmt: &mut source_span::fmt::Formatter,
// 	content: &mut String,
// 	from: Position,
// 	grammar: &Grammar,
// 	rule: &Function,
// 	range: R
// ) -> Position where [ty::Expr]: std::ops::Index<R, Output=[ty::Expr]> {
// 	let metrics = source_span::DEFAULT_METRICS;
// 	let mut span: Span = from.into();

// 	for symbol in &rule.arguments()[range] {
// 		if !content.is_empty() {
// 			content.push(' ');
// 			span.push(' ', &metrics);
// 			span.clear();
// 		}

// 		let instance = symbol.instance(grammar);
// 		for c in instance.chars() {
// 			content.push(c);
// 			span.push(c, &metrics);
// 			fmt.add(span, Some(format!("{}", symbol.format(grammar))), source_span::fmt::Style::Help)
// 		}
// 	}

// 	// format!("{}", fmt.render(content.chars().map(infaible_char), span.aligned(), &metrics).unwrap())
// 	span.end()
// }

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



// fn infaible_char(c: char) -> Result<char, std::convert::Infallible> {
// 	Ok(c)
// }

impl Ambiguity {
	pub fn title(&self) -> &str {
		match self {
			Self::ShiftReduce(_, _, _) => "LL0 shift/reduce conflict",
			Self::ReduceReduce(_, _, _) => "LL0 reduce/reduce conflict"
		}
	}

	pub fn self_conflict(&self) -> bool {
		match self {
			Self::ShiftReduce(_, shift_item, reduce_rule) => shift_item.function == *reduce_rule,
			Self::ReduceReduce(_, rule_a, rule_b) => rule_a == rule_b
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
			Ambiguity::ShiftReduce(_path, shift_item, reduce_rule_index) => {
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
					format!("shift `{}` <{}>", shift_rule.id().as_str(), shift_target.format(grammar)),
					reduce_span,
					format!("reduce `{}` <{}>", reduce_rule.id().as_str(), reduce_target.format(grammar))
				);

				block.add_note(out::NoteType::Note, format!("the following phrase is ambiguous: {}", Paint::new(example).bold()));
				block.add_note(out::NoteType::Note, format!("it has the following two conflicting decompositions:\n\n{}", formatted_example))
			},
			Ambiguity::ReduceReduce(path, _rule_a, rule_b) => {
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
			Self::ShiftReduce(_, _, _) => write!(f, "shift/reduce conflict"),
			Self::ReduceReduce(_, _, _) => write!(f, "reduce/reduce conflict")
		}
	}
}

pub enum State {
	Shift(BTreeMap<ty::Expr, u32>),
	Reduce(Index)
}

/// LL0 parsing table.
pub struct LL0 {
	states: Vec<State>
}

impl LL0 {
	pub fn from_non_deterministic(grammar: &Grammar, nd_table: &NonDeterministic) -> Result<Self, Loc<Ambiguity>> {
		let mut states = Vec::new();
		
		for (q, state) in nd_table.states().iter().enumerate() {
			let mut reduce = None;

			for item in &state.items {
				let rule = grammar.function(item.function).unwrap();
				if item.offset >= rule.arity() {
					if let Some(old_reduce) = reduce.replace(item.function) {
						// reduce/reduce conflict
						let path: Vec<_> = nd_table.path_to(q as u32).into_iter().map(|(_, symbol)| symbol).collect();
						
						let span = rule.span().unwrap_or_else(|| {
							grammar.ty(rule.return_ty()).unwrap().span().expect("no span")
						});
						
						return Err(Loc::new(Ambiguity::ReduceReduce(path, item.function, old_reduce), span))
					}
				}
			}

			let ll0_state = match reduce {
				Some(reduce) => {
					for item in &state.items {
						let rule = grammar.function(item.function).unwrap();
						if item.offset < rule.arity() {
							// shift/reduce conflict
							let path: Vec<_> = nd_table.path_to(q as u32).into_iter().map(|(_, symbol)| symbol).collect();

							let span = rule.span().unwrap_or_else(|| {
								grammar.ty(rule.return_ty()).unwrap().span().expect("no span")
							});

							return Err(Loc::new(Ambiguity::ShiftReduce(path, *item, reduce), span))
						}
					}

					State::Reduce(reduce)
				},
				None => {
					State::Shift(state.transitions.clone())
				}
			};

			states.push(ll0_state)
		}

		Ok(Self {
			states
		})
	}
}