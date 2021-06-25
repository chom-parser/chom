use crate::{charset::DisplayCharRange, poly::Grammar};
use btree_range_map::AnyRange;
use btree_slab::BTreeSet;
use std::{collections::HashSet, fmt};

mod automaton;
mod error;
mod nd_table;
pub mod regexp;
mod table;
pub mod token;

pub use automaton::*;
pub use error::*;
pub use nd_table::*;
pub use regexp::RegExp;
pub use table::*;
pub use token::Token;

impl State {
	pub fn display<'g, 'q>(&'q self, g: &'g Grammar) -> DisplayState<'g, 'q> {
		DisplayState(g, self)
	}
}

pub struct DisplayState<'g, 'q>(&'g Grammar, &'q State);

impl<'g, 'q> fmt::Display for DisplayState<'g, 'q> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.1 {
			State::Initial => write!(f, "initial"),
			State::Intermediate(q) => write!(f, "q{}", q),
			State::Final(id) => {
				let terminal = &self.0.terminals()[*id as usize].0;
				write!(f, "\"{}\"", terminal)
			}
		}
	}
}

pub struct DisplayStateSet<'g, 'q>(&'g Grammar, &'q BTreeSet<State>);

impl<'g, 'q> fmt::Display for DisplayStateSet<'g, 'q> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "\"")?;
		for (i, q) in self.1.iter().enumerate() {
			if i > 0 {
				write!(f, ",")?
			}

			match q {
				State::Initial => write!(f, "initial")?,
				State::Intermediate(q) => write!(f, "q{}", q)?,
				State::Final(id) => {
					let terminal = &self.0.terminals()[*id as usize].0;
					write!(f, "{}", terminal)?
				}
			}
		}

		write!(f, "\"")
	}
}

pub struct DisplayDetState<'g, 'q>(&'g Grammar, &'q DetState);

impl<'g, 'q> fmt::Display for DisplayDetState<'g, 'q> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.1 {
			DetState::Initial => write!(f, "initial"),
			DetState::Intermediate(i) => write!(f, "q{}", i),
			DetState::Final(token_id, i) => {
				let terminal = &self.0.terminals()[*token_id as usize].0;
				write!(f, "\"{}\" ({})", terminal, i)
			}
		}
	}
}

pub fn terminals_starting_with<'a, 'b>(
	aut: &'a DetAutomaton<BTreeSet<State>>,
	range: &'b AnyRange<char>,
) -> TerminalsStartingWith<'a, 'b> {
	TerminalsStartingWith {
		aut,
		range,
		initial_transitions: aut.successors(aut.initial_state()),
		reachable_terminals: None,
	}
}

pub struct TerminalsStartingWith<'a, 'b> {
	aut: &'a DetAutomaton<BTreeSet<State>>,
	range: &'b AnyRange<char>,
	initial_transitions: DetSuccessors<'a, BTreeSet<State>>,
	reachable_terminals: Option<TerminalsReachableFrom<'a>>,
}

impl<'a, 'b> Iterator for TerminalsStartingWith<'a, 'b> {
	type Item = u32;

	fn next(&mut self) -> Option<u32> {
		loop {
			match &mut self.reachable_terminals {
				Some(reachable_terminals) => match reachable_terminals.next() {
					Some(id) => break Some(id),
					None => self.reachable_terminals = None,
				},
				None => match self.initial_transitions.next() {
					Some((label, targets)) => {
						if self.range.intersects(label) {
							self.reachable_terminals =
								Some(TerminalsReachableFrom::new(self.aut, targets))
						}
					}
					None => break None,
				},
			}
		}
	}
}

pub struct TerminalsReachableFrom<'a> {
	aut: &'a DetAutomaton<BTreeSet<State>>,
	visited: HashSet<&'a BTreeSet<State>>,
	stack: Vec<&'a BTreeSet<State>>,
}

impl<'a> TerminalsReachableFrom<'a> {
	pub fn new(
		aut: &'a DetAutomaton<BTreeSet<State>>,
		state: &'a BTreeSet<State>,
	) -> TerminalsReachableFrom<'a> {
		TerminalsReachableFrom {
			aut,
			visited: HashSet::new(),
			stack: vec![state],
		}
	}
}

impl<'a> Iterator for TerminalsReachableFrom<'a> {
	type Item = u32;

	fn next(&mut self) -> Option<u32> {
		while let Some(states) = self.stack.pop() {
			if self.visited.insert(states) {
				for (_range, target) in self.aut.successors(states) {
					self.stack.push(target)
				}

				for q in states {
					if let State::Final(id) = q {
						return Some(*id);
					}
				}
			}
		}

		None
	}
}

fn build_prefix_to(aut: &DetAutomaton<BTreeSet<State>>, state: &BTreeSet<State>) -> String {
	build_prefix_to_from(
		aut,
		state,
		aut.initial_state(),
		String::new(),
		HashSet::new(),
	)
	.expect("no prefix leads to the given state")
}

fn build_prefix_to_from(
	aut: &DetAutomaton<BTreeSet<State>>,
	state: &BTreeSet<State>,
	from: &BTreeSet<State>,
	current: String,
	visited: HashSet<BTreeSet<State>>,
) -> Option<String> {
	for (range, target) in aut.successors(from) {
		if !visited.contains(target) {
			let mut next = current.clone();
			if range.is_empty() {
				range.is_empty();
				panic!("what?")
			}
			next.push(range.pick().unwrap());

			if target == state {
				return Some(next);
			}

			let mut visited = visited.clone();
			visited.insert(target.clone());

			if let Some(string) = build_prefix_to_from(aut, state, target, next, visited) {
				return Some(string);
			}
		}
	}

	None
}

impl Automaton {
	pub fn dot_write<W: std::io::Write>(&self, g: &Grammar, f: &mut W) -> std::io::Result<()> {
		write!(f, "digraph {{\n")?;

		for (source, transitions) in self.transitions() {
			for (label, targets) in transitions {
				for target in targets {
					let source = source.display(g);
					let target = target.display(g);

					match label {
						Some(label) => {
							write!(f, "\t{} -> {} [ label=\"{}\" ]\n", source, target, label)?
						}
						None => write!(f, "\t{} -> {}\n", source, target)?,
					}
				}
			}
		}

		write!(f, "}}")
	}
}

impl DetAutomaton<BTreeSet<State>> {
	pub fn dot_write<W: std::io::Write>(&self, g: &Grammar, f: &mut W) -> std::io::Result<()> {
		write!(f, "digraph {{\n")?;

		for (source, transitions) in self.transitions() {
			for (label, target) in transitions {
				let source = DisplayStateSet(g, source);
				let target = DisplayStateSet(g, target);
				write!(
					f,
					"\t{} -> {} [ label=\"{}\" ]\n",
					source,
					target,
					DisplayCharRange(label)
				)?
			}
		}

		write!(f, "}}")
	}
}

impl DetAutomaton<DetState> {
	pub fn dot_write<W: std::io::Write>(&self, g: &Grammar, f: &mut W) -> std::io::Result<()> {
		write!(f, "digraph {{\n")?;

		for (source, transitions) in self.transitions() {
			for (label, target) in transitions {
				let source = DisplayDetState(g, source);
				let target = DisplayDetState(g, target);
				write!(
					f,
					"\t{} -> {} [ label=\"{}\" ]\n",
					source,
					target,
					DisplayCharRange(label)
				)?
			}
		}

		write!(f, "}}")
	}
}
