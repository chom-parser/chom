use super::{regexp, Automaton, RegExp, State};
use crate::poly::{terminal, Grammar};

/// Non deterministic lexing table.
pub struct NDTable {
	/// Used to generate unique state ids.
	next_state: u32,

	/// Inner automaton.
	automaton: Automaton,
}

impl NDTable {
	/// Creates a new non deterministic lexing table from the given grammar.
	pub fn new(grammar: &Grammar) -> Self {
		let n = grammar.terminals().len() as u32;
		Self::for_terminals(grammar, 0..n)
	}

	/// Creates a new non dterministic lexing table for a subset of the terminal of the given grammar.
	pub fn for_terminals<T: IntoIterator<Item = u32>>(grammar: &Grammar, terminals: T) -> Self {
		let mut table = NDTable {
			next_state: 0,
			automaton: Automaton::new(),
		};

		for t in terminals {
			table.build(grammar, t)
		}

		table
	}

	/// Creates a new intermediate state.
	fn new_state(&mut self) -> State {
		let q = self.next_state;
		self.next_state += 1;
		State::Intermediate(q)
	}

	/// Adds the necessary automata transitions to recognize the given terminal.
	fn build(&mut self, grammar: &Grammar, id: u32) {
		let terminal = grammar.terminal(id).unwrap();
		match terminal.desc() {
			terminal::Desc::RegExp(exp) => {
				let (a, b) = self.build_regexp(grammar, exp);
				self.automaton.add(State::Initial, None, a);
				self.automaton.add(b, None, State::Final(id))
			}
			terminal::Desc::Whitespace(ws_index) => {
				let exp = RegExp::new(vec![regexp::Atom::Repeat(
					Box::new(regexp::Atom::Ref(*ws_index)),
					1,
					usize::MAX,
				)]);
				let (a, b) = self.build_regexp(grammar, &exp);
				self.automaton.add(State::Initial, None, a);
				self.automaton.add(b, None, State::Final(id))
			}
		}
	}

	fn build_regexp(&mut self, grammar: &Grammar, e: &RegExp) -> (State, State) {
		match e.0.split_first() {
			Some((first_atom, atoms)) => {
				let (i, mut q) = self.build_regexp_atom(grammar, first_atom);
				for atom in atoms {
					let (ai, aq) = self.build_regexp_atom(grammar, atom);
					self.automaton.add(q, None, ai);
					q = aq;
				}

				(i, q)
			}
			None => {
				let q = self.new_state();
				(q.clone(), q)
			}
		}
	}

	fn build_regexp_atom(&mut self, grammar: &Grammar, e: &regexp::Atom) -> (State, State) {
		use regexp::Atom::*;
		match e {
			Ref(i) => self.build_regexp(grammar, &grammar.regexp(*i).unwrap().exp),
			CharSet(set) => {
				let a = self.new_state();
				let b = self.new_state();
				self.automaton.add(a.clone(), Some(set.clone()), b.clone());
				(a, b)
			}
			Literal(str, case_sensitive) => {
				let i = self.new_state();

				let mut q = i.clone();
				for c in str.chars() {
					let next = self.new_state();
					let set = crate::CharSet::from_char(c, *case_sensitive);
					self.automaton.add(q, Some(set.clone()), next.clone());
					q = next;
				}

				(i, q)
			}
			Repeat(atom, min, max) => {
				let i = self.new_state();

				let mut q = i.clone();
				for _ in 0..*min {
					let (ai, af) = self.build_regexp_atom(grammar, atom);
					self.automaton.add(q, None, ai);
					q = af;
				}

				if *max == usize::MAX {
					let (ai, af) = self.build_regexp_atom(grammar, atom);
					self.automaton.add(q.clone(), None, ai);
					self.automaton.add(af, None, q.clone());
				} else {
					let mut qi = q;
					q = self.new_state();
					self.automaton.add(qi.clone(), None, q.clone());
					for _ in (min + 1)..=*max {
						let (ai, af) = self.build_regexp_atom(grammar, atom);
						self.automaton.add(qi, None, ai);
						self.automaton.add(af.clone(), None, q.clone());
						qi = af
					}
				}

				(i, q)
			}
			Or(regexps) => {
				let i = self.new_state();
				let f = self.new_state();

				for regexp in regexps {
					let (ei, ef) = self.build_regexp(grammar, regexp);
					self.automaton.add(i.clone(), None, ei);
					self.automaton.add(ef, None, f.clone());
				}

				(i, f)
			}
			Group(regexp) => self.build_regexp(grammar, regexp),
		}
	}

	pub fn automaton(&self) -> &Automaton {
		&self.automaton
	}
}
