use automaton::{
	Automaton,
	AutomatonMut
};
use std::collections::HashMap;
use crate::{
	Ident,
	CharSet,
	grammar::{
		RegExpDefinition,
		TypedRegExp,
		RegExp,
		RegExpAtom
	}
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum State {
	InitialOf(Ident),
	FinalOf(Ident),
	Anonymous(u32)
}

/// Lexing automaton.
pub type LexAutomaton = automaton::RangeHashAutomaton<State, char>;

/// Lexing table.
pub struct Table {
	next_state: u32,
	aut: LexAutomaton,
	defs: HashMap<Ident, State>
}

impl Table {
	fn new_state(&mut self) -> State {
		let q = State::Anonymous(self.next_state);
		self.next_state += 1;
		q
	}

	fn build(&mut self, def: &RegExpDefinition) {
		// ...
	}

	fn build_typed_regexp(&mut self, e: &TypedRegExp) {
		// ...
	}

	fn build_regexp(&mut self, e: &RegExp) -> (State, State) {
		match e.0.split_first() {
			Some((first_atom, atoms)) => {
				let (i, mut q) = self.build_regexp_atom(first_atom);
				for atom in atoms {
					let (ai, aq) = self.build_regexp_atom(atom);
					self.aut.add_epsilon_transition(q, ai);
					q = aq;
				}

				(i, q)
			},
			None => {
				let q = self.new_state();
				(q.clone(), q)
			}
		}
	}

	fn build_regexp_atom(&mut self, e: &RegExpAtom) -> (State, State) {
		use RegExpAtom::*;
		match e {
			Ref(id) => {
				panic!("TODO")
			},
			CharSet(set) => {
				let a = self.new_state();
				let b = self.new_state();
				self.aut.add_transitions(a.clone(), set.iter().cloned(), b.clone());
				(a, b)
			},
			Literal(str, case_sensitive) => {
				let mut aut = LexAutomaton::new();
				let i = self.new_state();

				let mut q = i.clone();
				for c in str.chars() {
					let next = self.new_state();
					let set = crate::CharSet::from_char(c, *case_sensitive);
					aut.add_transitions(q, set.iter().cloned(), next.clone());
					q = next;
				}

				(i, q)
			},
			Repeat(atom, min, max) => {
				let i = self.new_state();

				let mut q = i.clone();
				for _ in 0..*min {
					let (ai, af) = self.build_regexp_atom(atom);
					self.aut.add_epsilon_transition(q, ai);
					q = af;
				}

				if *max == usize::MAX {
					let (ai, af) = self.build_regexp_atom(atom);
					self.aut.add_epsilon_transition(q.clone(), ai);
					self.aut.add_epsilon_transition(af, q.clone());
				} else {
					let mut qi = q.clone();
					for _ in 0..*max {
						let (ai, af) = self.build_regexp_atom(atom);
						self.aut.add_epsilon_transition(qi, ai);
						self.aut.add_epsilon_transition(af.clone(), q.clone());
						qi = af
					}
				}

				(i, q)
			},
			Or(regexps) => {
				let i = self.new_state();
				let f = self.new_state();

				for regexp in regexps {
					let (ei, ef) = self.build_regexp(regexp);
					self.aut.add_epsilon_transition(i.clone(), ei);
					self.aut.add_epsilon_transition(ef, f.clone());
				}

				(i, f)
			},
			Capture(regexp) => {
				panic!("TODO")
			},
			Group(regexp) => {
				panic!("TODO")
			}
		}
	}
}
