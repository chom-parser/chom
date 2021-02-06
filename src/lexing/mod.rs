use source_span::{
	Loc
};
use std::collections::HashMap;
use crate::{
	Ident,
	CharSet,
	grammar::{
		Grammar,
		Error,
		RegExpDefinition,
		TypedRegExp,
		RegExp,
		RegExpAtom
	}
};

mod token;
mod automaton;

pub use token::*;
pub use automaton::*;

/// Lexing table.
pub struct Table {
	next_state: u32,
	automaton: Automaton
}

impl Table {
	pub fn new(grammar: &Grammar) -> Result<Table, Loc<Error>> {
		let mut table = Table {
			next_state: 0,
			automaton: Automaton::new()
		};

		for (id, def) in &grammar.regexps {
			let (a, b) = table.build(grammar, def.get().unwrap())?;
			table.automaton.add(State::Initial, None, a);
			table.automaton.add(b, None, State::Final(id.clone()));
		}

		Ok(table)
	}

	fn new_state(&mut self) -> State {
		let q = self.next_state;
		self.next_state += 1;
		State::Intermediate(q)
	}

	fn build(&mut self, grammar: &Grammar, def: &RegExpDefinition) -> Result<(State, State), Loc<Error>> {
		self.build_typed_regexp(grammar, &def.exp)
	}

	fn build_typed_regexp(&mut self, grammar: &Grammar, e: &TypedRegExp) -> Result<(State, State), Loc<Error>> {
		self.build_regexp(grammar, &e.exp)
	}

	fn build_regexp(&mut self, grammar: &Grammar, e: &RegExp) -> Result<(State, State), Loc<Error>> {
		match e.0.split_first() {
			Some((first_atom, atoms)) => {
				let (i, mut q) = self.build_regexp_atom(grammar, first_atom)?;
				for atom in atoms {
					let (ai, aq) = self.build_regexp_atom(grammar, atom)?;
					self.automaton.add(q, None, ai);
					q = aq;
				}

				Ok((i, q))
			},
			None => {
				let q = self.new_state();
				Ok((q.clone(), q))
			}
		}
	}

	fn build_regexp_atom(&mut self, grammar: &Grammar, e: &RegExpAtom) -> Result<(State, State), Loc<Error>> {
		use RegExpAtom::*;
		match e {
			Ref(id) => {
				self.build(grammar, grammar.regexp(id)?)
			},
			CharSet(set) => {
				let a = self.new_state();
				let b = self.new_state();
				self.automaton.add(a.clone(), Some(set.clone()), b.clone());
				Ok((a, b))
			},
			Literal(str, case_sensitive) => {
				let i = self.new_state();

				let mut q = i.clone();
				for c in str.chars() {
					let next = self.new_state();
					let set = crate::CharSet::from_char(c, *case_sensitive);
					self.automaton.add(q, Some(set.clone()), next.clone());
					q = next;
				}

				Ok((i, q))
			},
			Repeat(atom, min, max) => {
				let i = self.new_state();

				let mut q = i.clone();
				for _ in 0..*min {
					let (ai, af) = self.build_regexp_atom(grammar, atom)?;
					self.automaton.add(q, None, ai);
					q = af;
				}

				if *max == usize::MAX {
					let (ai, af) = self.build_regexp_atom(grammar, atom)?;
					self.automaton.add(q.clone(), None, ai);
					self.automaton.add(af, None, q.clone());
				} else {
					let mut qi = q.clone();
					for _ in 0..*max {
						let (ai, af) = self.build_regexp_atom(grammar, atom)?;
						self.automaton.add(qi, None, ai);
						self.automaton.add(af.clone(), None, q.clone());
						qi = af
					}
				}

				Ok((i, q))
			},
			Or(regexps) => {
				let i = self.new_state();
				let f = self.new_state();

				for regexp in regexps {
					let (ei, ef) = self.build_regexp(grammar, regexp)?;
					self.automaton.add(i.clone(), None, ei);
					self.automaton.add(ef, None, f.clone());
				}

				Ok((i, f))
			},
			Capture(regexp) => {
				self.build_regexp(grammar, regexp)
			},
			Group(regexp) => {
				self.build_regexp(grammar, regexp)
			}
		}
	}

	pub fn automaton(&self) -> &Automaton {
		&self.automaton
	}
}
