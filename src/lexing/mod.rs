use crate::{
	CharSet,
	grammar::{
		RegExp,
		RegExpAtom
	}
};

pub type State = u32;

/// Lexing table.
pub struct Table {
	next_state: State
}

/// Lexing automaton.
pub type Automaton = automaton::Automaton<State, CharSet>;

impl Table {
	fn new_state(&mut self) -> State {
		let q = self.next_state;
		self.next_state += 1;
		q
	}

	fn build_regexp_atom_automaton(&mut self, e: &RegExpAtom) -> Automaton {
		use RegExpAtom::*;
		match e {
			Ref(id) => {
				panic!("TODO")
			},
			CharSet(set) => {
				let mut aut = Automaton::new();
				let a = self.new_state();
				let b = self.new_state();
				aut.add_initial_state(a);
				aut.add_final_state(b);
				aut.add_transition(a, set.clone(), b);
				aut
			},
			Literal(str, case_sensitive) => {
				let mut aut = Automaton::new();
				let mut q = self.new_state();
				aut.add_initial_state(q);

				for c in str.chars() {
					let next = self.new_state();
					let set = crate::CharSet::from_char(c, *case_sensitive);
					aut.add_transition(q, set, next);
					q = next;
				}

				aut.add_final_state(q);
				aut
			},
			_ => panic!("TODO")
		}
	}
}
