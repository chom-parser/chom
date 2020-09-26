struct State {
	// ...
}

struct Transitions {
	// ...
}

struct Automaton {
	init: State,
	last: State,
	delta: HashMap<State, Transitions>
}

impl From<CharSet> for Automaton {
	fn from(set: CharSet) -> Automaton {
		let a = State::new();
		let b = State::new();
		let mut trans = Transitions::new();
		trans.add(set, b);
		let mut delta = HashMap::new();
		delta.insert(a, trans);
		Automaton {
			init: a,
			last: b,
			delta
		}
	}
}

impl Automaton {
	fn from(e: &RegExp) -> Automaton {
		match e {
			Ident(id) => table.automaton(id),
			CharSet(set) => set.into(),
			Literal(str, case_sensitive) => {
				let (first, rest) = str.split_first().unwrap();
				let set = if case_sensitive {
					CharSet::from(first)
				} else {
					CharSet::from_case_insensitive(first, case_sensitive)
				};

				let aut = set.into();
				let next = Literal(rest.to_string(), case_sensitive).into();
				aut.union(next)
			},
			Repeat(atom, min, max) => {
				let aut = Automaton::from(atom);
				aut.repeat(min, max);
			},
			Or(exps) => {
				// ...
			},
			Capture(exp) => {
				// ...
			},
			Group(exp) => {
				// ...
			}
		}
	}

	fn again(&mut self) -> State {
		// ...
	}

	fn repeat(&mut self, min: usize, max: usize) {
		let mut q = self.init;
		if min > 0 {
			q = self.last;
		}

		for i in 2..min {
			q = self.again();
		}

		if max == std::usize::MAX {
			self.
		}
	}
}

struct LexTable {
	// ...
}

impl LexTable {
	fn from_grammar(g: &Grammar) -> Result<LexTable> {
		let tokens = g.extract_tokens();
		// ...
	}
}
