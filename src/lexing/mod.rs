use std::{
	fmt,
	collections::{
		HashSet,
		HashMap
	}
};
use btree_slab::BTreeSet;
use btree_range_map::{
	AnyRange,
};
use source_span::{
	Loc
};
use crate::{
	out,
	grammar::{
		Grammar,
		Terminal,
		LocTerminal
	},
	charset::DisplayCharRange,
};

mod token;
mod automaton;

pub use token::*;
pub use automaton::*;

#[derive(Debug)]
pub enum Error {
	AmbiguousTerminals(u32, u32, TerminalAmbiguity)
}

#[derive(Debug)]
pub enum TerminalAmbiguity {
	ShiftReduce {
		prefix: String,
		next: btree_range_map::AnyRange<char>
	},
	ReduceReduce {
		token: String
	}
}

fn format_terminal(terminal: &Terminal) -> String {
	match terminal {
		Terminal::RegExp(RegExp(atoms)) if atoms.len() == 1 => {
			match &atoms[0] {
				RegExpAtom::Ref(id) => {
					format!("{}", id)
				},
				_ => format!("terminal {}", terminal)
			}
		},
		terminal => format!("terminal {}", terminal)
	}
}

impl Error {
	pub fn title(&self) -> String {
		match self {
			Error::AmbiguousTerminals(_, _, _) => format!("ambiguous terminals")
		}
	}

	pub fn fill_block(&self, grammar: &Grammar, block: &mut out::Block) {
		use yansi::Paint;

		match self {
			Error::AmbiguousTerminals(id, other_id, ambiguity) => {
				let terminal = &grammar.terminals()[*id as usize].0;
				let other_terminal = &grammar.terminals()[*other_id as usize].0;
				let other_span = grammar.terminals()[*other_id as usize].1.iter().next().unwrap().span();

				block.highlights_mut().add(other_span, Some("ambiguous with this terminal".to_string()), out::WARNING);
			
				match ambiguity {
					TerminalAmbiguity::ShiftReduce { prefix, next } => {
						block.add_note(out::NoteType::Note, format!("the following sequence of character is ambiguous:\n\n    {}{}\n\n", Paint::new(prefix).bold(), Paint::new(next.first().unwrap()).bold()));
						block.add_note(out::NoteType::Note, format!("this could be the {} {}, followed by some {} starting with {}", format_terminal(terminal), Paint::new(prefix).bold(), format_terminal(other_terminal), Paint::new(next.first().unwrap()).bold()));
						block.add_note(out::NoteType::Note, format!("this could also be some {} starting with {}{}", format_terminal(other_terminal), Paint::new(prefix).bold(), Paint::new(next.first().unwrap()).bold()));
					},
					TerminalAmbiguity::ReduceReduce { token } => {
						block.add_note(out::NoteType::Note, format!("the following sequence of character is ambiguous:\n\n    {}\n\n", Paint::new(token).bold()));
						block.add_note(out::NoteType::Note, format!("this could also be some {}", format_terminal(terminal)));
						block.add_note(out::NoteType::Note, format!("this could also be some {}", format_terminal(other_terminal)));
					}
				}
			},
			// _ => ()
		}
	}
}

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

pub struct Table {
	automaton: DetAutomaton<DetState>
}

impl Table {
	/// Creates a new lexing table from the given grammar.
	pub fn new(grammar: &Grammar) -> Result<Self, Loc<Error>> {
		let nd_table = NDTable::new(grammar)?;

		let stdout = std::io::stdout();
		let mut out = stdout.lock();
		nd_table.automaton().dot_write(grammar, &mut out).unwrap();
		// det_automaton.dot_write(grammar, &mut out).unwrap();

		let det_automaton = nd_table.automaton().determinize();
		let automaton = simplify_det_automaton(grammar, &det_automaton)?;

		Ok(Self {
			automaton
		})
	}

	pub fn automaton(&self) -> &DetAutomaton<DetState> {
		&self.automaton
	}
}

fn token_starting_with(aut: &DetAutomaton<BTreeSet<State>>, range: &AnyRange<char>) -> Option<u32> {
	for (label, targets) in aut.successors(aut.initial_state()) {
		if range.intersects(label) {
			return Some(build_token_from(aut, targets))
		}
	}

	None
}

fn build_token_from(aut: &DetAutomaton<BTreeSet<State>>, states: &BTreeSet<State>) -> u32 {
	let mut visited = HashSet::new();
	let mut stack = vec![states];
	
	while let Some(states) = stack.pop() {
		if visited.insert(states) {
			for q in states {
				if let State::Final(id) = q {
					return *id
				}
			}
	
			for (_range, target) in aut.successors(states) {
				stack.push(target)
			}
		}
	}

	panic!("no token can be built from the given state")
}

fn build_prefix_to(aut: &DetAutomaton<BTreeSet<State>>, state: &State) -> String {
	build_prefix_to_from(aut, state, aut.initial_state(), String::new(), HashSet::new()).expect("no prefix leads to the given state")
}

fn build_prefix_to_from(aut: &DetAutomaton<BTreeSet<State>>, state: &State, from: &BTreeSet<State>, current: String, visited: HashSet<BTreeSet<State>>) -> Option<String> {
	for (range, target) in aut.successors(from) {
		if !visited.contains(target) {
			let mut next = current.clone();
			next.push(range.first().unwrap());

			for q in target {
				if q == state {
					return Some(next)
				}
			}

			let mut visited = visited.clone();
			visited.insert(target.clone());

			if let Some(string) = build_prefix_to_from(aut, state, target, next, visited) {
				return Some(string)
			}
		}
	}

	None
}

fn simplify_det_automaton(grammar: &Grammar, aut: &DetAutomaton<BTreeSet<State>>) -> Result<DetAutomaton<DetState>, Loc<Error>> {
	let mut terminal_map = HashMap::new();
	let mut intermediate_count = 0;
	aut.try_map(|q| try_det_states(grammar, aut, &mut terminal_map, &mut intermediate_count, q))
}

fn try_det_states(grammar: &Grammar, aut: &DetAutomaton<BTreeSet<State>>, terminal_map: &mut HashMap<u32, u32>, intermediate_count: &mut u32, states: &BTreeSet<State>) -> Result<DetState, Loc<Error>> {
	let mut terminal = None;
	
	for q in states {
		match q {
			State::Initial => {
				// Note: we have already checked that there are no empty terminals.
				return Ok(DetState::Initial)
			},
			State::Intermediate(_) => (),
			State::Final(id) => {
				let span = grammar.terminals()[*id as usize].1.iter().next().unwrap().span();

				// Check for ambiguities.
				for (label, _other_states) in aut.successors(states) {
					if let Some(other_id) = token_starting_with(aut, label) {
						return Err(Loc::new(Error::AmbiguousTerminals(*id, other_id, TerminalAmbiguity::ShiftReduce {
							prefix: build_prefix_to(aut, q),
							next: label.clone()
						}), span))
					}
				}

				// for other_states in aut.reachable_states_from(states) {
				// 	for r in other_states {
				// 		match r {
				// 			State::Final(other_id) if other_id != id => {
				// 				let span = grammar.terminals()[*id as usize].1.iter().next().unwrap().span();
				// 				let other_span = grammar.terminals()[*other_id as usize].1.iter().next().unwrap().span();
				// 				return Err(Loc::new(Error::AmbiguousTerminals(*id, *other_id, other_span), span))
				// 			},
				// 			_ => ()
				// 		}
				// 	}
				// }

				if let Some(other_id) = terminal.replace(id) {
					return Err(Loc::new(Error::AmbiguousTerminals(*id, *other_id, TerminalAmbiguity::ReduceReduce {
						token: build_prefix_to(aut, q)
					}), span))
				}
			}
		}
	}

	match terminal {
		Some(id) => {
			let i = match terminal_map.get_mut(&id).cloned() {
				Some(i) => i,
				None => 0
			};

			terminal_map.insert(*id, i+1);
			Ok(DetState::Final(*id, i))
		},
		None => {
			let i = *intermediate_count;
			*intermediate_count = i + 1;
			Ok(DetState::Intermediate(i))
		}
	}
}

/// Non deterministic lexing table.
struct NDTable {
	next_state: u32,
	automaton: Automaton
}

impl NDTable {
	/// Creates a new non deterministic lexing table from the given grammar.
	pub fn new(grammar: &Grammar) -> Result<Self, Loc<Error>> {
		let mut table = NDTable {
			next_state: 0,
			automaton: Automaton::new()
		};

		for (id, (_, uses)) in grammar.terminals().iter().enumerate() {
			let terminal = uses.iter().next().unwrap();
			table.build(grammar, id as u32, terminal)
		}

		Ok(table)
	}

	/// Creates a new intermediate state.
	fn new_state(&mut self) -> State {
		let q = self.next_state;
		self.next_state += 1;
		State::Intermediate(q)
	}

	/// Adds the necessary automata transitions to recognize the given terminal.
	fn build(&mut self, grammar: &Grammar, id: u32, terminal: &LocTerminal) {
		match terminal {
			LocTerminal::RegExp(exp) => {
				let (a, b) = self.build_regexp(grammar, exp);
				self.automaton.add(State::Initial, None, a);
				self.automaton.add(b, None, State::Final(id))
			}
		}
	}

	fn build_regexp(&mut self, grammar: &Grammar, e: &LocRegExp) -> (State, State) {
		match e.0.split_first() {
			Some((first_atom, atoms)) => {
				let (i, mut q) = self.build_regexp_atom(grammar, first_atom);
				for atom in atoms {
					let (ai, aq) = self.build_regexp_atom(grammar, atom);
					self.automaton.add(q, None, ai);
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

	fn build_regexp_atom(&mut self, grammar: &Grammar, e: &LocRegExpAtom) -> (State, State) {
		use LocRegExpAtom::*;
		match e {
			Ref(id) => {
				self.build_regexp(grammar, &grammar.regexp(id).unwrap().exp.as_ref())
			},
			CharSet(set) => {
				let a = self.new_state();
				let b = self.new_state();
				self.automaton.add(a.clone(), Some(set.clone()), b.clone());
				(a, b)
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

				(i, q)
			},
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
					for _ in (min+1)..=*max {
						let (ai, af) = self.build_regexp_atom(grammar, atom);
						self.automaton.add(qi, None, ai);
						self.automaton.add(af.clone(), None, q.clone());
						qi = af
					}
				}

				(i, q)
			},
			Or(regexps) => {
				let i = self.new_state();
				let f = self.new_state();

				for regexp in regexps {
					let (ei, ef) = self.build_regexp(grammar, regexp);
					self.automaton.add(i.clone(), None, ei);
					self.automaton.add(ef, None, f.clone());
				}

				(i, f)
			},
			Group(regexp) => {
				self.build_regexp(grammar, regexp)
			},
 		}
	}

	pub fn automaton(&self) -> &Automaton {
		&self.automaton
	}
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
						Some(label) => write!(f, "\t{} -> {} [ label=\"{}\" ]\n", source, target, label)?,
						None => write!(f, "\t{} -> {}\n", source, target)?
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
				write!(f, "\t{} -> {} [ label=\"{}\" ]\n", source, target, DisplayCharRange(label))?
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
				write!(f, "\t{} -> {} [ label=\"{}\" ]\n", source, target, DisplayCharRange(label))?
			}
		}

		write!(f, "}}")
	}
}