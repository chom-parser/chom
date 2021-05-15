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
	CharSet,
	poly::{
		Grammar,
		terminal,
		Terminal
	},
	charset::DisplayCharRange,
};

pub mod regexp;
pub mod token;
mod automaton;

pub use regexp::RegExp;
pub use token::Token;
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
						block.add_note(out::NoteType::Note, format!("this could be the {} {}, followed by some {} starting with {}", terminal.format(grammar), Paint::new(prefix).bold(), other_terminal.format(grammar), Paint::new(next.first().unwrap()).bold()));
						block.add_note(out::NoteType::Note, format!("this could also be some {} starting with {}{}", other_terminal.format(grammar), Paint::new(prefix).bold(), Paint::new(next.first().unwrap()).bold()));
					},
					TerminalAmbiguity::ReduceReduce { token } => {
						block.add_note(out::NoteType::Note, format!("the following sequence of character is ambiguous:\n\n    {}\n\n", Paint::new(token).bold()));
						block.add_note(out::NoteType::Note, format!("this could also be some {}", terminal.format(grammar)));
						block.add_note(out::NoteType::Note, format!("this could also be some {}", other_terminal.format(grammar)));
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

		// let stdout = std::io::stdout();
		// let mut out = stdout.lock();
		// nd_table.automaton().dot_write(grammar, &mut out).unwrap();

		let det_automaton = nd_table.automaton().determinize();

		// let stdout = std::io::stdout();
		// let mut out = stdout.lock();
		// det_automaton.dot_write(grammar, &mut out).unwrap();

		let automaton = simplify_det_automaton(grammar, &det_automaton)?;

		Ok(Self {
			automaton
		})
	}

	pub fn automaton(&self) -> &DetAutomaton<DetState> {
		&self.automaton
	}
}

pub fn terminals_starting_with<'a, 'b>(aut: &'a DetAutomaton<BTreeSet<State>>, range: &'b AnyRange<char>) -> TerminalsStartingWith<'a, 'b> {
	TerminalsStartingWith {
		aut,
		range,
		initial_transitions: aut.successors(aut.initial_state()),
		reachable_terminals: None
	}
}

pub struct TerminalsStartingWith<'a, 'b> {
	aut: &'a DetAutomaton<BTreeSet<State>>,
	range: &'b AnyRange<char>,
	initial_transitions: DetSuccessors<'a, BTreeSet<State>>,
	reachable_terminals: Option<TerminalsReachableFrom<'a>>
}

impl<'a, 'b> Iterator for TerminalsStartingWith<'a, 'b> {
	type Item = u32;

	fn next(&mut self) -> Option<u32> {
		loop {
			match &mut self.reachable_terminals {
				Some(reachable_terminals) => {
					match reachable_terminals.next() {
						Some(id) => break Some(id),
						None => self.reachable_terminals = None
					}
				},
				None => {
					match self.initial_transitions.next() {
						Some((label, targets)) => {
							if self.range.intersects(label) {
								self.reachable_terminals = Some(TerminalsReachableFrom::new(self.aut, targets))
							}
						},
						None => break None
					}
				}
			}
		}
	}
}

pub struct TerminalsReachableFrom<'a> {
	aut: &'a DetAutomaton<BTreeSet<State>>,
	visited: HashSet<&'a BTreeSet<State>>,
	stack: Vec<&'a BTreeSet<State>>
}

impl<'a> TerminalsReachableFrom<'a> {
	pub fn new(
		aut: &'a DetAutomaton<BTreeSet<State>>,
		state: &'a BTreeSet<State>
	) -> TerminalsReachableFrom<'a> {
		TerminalsReachableFrom {
			aut,
			visited: HashSet::new(),
			stack: vec![state]
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
						return Some(*id)
					}
				}
			}
		}

		None
	}
}

fn build_prefix_to(aut: &DetAutomaton<BTreeSet<State>>, state: &BTreeSet<State>) -> String {
	build_prefix_to_from(aut, state, aut.initial_state(), String::new(), HashSet::new()).expect("no prefix leads to the given state")
}

fn build_prefix_to_from(aut: &DetAutomaton<BTreeSet<State>>, state: &BTreeSet<State>, from: &BTreeSet<State>, current: String, visited: HashSet<BTreeSet<State>>) -> Option<String> {
	for (range, target) in aut.successors(from) {
		if !visited.contains(target) {
			let mut next = current.clone();
			if range.is_empty() {
				range.is_empty();
				panic!("what?")
			}
			next.push(range.pick().unwrap());

			if target == state {
				return Some(next)
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

	#[derive(Hash, PartialEq, Eq)]
	pub enum Partition {
		Intermediate,
		Final(u32)
	}

	let partition = aut.try_partition(|states| {
		let mut terminal = None;
	
		for q in states {
			match q {
				State::Initial => {
					// Note: we have already checked that there are no empty terminals.
					return Ok(Partition::Intermediate)
				},
				State::Intermediate(_) => (),
				State::Final(id) => {
					if let Some(other_id) = terminal.replace(id) {
						let span = grammar.terminals()[*id as usize].1.iter().next().unwrap().span();
						return Err(Loc::new(Error::AmbiguousTerminals(*id, *other_id, TerminalAmbiguity::ReduceReduce {
							token: build_prefix_to(aut, states)
						}), span))
					}
				}
			}
		}

		match terminal {
			Some(id) => Ok(Partition::Final(*id)),
			None => Ok(Partition::Intermediate)
		}
	})?;

	let minimal_aut = aut.minimize(partition.into_iter().map(|(_, states)| states));

	let minimal_aut = minimal_aut.map(|class| {
		let mut states: BTreeSet<State> = BTreeSet::new();
		for q in class {
			states.extend(q.into_iter().cloned());
		}
		states
	});

	// let stdout = std::io::stdout();
	// let mut out = stdout.lock();
	// minimal_aut.dot_write(grammar, &mut out).unwrap();

	Ok(minimal_aut.map(|q| det_states(&mut terminal_map, &mut intermediate_count, q)))
}

fn det_states(terminal_map: &mut HashMap<u32, u32>, intermediate_count: &mut u32, states: &BTreeSet<State>) -> DetState {
	for q in states {
		match q {
			State::Initial => {
				// Note: we have already checked that there are no empty terminals.
				return DetState::Initial
			},
			State::Intermediate(_) => (),
			State::Final(id) => {
				// Note: we have already checked that there are no reduce-reduce conflicts.
				let i = match terminal_map.get_mut(&id).cloned() {
					Some(i) => i,
					None => 0
				};
	
				terminal_map.insert(*id, i+1);
				return DetState::Final(*id, i)
			}
		}
	}

	let i = *intermediate_count;
	*intermediate_count = i + 1;
	DetState::Intermediate(i)
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

		for (id, (terminal, _uses)) in grammar.terminals().iter().enumerate() {
			// let terminal = uses.iter().next().unwrap();
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
	fn build(&mut self, grammar: &Grammar, id: u32, terminal: &Terminal) {
		match terminal.desc() {
			terminal::Desc::RegExp(exp) => {
				let (a, b) = self.build_regexp(grammar, exp);
				self.automaton.add(State::Initial, None, a);
				self.automaton.add(b, None, State::Final(id))
			},
			terminal::Desc::Whitespace => {
				let ws = CharSet::whitespace();
				let exp = RegExp::new(vec![regexp::Atom::Repeat(Box::new(regexp::Atom::CharSet(ws)), 1, usize::MAX)]);
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
			},
			None => {
				let q = self.new_state();
				(q.clone(), q)
			}
		}
	}

	fn build_regexp_atom(&mut self, grammar: &Grammar, e: &regexp::Atom) -> (State, State) {
		use regexp::Atom::*;
		match e {
			Ref(i) => {
				self.build_regexp(grammar, &grammar.regexp(*i).unwrap().exp)
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