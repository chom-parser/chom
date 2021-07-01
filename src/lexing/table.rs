use super::{DetAutomaton, DetState, Error, NDTable, State, TerminalAmbiguity};
use crate::poly::Grammar;
use btree_slab::BTreeSet;
use source_span::Loc;
use std::collections::{HashMap, HashSet};

/// Deterministic lexing table.
pub struct Table {
	/// Automata of the graph.
	automata: Vec<DetAutomaton<DetState>>,

	/// Maps terminals to automata.
	sub_automata: HashMap<u32, u32>,
}

impl Table {
	pub fn root(&self) -> &DetAutomaton<DetState> {
		&self.automata[0]
	}

	pub fn root_index(&self) -> u32 {
		0
	}

	pub fn automaton(&self, i: u32) -> Option<&DetAutomaton<DetState>> {
		self.automata.get(i as usize)
	}

	pub fn sub_automaton_index(&self, terminal: u32) -> Option<u32> {
		self.sub_automata.get(&terminal).cloned()
	}

	pub fn sub_automaton(&self, terminal: u32) -> Option<&DetAutomaton<DetState>> {
		self.sub_automaton_index(terminal)
			.map(|i| &self.automata[i as usize])
	}

	pub fn new<'a>(grammar: &Grammar) -> Result<Self, Loc<Error>> {
		use std::collections::hash_map::Entry;
		let nd_table = NDTable::new(grammar);

		// let stdout = std::io::stdout();
		// let mut out = stdout.lock();
		// nd_table.automaton().dot_write(grammar, &mut out).unwrap();

		let det_automaton = nd_table.automaton().determinize();
		let det_states = det_automaton.states();

		// "Collocations" of a terminal: the other terminals that share a final state with it.
		struct Collocations<'a> {
			// Terminals that are always in the same final state.
			// Those are "parent" terminals
			// (or in reduce-reduce conflict if they have no independant final states).
			always: HashSet<u32>,

			// Terminal that are sometimes in the same final state.
			// Those are either "child" terminals
			// (or in reduce-reduce conflict if they have independant final states).
			// Each terminal is attached to a state in which it is final with the considered parent terminal.
			sometimes: HashSet<u32>,

			// Associate each terminal to a collocative state.
			states: HashMap<u32, &'a BTreeSet<State>>,
		}

		fn add_collocation<'a>(
			map: &mut HashMap<u32, Collocations<'a>>,
			t: u32,
			terminals: &HashSet<u32>,
			state: &'a BTreeSet<State>,
		) {
			match map.entry(t) {
				Entry::Vacant(entry) => {
					let mut always = terminals.clone();
					always.remove(&t);

					let mut states = HashMap::new();
					states.insert(t, state);

					entry.insert(Collocations {
						always,
						sometimes: HashSet::new(),
						states,
					});
				}
				Entry::Occupied(mut entry) => {
					let collocations = entry.get_mut();

					let sometimes = &mut collocations.sometimes;
					collocations.always.retain(move |u| {
						let contained = terminals.contains(u);
						if !contained {
							sometimes.insert(*u);
						}
						contained
					});

					for u in terminals {
						if t != *u && !collocations.always.contains(u) {
							collocations.sometimes.insert(*u);
							collocations.states.insert(*u, state);
						}
					}
				}
			}
		}

		// Compute the collocations.
		let mut collocations = HashMap::new();
		for det_state in &det_states {
			// Set of terminals recognized by `det_state`.
			let mut terminals = HashSet::new();
			for q in det_state.iter() {
				if let State::Final(t) = q {
					terminals.insert(*t);
				}
			}

			for t in &terminals {
				add_collocation(&mut collocations, *t, &terminals, det_state)
			}
		}

		// Acyclic transitive parent relation.
		let mut parents = HashMap::new();
		// Acyclic transitive child relation.
		let mut children = HashMap::new();

		fn add_item(map: &mut HashMap<u32, HashSet<u32>>, a: u32, b: u32) {
			match map.entry(a) {
				Entry::Vacant(entry) => {
					let mut set = HashSet::new();
					set.insert(b);
					entry.insert(set);
				}
				Entry::Occupied(mut entry) => {
					entry.get_mut().insert(b);
				}
			}
		}

		fn reduce_reduce_conflict(
			grammar: &Grammar,
			aut: &DetAutomaton<BTreeSet<State>>,
			t: u32,
			u: u32,
			t_collocations: &Collocations,
		) -> Loc<Error> {
			let span = grammar.terminals()[t as usize]
				.1
				.iter()
				.next()
				.unwrap()
				.span();
			return Loc::new(
				Error::AmbiguousTerminals(
					t,
					u,
					TerminalAmbiguity::ReduceReduce {
						token: super::build_prefix_to(aut, t_collocations.states.get(&u).unwrap()),
					},
				),
				span,
			);
		}

		// Build the parent-child relation.
		for (t, t_collocations) in &collocations {
			for u in &t_collocations.always {
				let u_collocations = collocations.get(u).unwrap();
				if u_collocations.always.contains(t) {
					return Err(reduce_reduce_conflict(
						grammar,
						&det_automaton,
						*t,
						*u,
						t_collocations,
					));
				}

				// `u` is a parent of `t`
				add_item(&mut parents, *t, *u);
				log::debug!("terminal {} is a parent of {}", u, t);
			}

			for u in &t_collocations.sometimes {
				let u_collocations = collocations.get(u).unwrap();
				if u_collocations.sometimes.contains(t) {
					return Err(reduce_reduce_conflict(
						grammar,
						&det_automaton,
						*t,
						*u,
						t_collocations,
					));
				}

				// `u` is a child of `t`
				add_item(&mut children, *t, *u);
				log::debug!("terminal {} is a child of {}", u, t);
			}
		}

		// For now the parent/child relations are transitives.
		// We now build the non-transitive version of both.
		// First we associate a depth to each terminal. 0 for the roots.
		let roots: HashSet<u32> = grammar
			.terminals()
			.iter()
			.enumerate()
			.map(|(t, _)| t as u32)
			.filter(|t| !parents.contains_key(t))
			.collect();
		let mut stack = Vec::new();
		stack.extend(roots.iter().cloned());
		let mut depth = HashMap::new();
		while let Some(t) = stack.pop() {
			let d = match parents.get(&t) {
				Some(parents) => parents
					.iter()
					.map(|u| depth.get(u).cloned().unwrap_or(0))
					.max()
					.unwrap_or(0),
				None => 0,
			};

			let new_depth = d + 1;

			if depth
				.insert(t, new_depth)
				.map(|old_depth| old_depth != new_depth)
				.unwrap_or(true)
			{
				if let Some(children) = children.get(&t) {
					stack.extend(children)
				}
			}
		}

		fn into_automaton(grammar: &Grammar, terminals: HashSet<u32>) -> DetAutomaton<DetState> {
			let nd_table = NDTable::for_terminals(grammar, terminals.into_iter());
			let det_automaton = nd_table.automaton().determinize();
			simplify_det_automaton_without_conflicts(grammar, &det_automaton)
		}

		// Automata.

		for u in &roots {
			log::debug!("root automaton recognizes terminal {}", u)
		}
		let mut automata = vec![into_automaton(grammar, roots)];
		// Maps each terminal to a sub-automaton.
		let mut sub_automata = HashMap::new();
		for (t, t_children) in children {
			let d = depth.get(&t).unwrap();
			let direct_t_children: HashSet<u32> = t_children
				.into_iter()
				.filter(|u| *depth.get(&u).unwrap() == d + 1)
				.collect();
			debug_assert!(!direct_t_children.is_empty());
			let index = automata.len() as u32;
			log::debug!("terminal {} leads to the automaton {}", t, index);
			for u in &direct_t_children {
				log::debug!("this automaton recognizes terminal {}", u)
			}
			automata.push(into_automaton(grammar, direct_t_children));
			sub_automata.insert(t, index);
		}

		log::debug!("lexing table is done.");
		// panic!("PAUSE");

		Ok(Table {
			automata,
			sub_automata,
		})
	}
}

/// Simplify the given deterministic automaton.
///
/// It is assumed that the automaton does not contains reduce-reduce conflicts:
/// every deterministic state contains at most one terminal state.
fn simplify_det_automaton_without_conflicts(
	grammar: &Grammar,
	aut: &DetAutomaton<BTreeSet<State>>,
) -> DetAutomaton<DetState> {
	let mut terminal_map = HashMap::new();
	let mut intermediate_count = 0;

	#[derive(Hash, PartialEq, Eq)]
	pub enum Partition {
		Intermediate,
		Final(u32),
	}

	let partition = aut.partition(|states| {
		for q in states {
			if let State::Final(id) = q {
				return Partition::Final(*id);
			}
		}

		Partition::Intermediate
	});

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

	minimal_aut.map(|q| det_states(&mut terminal_map, &mut intermediate_count, q))
}

fn simplify_det_automaton(
	grammar: &Grammar,
	aut: &DetAutomaton<BTreeSet<State>>,
) -> Result<DetAutomaton<DetState>, Loc<Error>> {
	let mut terminal_map = HashMap::new();
	let mut intermediate_count = 0;

	#[derive(Hash, PartialEq, Eq)]
	pub enum Partition {
		Intermediate,
		Final(u32),
	}

	let partition = aut.try_partition(|states| {
		let mut terminal = None;

		for q in states {
			match q {
				State::Initial => {
					// Note: we have already checked that there are no empty terminals.
					return Ok(Partition::Intermediate);
				}
				State::Intermediate(_) => (),
				State::Final(id) => {
					if let Some(other_id) = terminal.replace(id) {
						let span = grammar.terminals()[*id as usize]
							.1
							.iter()
							.next()
							.unwrap()
							.span();
						return Err(Loc::new(
							Error::AmbiguousTerminals(
								*id,
								*other_id,
								TerminalAmbiguity::ReduceReduce {
									token: super::build_prefix_to(aut, states),
								},
							),
							span,
						));
					}
				}
			}
		}

		match terminal {
			Some(id) => Ok(Partition::Final(*id)),
			None => Ok(Partition::Intermediate),
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

fn det_states(
	terminal_map: &mut HashMap<u32, u32>,
	intermediate_count: &mut u32,
	states: &BTreeSet<State>,
) -> DetState {
	for q in states {
		match q {
			State::Initial => {
				// Note: we have already checked that there are no empty terminals.
				return DetState::Initial;
			}
			State::Intermediate(_) => (),
			State::Final(id) => {
				// Note: we have already checked that there are no reduce-reduce conflicts.
				let i = match terminal_map.get_mut(&id).cloned() {
					Some(i) => i,
					None => 0,
				};

				terminal_map.insert(*id, i + 1);
				return DetState::Final(*id, i);
			}
		}
	}

	let i = *intermediate_count;
	*intermediate_count = i + 1;
	DetState::Intermediate(i)
}
