use std::{
	fmt,
	collections::{
		HashMap,
		HashSet
	},
	hash::Hash
};
use btree_slab::BTreeSet;
use btree_range_map::{
	RangeMap,
	AnyRange
};
use crate::{
	CharSet
};

/// State of non deterministic automaton.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum State {
	/// The (unique) initial state of the automaton.
	Initial,

	/// Some intermediate state of the automaton.
	Intermediate(u32),

	/// The final state of the terminal with the given identifier.
	Final(u32)
}

impl State {
	pub fn is_final(&self) -> bool {
		match self {
			State::Final(_) => true,
			_ => false
		}
	}
}

impl fmt::Display for State {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			State::Initial => write!(f, "initial"),
			State::Intermediate(q) => write!(f, "q{}", q),
			State::Final(q) => write!(f, "f{}", q)
		}
	}
}

/// State of deterministic automaton.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DetState {
	/// The (unique) initial state of the automaton.
	Initial,

	/// Intermediate state.
	Intermediate(u32),

	/// A final state of a terminal.
	/// 
	/// The first parameter is the terminal id,
	/// the second parameter is a number which, associated with the terminal id,
	/// uniquely identifies the state.
	Final(u32, u32)
}

/// Non deterministic lexing automaton.
pub struct Automaton {
	/// States.
	states: HashSet<State>,

	/// Transitions.
	transitions: HashMap<State, HashMap<Option<CharSet>, HashSet<State>>>
}

impl Automaton {
	/// Create a new empty non deterministic automaton.
	pub fn new() -> Automaton {
		Automaton {
			states: HashSet::new(),
			transitions: HashMap::new()
		}
	}

	pub fn transitions(&self) -> impl Iterator<Item=(&State, &HashMap<Option<CharSet>, HashSet<State>>)> {
		self.transitions.iter()
	}

	pub fn successors(&self, q: &State) -> Successors {
		Successors::new(self.transitions.get(q))
	}

	pub fn add(&mut self, source: State, label: Option<CharSet>, target: State) {
		self.states.insert(source.clone());
		self.states.insert(target.clone());

		match self.transitions.get_mut(&source) {
			Some(transitions) => {
				match transitions.get_mut(&label) {
					Some(states) => {
						states.insert(target);
					},
					None => {
						let mut states = HashSet::new();
						states.insert(target);
						transitions.insert(label, states);
					}
				}
			},
			None => {
				let mut transitions = HashMap::new();
				let mut states = HashSet::new();
				states.insert(target);
				transitions.insert(label, states);
				self.transitions.insert(source, transitions);
			}
		}
	}

	fn modulo_epsilon_state(&self, q: &State) -> BTreeSet<State> {
		let mut states = BTreeSet::new();
		let mut stack = vec![q];

		while let Some(q) = stack.pop() {
			if states.insert(q.clone()) {
				// add states reachable trough epsilon-transitions.
				if let Some(transitions) = self.transitions.get(q) {
					if let Some(epsilon_qs) = transitions.get(&None) {
						for t in epsilon_qs {
							stack.push(t)
						}
					}
				}
			}
		}

		states
	}

	fn determinize_transitions_for(&self, states: &BTreeSet<State>) -> HashMap<AnyRange<char>, BTreeSet<State>> {
		let mut map = RangeMap::new();
		
		for q in states {
			if let Some(transitions) = self.transitions.get(q) {
				for (label, targets) in transitions {
					if let Some(label) = label {
						for range in label.ranges() {
							map.update(range.clone(), |current_target_states_opt : Option<&BTreeSet<State>>| {
								let mut current_target_states = match current_target_states_opt {
									Some(current_target_states) => current_target_states.clone(),
									None => BTreeSet::new()
								};

								for q in targets {
									current_target_states.extend(self.modulo_epsilon_state(q));
								}

								Some(current_target_states)
							})
						}
					}
				}
			}
		}

		let mut simplified_map = HashMap::new();

		for (range, set) in map {
			simplified_map.insert(range, set);
		}

		simplified_map
	}

	pub fn determinize(&self) -> DetAutomaton<BTreeSet<State>> {
		let mut transitions = HashMap::new();

		// create the initial deterministic state.
		let initial_state = self.modulo_epsilon_state(&State::Initial);

		let mut visited_states = HashSet::new();
		let mut stack = vec![initial_state.clone()];
		while let Some(det_q) = stack.pop() {
			if !visited_states.contains(&det_q) {
				visited_states.insert(det_q.clone());
				
				let map = self.determinize_transitions_for(&det_q);

				for (_, next_det_q) in &map {
					stack.push(next_det_q.clone())
				}

				transitions.insert(det_q, map);
			}
		}

		DetAutomaton {
			initial_state,
			transitions
		}
	}
}

/// Deterministic epsilon-free automaton.
pub struct DetAutomaton<Q> {
	initial_state: Q,
	transitions: HashMap<Q, HashMap<AnyRange<char>, Q>>
}

impl<Q> DetAutomaton<Q> {
	pub fn new(initial_state: Q) -> Self {
		Self {
			initial_state,
			transitions: HashMap::new()
		}
	}

	pub fn initial_state(&self) -> &Q {
		&self.initial_state
	}

	pub fn transitions(&self) -> &HashMap<Q, HashMap<AnyRange<char>, Q>> {
		&self.transitions
	}

	pub fn successors(&self, q: &Q) -> DetSuccessors<Q> where Q: Eq + Hash {
		DetSuccessors::new(self.transitions.get(q))
	}

	pub fn reachable_states_from<'a>(&'a self, q: &'a Q) -> ReachableStates<'a, Q> {
		ReachableStates::new(self, q)
	}

	pub fn add(&mut self, source: Q, label: btree_range_map::AnyRange<char>, target: Q) where Q: Eq + Hash {
		if self.transitions.contains_key(&source) {
			let source_transitions = self.transitions.get_mut(&source).unwrap();
			source_transitions.insert(label, target);
		} else {
			let mut source_transitions = HashMap::new();
			source_transitions.insert(label, target);
			self.transitions.insert(source, source_transitions);
		}
	}

	pub fn try_map<P, F, E>(&self, mut f: F) -> Result<DetAutomaton<P>, E> where F: FnMut(&Q) -> Result<P, E>, P: Clone + Eq + Hash, Q: Clone + Eq + Hash {
		let mut map = HashMap::new();
		map.insert(self.initial_state.clone(), f(&self.initial_state)?);

		let mut result = DetAutomaton::new(map[&self.initial_state].clone());

		for (source, transitions) in &self.transitions {
			for (range, target) in transitions {
				let source = if map.contains_key(source) {
					map[source].clone()
				} else {
					let p = f(source)?;
					map.insert(source.clone(), p.clone());
					p
				};

				let target = if map.contains_key(target) {
					map[target].clone()
				} else {
					let p = f(target)?;
					map.insert(target.clone(), p.clone());
					p
				};

				result.add(source, range.clone(), target);
			}
		}

		Ok(result)
	}
}

pub struct Successors<'a> {
	inner: Option<std::collections::hash_map::Iter<'a, Option<CharSet>, HashSet<State>>>
}

impl<'a> Successors<'a> {
	pub fn new(map: Option<&'a HashMap<Option<CharSet>, HashSet<State>>>) -> Self {
		Self {
			inner: map.map(|map| map.iter())
		}
	}
}

impl<'a> Iterator for Successors<'a> {
	type Item = (&'a Option<CharSet>, &'a HashSet<State>);
	
	fn next(&mut self) -> Option<Self::Item> {
		self.inner.as_mut().map(|inner| inner.next()).flatten()
	}
}

pub struct DetSuccessors<'a, Q> {
	inner: Option<std::collections::hash_map::Iter<'a, AnyRange<char>, Q>>
}

impl<'a, Q> DetSuccessors<'a, Q> {
	pub fn new(map: Option<&'a HashMap<AnyRange<char>, Q>>) -> Self {
		Self {
			inner: map.map(|map| map.iter())
		}
	}
}

impl<'a, Q> Iterator for DetSuccessors<'a, Q> {
	type Item = (&'a AnyRange<char>, &'a Q);
	
	fn next(&mut self) -> Option<Self::Item> {
		self.inner.as_mut().map(|inner| inner.next()).flatten()
	}
}

pub struct ReachableStates<'a, Q> {
	aut: &'a DetAutomaton<Q>,
	visited: HashSet<&'a Q>,
	stack: Vec<&'a Q>
}

impl<'a, Q> ReachableStates<'a, Q> {
	fn new(aut: &'a DetAutomaton<Q>, q: &'a Q) -> Self {
		Self {
			aut,
			visited: HashSet::new(),
			stack: vec![q]
		}
	}
}

impl<'a, Q> Iterator for ReachableStates<'a, Q> where Q: Eq + Hash {
	type Item = &'a Q;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match self.stack.pop() {
				Some(q) => {
					if self.visited.insert(q) {
						match self.aut.transitions.get(q) {
							Some(q_transitions) => {
								for (_, target) in q_transitions {
									self.stack.push(target)
								}
							},
							None => ()
						}

						break Some(q)
					}
				},
				None => break None
			}
		}
	}
}