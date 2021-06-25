use crate::CharSet;
use btree_range_map::{AnyRange, RangeMap};
use btree_slab::BTreeSet;
use std::{
	collections::{BTreeMap, HashMap, HashSet},
	fmt,
	hash::Hash,
};

/// State of non deterministic automaton.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum State {
	/// The (unique) initial state of the automaton.
	Initial,

	/// Some intermediate state of the automaton.
	Intermediate(u32),

	/// The final state of the terminal with the given identifier.
	Final(u32),
}

impl State {
	pub fn is_final(&self) -> bool {
		match self {
			State::Final(_) => true,
			_ => false,
		}
	}
}

impl fmt::Display for State {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			State::Initial => write!(f, "initial"),
			State::Intermediate(q) => write!(f, "q{}", q),
			State::Final(q) => write!(f, "f{}", q),
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
	Final(u32, u32),
}

/// Non deterministic lexing automaton.
pub struct Automaton {
	/// States.
	states: HashSet<State>,

	/// Transitions.
	transitions: BTreeMap<State, HashMap<Option<CharSet>, HashSet<State>>>,
}

impl Automaton {
	/// Create a new empty non deterministic automaton.
	pub fn new() -> Automaton {
		Automaton {
			states: HashSet::new(),
			transitions: BTreeMap::new(),
		}
	}

	pub fn transitions(
		&self,
	) -> impl Iterator<Item = (&State, &HashMap<Option<CharSet>, HashSet<State>>)> {
		self.transitions.iter()
	}

	pub fn successors(&self, q: &State) -> Successors {
		Successors::new(self.transitions.get(q))
	}

	pub fn add(&mut self, source: State, label: Option<CharSet>, target: State) {
		self.states.insert(source.clone());
		self.states.insert(target.clone());

		match self.transitions.get_mut(&source) {
			Some(transitions) => match transitions.get_mut(&label) {
				Some(states) => {
					states.insert(target);
				}
				None => {
					let mut states = HashSet::new();
					states.insert(target);
					transitions.insert(label, states);
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

	fn determinize_transitions_for(
		&self,
		states: &BTreeSet<State>,
	) -> BTreeMap<AnyRange<char>, BTreeSet<State>> {
		let mut map = RangeMap::new();

		for q in states {
			if let Some(transitions) = self.transitions.get(q) {
				for (label, targets) in transitions {
					if let Some(label) = label {
						for range in label.ranges() {
							debug_assert!(!range.is_empty());

							map.update(
								range.clone(),
								|current_target_states_opt: Option<&BTreeSet<State>>| {
									let mut current_target_states = match current_target_states_opt
									{
										Some(current_target_states) => {
											current_target_states.clone()
										}
										None => BTreeSet::new(),
									};

									for q in targets {
										current_target_states.extend(self.modulo_epsilon_state(q));
									}

									Some(current_target_states)
								},
							);

							assert!(map.get(range.first().unwrap()).is_some());
						}
					}
				}
			}
		}

		let mut simplified_map = BTreeMap::new();

		for (range, set) in map {
			debug_assert!(!range.is_empty());
			simplified_map.insert(range, set);
		}

		simplified_map
	}

	pub fn determinize(&self) -> DetAutomaton<BTreeSet<State>> {
		let mut transitions = BTreeMap::new();

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
			transitions,
		}
	}
}

/// Deterministic epsilon-free automaton.
pub struct DetAutomaton<Q> {
	initial_state: Q,
	transitions: BTreeMap<Q, BTreeMap<AnyRange<char>, Q>>,
}

impl<Q: Ord> DetAutomaton<Q> {
	pub fn new(initial_state: Q) -> Self {
		Self {
			initial_state,
			transitions: BTreeMap::new(),
		}
	}

	pub fn initial_state(&self) -> &Q {
		&self.initial_state
	}

	pub fn transitions(&self) -> &BTreeMap<Q, BTreeMap<AnyRange<char>, Q>> {
		&self.transitions
	}

	pub fn successors(&self, q: &Q) -> DetSuccessors<Q> {
		DetSuccessors::new(self.transitions.get(q))
	}

	pub fn reachable_states_from<'a>(&'a self, q: &'a Q) -> ReachableStates<'a, Q> {
		ReachableStates::new(self, q)
	}

	pub fn add(&mut self, source: Q, label: btree_range_map::AnyRange<char>, target: Q) {
		if self.transitions.contains_key(&source) {
			let source_transitions = self.transitions.get_mut(&source).unwrap();
			source_transitions.insert(label, target);
		} else {
			let mut source_transitions = BTreeMap::new();
			source_transitions.insert(label, target);
			self.transitions.insert(source, source_transitions);
		}
	}

	pub fn select_states<F>(&self, f: F) -> BTreeSet<&Q>
	where
		Q: Hash + Eq,
		F: Fn(&Q) -> bool,
	{
		let mut set = BTreeSet::new();
		let mut visited = HashSet::new();
		self.select_states_from(&self.initial_state, &f, &mut visited, &mut set);
		set
	}

	pub fn states(&self) -> BTreeSet<&Q>
	where
		Q: Hash + Eq,
	{
		self.select_states(|_| true)
	}

	fn select_states_from<'a, F>(
		&'a self,
		q: &'a Q,
		f: &F,
		visited: &mut HashSet<&'a Q>,
		set: &mut BTreeSet<&'a Q>,
	) where
		Q: Hash + Eq,
		F: Fn(&Q) -> bool,
	{
		if visited.insert(q) {
			if f(q) {
				set.insert(q);
			}

			for (_, r) in self.successors(q) {
				self.select_states_from(r, f, visited, set)
			}
		}
	}

	pub fn partition<'a, P, F>(&'a self, f: F) -> HashMap<P, BTreeSet<&'a Q>>
	where
		Q: Ord + Hash + Eq,
		P: Hash + Eq,
		F: Fn(&Q) -> P,
	{
		// unsafe {
		// 	self.try_partition::<P, F, std::convert::Infallible>(|q| Ok(q)).unwrap_unchecked() // safe because infallible.
		// }
		// TODO use `unwrap_unchecked` when stabilized.
		self.try_partition::<P, _, std::convert::Infallible>(|q| Ok(f(q)))
			.unwrap()
	}

	pub fn try_partition<'a, P, F, E>(&'a self, f: F) -> Result<HashMap<P, BTreeSet<&'a Q>>, E>
	where
		Q: Ord + Hash + Eq,
		P: Hash + Eq,
		F: Fn(&Q) -> Result<P, E>,
	{
		let mut partition = HashMap::new();
		let mut visited = HashSet::new();
		self.try_partition_from(&self.initial_state, &f, &mut visited, &mut partition)?;
		Ok(partition)
	}

	fn try_partition_from<'a, P, F, E>(
		&'a self,
		q: &'a Q,
		f: &F,
		visited: &mut HashSet<&'a Q>,
		partition: &mut HashMap<P, BTreeSet<&'a Q>>,
	) -> Result<(), E>
	where
		Q: Ord + Hash + Eq,
		P: Hash + Eq,
		F: Fn(&Q) -> Result<P, E>,
	{
		if visited.insert(q) {
			let p = f(q)?;

			if partition.contains_key(&p) {
				partition.get_mut(&p).unwrap().insert(q);
			} else {
				let mut set = BTreeSet::new();
				set.insert(q);
				partition.insert(p, set);
			}

			for (_, r) in self.successors(q) {
				self.try_partition_from(r, f, visited, partition)?;
			}
		}

		Ok(())
	}

	/// Minimizes the automaton.
	// Hopcroft's algorithm.
	// https://en.wikipedia.org/wiki/DFA_minimization
	pub fn minimize<'a, P>(&'a self, partition: P) -> DetAutomaton<BTreeSet<&Q>>
	where
		Q: Ord + Hash + Eq,
		P: Iterator<Item = BTreeSet<&'a Q>>,
	{
		let mut partition: BTreeSet<_> = partition.collect();

		let mut working = partition.clone();

		while let Some(a) = working.pop_first() {
			let mut sources_by_label: HashMap<AnyRange<char>, BTreeSet<&Q>> = HashMap::new();

			for (source, targets) in &self.transitions {
				for (label, target) in targets {
					if a.contains(target) {
						if sources_by_label.contains_key(label) {
							let sources = sources_by_label.get_mut(label).unwrap();
							sources.insert(source);
						} else {
							let mut sources = BTreeSet::new();
							sources.insert(source);
							sources_by_label.insert(label.clone(), sources);
						}
					}
				}
			}

			for (_label, sources) in sources_by_label {
				for y in partition.clone() {
					if y.intersection(&sources).next().is_some()
						&& y.difference(&sources).next().is_some()
					{
						let intersection: BTreeSet<&Q> =
							y.intersection(&sources).cloned().collect();
						let difference: BTreeSet<&Q> = y.difference(&sources).cloned().collect();

						if working.contains(&y) {
							working.remove(&y);
							working.insert(intersection.clone());
							working.insert(difference.clone());
						} else {
							if intersection.len() <= difference.len() {
								working.insert(intersection.clone());
							} else {
								working.insert(difference.clone());
							}
						}

						partition.remove(&y);
						partition.insert(intersection);
						partition.insert(difference);
					}
				}
			}
		}

		let mut map = HashMap::new();
		for member in partition {
			for q in &member {
				map.insert(*q, member.clone());
			}
		}

		let mut result = DetAutomaton::new(map[&self.initial_state].clone());
		for (source, transitions) in &self.transitions {
			for (range, target) in transitions {
				result.add(map[source].clone(), range.clone(), map[target].clone());
			}
		}

		result
	}

	pub fn map<P, F>(&self, mut f: F) -> DetAutomaton<P>
	where
		F: FnMut(&Q) -> P,
		P: Clone + Ord + Eq + Hash,
		Q: Clone + Eq + Hash,
	{
		let mut map = HashMap::new();
		map.insert(self.initial_state.clone(), f(&self.initial_state));

		let mut result = DetAutomaton::new(map[&self.initial_state].clone());
		for (source, transitions) in &self.transitions {
			for (range, target) in transitions {
				let source = if map.contains_key(source) {
					map[source].clone()
				} else {
					let p = f(source);
					map.insert(source.clone(), p.clone());
					p
				};

				let target = if map.contains_key(target) {
					map[target].clone()
				} else {
					let p = f(target);
					map.insert(target.clone(), p.clone());
					p
				};

				result.add(source, range.clone(), target);
			}
		}

		result
	}

	pub fn try_map<P, F, E>(&self, mut f: F) -> Result<DetAutomaton<P>, E>
	where
		F: FnMut(&Q) -> Result<P, E>,
		P: Clone + Ord + Eq + Hash,
		Q: Clone + Eq + Hash,
	{
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
	inner: Option<std::collections::hash_map::Iter<'a, Option<CharSet>, HashSet<State>>>,
}

impl<'a> Successors<'a> {
	pub fn new(map: Option<&'a HashMap<Option<CharSet>, HashSet<State>>>) -> Self {
		Self {
			inner: map.map(|map| map.iter()),
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
	inner: Option<std::collections::btree_map::Iter<'a, AnyRange<char>, Q>>,
}

impl<'a, Q> DetSuccessors<'a, Q> {
	pub fn new(map: Option<&'a BTreeMap<AnyRange<char>, Q>>) -> Self {
		Self {
			inner: map.map(|map| map.iter()),
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
	stack: Vec<&'a Q>,
}

impl<'a, Q> ReachableStates<'a, Q> {
	fn new(aut: &'a DetAutomaton<Q>, q: &'a Q) -> Self {
		Self {
			aut,
			visited: HashSet::new(),
			stack: vec![q],
		}
	}
}

impl<'a, Q> Iterator for ReachableStates<'a, Q>
where
	Q: Ord + Eq + Hash,
{
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
							}
							None => (),
						}

						break Some(q);
					}
				}
				None => break None,
			}
		}
	}
}
