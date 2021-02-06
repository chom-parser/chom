use std::collections::{
	HashMap,
	HashSet
};
use btree_slab::BTreeSet;
use range_map::RangeMap;
use crate::{
	Ident,
	CharSet
};

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum State {
	Initial,
	Intermediate(u32),
	Final(Ident)
}

impl State {
	pub fn is_final(&self) -> bool {
		match self {
			State::Final(_) => true,
			_ => false
		}
	}
}

/// Non deterministic automaton.
pub struct Automaton {
	transitions: HashMap<State, HashMap<Option<CharSet>, HashSet<State>>>
}

impl Automaton {
	pub fn new() -> Automaton {
		Automaton {
			transitions: HashMap::new()
		}
	}

	pub fn add(&mut self, source: State, label: Option<CharSet>, target: State) {
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

	fn determinize_transitions_for(&self, states: &BTreeSet<State>) -> RangeMap<char, DetState> {
		let mut map = RangeMap::new();
		
		for q in states {
			if let Some(transitions) = self.transitions.get(q) {
				for (label, targets) in transitions {
					if let Some(label) = label {
						for range in label.ranges() {
							map.update(range.clone(), |current_target_states_opt : Option<&DetState>| {
								let mut current_target_states = match current_target_states_opt {
									Some(current_target_states) => current_target_states.clone(),
									None => BTreeSet::new()
								};

								current_target_states.extend(targets.iter().cloned());
								Some(current_target_states)
							})
						}
					}
				}
			}
		}

		map
	}

	pub fn determinize(&self) -> DetAutomaton {
		let mut initial_state = BTreeSet::new();
		let mut transitions = HashMap::new();

		// create the initial deterministic state.
		initial_state.insert(State::Initial);
		// add states reachable trough epsilon-transitions.
		if let Some(transitions) = self.transitions.get(&State::Initial) {
			if let Some(epsilon_qs) = transitions.get(&None) {
				initial_state.extend(epsilon_qs.iter().cloned())
			}
		}

		let mut visited_states = HashSet::new();
		let mut stack = vec![initial_state.clone()];
		while let Some(det_q) = stack.pop() {
			if !visited_states.contains(&det_q) {
				visited_states.insert(det_q.clone());
				
				let map = self.determinize_transitions_for(&det_q);

				for (_, next_det_q) in &map {
					stack.push(next_det_q.clone())
				}

				// TODO check for ambiguities.

				transitions.insert(det_q, map);
			}
		}

		DetAutomaton {
			initial_state,
			transitions
		}
	}
}

pub type DetState = BTreeSet<State>;

/// Deterministic epsilon-free automaton.
pub struct DetAutomaton {
	initial_state: DetState,
	transitions: HashMap<DetState, RangeMap<char, DetState>>
}

impl DetAutomaton {
	pub fn initial_state(&self) -> &DetState {
		&self.initial_state
	}

	pub fn transitions(&self) -> &HashMap<DetState, RangeMap<char, DetState>> {
		&self.transitions
	}
}