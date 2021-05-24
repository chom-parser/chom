use source_span::Loc;
use std::{
	collections::{
		HashSet,
		HashMap,
		BTreeMap,
		BTreeSet
	},
	io,
	fmt
};
use crate::{
	mono::{
		Grammar,
		ty
	},
	parsing::{
		Error,
		// FirstAndFollowGraph
	}
};
use super::{
	Item,
	ItemSet
};

pub struct State {
	pub items: ItemSet,
	pub transitions: BTreeMap<ty::Expr, u32>,
	pub predecessors: HashMap<ty::Expr, HashSet<u32>>
}

impl State {
	pub fn is_initial(&self) -> bool {
		self.items.iter().all(|item| item.offset == 0)
	}

	pub fn add_predecessor(&mut self, symbol: ty::Expr, q: u32) {
		use std::collections::hash_map::Entry;
		match self.predecessors.entry(symbol) {
			Entry::Occupied(mut entry) => {
				entry.get_mut().insert(q);
			},
			Entry::Vacant(entry) => {
				let mut preds = HashSet::new();
				preds.insert(q);
				entry.insert(preds);
			}
		}
	}
}

/// Parsing table.
pub struct NonDeterministic {
	states: Vec<State>
}

impl NonDeterministic {
	pub fn new(grammar: &Grammar) -> Self {
		let mut states = Vec::new();
		let mut map = HashMap::new();

		fn state_id(stack: &mut Vec<(u32, ItemSet)>, states: &mut Vec<State>, map: &mut HashMap<ItemSet, u32>, item_set: ItemSet) -> u32 {
			use std::collections::hash_map::Entry;
			match map.entry(item_set.clone()) {
				Entry::Occupied(entry) => *entry.get(),
				Entry::Vacant(entry) => {
					let i = states.len() as u32;
					stack.push((i, item_set.clone()));
					states.push(State {
						items: item_set,
						transitions: BTreeMap::new(),
						predecessors: HashMap::new()
					});
					entry.insert(i);
					i
				}
			}
		}

		let mut stack = Vec::new();
		for ty in grammar.types() {
			let mut item_set = ItemSet::new();

			for f in ty.constructors() {
				item_set.insert(Item::from_rule(f))
			}

			item_set.close(grammar);
			state_id(&mut stack, &mut states, &mut map, item_set);
		}

		while let Some((a, item_set)) = stack.pop() {
			for (t, next_item_set) in item_set.shift(grammar) {
				let b = state_id(&mut stack, &mut states, &mut map, next_item_set);
				
				states[a as usize].transitions.insert(t, b);
				states[b as usize].add_predecessor(t, a);
			}
		}

		Self {
			states
		}
	}

	pub fn states(&self) -> &[State] {
		&self.states
	}

	pub fn transitions_for(&self, q: u32) -> impl '_ + Iterator<Item=(ty::Expr, u32)> {
		self.states[q as usize].transitions.iter().map(|(a, b)| (*a, *b))
	}

	/// Find a path from an initial state leading to the given state `q`.
	pub fn path_to(&self, q: u32) -> Vec<(u32, ty::Expr)> {
		let mut stack = Vec::new();
		let mut visited = HashSet::new();
		stack.push((q, vec![]));

		while let Some((q, mut path)) = stack.pop() {
			if visited.insert(q) {
				let state = &self.states[q as usize];
				if state.is_initial() {
					path.reverse();
					return path
				} else {
					for (symbol, preds) in &state.predecessors {
						for pred in preds {
							let mut new_path = path.clone();
							new_path.push((*pred, *symbol));
							stack.push((*pred, new_path))
						}
					}
				}
			}
		}

		panic!("no initial state leading to the given state")
	}

	pub fn dot_write<W: io::Write>(&self, grammar: &Grammar, f: &mut W) -> io::Result<()> {
		write!(f, "digraph {{\n")?;

		for (q, state) in self.states.iter().enumerate() {
			let q = q as u32;

			write!(f, "\tq{} [ shape=plaintext, label=<{}> ]\n", q, state.items.dot_format(grammar))?;

			for (symbol, r) in self.transitions_for(q) {
				write!(f, "\tq{} -> q{} [ label=\"{}\" ]\n", q, r, symbol.format(grammar))?;
			}
		}

		write!(f, "}}")
	}
}