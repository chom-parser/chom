use source_span::{
	Span
};
use std::collections::{
	HashSet,
	BTreeMap
};
use crate::grammar::NonTerminal;

pub struct NonTerminals {
	/// Every non terminals.
	list: Vec<(NonTerminal, HashSet<Span>)>,

	/// Associate each terminal to its identifier.
	table: BTreeMap<NonTerminal, u32>
}

impl NonTerminals {
	pub fn new() -> Self {
		Self {
			list: Vec::new(),
			table: BTreeMap::new()
		}
	}

	pub fn id(&mut self, nt: NonTerminal, span: Span) -> u32 {
		match self.table.get(&nt).cloned() {
			Some(id) => {
				self.list[id as usize].1.insert(span);
				id
			},
			None => {
				let id = self.list.len() as u32;
				let mut locs = HashSet::new();
				locs.insert(span);
				self.list.push((nt.clone(), locs));
				self.table.insert(nt, id);
				id
			}
		}
	}

	pub fn into_vec(self) -> Vec<(NonTerminal, HashSet<Span>)> {
		self.list
	}
}