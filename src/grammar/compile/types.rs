use std::collections::{
	BTreeMap
};
use source_span::Loc;
use crate::{
	syntax::{
		self,
		Ident
	},
	grammar::{
		Type
	}
};
use super::Error;

pub struct Types {
	list: Vec<Loc<Type>>,
	map: BTreeMap<Ident, u32>,
}

impl Types {
	pub fn new(ast: &[Loc<syntax::Type>]) -> Result<Self, Loc<Error>> {
		let mut list = Vec::new();
		let mut map = BTreeMap::new();
		
		for def in ast {
			use std::collections::btree_map::Entry;
			match map.entry(def.id.as_ref().clone()) {
				Entry::Vacant(entry) => {
					let i = list.len() as u32;
					list.push(Loc::new(Type::new(def.id.as_ref().clone()), def.id.span()));
					entry.insert(i);
				},
				Entry::Occupied(entry) => {
					let old_def_index = entry.get();
					let old_def_span = list[*old_def_index as usize].span();
					return Err(Loc::new(Error::AlreadyDefinedType(def.id.as_ref().clone(), old_def_span), def.id.span()))
				}
			}
		}

		Ok(Self {
			list,
			map
		})
	}

	pub fn into_vec(self) -> Vec<Loc<Type>> {
		self.list
	}

	pub fn get(&self, id: &Loc<Ident>) -> Result<u32, Loc<Error>> {
		self.map.get(id.as_ref()).cloned().ok_or_else(|| Loc::new(Error::UndefinedType(id.as_ref().clone()), id.span()))
	}

	pub fn add_rule(&mut self, index: u32, rule: u32) {
		self.list[index as usize].add_rule(rule)
	}
}