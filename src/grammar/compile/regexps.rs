use source_span::{
	Span,
	Loc
};
use std::collections::{
	BTreeMap
};
use crate::{
	syntax::{
		self,
		Ident
	},
	lexing::regexp,
};
use super::{
	Error,
	ExternalTypes
};

pub struct RegExps {
	/// List of regular expressions.
	list: Vec<(Option<regexp::Definition>, u32, Loc<syntax::RegExpDefinition>)>,

	/// Associate each expect identifier to a pair containing
	/// in first argument the index of the regexp definition in `self.list` and
	/// in second argument the index of its type in `external_types`.
	map: BTreeMap<Ident, u32>,
}

impl RegExps {
	pub fn new(external_types: &ExternalTypes, ast: &[Loc<syntax::RegExpDefinition>]) -> Result<Self, Loc<Error>> {
		let mut list = Vec::new();
		let mut map = BTreeMap::new();

		for def in ast {
			let ty = external_types.get(def.ty.as_ref())?;
			
			use std::collections::btree_map::Entry;
			match map.entry(def.id.as_ref().clone()) {
				Entry::Vacant(entry) => {
					let i = list.len() as u32;
					list.push((None, ty, def.clone()));
					entry.insert(i);
				},
				Entry::Occupied(entry) => {
					let old_index = entry.get();
					let old_def = &list[*old_index as usize].2;
					return Err(Loc::new(Error::AlreadyDefinedRegExp(def.id.as_ref().clone(), old_def.span()), def.id.span()))
				}
			}
		}

		Ok(Self {
			list,
			map
		})
	}

	pub fn into_vec(self) -> Vec<(regexp::Definition, Loc<syntax::RegExpDefinition>)> {
		self.list.into_iter().map(|(def, _, ast)| (def.unwrap(), ast)).collect()
	}

	pub fn assert_defined(&self, id: &Ident, span: Span) -> Result<(), Loc<Error>> {
		self.get(id, span)?;
		Ok(())
	}

	pub fn ast(&self, index: u32) -> &Loc<syntax::RegExpDefinition> {
		&self.list[index as usize].2
	}

	pub fn get(&self, id: &Ident, span: Span) -> Result<u32, Loc<Error>> {
		Ok(*self.map.get(id).ok_or_else(|| Loc::new(Error::UndefinedRegExp(id.clone()), span))?)
	}

	pub fn define(&mut self, id: &Ident, def: regexp::Definition) {
		let index = self.map.get(id).unwrap();
		self.list[*index as usize].0 = Some(def);
	}
}