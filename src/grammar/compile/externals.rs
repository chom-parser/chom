use std::collections::{
	BTreeMap
};
use source_span::{
	Span,
	Loc
};
use crate::{
	syntax::{
		self,
		Ident
	},
	grammar::ExternalType
};
use super::Error;

pub struct ExternalTypes {
	list: Vec<(ExternalType, Option<Span>)>,

	map: BTreeMap<Ident, u32>
}

impl ExternalTypes {
	pub fn new(ast: Vec<Loc<syntax::Ident>>) -> Result<Self, Loc<Error>> {
		let mut list = Vec::new();
		let mut map = BTreeMap::new();

		map.insert(Ident(ExternalType::Unit.name().to_string()), list.len() as u32);
		list.push((ExternalType::Unit, None));

		for id in ast {
			let index = list.len();
			use std::collections::btree_map::Entry;
			match map.entry(id.as_ref().clone()) {
				Entry::Vacant(entry) => {
					list.push((ExternalType::from_ident(id.as_ref()), Some(id.span())));
					entry.insert(index as u32);
				},
				Entry::Occupied(entry) => {
					let old_index = entry.get();
					if let Some(old_def_span) = list[*old_index as usize].1 {
						return Err(Loc::new(Error::AlreadyDefinedExternalType(id.as_ref().clone(), old_def_span), id.span()))
					} else {
						list[*old_index as usize].1 = Some(id.span());
					}
				}
			}
		}

		Ok(Self {
			list,
			map
		})
	}

	pub fn get(&self, id: Option<&Loc<Ident>>) -> Result<u32, Loc<Error>> {
		match id {
			Some(id) => self.map.get(id).cloned().ok_or_else(|| Loc::new(Error::UndefinedExternalType(id.as_ref().clone()), id.span())),
			None => Ok(0)
		}
	}

	pub fn into_vec(self) -> Vec<(ExternalType, Option<Span>)> {
		self.list
	}
}