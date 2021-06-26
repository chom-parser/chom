use super::Error;
use crate::Ident;
use source_span::{Loc, Span};
use std::collections::BTreeMap;

pub struct ExternalTypes {
	list: Vec<(Ident, Option<Span>)>,

	map: BTreeMap<Ident, u32>,
}

impl ExternalTypes {
	pub fn new(ast: Vec<Loc<Ident>>) -> Result<Self, Loc<Error>> {
		let mut list = Vec::new();
		let mut map = BTreeMap::new();

		for id in ast {
			let (id, span) = id.into_raw_parts();
			let index = list.len();
			use std::collections::btree_map::Entry;
			match map.entry(id.clone()) {
				Entry::Vacant(entry) => {
					list.push((id, Some(span)));
					entry.insert(index as u32);
				}
				Entry::Occupied(entry) => {
					let old_index = entry.get();
					if let Some(old_def_span) = list[*old_index as usize].1 {
						return Err(Loc::new(
							Error::AlreadyDefinedExternalType(id, old_def_span),
							span,
						));
					} else {
						list[*old_index as usize].1 = Some(span);
					}
				}
			}
		}

		Ok(Self { list, map })
	}

	pub fn get(&self, id: &Loc<Ident>) -> Result<u32, Loc<Error>> {
		self.map.get(id).cloned().ok_or_else(|| {
			Loc::new(Error::UndefinedExternalType(id.as_ref().clone()), id.span())
		})
	}

	pub fn into_vec(self) -> Vec<(Ident, Option<Span>)> {
		self.list
	}
}
