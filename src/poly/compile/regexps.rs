use super::{Error, ExternalTypes};
use crate::{
	Ident,
	lexing::regexp,
	syntax,
};
use source_span::{Loc, Span};
use std::collections::BTreeMap;

pub struct RegExps {
	/// List of regular expressions.
	list: Vec<(
		Option<regexp::Definition>,
		Option<u32>,
		Option<Loc<syntax::regexp::Definition>>,
	)>,

	/// Associate each expect identifier to a pair containing
	/// in first argument the index of the regexp definition in `self.list` and
	/// in second argument the index of its type in `external_types`.
	map: BTreeMap<Ident, u32>,

	/// Index of the `WS` regexp.
	ws_index: u32,
}

impl RegExps {
	pub fn new(
		external_types: &ExternalTypes,
		ast: &[Loc<syntax::regexp::Definition>],
	) -> Result<Self, Loc<Error>> {
		use std::collections::btree_map::Entry;

		let mut list = Vec::new();
		let mut map = BTreeMap::new();

		for def in ast {
			let ty = def.ty.as_ref().map(|ty| external_types.get(ty)).transpose()?;

			match map.entry(def.id.as_ref().clone()) {
				Entry::Vacant(entry) => {
					let i = list.len() as u32;
					list.push((None, ty, Some(def.clone())));
					entry.insert(i);
				}
				Entry::Occupied(entry) => {
					let old_index = entry.get();
					let old_def = list[*old_index as usize].2.as_ref().unwrap();
					return Err(Loc::new(
						Error::AlreadyDefinedRegExp(def.id.as_ref().clone(), old_def.span()),
						def.id.span(),
					));
				}
			}
		}

		// Whitespace regexp.
		let ws_id = Ident::new("WS".to_string()).unwrap();
		let ws_index = match map.entry(ws_id.clone()) {
			Entry::Vacant(entry) => {
				let i = list.len() as u32;
				let exp =
					regexp::RegExp::new(vec![regexp::Atom::CharSet(crate::CharSet::whitespace())]);
				list.push((Some(regexp::Definition { id: ws_id, ty: None, exp }), None, None));
				entry.insert(i);
				i
			}
			Entry::Occupied(entry) => *entry.get(),
		};

		Ok(Self {
			list,
			map,
			ws_index,
		})
	}

	/// Returns the index of the `WS` regexp.
	pub fn ws_index(&self) -> u32 {
		self.ws_index
	}

	pub fn into_vec(self) -> Vec<(regexp::Definition, Option<Loc<syntax::regexp::Definition>>)> {
		self.list
			.into_iter()
			.map(|(def, _, ast)| (def.unwrap(), ast))
			.collect()
	}

	pub fn assert_defined(&self, id: &Ident, span: Span) -> Result<(), Loc<Error>> {
		self.get(id, span)?;
		Ok(())
	}

	pub fn ast(&self, index: u32) -> Option<&Loc<syntax::regexp::Definition>> {
		self.list[index as usize].2.as_ref()
	}

	pub fn get(&self, id: &Ident, span: Span) -> Result<u32, Loc<Error>> {
		Ok(*self
			.map
			.get(id)
			.ok_or_else(|| Loc::new(Error::UndefinedRegExp(id.clone()), span))?)
	}

	pub fn define(&mut self, id: &Ident, def: regexp::Definition) {
		let index = self.map.get(id).unwrap();
		self.list[*index as usize].0 = Some(def);
	}
}
