use super::Error;
use crate::{
	poly::Type,
	syntax::{self, Caused, Ident},
};
use source_span::Loc;
use std::collections::BTreeMap;

pub struct Types {
	list: Vec<Caused<Type>>,

	id_map: BTreeMap<Ident, u32>,
	// desc_map: BTreeMap<ty::Id, u32>,
}

impl Types {
	pub fn new(ast: &[Loc<syntax::ty::Definition>]) -> Result<Self, Loc<Error>> {
		let mut list = Vec::new();
		let mut map = BTreeMap::new();

		for def in ast {
			use std::collections::btree_map::Entry;
			match map.entry(def.id.as_ref().clone()) {
				Entry::Vacant(entry) => {
					let i = list.len() as u32;
					list.push(Caused::explicit(
						Type::defined(def.id.as_ref().clone()),
						def.id.span(),
					));
					entry.insert(i);
				}
				Entry::Occupied(entry) => {
					let old_def_index = entry.get();
					let old_def_span = list[*old_def_index as usize].span().unwrap();
					return Err(Loc::new(
						Error::AlreadyDefinedType(def.id.as_ref().clone(), old_def_span),
						def.id.span(),
					));
				}
			}
		}

		Ok(Self {
			list,
			id_map: map,
			// desc_map: BTreeMap::new(),
		})
	}

	pub fn into_vec(self) -> Vec<Caused<Type>> {
		self.list
	}

	fn add_type(&mut self, id: &Loc<Ident>, ty: Type) -> u32 {
		let i = self.list.len() as u32;
		let ty = Caused::explicit(ty, id.span());
		self.list.push(ty);
		self.id_map.insert(id.as_ref().clone(), i);
		i
	}

	pub fn get_by_id(&mut self, id: &Loc<Ident>) -> Result<u32, Loc<Error>> {
		if self.id_map.contains_key(id.as_ref()) {
			Ok(self.id_map.get(id.as_ref()).unwrap().clone())
		} else {
			match id.as_str() {
				"option" => Ok(self.add_type(id, Type::primitive_option())),
				"list" => Ok(self.add_type(id, Type::primitive_list())),
				_ => Err(Loc::new(
					Error::UndefinedType(id.as_ref().clone()),
					id.span(),
				)),
			}
		}
	}

	// pub fn get_by_desc(&mut self, id: ty::Id, span: Span) -> u32 {
	// 	match id {
	// 		ty::Id::Defined(id) => self.id_map.get(&id).cloned().unwrap(),
	// 		_ => {
	// 			use std::collections::btree_map::Entry;
	// 			match self.desc_map.entry(id.clone()) {
	// 				Entry::Vacant(entry) => {
	// 					let i = self.list.len() as u32;
	// 					self.list.push(Caused::explicit(Type::new(id), span));
	// 					entry.insert(i);
	// 					i
	// 				}
	// 				Entry::Occupied(entry) => *entry.get(),
	// 			}
	// 		}
	// 	}
	// }

	// pub fn insert_by_desc(&mut self, id: ty::Id, span: Span) -> (u32, bool) {
	// 	match id {
	// 		ty::Id::Defined(id) => (self.id_map.get(&id).cloned().unwrap(), false),
	// 		_ => {
	// 			use std::collections::btree_map::Entry;
	// 			match self.desc_map.entry(id.clone()) {
	// 				Entry::Vacant(entry) => {
	// 					let i = self.list.len() as u32;
	// 					self.list.push(Caused::explicit(Type::new(id), span));
	// 					entry.insert(i);
	// 					(i, true)
	// 				}
	// 				Entry::Occupied(entry) => (*entry.get(), false),
	// 			}
	// 		}
	// 	}
	// }

	pub fn add_constructor(&mut self, index: u32, function: u32) {
		self.list[index as usize].add_constructor(function)
	}

	// pub fn init_special_types(&mut self, functions: &mut Functions) {
	// 	let mut stack = Vec::new();

	// 	for (ty_index, ty) in self.list.iter().enumerate() {
	// 		match ty.desc() {
	// 			ty::Desc::Nammed(_) => (),
	// 			desc => stack.push((ty_index as u32, desc.clone(), ty.span()))
	// 		}
	// 	}

	// 	while let Some((ty_index, desc, source)) = stack.pop() {
	// 		match desc {
	// 			ty::Desc::Nammed(_) => unreachable!(),
	// 			ty::Desc::Repeat(inner_ty, min, max, separator) => {
	// 				// Generate the continuation type.
	// 				let next_ty_index = if max > 1 {
	// 					let new_min = if min > 0 {
	// 						min - 1
	// 					} else {
	// 						0
	// 					};

	// 					let new_max = if max == usize::MAX {
	// 						max
	// 					} else {
	// 						max - 1
	// 					};

	// 					let new_desc = ty::Desc::Repeat(inner_ty, new_min, new_max, separator);
	// 					let (next_ty_index, inserted) = self.insert_by_desc(new_desc.clone(), source);
	// 					if inserted {
	// 						stack.push((next_ty_index, new_desc, source))
	// 					}

	// 					Some(next_ty_index)
	// 				} else {
	// 					None
	// 				};

	// 				let cons_rule = {
	// 					let mut symbols = vec![MaybeLoc::new(rule::Symbol::NonTerminal(inner_ty), None)];

	// 					if let Some(next_ty_index) = next_ty_index {
	// 						if let Some(separator) = separator {
	// 							symbols.push(MaybeLoc::new(rule::Symbol::Terminal(separator.terminal), None))
	// 						}

	// 						symbols.push(MaybeLoc::new(rule::Symbol::NonTerminal(next_ty_index), None))
	// 					}

	// 					let rule = Rule::new(Some(rule::Constructor::Cons), ty_index, symbols);
	// 					let rule_index = functions.len() as u32;
	// 					functions.push(MaybeLoc::new(rule, None));
	// 					rule_index
	// 				};

	// 				let one_rule = if let Some(next_ty_index) = next_ty_index {
	// 					if max > 0 {
	// 						if let Some(separator) = separator {
	// 							if !separator.strong {
	// 								let rule = Rule::new(Some(rule::Constructor::ConsNil), ty_index, vec![
	// 									MaybeLoc::new(rule::Symbol::NonTerminal(next_ty_index), None)
	// 								]);
	// 								let rule_index = functions.len() as u32;
	// 								functions.push(MaybeLoc::new(rule, None));
	// 								Some(rule_index)
	// 							} else {
	// 								None
	// 							}
	// 						} else {
	// 							None
	// 						}
	// 					} else {
	// 						None
	// 					}
	// 				} else {
	// 					None
	// 				};

	// 				let nil_rule = if min == 0 {
	// 					let rule = Rule::new(Some(rule::Constructor::Nil), ty_index, vec![]);
	// 					let rule_index = functions.len() as u32;
	// 					functions.push(MaybeLoc::new(rule, None));
	// 					Some(rule_index)
	// 				} else {
	// 					None
	// 				};

	// 				let ty = &mut self.list[ty_index as usize];
	// 				ty.add_rule(cons_rule);

	// 				if let Some(one_rule) = one_rule {
	// 					ty.add_rule(one_rule)
	// 				}

	// 				if let Some(nil_rule) = nil_rule {
	// 					ty.add_rule(nil_rule)
	// 				}
	// 			}
	// 		}
	// 	}
	// }
}
