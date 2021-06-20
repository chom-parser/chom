use source_span::Loc;
use std::{
	fmt,
	collections::{
		BTreeMap,
		BTreeSet
	}
};
use crate::mono::{
	Index,
	Grammar,
	ty
};

pub mod non_deterministic;
pub mod lr0;
pub mod lalr1;

pub use non_deterministic::NonDeterministic;
pub use lr0::LR0;
pub use lalr1::LALR1;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Item {
	/// Function index.
	pub function: (u32, ty::Instance),

	/// Offset in the rule.
	pub offset: u32
}

impl Item {
	pub fn from_rule(function: (u32, ty::Instance)) -> Self {
		Self {
			function,
			offset: 0
		}
	}

	pub fn shifted(&self) -> Self {
		Self {
			function: self.function,
			offset: self.offset + 1
		}
	}

	pub fn format<'a, 'g>(&self, grammar: &'g Grammar<'a>) -> FormattedItem<'a, 'g, '_> {
		FormattedItem(grammar, self)
	}
}

pub struct FormattedItem<'a, 'g, 'i>(&'g Grammar<'a>, &'i Item);

impl<'a, 'g, 'i> fmt::Display for FormattedItem<'a, 'g, 'i> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let fun = self.0.function(self.1.function).unwrap();
		for (i, arg) in fun.arguments().iter().enumerate() {
			if i as u32 == self.1.offset {
				write!(f, "•")?
			}

			arg.format(self.0).fmt(f)?
		}

		if self.1.offset == fun.arity() {
			write!(f, "•")?
		}

		Ok(())
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ItemSet {
	items: BTreeSet<Item>
}

impl ItemSet {
	pub fn new() -> Self {
		Self {
			items: BTreeSet::new()
		}
	}

	pub fn len(&self) -> usize {
		self.items.len()
	}

	pub fn iter(&self) -> std::collections::btree_set::Iter<Item> {
		self.items.iter()
	}

	pub fn insert(&mut self, item: Item) {
		self.items.insert(item);
	}

	pub fn close(&mut self, grammar: &Grammar) {
		let mut stack: Vec<_> = self.items.iter().cloned().collect();

		while let Some(item) = stack.pop() {
			let f = grammar.function(item.function).unwrap();
			
			if let Some(arg) = f.argument(item.offset) {
				match arg {
					ty::Expr::Terminal(_) => (),
					ty::Expr::Type(ty) => {
						for ty_cons in grammar.ty(ty).unwrap().constructors() {
							let ty_item = Item {
								function: ty_cons,
								offset: 0
							};

							if self.items.insert(ty_item) {
								stack.push(ty_item)
							}
						}
					}
				}
			}
		}
	}

	pub fn shift(&self, grammar: &Grammar) -> BTreeMap<ty::Expr, ItemSet> {
		let mut map: BTreeMap<ty::Expr, ItemSet> = BTreeMap::new();
		
		for item in &self.items {
			let f = grammar.function(item.function).unwrap();
			if let Some(symbol) = f.argument(item.offset) {
				use std::collections::btree_map::Entry;
				match map.entry(symbol) {
					Entry::Occupied(mut entry) => {
						entry.get_mut().insert(item.shifted())
					},
					Entry::Vacant(entry) => {
						let mut set = ItemSet::new();
						set.insert(item.shifted());
						entry.insert(set);
					}
				}
			}
		}

		for (_, set) in &mut map {
			set.close(grammar)
		}
		
		map
	}

	// pub fn reduce(&self, grammar: &Grammar) -> Result<Option<Index>, Loc<Ambiguity>> {
	// 	let mut target = None;

	// 	for item in &self.items {
	// 		let f = grammar.function(item.function).unwrap();
	// 		if item.offset >= f.arity() {
	// 			target = Some(match target {
	// 				Some(other_target) => {
	// 					let span = f.span().unwrap_or_else(|| {
	// 						grammar.ty(f.return_ty()).unwrap().span().expect("no span")
	// 					});
						
	// 					return Err(Loc::new(Ambiguity::ReduceReduce(item.function, other_target), span))
	// 				},
	// 				None => item.function
	// 			})
	// 		}
	// 	}

	// 	Ok(target)
	// }

	pub fn format<'a, 'g>(&self, grammar: &'g Grammar<'a>) -> FormattedItemSet<'a, 'g, '_> {
		FormattedItemSet(grammar, self)
	}

	pub fn dot_format<'a, 'g>(&self, grammar: &'g Grammar<'a>) -> DotFormattedItemSet<'a, 'g, '_> {
		DotFormattedItemSet(grammar, self)
	}
}

impl<'a> IntoIterator for &'a ItemSet {
	type Item = &'a Item;
	type IntoIter = std::collections::btree_set::Iter<'a, Item>;

	fn into_iter(self) -> Self::IntoIter {
		self.items.iter()
	}
}

pub struct FormattedItemSet<'a, 'g, 's>(&'g Grammar<'a>, &'s ItemSet);

impl<'a, 'g, 's> fmt::Display for FormattedItemSet<'a, 'g, 's> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for item in &self.1.items {
			write!(f, "\t{}\n", item.format(self.0))?
		}

		Ok(())
	}
}

pub struct DotFormattedItemSet<'a, 'g, 's>(&'g Grammar<'a>, &'s ItemSet);

impl<'a, 'g, 's> fmt::Display for DotFormattedItemSet<'a, 'g, 's> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<table>")?;

		for item in &self.1.items {
			write!(f, "<tr><td>{}</td></tr>", item.format(self.0))?
		}

		write!(f, "</table>")
	}
}