use crate::mono::{function, ty, Function, Grammar, Index};
use std::{
	collections::{BTreeMap, BTreeSet},
	fmt,
};

pub mod lalr1;
pub mod lr0;
pub mod non_deterministic;

pub use lalr1::LALR1;
pub use lr0::LR0;
pub use non_deterministic::NonDeterministic;

pub enum Table {
	LR0(LR0),
	LALR1(LALR1),
}

/// Item rule.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Rule {
	Initial(Index),
	Function(Index),
}

pub enum RuleId<'a> {
	Initial(&'a ty::Id),
	Function(&'a function::Id),
}

impl<'a> RuleId<'a> {
	pub fn as_str(&self) -> &str {
		match self {
			Self::Initial(id) => id.name(),
			Self::Function(id) => id.as_str(),
		}
	}
}

impl Rule {
	pub fn len(&self, grammar: &Grammar) -> u32 {
		match self {
			Self::Initial(_) => 2,
			Self::Function(index) => {
				let f = grammar.function(*index).unwrap();
				f.arity()
			}
		}
	}

	pub fn is_initial(&self) -> bool {
		match self {
			Self::Initial(_) => true,
			_ => false,
		}
	}

	pub fn id<'a>(&self, grammar: &'a Grammar) -> RuleId<'a> {
		match self {
			Self::Initial(index) => RuleId::Initial(grammar.ty(*index).unwrap().id()),
			Self::Function(index) => RuleId::Function(grammar.function(*index).unwrap().id()),
		}
	}

	pub fn span(&self, grammar: &Grammar) -> Option<source_span::Span> {
		match self {
			Self::Initial(index) => grammar.ty(*index).unwrap().span(),
			Self::Function(index) => grammar.function(*index).unwrap().span(),
		}
	}

	pub fn return_ty(&self, grammar: &Grammar) -> Index {
		match self {
			Self::Initial(index) => *index,
			Self::Function(index) => {
				let f = grammar.function(*index).unwrap();
				f.return_ty()
			}
		}
	}

	pub fn symbol(&self, grammar: &Grammar, i: u32) -> Option<Symbol> {
		match self {
			Self::Initial(index) => match i {
				0 => Some(Symbol::Expr(ty::Expr::Type(*index))),
				1 => Some(Symbol::EndOfStream),
				_ => None,
			},
			Self::Function(index) => {
				let f = grammar.function(*index).unwrap();
				f.argument(i).map(Symbol::Expr)
			}
		}
	}

	pub fn symbols<'g, 'p>(&self, grammar: &'g Grammar<'p>) -> Symbols<'g, 'p> {
		match self {
			Self::Initial(index) => Symbols::Initial(*index, 0),
			Self::Function(index) => {
				let f = grammar.function(*index).unwrap();
				Symbols::Function(&f, 0)
			}
		}
	}
}

pub enum Symbols<'g, 'p> {
	Initial(Index, u8),
	Function(&'g Function<'p>, u32),
}

impl<'g, 'p> Iterator for Symbols<'g, 'p> {
	type Item = Symbol;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Initial(index, i) => match *i {
				0 => {
					*i += 1;
					Some(Symbol::Expr(ty::Expr::Type(*index)))
				}
				1 => {
					*i += 1;
					Some(Symbol::EndOfStream)
				}
				_ => None,
			},
			Self::Function(f, i) => match f.argument(*i) {
				Some(a) => {
					*i += 1;
					Some(Symbol::Expr(a))
				}
				None => None,
			},
		}
	}
}

// Rule symbol.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Symbol {
	Expr(ty::Expr),
	EndOfStream,
}

impl Symbol {
	pub fn format<'a, 'g>(&self, grammar: &'g Grammar<'a>) -> FormattedSymbol<'a, 'g, '_> {
		match self {
			Self::Expr(e) => FormattedSymbol::Expr(e.format(grammar)),
			Self::EndOfStream => FormattedSymbol::EndOfStream,
		}
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		match self {
			Self::Expr(e) => e.instance(grammar),
			Self::EndOfStream => String::new(),
		}
	}
}

pub enum FormattedSymbol<'a, 'g, 's> {
	Expr(ty::FormattedExpr<'a, 'g, 's>),
	EndOfStream,
}

impl<'a, 'g, 's> fmt::Display for FormattedSymbol<'a, 'g, 's> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Expr(e) => e.fmt(f),
			Self::EndOfStream => write!(f, "EOS"),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Item {
	/// Rule.
	pub rule: Rule,

	/// Offset in the rule.
	pub offset: u32,
}

impl Item {
	pub fn initial(ty: Index) -> Self {
		Self {
			rule: Rule::Initial(ty),
			offset: 0,
		}
	}

	pub fn from_rule(function: Index) -> Self {
		Self {
			rule: Rule::Function(function),
			offset: 0,
		}
	}

	pub fn is_initial(&self) -> bool {
		self.rule.is_initial()
	}

	pub fn current_symbol(&self, grammar: &Grammar) -> Option<Symbol> {
		self.rule.symbol(grammar, self.offset)
	}

	pub fn shifted(&self, grammar: &Grammar) -> Option<(Symbol, Self)> {
		self.rule.symbol(grammar, self.offset).map(|symbol| {
			(
				symbol,
				Self {
					rule: self.rule,
					offset: self.offset + 1,
				},
			)
		})
	}

	pub fn format<'a, 'g>(&self, grammar: &'g Grammar<'a>) -> FormattedItem<'a, 'g, '_> {
		FormattedItem(grammar, self)
	}
}

pub struct FormattedItem<'a, 'g, 'i>(&'g Grammar<'a>, &'i Item);

impl<'a, 'g, 'i> fmt::Display for FormattedItem<'a, 'g, 'i> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for (i, arg) in self.1.rule.symbols(self.0).enumerate() {
			if i as u32 == self.1.offset {
				if i == 0 {
					write!(f, "• ")?
				} else {
					write!(f, " • ")?
				}
			} else if i > 0 {
				write!(f, " ")?
			}

			arg.format(self.0).fmt(f)?
		}

		if self.1.offset == self.1.rule.len(self.0) {
			if self.1.offset == 0 {
				write!(f, "•")?
			} else {
				write!(f, " •")?
			}
		}

		Ok(())
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ItemSet {
	items: BTreeSet<Item>,
}

impl ItemSet {
	pub fn new() -> Self {
		Self {
			items: BTreeSet::new(),
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
			if let Some(arg) = item.current_symbol(grammar) {
				match arg {
					Symbol::Expr(ty::Expr::Type(ty)) => {
						for ty_cons in grammar.ty(ty).unwrap().constructors() {
							let ty_item = Item {
								rule: Rule::Function(ty_cons),
								offset: 0,
							};

							if self.items.insert(ty_item) {
								stack.push(ty_item)
							}
						}
					}
					_ => (),
				}
			}
		}
	}

	pub fn shift(&self, grammar: &Grammar) -> BTreeMap<Symbol, ItemSet> {
		let mut map: BTreeMap<Symbol, ItemSet> = BTreeMap::new();

		for item in &self.items {
			if let Some((symbol, shifted_item)) = item.shifted(grammar) {
				use std::collections::btree_map::Entry;
				match map.entry(symbol) {
					Entry::Occupied(mut entry) => entry.get_mut().insert(shifted_item),
					Entry::Vacant(entry) => {
						let mut set = ItemSet::new();
						set.insert(shifted_item);
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
