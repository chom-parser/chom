use std::fmt;
use source_span::Loc;
use crate::{
	Grammar,
	syntax::{
		MaybeLoc,
		Ident
	}
};

pub enum Constructor {
	/// Nammed constructor.
	Ident(Loc<Ident>),

	/// List constructor.
	Cons,

	/// Nil constructor.
	Nil,

	/// Last element list constructor.
	ConsNil
}

/// Syntax rule.
pub struct Rule {
	/// Optional identifier.
	cons: Option<Constructor>,

	/// Return type.
	ty: u32,

	/// Parameters.
	symbols: Vec<MaybeLoc<Symbol>>
}

impl Rule {
	pub fn new(cons: Option<Constructor>, ty: u32, symbols: Vec<MaybeLoc<Symbol>>) -> Self {
		Rule {
			cons,
			ty,
			symbols
		}
	}

	pub fn id(&self) -> Option<&Loc<Ident>> {
		match &self.cons {
			Some(Constructor::Ident(id)) => Some(id),
			_ => None
		}
	}
	
	pub fn ty(&self) -> u32 {
		self.ty
	}

	pub fn symbols(&self) -> &[MaybeLoc<Symbol>] {
		&self.symbols
	}

	pub fn symbol(&self, offset: usize) -> Option<&MaybeLoc<Symbol>> {
		self.symbols.get(offset)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		let mut string = String::new();
		
		for symbol in &self.symbols {
			string.extend(symbol.instance(grammar).chars())
		}
		
		string
	}
}

/// Rule symbol.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Symbol {
	Terminal(u32),
	NonTerminal(u32)
}

impl Symbol {
	pub fn format<'g>(&self, grammar: &'g Grammar) -> FormattedSymbol<'g, '_> {
		FormattedSymbol(grammar, self)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		match self {
			Self::Terminal(i) => grammar.terminal(*i).unwrap().instance(grammar),
			Self::NonTerminal(i) => grammar.ty(*i).unwrap().instance(grammar)
		}
	}
}

pub struct FormattedSymbol<'g, 's>(&'g Grammar, &'s Symbol);

impl<'g, 's> fmt::Display for FormattedSymbol<'g, 's> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.1 {
			Symbol::Terminal(t) => {
				let t = self.0.terminal(*t).unwrap();
				t.format(self.0).fmt(f)
			},
			Symbol::NonTerminal(nt) => {
				let ty = self.0.ty(*nt).unwrap();
				ty.format(self.0).fmt(f)
			}
		}
	}
}