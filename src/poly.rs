use crate::{
	lexing::{regexp, RegExp},
	syntax::{self, Caused},
	Ident,
};
use source_span::{Loc, Span};
use std::collections::HashSet;

mod compile;
pub mod function;
pub mod terminal;
pub mod ty;

pub use compile::Error;
pub use function::Function;
pub use terminal::Terminal;
pub use ty::Type;

pub struct Grammar {
	/// Extern types.
	externs: Vec<(Ident, Option<Span>)>,

	/// Regular expressions.
	regexps: Vec<(regexp::Definition, Option<Loc<syntax::regexp::Definition>>)>,

	/// Terminals.
	terminals: Vec<(Terminal, HashSet<Loc<syntax::RegExp>>)>,

	/// Types.
	types: Vec<Caused<Type>>,

	/// Functions.
	functions: Vec<Caused<Function>>,
}

impl Grammar {
	pub(crate) fn from_raw_parts(
		externs: Vec<(Ident, Option<Span>)>,
		regexps: Vec<(regexp::Definition, Option<Loc<syntax::regexp::Definition>>)>,
		terminals: Vec<(Terminal, HashSet<Loc<syntax::RegExp>>)>,
		types: Vec<Caused<Type>>,
		functions: Vec<Caused<Function>>,
	) -> Self {
		let g = Self {
			externs,
			regexps,
			terminals,
			types,
			functions,
		};

		g
	}

	pub fn extern_types(&self) -> &[(Ident, Option<Span>)] {
		&self.externs
	}

	pub fn extern_type(&self, index: u32) -> Option<&Ident> {
		self.externs.get(index as usize).map(|p| &p.0)
	}

	pub fn regexps(&self) -> &[(regexp::Definition, Option<Loc<syntax::regexp::Definition>>)] {
		&self.regexps
	}

	pub fn regexp(&self, index: u32) -> Option<&regexp::Definition> {
		self.regexps.get(index as usize).map(|p| &p.0)
	}

	pub fn terminals(&self) -> &[(Terminal, HashSet<Loc<syntax::RegExp>>)] {
		&self.terminals
	}

	pub fn terminal(&self, index: u32) -> Option<&Terminal> {
		self.terminals.get(index as usize).map(|t| &t.0)
	}

	pub fn types(&self) -> &[Caused<Type>] {
		&self.types
	}

	pub fn ty(&self, index: u32) -> Option<&Caused<Type>> {
		self.types.get(index as usize)
	}

	pub fn ty_expr(&self, index: u32) -> Option<ty::Expr> {
		self.ty(index)
			.map(|ty| ty::Expr::Type(index, (0..ty.arity()).map(|i| ty::Expr::Var(i)).collect()))
	}

	pub fn functions(&self) -> &[Caused<Function>] {
		&self.functions
	}

	pub fn function(&self, index: u32) -> Option<&Caused<Function>> {
		self.functions.get(index as usize)
	}
}
