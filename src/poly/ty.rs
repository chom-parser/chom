use std::fmt;
use crate::syntax::Ident;
use super::Grammar;

/// Polymorphic type identifier.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Id {
	Primitive(Primitive),
	Defined(Ident)
}

impl Id {
	pub fn name(&self) -> &str {
		match self {
			Self::Primitive(p) => p.name(),
			Self::Defined(id) => id.as_str()
		}
	}
}

/// Primitive type id.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Primitive {
	Option,
	List
}

impl Primitive {
	pub fn name(&self) -> &str {
		match self {
			Self::Option => "option",
			Self::List => "list"
		}
	}
}

/// Polymorphic type.
pub struct Type {
	id: Id,
	parameters: Vec<Parameter>,
	constructors: Vec<u32>
}

/// Type parameter.
pub enum Parameter {
	NonTerminal(Ident),
	Terminal(Ident)
}

impl fmt::Display for Parameter {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::NonTerminal(id) => write!(f, "<{}>", id),
			Self::Terminal(id) => id.fmt(f)
		}
	}
}

impl Type {
	pub fn new(id: Id) -> Self {
		Self {
			id,
			parameters: Vec::new(),
			constructors: Vec::new()
		}
	}

	pub fn defined(id: Ident) -> Self {
		Self::new(Id::Defined(id))
	}

	pub fn id(&self) -> &Id {
		&self.id
	}

	pub fn name(&self) -> &str {
		self.id().name()
	}

	pub fn defined_id(&self) -> Option<&Ident> {
		match &self.id {
			Id::Defined(id) => Some(id),
			_ => None
		}
	}

	pub fn parameters(&self) -> &[Parameter] {
		&self.parameters
	}

	pub fn parameter(&self, index: u32) -> Option<&Parameter> {
		self.parameters.get(index as usize)
	}

	pub fn constructors(&self) -> &[u32] {
		&self.constructors
	}

	pub fn add_constructor(&mut self, rule: u32) {
		self.constructors.push(rule)
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.id.name().fmt(f)?;

		for p in &self.parameters {
			write!(f, " {}", p)?;
		}

		Ok(())
	}
}

/// Expression.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expr {
	Var(u32),
	Terminal(u32),
	Type(u32, Vec<Expr>)
}

impl Expr {
	pub fn format<'g>(&self, context: &'g Type, grammar: &'g Grammar) -> FormattedExpr<'g, '_> {
		FormattedExpr(grammar, context, self)
	}
}

pub struct FormattedExpr<'g, 'e>(&'g Grammar, &'g Type, &'e Expr);

impl<'g, 'e> fmt::Display for FormattedExpr<'g, 'e> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.2 {
			Expr::Var(x) => {
				self.1.parameter(*x).unwrap().fmt(f)
			}
			Expr::Terminal(t) => {
				let t = self.0.terminal(*t).unwrap();
				t.format(self.0).fmt(f)
			},
			Expr::Type(nt, args) => {
				let ty = self.0.ty(*nt).unwrap();
				ty.name().fmt(f)
			}
		}
	}
}