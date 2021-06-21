use super::{Function, Ident, RegExp};
use source_span::Loc;

/// Type definition.
pub struct Definition {
	pub id: Loc<Ident>,
	pub parameters: Vec<Loc<Parameter>>,
	pub constructors: Vec<Loc<Function>>,
}

/// Type parameter.
pub enum Parameter {
	Terminal(Loc<Ident>),
	NonTerminal(Loc<Ident>),
}

/// Type expression.
pub enum Expr {
	Terminal(RegExp),
	NonTerminal(Loc<Ident>, Vec<Loc<Expr>>),
}
