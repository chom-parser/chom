use source_span::Loc;
use crate::syntax::Ident;

/// Syntax rule.
pub struct Rule {
	/// Optional identifier.
	pub id: Option<Loc<Ident>>,

	/// Return type.
	pub ty: u32,

	/// Parameters.
	pub items: Vec<Loc<Item>>
}

/// Rule item.
pub enum Item {
	Terminal(u32),
	NonTerminal(u32)
}