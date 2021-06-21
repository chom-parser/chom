use source_span::Loc;

use super::{regexp, ty, Ident};

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: Vec<Loc<regexp::Definition>>,
	pub types: Vec<Loc<ty::Definition>>,
}
