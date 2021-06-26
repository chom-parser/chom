use source_span::Loc;
use crate::Ident;
use super::{regexp, ty};

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: Vec<Loc<regexp::Definition>>,
	pub types: Vec<Loc<ty::Definition>>,
}
