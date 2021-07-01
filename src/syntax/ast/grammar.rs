use super::{regexp, ty};
use crate::Ident;
use source_span::Loc;

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: Vec<Loc<regexp::Definition>>,
	pub types: Vec<Loc<ty::Definition>>,
}
