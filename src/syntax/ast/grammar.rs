use source_span::Loc;

use super::{
	Ident,
	regexp,
	ty
};

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: Vec<Loc<regexp::Definition>>,
	pub types: Vec<Loc<ty::Definition>>
}