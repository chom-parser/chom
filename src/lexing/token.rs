use crate::{
	Ident,
	CharSet
};

pub enum Token {
	RegExp(TypedRegExp)
}

pub struct TypedRegExp {
	pub ty: Option<Ident>,
	pub exp: RegExp
}

pub struct RegExp(pub Vec<RegExpAtom>);

pub enum RegExpAtom {
	Ref(Ident),
	CharSet(CharSet),
	Literal(String, bool),
	Repeat(Box<RegExpAtom>, usize, usize),
	Or(Vec<RegExp>),
	Capture(RegExp),
	Group(RegExp),
}