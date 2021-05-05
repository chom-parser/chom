use std::fmt;
use source_span::Loc;
use crate::CharSet;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Ident(pub String);

impl Ident {
	pub fn as_str(&self) -> &str {
		&self.0
	}
}

impl fmt::Display for Ident {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.0.fmt(f)
	}
}

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: Vec<Loc<RegExpDefinition>>,
	pub types: Vec<Loc<Type>>
}

pub struct Type {
	pub id: Loc<Ident>,
	pub rules: Vec<Loc<Rule>>
}

pub struct Rule {
	pub id: Option<Loc<Ident>>,
	pub items: Vec<Loc<Item>>
}

pub enum Item {
	Terminal(Terminal),
	NonTerminal(NonTerminal)
}

pub enum Terminal {
	RegExp(RegExp)
}

pub enum NonTerminal {
	Type(Loc<Ident>),
	Repeat(Loc<Ident>, usize, usize, Option<Loc<Separator>>),
}

pub struct Separator {
	pub strong: bool,
	pub terminal: Loc<Terminal>
}

pub struct RegExpDefinition {
	pub id: Loc<Ident>,
	pub ty: Option<Loc<Ident>>,
	pub exp: Loc<RegExp>
}

pub struct RegExp(pub Vec<Loc<RegExpAtom>>);

pub enum RegExpAtom {
	Ident(Ident),
	CharSet(CharSet, bool),
	Literal(String, bool),
	Repeat(Box<Loc<RegExpAtom>>, usize, usize),
	Or(Vec<Loc<RegExp>>),
	// Capture(Loc<RegExp>),
	Group(Loc<RegExp>),
	// Cast(Loc<RegExp>, Loc<Ident>)
}
