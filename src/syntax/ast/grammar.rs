use source_span::Loc;

use super::{
	Ident,
	RegExpDefinition,
	RegExp
};

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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Terminal {
	RegExp(RegExp)
}

// impl Terminal {
// 	pub fn compiled(&self) -> crate::grammar::Terminal {
// 		match self {
// 			Self::RegExp(e) => crate::grammar::Terminal::RegExp(e.compiled())
// 		}
// 	}
// }

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NonTerminal {
	Type(Loc<Ident>),
	Repeat(Loc<Ident>, usize, usize, Option<Loc<Separator>>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Separator {
	pub strong: bool,
	pub terminal: Loc<Terminal>
}