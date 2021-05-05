use std::collections::{
	HashMap,
	HashSet
};
use std::fmt;
use source_span::{
	Loc
};
use crate::{
	Ident,
	lexing::{
		RegExp,
		LocRegExp,
		LocRegExpAtom
	}
};

mod compile;

pub use compile::Error;

pub struct Grammar {
	externs: Vec<Loc<Ident>>,
	regexps: HashMap<Ident, Loc<RegExpDefinition>>,
	types: HashMap<Ident, Loc<Type>>,
	terminals: Vec<(Terminal, HashSet<Loc<LocTerminal>>)>
}

impl Grammar {
	pub(crate) fn from_raw_parts(
		externs: Vec<Loc<Ident>>,
		regexps: HashMap<Ident, Loc<RegExpDefinition>>,
		types: HashMap<Ident, Loc<Type>>,
		terminals: Vec<(Terminal, HashSet<Loc<LocTerminal>>)>
	) -> Self {
		Self {
			externs,
			regexps,
			types,
			terminals
		}
	}

	pub fn extern_types(&self) -> &[Loc<Ident>] {
		&self.externs
	}

	pub fn regexps(&self) -> impl Iterator<Item = (&Ident, &Loc<RegExpDefinition>)> {
		self.regexps.iter()
	}

	pub fn types(&self) -> impl Iterator<Item = (&Ident, &Loc<Type>)> {
		self.types.iter()
	}

	pub fn regexp(&self, id: &Ident) -> Option<&Loc<RegExpDefinition>> {
		self.regexps.get(id)
	}

	pub fn terminals(&self) -> &[(Terminal, HashSet<Loc<LocTerminal>>)] {
		&self.terminals
	}
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
	Terminal(u32),
	NonTerminal(NonTerminal)
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Terminal {
	RegExp(RegExp)
}

impl fmt::Display for Terminal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::RegExp(exp) => exp.fmt(f)
		}
	}
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum LocTerminal {
	RegExp(LocRegExp)
}

impl LocTerminal {
	pub fn stripped(&self) -> Terminal {
		match self {
			Self::RegExp(exp) => Terminal::RegExp(exp.stripped())
		}
	}
}

impl fmt::Display for LocTerminal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::RegExp(exp) => exp.fmt(f)
		}
	}
}

pub enum NonTerminal {
	Type(Loc<Ident>),
	Repeat(Loc<Ident>, usize, usize, Option<Loc<Separator>>),
}

impl fmt::Display for NonTerminal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Type(ident) => write!(f, "<{}>", ident.as_ref()),
			Self::Repeat(ident, min, max, sep) => {
				match sep {
					Some(sep) => {
						match (*min, *max) {
							(0, usize::MAX) => write!(f, "<{}*{}>", ident, sep),
							(1, usize::MAX) => write!(f, "<{}+{}>", ident, sep),
							_ => unimplemented!()
						}
					},
					None => {
						match (*min, *max) {
							(0, usize::MAX) => write!(f, "<{}*>", ident),
							(1, usize::MAX) => write!(f, "<{}+>", ident),
							(0, 1) => write!(f, "<{}?>", ident),
							_ => unimplemented!()
						}
					}
				}
			}
		}
	}
}

pub struct Separator {
	/// If true, then every item of the list must be followed by a separator.
	pub strong: bool,

	/// Identifier of the temrinal used as separator.
	pub terminal: Loc<u32>
}

impl fmt::Display for Separator {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.strong {
			write!(f, "!")?;
		}

		self.terminal.as_ref().fmt(f)
	}
}

pub struct RegExpDefinition {
	pub id: Loc<Ident>,
	pub ty: Option<Loc<Ident>>,
	pub exp: Loc<LocRegExp>
}

impl fmt::Display for RegExpDefinition {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ty) = &self.ty {
			write!(f, "regexp {}: {} = {}", self.id.as_ref(), ty, self.exp)
		} else {
			write!(f, "regexp {} = {}", self.id.as_ref(), self.exp)
		}
	}
}

