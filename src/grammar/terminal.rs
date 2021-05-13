use std::{
	fmt,
	cmp::Ordering
};
use once_cell::unsync::OnceCell;
use crate::{
	lexing::{
		Token,
		RegExp,
		regexp
	}
};
use super::Grammar;

pub struct Terminal {
	desc: Desc,
	token: OnceCell<Token>
}

impl Terminal {
	pub fn new(desc: Desc) -> Self {
		Self {
			desc,
			token: OnceCell::new()
		}
	}

	pub fn whitespace() -> Self {
		Self::new(Desc::Whitespace)
	}

	pub fn regexp(exp: RegExp) -> Self {
		Self::new(Desc::RegExp(exp))
	}

	pub fn desc(&self) -> &Desc {
		&self.desc
	}

	pub fn token(&self) -> Option<&Token> {
		self.token.get()
	}

	pub fn init_token(&self, grammar: &Grammar) {
		match &self.desc {
			Desc::RegExp(exp) => {
				self.token.set(exp.token(grammar)).ok().unwrap()
			},
			Desc::Whitespace => ()
		}
	}

	pub fn format<'s, 'g>(&'s self, grammar: &'g Grammar) -> Formatted<'g, 's> {
		Formatted(grammar, &self.desc)
	}
}

impl fmt::Display for Terminal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.desc.fmt(f)
	}
}

impl PartialEq for Terminal {
	fn eq(&self, other: &Self) -> bool {
		self.desc.eq(&other.desc)
	}
}

impl Eq for Terminal {}

impl PartialOrd for Terminal {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.desc.partial_cmp(&other.desc)
	}
}

impl Ord for Terminal {
	fn cmp(&self, other: &Self) -> Ordering {
		self.desc.cmp(&other.desc)
	}
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
/// Terminal description.
pub enum Desc {
	RegExp(RegExp),
	Whitespace
}

impl fmt::Display for Desc {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::RegExp(exp) => exp.fmt(f),
			Self::Whitespace => write!(f, "WS*")
		}
	}
}

pub struct Formatted<'g, 'd>(&'g Grammar, &'d Desc);

impl<'g, 'd> fmt::Display for Formatted<'g, 'd> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.1 {
			Desc::RegExp(exp) => {
				let atoms = exp.atoms();
				if atoms.len() == 1 {
					match &atoms[0] {
						regexp::Atom::Ref(i) => {
							let exp = self.0.regexp(*i).unwrap();
							return exp.id.fmt(f)
						},
						_ => ()
					}
				}

				write!(f, "terminal {}", exp)
			},
			Desc::Whitespace => write!(f, "whitespace")
		}
	}
}