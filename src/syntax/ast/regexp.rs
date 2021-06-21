use crate::CharSet;
use source_span::Loc;
use std::fmt;

use super::Ident;

#[derive(Clone)]
pub struct Definition {
	pub id: Loc<Ident>,
	pub ty: Option<Loc<Ident>>,
	pub exp: Loc<RegExp>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct RegExp(pub Vec<Loc<Atom>>);

impl fmt::Display for RegExp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use itertools::Itertools;
		self.0.iter().format(" ").fmt(f)
	}
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Atom {
	Ref(Ident),
	CharSet(CharSet),
	Literal(String, bool),
	Repeat(Box<Loc<Atom>>, usize, usize),
	Or(Vec<Loc<RegExp>>),
	Group(Loc<RegExp>),
}

impl fmt::Display for Atom {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Ref(id) => id.fmt(f),
			Self::CharSet(set) => set.fmt(f),
			Self::Literal(string, case_sensitive) => {
				if *case_sensitive {
					write!(f, "'{}'", string)
				} else {
					write!(f, "\"{}\"", string)
				}
			}
			Self::Repeat(atom, min, max) => match (*min, *max) {
				(0, usize::MAX) => write!(f, "{}*", atom),
				(1, usize::MAX) => write!(f, "{}+", atom),
				(0, 1) => write!(f, "{}?", atom),
				_ => unimplemented!(),
			},
			Self::Or(exps) => {
				use itertools::Itertools;
				exps.iter().format(" | ").fmt(f)
			}
			Self::Group(exp) => {
				write!(f, "({})", exp)
			}
		}
	}
}
