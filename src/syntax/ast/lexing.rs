use std::fmt;
use source_span::Loc;
use crate::CharSet;

use super::Ident;

#[derive(Clone)]
pub struct RegExpDefinition {
	pub id: Loc<Ident>,
	pub ty: Option<Loc<Ident>>,
	pub exp: Loc<RegExp>
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct RegExp(pub Vec<Loc<RegExpAtom>>);

// impl RegExp {
// 	pub fn compiled(&self) -> crate::lexing::RegExp {
// 		crate::lexing::RegExp::new(self.0.iter().map(|atom| atom.compiled()).collect())
// 	}
// }

impl fmt::Display for RegExp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use itertools::Itertools;
		self.0.iter().format(" ").fmt(f)
	}
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum RegExpAtom {
	Ref(Ident),
	CharSet(CharSet),
	Literal(String, bool),
	Repeat(Box<Loc<RegExpAtom>>, usize, usize),
	Or(Vec<Loc<RegExp>>),
	Group(Loc<RegExp>)
}

// impl RegExpAtom {
// 	fn compiled(&self) -> crate::lexing::RegExpAtom {
// 		match self {
// 			Self::Ref(id) => crate::lexing::RegExpAtom::Ref(id.clone()),
// 			Self::CharSet(set) => crate::lexing::RegExpAtom::CharSet(set.clone()),
// 			Self::Literal(lit, case_sensitive) => crate::lexing::RegExpAtom::Literal(lit.clone(), *case_sensitive),
// 			Self::Repeat(atom, min, max) => crate::lexing::RegExpAtom::Repeat(Box::new(atom.compiled()), *min, *max),
// 			Self::Or(exps) => crate::lexing::RegExpAtom::Or(exps.iter().map(|exp| exp.compiled()).collect()),
// 			Self::Group(exp) => crate::lexing::RegExpAtom::Group(exp.compiled())
// 		}
// 	}
// }

impl fmt::Display for RegExpAtom {
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
			},
			Self::Repeat(atom, min, max) => {
				match (*min, *max) {
					(0, usize::MAX) => write!(f, "{}*", atom),
					(1, usize::MAX) => write!(f, "{}+", atom),
					(0, 1) => write!(f, "{}?", atom),
					_ => unimplemented!()
				}
			},
			Self::Or(exps) => {
				use itertools::Itertools;
				exps.iter().format(" | ").fmt(f)
			},
			Self::Group(exp) => {
				write!(f, "({})", exp)
			}
		}
	}
}