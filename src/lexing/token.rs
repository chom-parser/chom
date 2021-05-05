use std::fmt;
use source_span::Loc;
use crate::{
	Ident,
	CharSet
};

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Token {
	RegExp(RegExp)
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct RegExp(pub Vec<RegExpAtom>);

impl fmt::Display for RegExp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use itertools::Itertools;
		self.0.iter().format(" ").fmt(f)
	}
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct LocRegExp(pub Vec<Loc<LocRegExpAtom>>);

impl LocRegExp {
	pub fn stripped(&self) -> RegExp {
		RegExp(self.0.iter().map(|atom| atom.stripped()).collect())
	}
}

impl fmt::Display for LocRegExp {
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
	Repeat(Box<RegExpAtom>, usize, usize),
	Or(Vec<RegExp>),
	// Capture(RegExp),
	Group(RegExp),
	// Cast(RegExp, Ident)
}

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
			// Self::Capture(exp) => {
			// 	write!(f, "{{ {} }}", exp)
			// },
			Self::Group(exp) => {
				write!(f, "({})", exp)
			},
			// Self::Cast(exp, ty) => {
			// 	write!(f, "{} : {}", exp, ty)
			// }
		}
	}
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum LocRegExpAtom {
	Ref(Ident),
	CharSet(CharSet),
	Literal(String, bool),
	Repeat(Box<Loc<LocRegExpAtom>>, usize, usize),
	Or(Vec<Loc<LocRegExp>>),
	// Capture(Loc<LocRegExp>),
	Group(Loc<LocRegExp>),
	// Cast(Loc<LocRegExp>, Loc<Ident>)
}

impl LocRegExpAtom {
	fn stripped(&self) -> RegExpAtom {
		match self {
			Self::Ref(id) => RegExpAtom::Ref(id.clone()),
			Self::CharSet(set) => RegExpAtom::CharSet(set.clone()),
			Self::Literal(lit, case_sensitive) => RegExpAtom::Literal(lit.clone(), *case_sensitive),
			Self::Repeat(atom, min, max) => RegExpAtom::Repeat(Box::new(atom.stripped()), *min, *max),
			Self::Or(exps) => RegExpAtom::Or(exps.iter().map(|exp| exp.stripped()).collect()),
			// Self::Capture(exp) => RegExpAtom::Capture(exp.stripped()),
			Self::Group(exp) => RegExpAtom::Group(exp.stripped()),
			// Self::Cast(exp, ty) => RegExpAtom::Cast(exp.stripped(), ty.as_ref().clone())
		}
	}
}

impl fmt::Display for LocRegExpAtom {
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
			// Self::Capture(exp) => {
			// 	write!(f, "{{ {} }}", exp)
			// },
			Self::Group(exp) => {
				write!(f, "({})", exp)
			},
			// Self::Cast(exp, ty) => {
			// 	write!(f, "{} : {}", exp, ty)
			// }
		}
	}
}