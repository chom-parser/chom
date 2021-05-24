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

	pub fn as_regexp_ref(&self) -> Option<u32> {
		if let Desc::RegExp(exp) = &self.desc {
			if exp.atoms().len() == 1 {
				if let regexp::Atom::Ref(ty) = exp.atoms().first().unwrap() {
					return Some(*ty)
				}
			}
		}

		None
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
		Formatted(grammar, &self.desc, FormatStyle::Canonical)
	}

	pub fn format_with<'s, 'g>(&'s self, grammar: &'g Grammar, style: FormatStyle) -> Formatted<'g, 's> {
		Formatted(grammar, &self.desc, style)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		match self.desc() {
			Desc::RegExp(exp) => exp.instance(grammar),
			Desc::Whitespace => " ".to_string()
		}
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

pub enum FormatStyle {
	Canonical,
	Human
}

pub struct Formatted<'g, 'd>(&'g Grammar, &'d Desc, FormatStyle);

impl<'g, 'd> fmt::Display for Formatted<'g, 'd> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.2 {
			FormatStyle::Canonical => {
				match self.1 {
					Desc::RegExp(exp) => exp.format(self.0).fmt(f),
					Desc::Whitespace => write!(f, "WS*")
				}
			},
			FormatStyle::Human => {
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
	}
}