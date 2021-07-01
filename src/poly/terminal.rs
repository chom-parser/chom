use super::Grammar;
use crate::{
	lexing::{regexp, RegExp, Token},
	Ident,
};
use std::{cmp::Ordering, fmt};

/// Grammar terminal.
pub struct Terminal {
	desc: Desc,
}

impl Terminal {
	pub fn new(desc: Desc) -> Self {
		Self { desc }
	}

	pub fn as_regexp_ref(&self) -> Option<u32> {
		if let Desc::RegExp(exp) = &self.desc {
			if exp.atoms().len() == 1 {
				if let regexp::Atom::Ref(ty) = exp.atoms().first().unwrap() {
					return Some(*ty);
				}
			}
		}

		None
	}

	pub fn whitespace(ws_index: u32) -> Self {
		Self::new(Desc::Whitespace(ws_index))
	}

	pub fn regexp(exp: RegExp) -> Self {
		Self::new(Desc::RegExp(exp))
	}

	pub fn desc(&self) -> &Desc {
		&self.desc
	}

	/// Returns the identifier associated to the terminal.
	///
	/// It corresponds to the identifier of the referenced regexp,
	/// if the terminal is a direct regexp reference.
	pub fn id<'g>(&self, grammar: &'g Grammar) -> Option<&'g Ident> {
		match &self.desc {
			Desc::RegExp(exp) => exp.id(grammar),
			Desc::Whitespace(_) => None,
		}
	}

	pub fn token(&self, grammar: &Grammar) -> Option<Token> {
		match &self.desc {
			Desc::RegExp(exp) => Some(exp.token(grammar)),
			Desc::Whitespace(_) => None,
		}
	}

	pub fn extern_type(&self, grammar: &Grammar) -> Option<u32> {
		match &self.desc {
			Desc::RegExp(exp) => exp.extern_type(grammar),
			Desc::Whitespace(_) => None,
		}
	}

	pub fn format<'s, 'g>(&'s self, grammar: &'g Grammar) -> Formatted<'g, 's> {
		Formatted(grammar, &self.desc, FormatStyle::Canonical)
	}

	pub fn format_with<'s, 'g>(
		&'s self,
		grammar: &'g Grammar,
		style: FormatStyle,
	) -> Formatted<'g, 's> {
		Formatted(grammar, &self.desc, style)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		match self.desc() {
			Desc::RegExp(exp) => exp.instance(grammar),
			Desc::Whitespace(_) => " ".to_string(), // TODO actually instanciate the `WS` regexp.
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
	/// Regular expression.
	RegExp(RegExp),

	/// Special whitespaces terminal.
	Whitespace(u32),
}

impl fmt::Display for Desc {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::RegExp(exp) => exp.fmt(f),
			Self::Whitespace(_) => write!(f, "WS*"),
		}
	}
}

pub enum FormatStyle {
	Canonical,
	Human,
}

pub struct Formatted<'g, 'd>(&'g Grammar, &'d Desc, FormatStyle);

impl<'g, 'd> fmt::Display for Formatted<'g, 'd> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.2 {
			FormatStyle::Canonical => match self.1 {
				Desc::RegExp(exp) => exp.format(self.0).fmt(f),
				Desc::Whitespace(_) => write!(f, "WS*"),
			},
			FormatStyle::Human => match self.1 {
				Desc::RegExp(exp) => {
					let atoms = exp.atoms();
					if atoms.len() == 1 {
						match &atoms[0] {
							regexp::Atom::Ref(i) => {
								let exp = self.0.regexp(*i).unwrap();
								return exp.id.fmt(f);
							}
							_ => (),
						}
					}

					write!(f, "terminal {}", exp)
				}
				Desc::Whitespace(_) => write!(f, "whitespace"),
			},
		}
	}
}
