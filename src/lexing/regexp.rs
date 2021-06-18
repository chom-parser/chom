use std::{
	fmt,
	hash::{
		Hash,
		Hasher
	},
	cmp::{
		PartialOrd,
		Ord,
		Ordering
	}
};
use once_cell::unsync::OnceCell;
use crate::{
	Ident,
	CharSet,
	poly::{
		Grammar,
		ExternalType
	}
};
use super::{
	Token,
	token
};

pub struct Definition {
	pub id: Ident,
	pub ty: u32,
	pub exp: RegExp
}

impl Definition {
	pub fn format<'g>(&self, grammar: &'g Grammar) -> FormattedDefinition<'g, '_> {
		FormattedDefinition(grammar, self)
	}
}

pub struct FormattedDefinition<'g, 'd>(&'g Grammar, &'d Definition);

impl<'g, 'd> fmt::Display for FormattedDefinition<'g, 'd> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let ty = self.0.extern_type(self.1.ty).unwrap();
		if *ty != ExternalType::Unit {
			write!(f, "regexp {}: {} = {}", self.1.id, ty, self.1.exp)
		} else {
			write!(f, "regexp {} = {}", self.1.id, self.1.exp)
		}
	}
}

#[derive(Clone)]
pub struct RegExp(pub Vec<Atom>, OnceCell<Token>);

impl PartialEq for RegExp {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0
	}
}

impl Eq for RegExp {}

impl PartialOrd for RegExp {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.0.partial_cmp(&other.0)
	}
}

impl Ord for RegExp {
	fn cmp(&self, other: &Self) -> Ordering {
		self.0.cmp(&other.0)
	}
}

impl Hash for RegExp {
	fn hash<H: Hasher>(&self, h: &mut H) {
		self.0.hash(h)
	}
}

impl RegExp {
	pub fn new(atoms: Vec<Atom>) -> Self {
		Self(atoms, OnceCell::new())
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn atoms(&self) -> &[Atom] {
		&self.0
	}

	pub fn as_reference(&self) -> Option<u32> {
		if self.len() == 1 {
			self.0.first().unwrap().as_reference()
		} else {
			None
		}
	}

	pub fn token(&self, grammar: &Grammar) -> Token {
		if self.len() == 1 {
			if let Some(token) = self.0.first().unwrap().token(grammar) {
				return token
			}
		}

		if let Some(name) = self.composite_name(grammar) {
			return Token::Composed(name, token::Convertion::from_regexp(grammar, self))
		}

		Token::Anonymous(0, token::Convertion::from_regexp(grammar, self))
	}

	fn composite_name(&self, grammar: &Grammar) -> Option<Vec<String>> {
		let mut name = Vec::new();
	
		for atom in &self.0 {
			match atom.token(grammar) {
				Some(Token::Named(n, _)) => name.push(n.as_str().to_string()),
				Some(Token::Keyword(k)) => name.push(k.clone()),
				Some(Token::Composed(n, _)) => name.extend(n.iter().cloned()),
				_ => return None
			}
		}

		Some(name)
	}

	pub fn format<'g>(&self, grammar: &'g Grammar) -> Formatted<'g, '_> {
		Formatted(grammar, self)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		let mut string = String::new();

		for atom in self.atoms() {
			string.extend(atom.instance(grammar).chars())
		}

		string
	}
}

impl fmt::Display for RegExp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use itertools::Itertools;
		self.0.iter().format(" ").fmt(f)
	}
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Atom {
	/// Defined regexp reference.
	/// 
	/// The first parameter is the index of the regexp definition in the grammar.
	Ref(u32),
	CharSet(CharSet),
	Literal(String, bool),
	Repeat(Box<Atom>, usize, usize),
	Or(Vec<RegExp>),
	Group(RegExp),
}

impl Atom {
	pub fn as_reference(&self) -> Option<u32> {
		match self {
			Self::Ref(i) => Some(*i),
			Self::Group(g) => g.as_reference(),
			_ => None
		}
	}

	pub fn token(&self, grammar: &Grammar) -> Option<Token> {
		 match self {
			Self::Ref(i) => {
				let exp = grammar.regexp(*i).unwrap();
				let ty = grammar.extern_type(exp.ty).unwrap();
				Some(Token::Named(exp.id.clone(), token::Convertion::new_opt(&exp.id, ty)))
			},
			Self::CharSet(set) => {
				if set.len() == 1usize {
					char_as_token(set.first().unwrap())
				} else {
					None
				}
			},
			Self::Literal(string, true) => {
				if string.len() == 1 {
					if let Some(token) = char_as_token(string.chars().next().unwrap()) {
						return Some(token)
					}
				}

				Some(Token::Keyword(string.clone()))
			},
			Self::Literal(string, false) => {
				Some(Token::Keyword(string.clone()))
			},
			Self::Repeat(_, _, _) => None,
			Self::Or(_) => None,
			Self::Group(g) => {
				Some(g.token(grammar).clone())
			},
		}
	}

	pub fn format<'g>(&self, grammar: &'g Grammar) -> FormattedAtom<'g, '_> {
		FormattedAtom(grammar, self)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		match self {
			Self::Ref(i) => {
				grammar.regexp(*i).unwrap().exp.instance(grammar)
			},
			Self::CharSet(set) => {
				set.first().unwrap().to_string()
			},
			Self::Literal(string, _) => {
				string.clone()
			},
			Self::Repeat(atom, min, _) => {
				let mut string = String::new();

				let inner = atom.instance(grammar);
				for _ in 0..*min {
					string.extend(inner.chars())
				}

				string
			},
			Self::Or(exps) => {
				exps.first().unwrap().instance(grammar)
			},
			Self::Group(g) => {
				g.instance(grammar)
			}
		}
	}
}

fn char_as_token(c: char) -> Option<Token> {
	if let Some(o) = token::Operator::from_char(c) {
		Some(Token::Operator(o))
	} else if let Some(p) = token::Punct::from_char(c) {
		Some(Token::Punct(p))
	} else if let Some(b) = token::Delimiter::from_opening_char(c) {
		Some(Token::Begin(b))
	} else if let Some(e) = token::Delimiter::from_closing_char(c) {
		Some(Token::End(e))
	} else {
		None
	}
}

impl fmt::Display for Atom {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Ref(i) => i.fmt(f),
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

pub struct Formatted<'g, 'e>(&'g Grammar, &'e RegExp);

impl<'g, 'e> fmt::Display for Formatted<'g, 'e> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for atom in self.1.atoms() {
			atom.format(self.0).fmt(f)?
		}

		Ok(())
	}
}

pub struct FormattedAtom<'g, 'a>(&'g Grammar, &'a Atom);

impl<'g, 'a> fmt::Display for FormattedAtom<'g, 'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.1 {
			Atom::Ref(i) => {
				self.0.regexp(*i).unwrap().id.fmt(f)
			},
			Atom::CharSet(set) => set.fmt(f),
			Atom::Literal(string, case_sensitive) => {
				if *case_sensitive {
					write!(f, "'{}'", string)
				} else {
					write!(f, "\"{}\"", string)
				}
			},
			Atom::Repeat(atom, min, max) => {
				match (*min, *max) {
					(0, usize::MAX) => write!(f, "{}*", atom),
					(1, usize::MAX) => write!(f, "{}+", atom),
					(0, 1) => write!(f, "{}?", atom),
					_ => unimplemented!()
				}
			},
			Atom::Or(exps) => {
				use itertools::Itertools;
				exps.iter().format(" | ").fmt(f)
			},
			Atom::Group(exp) => {
				write!(f, "({})", exp)
			}
		}
	}
}