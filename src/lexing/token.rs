use crate::Ident;

mod delimiter;
mod operator;
mod punct;

pub use delimiter::*;
pub use operator::*;
pub use punct::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Token classes.
pub enum Class {
	Anonymous(u32, Option<u32>),
	Named(Ident, Option<u32>),
	// Composed(Vec<String>, Option<Convertion>),
	Keyword,
	Operator,
	Punct,
	Begin,
	End,
}

impl Class {
	pub fn has_parameter(&self) -> bool {
		match self {
			Self::Anonymous(_, c) | Self::Named(_, c) => c.is_some(),
			_ => true,
		}
	}
}

/// Lexing tokens.
#[derive(Clone)]
pub enum Token {
	Anonymous(u32, Option<u32>),
	Named(Ident, Option<u32>),
	// Composed(Vec<String>, Option<Convertion>),
	Keyword(String),
	Operator(Operator),
	Punct(Punct),
	Begin(Delimiter),
	End(Delimiter),
}

impl Token {
	pub fn parameter_type(&self) -> Option<u32> {
		match self {
			Self::Anonymous(_, c) | Self::Named(_, c) => *c,
			_ => None,
		}
	}

	pub fn has_parameter(&self) -> bool {
		match self {
			Self::Anonymous(_, c) | Self::Named(_, c) => c.is_some(),
			_ => false,
		}
	}

	pub fn class(&self) -> Class {
		match self {
			Token::Anonymous(i, ty) => Class::Anonymous(*i, ty.clone()),
			Token::Named(id, ty) => Class::Named(id.clone(), ty.clone()),
			Token::Keyword(_) => Class::Keyword,
			Token::Operator(_) => Class::Operator,
			Token::Punct(_) => Class::Punct,
			Token::Begin(_) => Class::Begin,
			Token::End(_) => Class::End,
		}
	}
}
