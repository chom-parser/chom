use crate::{
	lexing::RegExp,
	poly::{ExternalType, Grammar},
	syntax::Ident,
};

mod delimiter;
mod operator;
mod punct;

pub use delimiter::*;
pub use operator::*;
pub use punct::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Convertion {
	pub target: u32,
	pub from: Ident,
}

impl Convertion {
	pub fn new_opt(grammar: &Grammar, from: &Ident, target: u32) -> Option<Self> {
		let ty = grammar.extern_type(target).unwrap();
		if *ty == ExternalType::Unit {
			None
		} else {
			Some(Self {
				target,
				from: from.clone(),
			})
		}
	}

	pub fn from_regexp(grammar: &Grammar, e: &RegExp) -> Option<Self> {
		e.as_reference()
			.map(|i| {
				let exp = grammar.regexp(i).unwrap();
				Self::new_opt(grammar, &exp.id, exp.ty)
			})
			.flatten()
	}
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Token classes.
pub enum Class {
	Anonymous(u32, Option<Convertion>),
	Named(Ident, Option<Convertion>),
	Composed(Vec<String>, Option<Convertion>),
	Keyword,
	Operator,
	Punct,
	Begin,
	End,
}

impl Class {
	pub fn has_parameter(&self) -> bool {
		match self {
			Self::Anonymous(_, c) | Self::Named(_, c) | Self::Composed(_, c) => c.is_some(),
			_ => true,
		}
	}
}

/// Lexing tokens.
#[derive(Clone)]
pub enum Token {
	Anonymous(u32, Option<Convertion>),
	Named(Ident, Option<Convertion>),
	Composed(Vec<String>, Option<Convertion>),
	Keyword(String),
	Operator(Operator),
	Punct(Punct),
	Begin(Delimiter),
	End(Delimiter),
}

impl Token {
	pub fn conversion(&self) -> Option<&Convertion> {
		match self {
			Self::Anonymous(_, c) | Self::Named(_, c) | Self::Composed(_, c) => c.as_ref(),
			_ => None,
		}
	}

	pub fn has_parameter(&self) -> bool {
		match self {
			Self::Anonymous(_, c) | Self::Named(_, c) | Self::Composed(_, c) => c.is_some(),
			_ => false,
		}
	}

	pub fn class(&self) -> Class {
		match self {
			Token::Anonymous(i, ty) => Class::Anonymous(*i, ty.clone()),
			Token::Named(id, ty) => Class::Named(id.clone(), ty.clone()),
			Token::Composed(v, ty) => Class::Composed(v.clone(), ty.clone()),
			Token::Keyword(_) => Class::Keyword,
			Token::Operator(_) => Class::Operator,
			Token::Punct(_) => Class::Punct,
			Token::Begin(_) => Class::Begin,
			Token::End(_) => Class::End,
		}
	}
}
