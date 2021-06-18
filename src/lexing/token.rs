use crate::{
	syntax::Ident,
	lexing::RegExp,
	poly::{
		Grammar,
		ExternalType,
	}
};

mod operator;
mod punct;
mod delimiter;

pub use operator::*;
pub use punct::*;
pub use delimiter::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Convertion {
	pub target: ExternalType,
	pub from: Ident
}

impl Convertion {
	pub fn new_opt(from: &Ident, target: &ExternalType) -> Option<Self> {
		if *target == ExternalType::Unit {
			None
		} else {
			Some(Self {
				target: target.clone(),
				from: from.clone()
			})
		}
	}

	pub fn from_regexp(grammar: &Grammar, e: &RegExp) -> Option<Self> {
		e.as_reference().map(|i| {
			let exp = grammar.regexp(i).unwrap();
			let ty = grammar.extern_type(exp.ty).unwrap();
			Self::new_opt(&exp.id, ty)
		}).flatten()
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
	End
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
	End(Delimiter)
}

impl Token {
	pub fn conversion(&self) -> Option<&Convertion> {
		match self {
			Token::Anonymous(_, c) => c.as_ref(),
			Token::Named(_, c) => c.as_ref(),
			Token::Composed(_, c) => c.as_ref(),
			_ => None
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
			Token::End(_) => Class::End
		}
	}
}