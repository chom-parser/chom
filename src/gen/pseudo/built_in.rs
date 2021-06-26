use std::collections::HashMap;
use crate::{
	mono,
	Ident,
	lexing::Token
};
use super::{
	ty::{
		self,
		Enum,
		VariantDesc
	},
	Pattern
};

pub use crate::lexing::token::{
	Delimiter,
	Operator,
	Punct
};

/// Built-in types.
pub struct Types {
	pub tokens: super::Type,
	pub keywords: Option<super::Type>,
	pub operators: Option<super::Type>,
	pub delimiters: Option<super::Type>,
	pub puncts: Option<super::Type>
}

impl Types {
	pub fn new(grammar: &mono::Grammar, lexer_module: u32, grammar_extern_type: &HashMap<u32, u32>) -> (Self, HashMap<u32, Pattern>) {
		let mut tokens = Enum::new();
		let mut token_keyword_variant = None;
		let mut token_begin_variant = None;
		let mut token_end_variant = None;
		let mut token_operator_variant = None;
		let mut token_punct_variant = None;

		let mut keywords = Enum::new();
		let mut delimiters = Enum::new();
		let mut operators = Enum::new();
		let mut puncts = Enum::new();

		let mut grammar_terminal = HashMap::new();

		for (index, (terminal, _)) in grammar.terminals().iter().enumerate() {
			let index = index as u32;
			if let Some(token) = terminal.token(grammar.poly()) {
				let ty = token.parameter_type().map(|t| grammar_extern_type.get(&t).cloned()).flatten().map(ty::Expr::Defined);
				let payload = ty.as_ref().map(|_| Box::new(Pattern::Payload));
				let desc = match ty {
					Some(ty) => VariantDesc::Tuple(vec![ty]),
					None => VariantDesc::Empty
				};
				let pattern = match token {
					Token::Named(id, _) => {
						let v = tokens.add_variant(ty::Variant::Defined(id, desc));
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, payload)
					},
					Token::Anonymous(i, _) => {
						let id = Ident::new(format!("token{}", i)).unwrap();
						let v = tokens.add_variant(ty::Variant::Defined(id, desc));
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, payload)
					},
					Token::Keyword(k) => {
						let id = Ident::new(k).unwrap();
						let kv = keywords.add_variant(ty::Variant::BuiltIn(Variant::Keyword(id)));
						let v = *token_keyword_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Keyword)))
						});
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, Some(
							Box::new(Pattern::Variant(ty::Ref::BuiltIn(Type::Keyword), kv, None))
						))
					},
					Token::Begin(d) => {
						let dv = delimiters.add_variant(ty::Variant::BuiltIn(Variant::Delimiter(d)));
						let v = *token_begin_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Begin)))
						});
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, Some(
							Box::new(Pattern::Variant(ty::Ref::BuiltIn(Type::Delimiter), dv, None))
						))
					}
					Token::End(d) => {
						let dv = delimiters.add_variant(ty::Variant::BuiltIn(Variant::Delimiter(d)));
						let v = *token_end_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::End)))
						});
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, Some(
							Box::new(Pattern::Variant(ty::Ref::BuiltIn(Type::Delimiter), dv, None))
						))
					},
					Token::Operator(o) => {
						let ov = operators.add_variant(ty::Variant::BuiltIn(Variant::Operator(o)));
						let v = *token_operator_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Operator)))
						});
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, Some(
							Box::new(Pattern::Variant(ty::Ref::BuiltIn(Type::Operator), ov, None))
						))
					},
					Token::Punct(p) => {
						let pv = puncts.add_variant(ty::Variant::BuiltIn(Variant::Punct(p)));
						let v = *token_punct_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Punct)))
						});
						Pattern::Variant(ty::Ref::BuiltIn(Type::Token), v, Some(
							Box::new(Pattern::Variant(ty::Ref::BuiltIn(Type::Punct), pv, None))
						))
					}
				};

				grammar_terminal.insert(index, pattern);
			}
		}

		let built_in = Self {
			tokens: super::Type::new(lexer_module, Ident::new("Token").unwrap(), None, ty::Desc::Enum(tokens)),
			keywords: keywords.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Keyword").unwrap(), None, ty::Desc::Enum(e))),
			operators: operators.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Operator").unwrap(), None, ty::Desc::Enum(e))),
			delimiters: delimiters.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Delimiter").unwrap(), None, ty::Desc::Enum(e))),
			puncts: puncts.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Punct").unwrap(), None, ty::Desc::Enum(e)))
		};

		(built_in, grammar_terminal)
	}
}

/// Built-in types.
pub enum Type {
	/// Tokens type.
	Token,

	/// Keywords enumerator.
	Keyword,

	/// Operators enumerator.
	Operator,

	/// Delimiters enumerator.
	Delimiter,

	/// Punctuation enumerator.
	Punct
}

pub enum Variant {
	/// Built-in token variant.
	Token(TokenVariant),

	/// Keyword.
	Keyword(Ident),

	/// Delimiter.
	Delimiter(Delimiter),

	/// Operator.
	Operator(Operator),

	/// Punctuation.
	Punct(Punct)
}

impl Variant {
	pub fn parameter(&self) -> Option<&ty::Expr> {
		match self {
			Self::Token(t) => Some(t.parameter()),
			Self::Keyword(_) => None,
			Self::Delimiter(_) => None,
			Self::Operator(_) => None,
			Self::Punct(_) => None
		}
	}
}

pub enum TokenVariant {
	/// Keyword token variant.
	Keyword,

	/// Operator token variant.
	Operator,

	/// Group begin token variant.
	Begin,

	/// Group end token variant.
	End,

	/// Punctuation char token variant.
	Punct
}

impl TokenVariant {
	pub fn parameter(&self) -> &ty::Expr {
		match self {
			Self::Keyword => &ty::Expr::BuiltIn(Type::Keyword),
			Self::Operator => &ty::Expr::BuiltIn(Type::Operator),
			Self::Begin => &ty::Expr::BuiltIn(Type::Delimiter),
			Self::End => &ty::Expr::BuiltIn(Type::Delimiter),
			Self::Punct => &ty::Expr::BuiltIn(Type::Punct)
		}
	}
}