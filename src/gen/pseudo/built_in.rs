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
	Pattern,
	Id,
	Expr
};

pub use crate::lexing::token::{
	Delimiter,
	Operator,
	Punct
};

/// Built-in types.
pub struct Types {
	/// Enum type of tokens.
	/// 
	/// Defined in the lexer.
	pub tokens: super::Type,

	/// Enum type of keyword tokens.
	pub keywords: Option<super::Type>,
	
	/// Enum type operator tokens.
	pub operators: Option<super::Type>,

	/// Enum type of delimiter tokens.
	pub delimiters: Option<super::Type>,
	
	/// Enum type of punctuation tokens.
	pub puncts: Option<super::Type>,

	/// Enum type of AST nodes (types/non terminals).
	/// 
	/// Defined in the parser.
	pub nodes: super::Type,

	/// Enum type of tokens or AST nodes.
	/// 
	/// Defined in the parser.
	pub items: super::Type,

	/// Maps each grammar terminal to a token pattern.
	tokens_patterns: HashMap<u32, Pattern>,

	/// Maps each grammar type to a variant index in `nodes`.
	nodes_variants: HashMap<mono::Index, u32>,
}

impl Types {
	pub fn token_count(&self) -> usize {
		self.tokens_patterns.len()
	}

	pub fn token_pattern<F>(&self, index: u32, f: F) -> Pattern where F: FnMut() -> Id {
		let pattern = self.tokens_patterns.get(&index).unwrap();
		pattern.bind_any(f)
	}

	pub fn token_expr<F>(&self, index: u32, f: F) -> Expr where F: FnMut() -> Expr {
		let pattern = self.tokens_patterns.get(&index).unwrap();
		pattern.as_expr(|_| f())
	}

	pub fn node_pattern(&self, index: mono::Index, id: Id) -> Pattern {
		Pattern::Cons(
			ty::Ref::BuiltIn(Type::Node),
			*self.nodes_variants.get(&index).unwrap(),
			vec![Pattern::Bind(id)]
		)
	}

	pub fn node_expr(&self, index: mono::Index, e: Expr) -> Expr {
		Expr::Cons(
			ty::Ref::BuiltIn(Type::Node),
			*self.nodes_variants.get(&index).unwrap(),
			vec![e]
		)
	}

	pub fn item_token_pattern<F>(&self, index: u32, f: F) -> Pattern where F: FnMut() -> Id {
		Pattern::Cons(
			ty::Ref::BuiltIn(Type::Item),
			0,
			vec![self.token_pattern(index, f)]
		)
	}

	pub fn item_token_expr<F>(&self, index: u32, f: F) -> Expr where F: FnMut() -> Expr {
		Expr::Cons(
			ty::Ref::BuiltIn(Type::Item),
			0,
			vec![self.token_expr(index, f)]
		)
	}

	pub fn item_node_pattern(&self, index: mono::Index, id: Id) -> Pattern {
		Pattern::Cons(
			ty::Ref::BuiltIn(Type::Item),
			1,
			vec![self.node_pattern(index, id)]
		)
	}

	pub fn item_node_expr(&self, index: mono::Index, e: Expr) -> Expr {
		Expr::Cons(
			ty::Ref::BuiltIn(Type::Item),
			1,
			vec![self.node_expr(index, e)]
		)
	}

	pub fn new(
		grammar: &mono::Grammar,
		lexer_module: u32,
		parser_module: u32,
		grammar_extern_type: &HashMap<u32, u32>
	) -> Self {
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

		let mut tokens_patterns = HashMap::new();

		for (index, (terminal, _)) in grammar.terminals().iter().enumerate() {
			let index = index as u32;
			if let Some(token) = terminal.token(grammar.poly()) {
				let ty = token.parameter_type().map(|t| grammar_extern_type.get(&t).cloned()).flatten().map(|i| ty::Expr::Defined(i, Vec::new()));
				let desc = match ty {
					Some(ty) => VariantDesc::Tuple(vec![ty]),
					None => VariantDesc::Tuple(Vec::new())
				};
				let pattern = match token {
					Token::Named(id, ty) => {
						let v = tokens.add_variant(ty::Variant::Defined(id, desc));
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, if ty.is_some() { vec![Pattern::Any] } else { Vec::new() })
					},
					Token::Anonymous(i, ty) => {
						let id = Ident::new(format!("token{}", i)).unwrap();
						let v = tokens.add_variant(ty::Variant::Defined(id, desc));
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, if ty.is_some() { vec![Pattern::Any] } else { Vec::new() })
					},
					Token::Keyword(k) => {
						let id = Ident::new(k).unwrap();
						let kv = keywords.add_variant(ty::Variant::BuiltIn(Variant::Keyword(id)));
						let v = *token_keyword_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Keyword)))
						});
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, vec![
							Pattern::Cons(ty::Ref::BuiltIn(Type::Keyword), kv, Vec::new())
						])
					},
					Token::Begin(d) => {
						let dv = delimiters.add_variant(ty::Variant::BuiltIn(Variant::Delimiter(d)));
						let v = *token_begin_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Begin)))
						});
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, vec![
							Pattern::Cons(ty::Ref::BuiltIn(Type::Delimiter), dv, Vec::new())
						])
					}
					Token::End(d) => {
						let dv = delimiters.add_variant(ty::Variant::BuiltIn(Variant::Delimiter(d)));
						let v = *token_end_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::End)))
						});
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, vec![
							Pattern::Cons(ty::Ref::BuiltIn(Type::Delimiter), dv, Vec::new())
						])
					},
					Token::Operator(o) => {
						let ov = operators.add_variant(ty::Variant::BuiltIn(Variant::Operator(o)));
						let v = *token_operator_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Operator)))
						});
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, vec![
							Pattern::Cons(ty::Ref::BuiltIn(Type::Operator), ov, Vec::new())
						])
					},
					Token::Punct(p) => {
						let pv = puncts.add_variant(ty::Variant::BuiltIn(Variant::Punct(p)));
						let v = *token_punct_variant.get_or_insert_with(|| {
							tokens.add_variant(ty::Variant::BuiltIn(Variant::Token(TokenVariant::Punct)))
						});
						Pattern::Cons(ty::Ref::BuiltIn(Type::Token), v, vec![
							Pattern::Cons(ty::Ref::BuiltIn(Type::Punct), pv, Vec::new())
						])
					}
				};

				tokens_patterns.insert(index, pattern);
			}
		}

		let mut nodes = Enum::new();
		let mut nodes_variants = HashMap::new();
		for (index, ty) in grammar.enumerate_types() {
			panic!("TODO")
		}

		let mut items = Enum::new();
		items.add_variant(ty::Variant::BuiltIn(Variant::Item(ItemVariant::Token)));
		items.add_variant(ty::Variant::BuiltIn(Variant::Item(ItemVariant::Node)));

		Self {
			tokens: super::Type::new(lexer_module, Ident::new("Token").unwrap(), None, ty::Desc::Enum(tokens)),
			keywords: keywords.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Keyword").unwrap(), None, ty::Desc::Enum(e))),
			operators: operators.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Operator").unwrap(), None, ty::Desc::Enum(e))),
			delimiters: delimiters.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Delimiter").unwrap(), None, ty::Desc::Enum(e))),
			puncts: puncts.not_empty().map(|e| super::Type::new(lexer_module, Ident::new("Punct").unwrap(), None, ty::Desc::Enum(e))),
			nodes: super::Type::new(parser_module, Ident::new("Node").unwrap(), None, ty::Desc::Enum(nodes)),
			items: super::Type::new(parser_module, Ident::new("Item").unwrap(), None, ty::Desc::Enum(items)),
			tokens_patterns,
			nodes_variants
		}
	}
}

/// Built-in types.
#[derive(Clone, Copy)]
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
	Punct,

	/// Node enumerator.
	Node,

	/// Token or node.
	Item
}

#[derive(Clone)]
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
	Punct(Punct),

	/// Node.
	Node,

	/// Item variant.
	Item(ItemVariant)
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

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
pub enum ItemVariant {
	Token,
	Node
}

impl ItemVariant {
	pub fn parameter(&self) -> &ty::Expr {
		match self {
			Self::Token => &ty::Expr::BuiltIn(Type::Token),
			Self::Node => &ty::Expr::BuiltIn(Type::Node)
		}
	}
}