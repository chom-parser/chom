use super::{Expr, FunctionId, Namespace, Pattern, TypeExpr, TypeId};
use crate::{
	gen::{id, Id},
	lexing::Token,
	mono, Ident,
};
use chom_ir::{
	function,
	ty::{self, Enum, VariantDesc},
	Function,
	expr::SpanExpr
};
use std::collections::{hash_map::Entry, HashMap};

pub use crate::lexing::token::{Delimiter, Operator, Punct};

struct TokenData<'a, 'p> {
	pattern: Pattern<'a, 'p>,
	parsing_function: Option<u32>,
}

/// Provided types.
///
/// Stores the IR type index of each provided type.
pub struct Types<'a, 'p> {
	/// Enum type of tokens.
	///
	/// Defined in the lexer.
	pub token_ty: u32,

	/// Enum type of keyword tokens.
	pub keyword_ty: Option<u32>,

	/// Enum type operator tokens.
	pub operator_ty: Option<u32>,

	/// Enum type of delimiter tokens.
	pub delimiter_ty: Option<u32>,

	/// Enum type of punctuation tokens.
	pub punct_ty: Option<u32>,

	/// Maps each grammar terminal to a token pattern.
	tokens_data: HashMap<u32, TokenData<'a, 'p>>,

	/// Index of the function used to locate an lexing
	/// error.
	pub locate_lexer_err_function: Option<u32>,

	/// Enum type of AST nodes (types/non terminals).
	///
	/// Defined in the parser.
	pub node_ty: u32,

	/// Maps each grammar type to a variant index in `nodes_variants`.
	nodes_variants_map: HashMap<mono::Index, u32>,

	/// Parsing error type.
	pub error_ty: u32,

	/// Enum type of tokens or AST nodes.
	///
	/// Defined in the parser.
	pub item_ty: u32,

	/// Lexer type.
	pub lexer_ty: u32,

	/// Index of the function used to convert a
	/// lexing error into a parsing error.
	pub lexer_to_parser_err_function: u32,
}

impl<'a, 'p> Types<'a, 'p> {
	pub fn lexer_type(&self) -> ty::Ref {
		ty::Ref::Defined(self.lexer_ty)
	}

	pub fn token_count(&self) -> usize {
		self.tokens_data.len()
	}

	pub fn token_type(&self) -> ty::Ref {
		ty::Ref::Defined(self.token_ty)
	}

	pub fn token_type_expr(&self) -> TypeExpr<'a, 'p> {
		ty::Expr::Instance(self.token_type(), Vec::new())
	}

	pub fn token_pattern<F>(&self, index: u32, f: F) -> Pattern<'a, 'p>
	where
		F: Copy + Fn() -> Id,
	{
		let pattern = &self.tokens_data.get(&index).unwrap().pattern;
		pattern.bind_any(f)
	}

	/// Returns the index of the extern parsing function associated
	/// to the given grammar terminal.
	pub fn token_parsing_function(&self, index: u32) -> Option<u32> {
		let data = self.tokens_data.get(&index).unwrap();
		data.parsing_function
	}

	/// Generate an expression building a token corresponding to the given grammar terminal.
	///
	/// If the token takes a value, the input function is called
	/// with the index of the token's parsing function declared in the IR.
	pub fn token_expr<F>(&self, index: u32, f: F) -> Expr<'a, 'p>
	where
		F: Copy + Fn(u32) -> Expr<'a, 'p>,
	{
		let data = self.tokens_data.get(&index).unwrap();
		data.pattern.as_expr(|_| f(data.parsing_function.unwrap()))
	}

	pub fn locate_lexer_err_function(&self) -> Option<u32> {
		self.locate_lexer_err_function
	}

	pub fn node_pattern(&self, index: mono::Index, id: Id) -> Pattern<'a, 'p> {
		Pattern::Cons(
			ty::Ref::Defined(self.node_ty),
			*self.nodes_variants_map.get(&index).unwrap(),
			vec![Pattern::Bind(id)],
		)
	}

	pub fn node_expr(&self, index: mono::Index, e: Expr<'a, 'p>) -> Expr<'a, 'p> {
		Expr::Cons(
			ty::Ref::Defined(self.node_ty),
			*self.nodes_variants_map.get(&index).unwrap(),
			vec![e],
		)
	}

	/// Generate an expression building a token corresponding to the given grammar terminal,
	/// inside a `Item::Token` variant.
	///
	/// If the token takes a value, the input function is called
	/// with the index of the token's parsing function declared in the IR.
	pub fn item_token_pattern<F>(&self, index: u32, f: F) -> Pattern<'a, 'p>
	where
		F: Copy + Fn() -> Id,
	{
		Pattern::Cons(
			ty::Ref::Defined(self.item_ty),
			0,
			vec![self.token_pattern(index, f)],
		)
	}

	pub fn item_token_expr<F>(&self, index: u32, f: F) -> Expr<'a, 'p>
	where
		F: Copy + Fn(u32) -> Expr<'a, 'p>,
	{
		Expr::Cons(
			ty::Ref::Defined(self.item_ty),
			0,
			vec![self.token_expr(index, f)],
		)
	}

	pub fn item_node_pattern(&self, index: mono::Index, id: Id) -> Pattern<'a, 'p> {
		Pattern::Cons(
			ty::Ref::Defined(self.item_ty),
			1,
			vec![self.node_pattern(index, id)],
		)
	}

	pub fn item_node_expr(&self, index: mono::Index, e: Expr<'a, 'p>) -> Expr<'a, 'p> {
		Expr::Cons(
			ty::Ref::Defined(self.item_ty),
			1,
			vec![self.node_expr(index, e)],
		)
	}

	pub fn error_type_expr(&self) -> TypeExpr<'a, 'p> {
		TypeExpr::Instance(ty::Ref::Defined(self.error_ty), Vec::new())
	}

	pub fn unexpected_token_expr(&self, e: Expr<'a, 'p>) -> Expr<'a, 'p> {
		Expr::Cons(
			ty::Ref::Defined(self.error_ty),
			1,
			vec![e]
		)
	}

	pub fn unexpected_node_expr(&self, e: Expr<'a, 'p>) -> Expr<'a, 'p> {
		Expr::Cons(
			ty::Ref::Defined(self.error_ty),
			2,
			vec![e]
		)
	}

	pub fn lexer_to_parser_err_function(&self) -> u32 {
		self.lexer_to_parser_err_function
	}

	/// The creates all the provided types in the intermediate representation.
	pub fn new(
		config: &crate::gen::Config,
		ir: &mut chom_ir::Context<Namespace<'a, 'p>>,
		extern_module: u32,
		extern_error_ty: u32,
		lexer_module: u32,
		parser_module: u32,
		grammar_extern_type: &HashMap<u32, u32>,
		grammar_type: &HashMap<u32, u32>,
	) -> Self {
		let grammar = ir.id().grammar();

		let token_ty = ir.add_type(super::Type::new(
			lexer_module,
			TypeId::Provided(Type::Token),
			ty::Desc::Opaque,
		));
		let mut keyword_ty: Option<(u32, u32)> = None;
		let mut delimiter_ty: Option<u32> = None;
		let mut delimiter_variant: HashMap<Delimiter, u32> = HashMap::new();
		let mut token_begin_variant: Option<u32> = None;
		let mut token_end_variant: Option<u32> = None;
		let mut operator_ty: Option<(u32, u32)> = None;
		let mut punct_ty: Option<(u32, u32)> = None;

		// let mut named_variants = Vec::new();
		// let mut keyword_variants = Vec::new();

		let mut tokens = Enum::new();
		let mut keywords = Enum::new();
		let mut delimiters = Enum::new();
		let mut operators = Enum::new();
		let mut puncts = Enum::new();

		let mut tokens_data = HashMap::new();

		for (index, (terminal, _)) in grammar.terminals().iter().enumerate() {
			let index = index as u32;
			if let Some(token) = terminal.token(grammar.poly()) {
				// Create the pattern input descriptor
				// and token parsing function (if any).
				let (desc, parsing_function) = match token.parameter_type() {
					Some(extern_ty) => {
						let ir_ty = grammar_extern_type.get(&extern_ty).unwrap().clone();
						let ty_expr = TypeExpr::Instance(ty::Ref::Defined(ir_ty), Vec::new());
						let desc = VariantDesc::Tuple(vec![ty_expr.clone()]);
						let ir_function = ir.add_function(Function::new(
							function::Owner::Module(extern_module),
							FunctionId::ExternParser(extern_ty),
							function::Signature::extern_parser(
								Id::Extern(id::Extern::String),
								ty_expr,
								ty::Expr::Instance(ty::Ref::Defined(extern_error_ty), Vec::new()),
							),
							None,
						));
						(desc, Some(ir_function))
					}
					None => (VariantDesc::empty(), None),
				};
				let pattern = match token {
					Token::Named(id, ty) => {
						// Register the token name.
						let index = ir.id_mut().new_named_variant(id.clone());

						// Create the `Token::Name` variant.
						let v = tokens.add_variant(ty::Variant::Defined(
							super::VariantId::Provided(Variant::Token(TokenVariant::Named(index))),
							desc,
						));

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							v,
							if ty.is_some() {
								vec![Pattern::Any]
							} else {
								Vec::new()
							},
						)
					}
					Token::Anonymous(i, ty) => {
						// Register the token name.
						let index = ir
							.id_mut()
							.new_named_variant(Ident::new(format!("token{}", i)).unwrap());

						// Create the `Token::Name` variant.
						let v = tokens.add_variant(ty::Variant::Defined(
							super::VariantId::Provided(Variant::Token(TokenVariant::Named(index))),
							desc,
						));

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							v,
							if ty.is_some() {
								vec![Pattern::Any]
							} else {
								Vec::new()
							},
						)
					}
					Token::Keyword(k) => {
						// Create the `Keyword` type is not already done,
						// along with the `Token::Keyword` variant.
						let (keyword_ty, token_keyword_variant) =
							*keyword_ty.get_or_insert_with(|| {
								let ir_ty = ir.add_type(super::Type::new(
									lexer_module,
									super::TypeId::Provided(Type::Keyword),
									ty::Desc::Opaque,
								));
								let variant = ty::Variant::Defined(
									super::VariantId::Provided(Variant::Token(
										TokenVariant::Keyword,
									)),
									VariantDesc::Tuple(vec![TypeExpr::Instance(
										ty::Ref::Defined(ir_ty),
										vec![],
									)]),
								);
								(ir_ty, tokens.add_variant(variant))
							});

						// Register the keyword.
						let index = ir.id_mut().new_keyword_variant(Ident::new(k).unwrap());

						// Create the keyword variant.
						let id = super::VariantId::Provided(Variant::Keyword(index));
						let variant = ty::Variant::Defined(id, desc);
						let keyword_variant = keywords.add_variant(variant);

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							token_keyword_variant,
							vec![Pattern::Cons(
								ty::Ref::Defined(keyword_ty),
								keyword_variant,
								Vec::new(),
							)],
						)
					}
					Token::Begin(d) => {
						// Create the `Delimiter` type is not already done.
						let delimiter_ty = *delimiter_ty.get_or_insert_with(|| {
							ir.add_type(super::Type::new(
								lexer_module,
								super::TypeId::Provided(Type::Delimiter),
								ty::Desc::Opaque,
							))
						});

						// Create the `Delimiter::Begin` variant if not already done.
						let token_begin_variant = *token_begin_variant.get_or_insert_with(|| {
							let variant = ty::Variant::Defined(
								super::VariantId::Provided(Variant::Token(TokenVariant::Begin)),
								VariantDesc::Tuple(vec![TypeExpr::Instance(
									ty::Ref::Defined(delimiter_ty),
									vec![],
								)]),
							);
							tokens.add_variant(variant)
						});

						// Create the delimiter variant if not already done.
						let delimiter_variant = match delimiter_variant.entry(d) {
							Entry::Occupied(entry) => *entry.get(),
							Entry::Vacant(entry) => {
								let id = super::VariantId::Provided(Variant::Delimiter(d));
								let variant = ty::Variant::Defined(id, desc);
								*entry.insert(delimiters.add_variant(variant))
							}
						};

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							token_begin_variant,
							vec![Pattern::Cons(
								ty::Ref::Defined(delimiter_ty),
								delimiter_variant,
								Vec::new(),
							)],
						)
					}
					Token::End(d) => {
						// Create the `Delimiter` type is not already done.
						let delimiter_ty = *delimiter_ty.get_or_insert_with(|| {
							ir.add_type(super::Type::new(
								lexer_module,
								super::TypeId::Provided(Type::Delimiter),
								ty::Desc::Opaque,
							))
						});

						// Create the `Delimiter::End` variant if not already done.
						let token_end_variant = *token_end_variant.get_or_insert_with(|| {
							let variant = ty::Variant::Defined(
								super::VariantId::Provided(Variant::Token(TokenVariant::End)),
								VariantDesc::Tuple(vec![TypeExpr::Instance(
									ty::Ref::Defined(delimiter_ty),
									vec![],
								)]),
							);
							tokens.add_variant(variant)
						});

						// Create the delimiter variant if not already done.
						let delimiter_variant = match delimiter_variant.entry(d) {
							Entry::Occupied(entry) => *entry.get(),
							Entry::Vacant(entry) => {
								let id = super::VariantId::Provided(Variant::Delimiter(d));
								let variant = ty::Variant::Defined(id, desc);
								*entry.insert(delimiters.add_variant(variant))
							}
						};

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							token_end_variant,
							vec![Pattern::Cons(
								ty::Ref::Defined(delimiter_ty),
								delimiter_variant,
								Vec::new(),
							)],
						)
					}
					Token::Operator(o) => {
						// Create the `Operator` type is not already done,
						// along with the `Token::Operator` variant.
						let (operator_ty, token_operator_variant) = *operator_ty
							.get_or_insert_with(|| {
								let ir_ty = ir.add_type(super::Type::new(
									lexer_module,
									super::TypeId::Provided(Type::Operator),
									ty::Desc::Opaque,
								));
								let variant = ty::Variant::Defined(
									super::VariantId::Provided(Variant::Token(
										TokenVariant::Operator,
									)),
									VariantDesc::Tuple(vec![TypeExpr::Instance(
										ty::Ref::Defined(ir_ty),
										vec![],
									)]),
								);
								(ir_ty, tokens.add_variant(variant))
							});

						// Create the keyword variant.
						let id = super::VariantId::Provided(Variant::Operator(o));
						let variant = ty::Variant::Defined(id, desc);
						let operator_variant = operators.add_variant(variant);

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							token_operator_variant,
							vec![Pattern::Cons(
								ty::Ref::Defined(operator_ty),
								operator_variant,
								Vec::new(),
							)],
						)
					}
					Token::Punct(p) => {
						// Create the `Punct` type is not already done,
						// along with the `Token::Punct` variant.
						let (punct_ty, token_punct_variant) = *punct_ty.get_or_insert_with(|| {
							let ir_ty = ir.add_type(super::Type::new(
								lexer_module,
								super::TypeId::Provided(Type::Punct),
								ty::Desc::Opaque,
							));
							let variant = ty::Variant::Defined(
								super::VariantId::Provided(Variant::Token(TokenVariant::Punct)),
								VariantDesc::Tuple(vec![TypeExpr::Instance(
									ty::Ref::Defined(ir_ty),
									vec![],
								)]),
							);
							(ir_ty, tokens.add_variant(variant))
						});

						// Create the keyword variant.
						let id = super::VariantId::Provided(Variant::Punct(p));
						let variant = ty::Variant::Defined(id, desc);
						let punct_variant = puncts.add_variant(variant);

						// Create the associated pattern.
						Pattern::Cons(
							ty::Ref::Defined(token_ty),
							token_punct_variant,
							vec![Pattern::Cons(
								ty::Ref::Defined(punct_ty),
								punct_variant,
								Vec::new(),
							)],
						)
					}
				};

				tokens_data.insert(
					index,
					TokenData {
						pattern,
						parsing_function,
					},
				);
			}
		}

		fn type_expr<'a, 'p>(
			grammar: &'a mono::Grammar<'p>,
			grammar_extern_type: &HashMap<u32, u32>,
			grammar_type: &HashMap<u32, u32>,
			index: mono::Index,
		) -> TypeExpr<'a, 'p> {
			let ty = grammar.ty(index).unwrap();
			let params = ty
				.parameters()
				.iter()
				.filter_map(|p| match p {
					mono::ty::Expr::Terminal(index) => {
						let t = grammar.terminal(*index).unwrap();
						t.extern_type(grammar.poly()).map(|i| {
							let ty = *grammar_extern_type.get(&i).unwrap();
							ty::Expr::Instance(ty::Ref::Defined(ty), Vec::new())
						})
					}
					mono::ty::Expr::Type(index) => Some(type_expr(
						grammar,
						grammar_extern_type,
						grammar_type,
						*index,
					)),
				})
				.collect();
			let generated_index = *grammar_type.get(&index.0).unwrap();
			ty::Expr::Instance(ty::Ref::Defined(generated_index), params)
		}

		let mut nodes = Enum::new();
		// let mut nodes_variants = Vec::new();
		let mut nodes_variants_map = HashMap::new();
		for (index, ty) in grammar.enumerate_types() {
			// Register node variant.
			let node_variant_index = ir.id_mut().new_node_variant(ty.composed_id(grammar));

			// Create IR type expression.
			let expr = type_expr(grammar, grammar_extern_type, grammar_type, index);

			// Create `Node::Type` variant.
			let id = super::VariantId::Provided(Variant::Node(node_variant_index));
			let variant = ty::Variant::Defined(id, VariantDesc::Tuple(vec![expr]));
			let v = nodes.add_variant(variant);
			nodes_variants_map.insert(index, v);
		}

		let node_ty = ir.add_type(super::Type::new(
			parser_module,
			super::TypeId::Provided(Type::Node),
			ty::Desc::Enum(nodes),
		));

		let mut items = Enum::new();

		// Create the `Item::Token` variant.
		items.add_variant(ty::Variant::Defined(
			super::VariantId::Provided(Variant::Item(ItemVariant::Token)),
			VariantDesc::Tuple(vec![ty::Expr::Instance(
				ty::Ref::Defined(token_ty),
				Vec::new(),
			)]),
		));

		// Create the `Item::Node` variant.
		items.add_variant(ty::Variant::Defined(
			super::VariantId::Provided(Variant::Item(ItemVariant::Node)),
			VariantDesc::Tuple(vec![ty::Expr::Instance(
				ty::Ref::Defined(node_ty),
				Vec::new(),
			)]),
		));

		let item_ty = ir.add_type(super::Type::new(
			parser_module,
			super::TypeId::Provided(Type::Item),
			ty::Desc::Enum(items),
		));

		let mut errors = Enum::new();
		errors.add_variant(ty::Variant::Defined(
			super::VariantId::Provided(Variant::Error(ErrorVariant::Lexer)),
			VariantDesc::Tuple(vec![ty::Expr::Instance(
				ty::Ref::Defined(extern_error_ty),
				Vec::new()
			)])
		));
		errors.add_variant(ty::Variant::Defined(
			super::VariantId::Provided(Variant::Error(ErrorVariant::UnexpectedToken)),
			VariantDesc::Tuple(vec![ty::Expr::option(ty::Expr::Instance(
				ty::Ref::Defined(token_ty),
				Vec::new()
			))])
		));
		errors.add_variant(ty::Variant::Defined(
			super::VariantId::Provided(Variant::Error(ErrorVariant::UnexpectedNode)),
			VariantDesc::Tuple(vec![ty::Expr::Instance(
				ty::Ref::Defined(node_ty),
				Vec::new(),
			)])
		));

		let error_ty = ir.add_type(super::Type::new(
			parser_module,
			super::TypeId::Provided(Type::Error),
			ty::Desc::Enum(errors)
		));

		fn define_type<'a, 'p>(
			ir: &mut chom_ir::Context<Namespace<'a, 'p>>,
			index: Option<u32>,
			enm: ty::Enum<Namespace<'a, 'p>>,
		) -> Option<u32> {
			index.map(|index| {
				ir.ty_mut(ty::Ref::Defined(index))
					.unwrap()
					.set_desc(ty::Desc::Enum(enm));
				index
			})
		}

		ir.ty_mut(ty::Ref::Defined(token_ty))
			.unwrap()
			.set_desc(ty::Desc::Enum(tokens));

		let locate_lexer_err_function = if config.locate {
			Some(ir.add_function(Function::new(
				function::Owner::Module(lexer_module),
				FunctionId::LocateLexerError,
				function::Signature::new(
					vec![
						function::Arg::new(Id::Aux(id::Aux::Span), false, ty::Expr::span()),
						function::Arg::new(Id::Aux(id::Aux::Error), false, ty::Expr::Instance(ty::Ref::Defined(extern_error_ty), Vec::new()))
					],
					ty::Expr::locate(ty::Expr::Instance(ty::Ref::Defined(extern_error_ty), Vec::new()))
				),
				Some(Expr::locate(
					Expr::Get(Id::Aux(id::Aux::Error)),
					Expr::Get(Id::Aux(id::Aux::Span))
				))
			)))
		} else {
			None
		};

		let lexer_to_parser_err_function = ir.add_function(Function::new(
			function::Owner::Module(parser_module),
			FunctionId::LexerToParserError,
			function::Signature::new(
				vec![
					function::Arg::new(Id::Aux(id::Aux::Error), false, config.loc_type(ty::Expr::Instance(ty::Ref::Defined(extern_error_ty), Vec::new())))
				],
				config.loc_type(ty::Expr::Instance(ty::Ref::Defined(error_ty), Vec::new()))
			),
			Some(if config.locate {
				Expr::Span(SpanExpr::Unwrap(
					Some(Id::Aux(id::Aux::SpanlessError)),
					Some(Id::Aux(id::Aux::Span)),
					Box::new(Expr::Get(Id::Aux(id::Aux::Error))),
					Box::new(Expr::locate(
						Expr::Cons(
							ty::Ref::Defined(error_ty),
							0,
							vec![Expr::Get(Id::Aux(id::Aux::SpanlessError))]
						),
						Expr::Get(Id::Aux(id::Aux::Span))
					))
				))
			} else {
				Expr::Cons(
					ty::Ref::Defined(error_ty),
					0,
					vec![Expr::Get(Id::Aux(id::Aux::Error))]
				)
			})
		));

		Self {
			token_ty,
			keyword_ty: define_type(ir, keyword_ty.map(|(i, _)| i), keywords),
			operator_ty: define_type(ir, operator_ty.map(|(i, _)| i), operators),
			delimiter_ty: define_type(ir, delimiter_ty, delimiters),
			punct_ty: define_type(ir, punct_ty.map(|(i, _)| i), puncts),
			tokens_data,
			locate_lexer_err_function,
			error_ty,
			node_ty,
			nodes_variants_map,
			item_ty,
			lexer_ty: ir.add_type(super::Type::new(
				lexer_module,
				TypeId::Provided(Type::Lexer),
				ty::Desc::Lexer,
			)),
			lexer_to_parser_err_function
		}
	}
}

/// Built-in types.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
	/// Lexer type.
	Lexer,

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
	Item,

	/// Parsing error.
	Error
}

impl Type {
	pub fn ident(&self) -> Ident {
		match self {
			Self::Lexer => Ident::new("Lexer").unwrap(),
			Self::Token => Ident::new("Token").unwrap(),
			Self::Keyword => Ident::new("Keyword").unwrap(),
			Self::Operator => Ident::new("Operator").unwrap(),
			Self::Delimiter => Ident::new("Delimiter").unwrap(),
			Self::Punct => Ident::new("Punct").unwrap(),
			Self::Node => Ident::new("Node").unwrap(),
			Self::Item => Ident::new("Item").unwrap(),
			Self::Error => Ident::new("Error").unwrap(),
		}
	}
}

#[derive(Clone, Copy)]
pub enum Variant {
	/// Built-in token variant.
	Token(TokenVariant),

	/// Keyword.
	Keyword(u32),

	/// Delimiter.
	Delimiter(Delimiter),

	/// Operator.
	Operator(Operator),

	/// Punctuation.
	Punct(Punct),

	/// Node.
	Node(u32),

	/// Item variant.
	Item(ItemVariant),

	/// Parsing error variant.
	Error(ErrorVariant),
}

#[derive(Clone, Copy)]
pub enum TokenVariant {
	/// Named token variant.
	Named(u32),

	/// Keyword token variant.
	Keyword,

	/// Operator token variant.
	Operator,

	/// Group begin token variant.
	Begin,

	/// Group end token variant.
	End,

	/// Punctuation char token variant.
	Punct,
}

#[derive(Clone, Copy)]
pub enum ItemVariant {
	Token,
	Node,
}

impl ItemVariant {
	pub fn ident(&self) -> Ident {
		match self {
			Self::Token => Ident::new("Token").unwrap(),
			Self::Node => Ident::new("Node").unwrap(),
		}
	}
}

#[derive(Clone, Copy)]
pub enum ErrorVariant {
	Lexer,
	UnexpectedToken,
	UnexpectedNode,
}

impl ErrorVariant {
	pub fn ident(&self) -> Ident {
		match self {
			Self::Lexer => Ident::new("Lexer").unwrap(),
			Self::UnexpectedToken => Ident::new("UnexpectedToken").unwrap(),
			Self::UnexpectedNode => Ident::new("UnexpectedNode").unwrap(),
		}
	}
}
