use super::{Generate, Target};
use crate::{
	gen::{pseudo, Parser},
	lexing, mono, Ident,
};
use proc_macro2::TokenStream;
use quote::quote;
use std::path::{Path, PathBuf};

/// Rust target.
pub struct Rust;

impl Target for Rust {
	type Output = TokenStream;

	fn module_filename<P: AsRef<Path>>(
		&self,
		root: P,
		module_path: pseudo::module::Path,
	) -> PathBuf {
		let mut file_path = root.as_ref().to_path_buf();

		for id in module_path {
			if let pseudo::module::Id::Named(name) = id {
				file_path.push(crate::util::to_snake_case(&name));
			}
		}

		file_path.set_extension("rs");
		file_path
	}
}

#[derive(Clone, Copy)]
pub struct Scope {
	/// Current module.
	module: u32,

	/// Loop label.
	label: Option<pseudo::expr::Label>,

	/// If false, a `return`/`break` statement is introduced
	/// when generating an expression without successor.
	pure: bool,
}

impl Scope {
	pub fn new(module: u32) -> Self {
		Self {
			module,
			label: None,
			pure: true,
		}
	}

	pub fn is_pure(&self) -> bool {
		self.pure
	}

	// pub fn label(&self) -> Option<pseudo::expr::Label> {
	// 	self.label
	// }

	pub fn is_in_loop(&self, label: pseudo::expr::Label) -> bool {
		self.label.map(|l| l == label).unwrap_or(false)
	}

	pub fn pure(self) -> Self {
		Self { pure: true, ..self }
	}

	pub fn impure(self, label: pseudo::expr::Label) -> Self {
		Self {
			label: Some(label),
			pure: false,
			..self
		}
	}
}

/// Generate code inside a scope.
pub trait GenerateIn<T> {
	fn generate_in(&self, context: &pseudo::Context, scope: Scope, t: &T) -> TokenStream;
}

impl Generate<pseudo::module::Id> for Rust {
	fn generate(&self, _: &pseudo::Context, id: &pseudo::module::Id) -> TokenStream {
		self.module_id(id)
	}
}

impl Generate<pseudo::ty::Ref> for Rust {
	fn generate(&self, context: &pseudo::Context, r: &pseudo::ty::Ref) -> TokenStream {
		let ty = context.ty(*r).unwrap();
		let module_path = self.module_path(context.module_path(ty.module()).unwrap());
		let id = self.type_id(ty.id());
		quote! { #module_path::#id }
	}
}

impl Generate<Box<pseudo::ty::Expr>> for Rust {
	fn generate(&self, context: &pseudo::Context, e: &Box<pseudo::ty::Expr>) -> TokenStream {
		self.generate(context, e.as_ref())
	}
}

impl Generate<pseudo::ty::Expr> for Rust {
	fn generate(&self, context: &pseudo::Context, e: &pseudo::ty::Expr) -> TokenStream {
		use pseudo::ty::Expr;
		match e {
			Expr::Unit => quote! { () },
			Expr::Var(i) => {
				let id = quote::format_ident!("T{}", i);
				quote! { #id }
			}
			Expr::BuiltIn(ty) => self.generate(context, ty),
			Expr::Defined(i, args) => {
				let ty = context.defined_type(*i).unwrap();
				let module_path = self.module_path(context.module_path(ty.module()).unwrap());
				let id = self.type_id(ty.id());
				if args.is_empty() {
					quote! { #module_path::#id }
				} else {
					let args = args.iter().map(|a| self.generate(context, a));
					quote! { #module_path::#id <#(#args),*> }
				}
			}
			Expr::Heap(e) => {
				let e = self.generate(context, e);
				quote! { Box<#e> }
			}
			Expr::List(e) => {
				let e = self.generate(context, e);
				quote! { Vec<#e> }
			}
			Expr::Option(e) => {
				let e = self.generate(context, e);
				quote! { Option<#e> }
			}
			Expr::Loc(e) => {
				let e = self.generate(context, e);
				quote! { ::source_span::Loc<#e> }
			}
		}
	}
}

impl Generate<pseudo::built_in::Type> for Rust {
	fn generate(&self, context: &pseudo::Context, ty: &pseudo::built_in::Type) -> TokenStream {
		use pseudo::built_in::Type;
		let lexer_module_path = self.module_path(context.lexer_module_path());
		let parser_module_path = self.module_path(context.parser_module_path());
		match ty {
			Type::Token => quote! { #lexer_module_path::Token },
			Type::Keyword => quote! { #lexer_module_path::Keyword },
			Type::Operator => quote! { #lexer_module_path::Operator },
			Type::Delimiter => quote! { #lexer_module_path::Delimiter },
			Type::Punct => quote! { #lexer_module_path::Punct },
			Type::Node => quote! { #parser_module_path::Node },
			Type::Item => quote! { #parser_module_path::Item },
		}
	}
}

impl Rust {
	fn module_id(&self, id: &pseudo::module::Id) -> TokenStream {
		match id {
			pseudo::module::Id::Root => quote! { crate },
			pseudo::module::Id::Named(name) => {
				let id = quote::format_ident!("{}", crate::util::to_snake_case(&name));
				quote! { #id }
			}
		}
	}

	fn type_id(&self, id: &pseudo::ty::Id) -> TokenStream {
		use pseudo::ty::Id;
		match id {
			Id::BuiltIn(ty) => {
				use pseudo::built_in::Type;
				match ty {
					Type::Token => quote! { Token },
					Type::Keyword => quote! { Keyword },
					Type::Operator => quote! { Operator },
					Type::Delimiter => quote! { Delimiter },
					Type::Punct => quote! { Punct },
					Type::Node => quote! { Node },
					Type::Item => quote! { Item },
				}
			}
			Id::Defined(ident) => {
				let id = quote::format_ident!("{}", ident.to_caml_case());
				quote! { #id }
			}
		}
	}

	fn monomorphic_type_id(&self, context: &pseudo::Context, index: mono::Index) -> TokenStream {
		let name = context
			.grammar()
			.ty(index)
			.unwrap()
			.composed_id(context.grammar());
		let id = quote::format_ident!("{}", name.to_caml_case());
		quote! { #id }
	}

	fn defined_variant_id(&self, id: &Ident) -> TokenStream {
		let id = quote::format_ident!("{}", id.to_caml_case());
		quote! { #id }
	}

	fn field_id(&self, name: &Ident) -> TokenStream {
		let id = quote::format_ident!("{}", name.to_snake_case());
		quote! { #id }
	}

	fn is_member(&self, id: pseudo::Id) -> bool {
		use pseudo::{id, Id};
		match id {
			Id::Lexer(id::Lexer::Buffer) | Id::Lexer(id::Lexer::Span) => true,
			_ => false,
		}
	}

	fn is_mutable(&self, id: pseudo::Id) -> bool {
		use pseudo::{id, Id};
		match id {
			Id::Lexer(id::Lexer::State)
			| Id::Parser(id::Parser::Position)
			| Id::Parser(id::Parser::Stack)
			| Id::Parser(id::Parser::AnyNodeOpt)
			| Id::Parser(id::Parser::State) => true,
			_ => false,
		}
	}

	fn module_path(&self, path: pseudo::module::Path) -> TokenStream {
		let mut tokens = TokenStream::new();

		for id in path {
			if !tokens.is_empty() {
				tokens.extend(quote! { :: })
			}
			tokens.extend(self.module_id(id))
		}

		tokens
	}

	fn generate_lexer_definition(&self) -> TokenStream {
		quote! {
			/// Lexer.
			pub struct Lexer<I: ::std::iter::Iterator, M> {
				/// Source character stream.
				source: ::std::iter::Peekable<I>,

				/// Token buffer.
				buffer: ::std::string::String,

				/// Character metrics.
				metrics: M,

				/// Token span.
				span: ::source_span::Span
			}
		}
	}

	fn generate_parser_definition(&self, context: &pseudo::Context) -> TokenStream {
		let extern_module_path = self.module_path(context.extern_module_path());
		let lexer_module_path = self.module_path(context.lexer_module_path());
		quote! {
			/// Parsing errors.
			pub enum Error {
				/// Error comming from the lexer.
				Lexer(#extern_module_path::Error),

				/// Unexpected lexer token.
				UnexpectedToken(Option<#lexer_module_path::Token>),

				/// Unexpected AST node.
				UnexpectedNode(Node)
			}

			impl From<#extern_module_path::Error> for Error {
				fn from(e: #extern_module_path::Error) -> Self {
					Self::Lexer(e)
				}
			}
		}
	}

	fn generate_lexer_impl(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		expr: &pseudo::Expr,
	) -> TokenStream {
		let extern_module_path = self.module_path(context.extern_module_path());
		let body = self.generate_in(context, scope, expr);

		quote! {
			impl<
				E: ::std::convert::Into<#extern_module_path::Error>,
				I: ::std::iter::Iterator<Item = ::std::result::Result<char, E>>,
				M: ::source_span::Metrics,
			> Lexer<I, M> {
				fn peek_char(
					&mut self
				) -> ::std::result::Result<
					::std::option::Option<char>,
					::source_span::Loc<#extern_module_path::Error>,
				> {
					match self.source.peek() {
						Some(Ok(c)) => Ok(Some(*c)),
						Some(Err(_)) => Err(self.consume_char().unwrap_err()),
						None => Ok(None),
					}
				}

				fn consume_char(
					&mut self,
				) -> ::std::result::Result<(), ::source_span::Loc<#extern_module_path::Error>> {
					match self.source.next() {
						Some(Ok(c)) => {
							self.buffer.push(c);
							self.span.push(c, &self.metrics);
							Ok(())
						}
						Some(Err(e)) => Err(::source_span::Loc::new(e.into(), self.span.end().into())),
						None => Ok(()),
					}
				}

				fn next_token(
					&mut self,
				) -> ::std::result::Result<
					::std::option::Option<::source_span::Loc<Token>>,
					::source_span::Loc<#extern_module_path::Error>,
				> {
					#body
				}
			}
		}
	}

	fn generate_parser(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		index: mono::Index,
		parser: &Parser,
	) -> TokenStream {
		let ty = context.grammar().ty(index).unwrap();
		let id = quote::format_ident!(
			"parse_{}",
			ty.composed_id(context.grammar()).to_snake_case()
		);

		let token_type_path = self.generate(
			context,
			&pseudo::ty::Ref::BuiltIn(pseudo::built_in::Type::Token),
		);
		let result_type_path = self.generate(context, &context.type_expr(index));

		match parser {
			Parser::LR0(expr) => {
				let body = self.generate_in(context, scope, expr);
				quote! {
					fn #id<
						L: ::std::iter::Iterator<
							Item = ::std::result::Result<
								::source_span::Loc<#token_type_path>,
								::source_span::Loc<Error>,
							>,
						>,
					>(
						lexer: &mut L,
					) -> ::std::result::Result<::source_span::Loc<#result_type_path>, ::source_span::Loc<Error>> {
						#body
					}
				}
			}
			Parser::LALR1(expr) => {
				unimplemented!()
			}
		}
	}
}

impl Generate<pseudo::Module> for Rust {
	fn generate(&self, context: &pseudo::Context, module: &pseudo::Module) -> TokenStream {
		use pseudo::module::Role;
		use pseudo::Routine;

		let scope = Scope::new(module.index());

		let roles = module.roles().map(|r| match r {
			Role::Lexer => self.generate_lexer_definition(),
			Role::ParserRoot => self.generate_parser_definition(context),
			_ => TokenStream::new(),
		});

		let types = module.types().map(|t| {
			let ty = context.ty(t).unwrap();
			self.generate(context, ty)
		});

		let routines = module.routines().map(|r| match r {
			Routine::Lexer(expr) => self.generate_lexer_impl(context, scope, expr),
			Routine::Parser(index, parser) => self.generate_parser(context, scope, *index, parser),
		});

		let submodules = module.modules().map(|index| {
			let m = context.module(index).unwrap();
			let id = self.generate(context, m.id());
			let inner = self.generate(context, m);
			quote! {
				mod #id {
					#inner
				}
			}
		});

		quote! {
			#(#roles)*
			#(#types)*
			#(#routines)*
			#(#submodules)*
		}
	}
}

impl Generate<pseudo::Type> for Rust {
	fn generate(&self, context: &pseudo::Context, ty: &pseudo::Type) -> TokenStream {
		use pseudo::ty::Desc;
		let id = self.generate(context, ty.id());
		let params = if ty.parameters().is_empty() {
			None
		} else {
			let params = ty
				.parameters()
				.iter()
				.map(|p| quote::format_ident!("{}", p.to_caml_case()));
			Some(quote! { <#(#params),*> })
		};
		match ty.desc() {
			Desc::Opaque => quote! { pub struct #id #params; },
			Desc::Enum(enm) => {
				let variants = enm.variants().iter().map(|v| {
					use pseudo::ty::Variant;
					match v {
						Variant::BuiltIn(v) => {
							let id = self.generate(context, v);
							let param = v.parameter().map(|p| {
								let p = self.generate(context, p);
								quote! { (#p) }
							});
							quote! { #id #param }
						}
						Variant::Defined(id, desc) => {
							use pseudo::ty::VariantDesc;
							let id = self.defined_variant_id(id);
							let desc = match desc {
								_ if desc.is_empty() => None,
								VariantDesc::Tuple(args) => {
									let args = args.iter().map(|a| self.generate(context, a));
									Some(quote! { (#(#args),*) })
								}
								VariantDesc::Struct(strct) => {
									let fields = strct.fields().iter().map(|f| {
										let id = self.field_id(&f.id);
										let ty = self.generate(context, &f.ty);
										quote! { #id: #ty }
									});
									Some(quote! { { #(#fields),* } })
								}
							};
							quote! { #id #desc }
						}
					}
				});
				quote! { pub enum #id { #(#variants),* } }
			}
			Desc::Struct(strct) => {
				let fields = strct.fields().iter().map(|f| {
					let id = self.field_id(&f.id);
					let ty = self.generate(context, &f.ty);
					quote! { #id: #ty }
				});
				quote! { pub struct #id { #(#fields),* } }
			}
			Desc::TupleStruct(args) => {
				let args = args.iter().map(|a| self.generate(context, a));
				quote! { pub struct #id (#(pub #args),*); }
			}
		}
	}
}

impl Generate<pseudo::ty::Id> for Rust {
	fn generate(&self, context: &pseudo::Context, id: &pseudo::ty::Id) -> TokenStream {
		use pseudo::ty::Id;
		match id {
			Id::BuiltIn(ty) => self.generate(context, ty),
			Id::Defined(id) => {
				let id = quote::format_ident!("{}", id.to_caml_case());
				quote! { #id }
			}
		}
	}
}

impl Generate<pseudo::ty::Variant> for Rust {
	fn generate(&self, context: &pseudo::Context, v: &pseudo::ty::Variant) -> TokenStream {
		use pseudo::ty::Variant;
		match v {
			Variant::BuiltIn(v) => self.generate(context, v),
			Variant::Defined(id, _) => self.defined_variant_id(id),
		}
	}
}

impl Generate<pseudo::built_in::Variant> for Rust {
	fn generate(&self, context: &pseudo::Context, v: &pseudo::built_in::Variant) -> TokenStream {
		use pseudo::built_in::Variant;
		match v {
			Variant::Token(v) => self.generate(context, v),
			Variant::Keyword(kw) => {
				let id = quote::format_ident!("{}", kw.to_caml_case());
				quote! { #id }
			}
			Variant::Operator(v) => self.generate(context, v),
			Variant::Punct(p) => self.generate(context, p),
			Variant::Delimiter(d) => self.generate(context, d),
			Variant::Node(index, _) => self.monomorphic_type_id(context, *index),
			Variant::Item(v) => self.generate(context, v),
		}
	}
}

impl Generate<pseudo::built_in::TokenVariant> for Rust {
	fn generate(&self, _: &pseudo::Context, v: &pseudo::built_in::TokenVariant) -> TokenStream {
		use pseudo::built_in::TokenVariant;
		match v {
			TokenVariant::Keyword => quote! { Keyword },
			TokenVariant::Operator => quote! { Operator },
			TokenVariant::Begin => quote! { Begin },
			TokenVariant::End => quote! { End },
			TokenVariant::Punct => quote! { Punct },
		}
	}
}

impl Generate<pseudo::built_in::ItemVariant> for Rust {
	fn generate(&self, _: &pseudo::Context, v: &pseudo::built_in::ItemVariant) -> TokenStream {
		use pseudo::built_in::ItemVariant;
		match v {
			ItemVariant::Token => quote! { Token },
			ItemVariant::Node => quote! { Node },
		}
	}
}

impl Generate<lexing::token::Operator> for Rust {
	fn generate(&self, _: &pseudo::Context, op: &lexing::token::Operator) -> TokenStream {
		let id = quote::format_ident!("{}", crate::util::to_caml_case(op.name()));
		quote! { #id }
	}
}

impl Generate<lexing::token::Punct> for Rust {
	fn generate(&self, _: &pseudo::Context, p: &lexing::token::Punct) -> TokenStream {
		let id = quote::format_ident!("{}", crate::util::to_caml_case(p.name()));
		quote! { #id }
	}
}

impl Generate<lexing::token::Delimiter> for Rust {
	fn generate(&self, _: &pseudo::Context, d: &lexing::token::Delimiter) -> TokenStream {
		let id = quote::format_ident!("{}", crate::util::to_caml_case(d.name()));
		quote! { #id }
	}
}

impl GenerateIn<pseudo::expr::BuildArgs> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		args: &pseudo::expr::BuildArgs,
	) -> TokenStream {
		use pseudo::expr::BuildArgs;
		match args {
			BuildArgs::Tuple(args) => {
				if args.is_empty() {
					TokenStream::new()
				} else {
					let args = args.iter().map(|a| self.generate_in(context, scope, a));
					quote! { ( #(#args),* ) }
				}
			}
			BuildArgs::Struct(bindings) => {
				let bindings = bindings.iter().map(|b| {
					let field = self.field_id(&b.name);
					let expr = self.generate_in(context, scope, &b.expr);
					quote! { #field: #expr }
				});
				quote! { { #(#bindings),* } }
			}
		}
	}
}

impl GenerateIn<Box<pseudo::Expr>> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		e: &Box<pseudo::Expr>,
	) -> TokenStream {
		self.generate_in(context, scope, e.as_ref())
	}
}

impl GenerateIn<pseudo::Expr> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		e: &pseudo::Expr,
	) -> TokenStream {
		use pseudo::Expr;
		let expr = match e {
			Expr::Constant(c) => self.generate(context, c),
			Expr::Get(id) => self.generate(context, id),
			Expr::Set(id, value, next) => {
				let gid = self.generate(context, id);
				let value = self.generate_in(context, scope.pure(), value);
				let next = self.generate_in(context, scope, next);
				if self.is_member(*id) || (!scope.is_pure() && self.is_mutable(*id)) {
					quote! {
						#gid = #value;
						#next
					}
				} else {
					let mutable = if self.is_mutable(*id) {
						Some(quote! { mut })
					} else {
						None
					};
					quote! {
						let #mutable #gid = #value;
						#next
					}
				}
			}
			Expr::Lexer(op) => self.generate_in(context, scope, op),
			Expr::Parser(op) => self.generate_in(context, scope, op),
			Expr::New(ty, args) => {
				let ty = self.generate(context, ty);
				let args = self.generate_in(context, scope.pure(), args);
				quote! { #ty #args }
			}
			Expr::Cons(ty, v, args) => {
				let variant = self.generate(
					context,
					context.ty(*ty).unwrap().as_enum().variant(*v).unwrap(),
				);
				let ty = self.generate(context, ty);
				let args = self.generate_in(context, scope.pure(), args);
				quote! { #ty :: #variant #args }
			}
			Expr::Some(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { Some(#expr) }
			}
			Expr::None => quote! { None },
			Expr::Ok(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { Ok(#expr) }
			}
			Expr::Err(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { Err(#expr) }
			}
			Expr::Error(err) => self.generate_in(context, scope.pure(), err),
			Expr::Locate(expr, loc) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				let loc = self.generate_in(context, scope.pure(), loc);
				quote! { ::source_span::Loc::new(#expr, #loc) }
			}
			Expr::Heap(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { Box::new(#expr) }
			}
			Expr::Match { expr, cases } => {
				let expr = self.generate_in(context, scope.pure(), expr);
				let cases = cases.iter().map(|c| self.generate_in(context, scope, c));
				quote! {
					match #expr {
						#(#cases)*
					}
				}
			}
			Expr::TailRecursion { body, label, .. } => {
				let body = self.generate_in(context, scope.impure(*label), body);
				quote! {
					loop {
						#body
					}
				}
			}
			Expr::Recurse(label, _) => {
				if scope.is_in_loop(*label) {
					quote! {}
				} else {
					let label = self.generate(context, label);
					quote! { continue #label }
				}
			}
			Expr::Unreachable => quote! { unreachable!() },
		};

		if scope.is_pure() || e.is_continued() {
			expr
		} else {
			quote! { break #expr }
		}
	}
}

impl Generate<pseudo::expr::Label> for Rust {
	fn generate(&self, _: &pseudo::Context, label: &pseudo::expr::Label) -> TokenStream {
		use pseudo::expr::Label;
		match label {
			Label::Lexer(0) => quote! { 'lexer },
			_ => quote! {},
		}
	}
}

impl GenerateIn<pseudo::expr::MatchCase> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		case: &pseudo::expr::MatchCase,
	) -> TokenStream {
		let pattern = self.generate(context, &case.pattern);
		let expr = self.generate_in(context, scope, &case.expr);
		quote! { #pattern => { #expr } }
	}
}

impl Generate<Box<pseudo::Pattern>> for Rust {
	fn generate(&self, context: &pseudo::Context, pattern: &Box<pseudo::Pattern>) -> TokenStream {
		self.generate(context, pattern.as_ref())
	}
}

impl Generate<pseudo::Pattern> for Rust {
	fn generate(&self, context: &pseudo::Context, pattern: &pseudo::Pattern) -> TokenStream {
		use pseudo::Pattern;
		match pattern {
			Pattern::Any => quote! { _ },
			Pattern::BindAny(id) => self.generate(context, id),
			Pattern::Bind(id, p) => {
				let id = self.generate(context, id);
				let gp = self.generate(context, p);
				if p.is_union() {
					quote! { #id @ ( #gp ) }
				} else {
					quote! { #id @ #gp }
				}
			}
			Pattern::Constant(c) => self.generate(context, c),
			Pattern::Cons(ty, v, args) => {
				let variant = self.generate(
					context,
					context.ty(*ty).unwrap().as_enum().variant(*v).unwrap(),
				);
				let ty = self.generate(context, ty);
				if args.is_empty() {
					quote! { #ty :: #variant }
				} else {
					let args = args.iter().map(|a| self.generate(context, a));
					quote! { #ty :: #variant ( #(#args),* ) }
				}
			}
			Pattern::Some(p) => {
				let p = self.generate(context, p);
				quote! { Some(#p) }
			}
			Pattern::None => quote! { None },
			Pattern::Or(patterns) => {
				let patterns = patterns.iter().map(|p| self.generate(context, p));
				quote! { #(#patterns)|* }
			}
		}
	}
}

impl GenerateIn<pseudo::expr::Error> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		err: &pseudo::expr::Error,
	) -> TokenStream {
		use pseudo::expr::Error;
		match err {
			Error::UnexpectedChar(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				let extern_module_path = self.module_path(context.extern_module_path());
				quote! { #extern_module_path::unexpected(#expr) }
			}
			Error::UnexpectedToken(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { Error::UnexpectedToken(#expr) }
			}
			Error::UnexpectedNode(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { Error::UnexpectedNode(#expr) }
			}
		}
	}
}

impl GenerateIn<pseudo::expr::LexerOperation> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		op: &pseudo::expr::LexerOperation,
	) -> TokenStream {
		use pseudo::expr::LexerOperation as Operation;
		match op {
			Operation::PeekChar => {
				quote! { self.peek_char()? }
			}
			Operation::BufferParse(index) => {
				let terminal = context.grammar().terminal(*index).unwrap();
				let ident = terminal.id(context.grammar().poly()).unwrap();
				let id = quote::format_ident!("{}", ident.to_snake_case());
				let extern_module_path = self.module_path(context.extern_module_path());
				quote! { #extern_module_path::#id(self.buffer.as_str()).map_err(|e| ::source_span::Loc::new(e, self.span))? }
			}
			Operation::BufferIter => quote! { self.buffer.chars() },
			Operation::BufferCharsNext(next) => {
				let next = self.generate_in(context, scope, next);
				quote! { let char_opt = chars.next(); #next }
			}
			Operation::Clear(clear_span, next) => {
				let next = self.generate_in(context, scope, next);
				let clear_span = if *clear_span {
					Some(quote! { self.span.clear(); })
				} else {
					None
				};
				quote! { self.buffer.clear(); #clear_span #next }
			}
			Operation::ConsumeChar(_, next) => {
				let next = self.generate_in(context, scope, next);
				quote! { self.consume_char()?; #next }
			}
		}
	}
}

impl GenerateIn<pseudo::expr::ParserOperation> for Rust {
	fn generate_in(
		&self,
		context: &pseudo::Context,
		scope: Scope,
		op: &pseudo::expr::ParserOperation,
	) -> TokenStream {
		use pseudo::expr::ParserOperation as Operation;
		match op {
			Operation::StackNew => {
				quote! { Vec::new() }
			}
			Operation::StackPush(value, next) => {
				let value = self.generate_in(context, scope.pure(), value);
				let next = self.generate_in(context, scope, next);
				quote! { stack.push((#value, state)); #next }
			}
			Operation::StackPop(value, state, next) => {
				let next = self.generate_in(context, scope, next);
				if value.is_none() && state.is_none() {
					quote! { stack.pop(); #next }
				} else {
					let value = match value {
						Some(v) => self.generate(context, v),
						None => quote! { _ },
					};

					let state = match state {
						Some(s) => self.generate(context, s),
						None => quote! { _ },
					};

					quote! {
						let (#value, #state) = stack.pop().unwrap();
						#next
					}
				}
			}
			Operation::PatternUnwrap(pattern, value, next) => {
				let next = self.generate_in(context, scope, next);
				let value = self.generate_in(context, scope.pure(), value);
				let pattern = self.generate(context, pattern);
				quote! {
					if let #pattern = #value {
						#next
					} else {
						unreachable!()
					}
				}
			}
			Operation::LexerNext(next) => {
				let next = self.generate_in(context, scope, next);
				quote! { let any_token_opt = lexer.next().transpose().map_err(|e| e.inner_into())?; #next }
			}
			Operation::PositionNew => {
				quote! { ::source_span::Position::default() }
			}
			Operation::PositionToSpan(expr) => {
				let expr = self.generate_in(context, scope.pure(), expr);
				quote! { #expr . into() }
			}
			Operation::LocOptTranspose(value, default_span) => {
				let value = self.generate_in(context, scope.pure(), value);
				let default_span = self.generate_in(context, scope.pure(), default_span);
				quote! {
					::source_span::Loc::transposed(#value, #default_span)
				}
			}
			Operation::LocUnwrap(target_value, target_span, value, next) => {
				let target_value = match target_value {
					Some(id) => self.generate(context, id),
					None => quote! { _ },
				};

				let target_span = match target_span {
					Some(id) => self.generate(context, id),
					None => quote! { _ },
				};

				let value = self.generate_in(context, scope.pure(), value);
				let next = self.generate_in(context, scope, next);
				quote! {
					let (#target_value, #target_span) = #value.into_raw_parts();
					#next
				}
			}
			Operation::LocUnion(a, b) => {
				let a = self.generate_in(context, scope.pure(), a);
				let b = self.generate_in(context, scope.pure(), b);
				quote! {
					#a.union(#b)
				}
			}
		}
	}
}

impl Generate<pseudo::Constant> for Rust {
	fn generate(&self, _: &pseudo::Context, c: &pseudo::Constant) -> TokenStream {
		use pseudo::Constant;
		match c {
			Constant::Int(n) => quote! { #n },
			Constant::Char(c) => quote! { #c },
			Constant::CharRange(a, b) => quote! { #a..=#b },
		}
	}
}

impl Generate<pseudo::Id> for Rust {
	fn generate(&self, context: &pseudo::Context, id: &pseudo::Id) -> TokenStream {
		use pseudo::Id;
		match id {
			Id::Lexer(id) => self.generate(context, id),
			Id::Parser(id) => self.generate(context, id),
		}
	}
}

impl Generate<pseudo::id::Lexer> for Rust {
	fn generate(&self, _: &pseudo::Context, id: &pseudo::id::Lexer) -> TokenStream {
		use pseudo::id::Lexer;
		match id {
			Lexer::Itself => quote! { self },
			Lexer::Buffer => quote! { self.buffer },
			Lexer::Span => quote! { self.span },
			Lexer::State => quote! { state },
			Lexer::BufferChars => quote! { chars },
			Lexer::CharOpt => quote! { c_opt },
			Lexer::Char => quote! { c },
			Lexer::SubToken => quote! { sub_token },
			Lexer::Unexpected => quote! { unexpected },
		}
	}
}

impl Generate<pseudo::id::Parser> for Rust {
	fn generate(&self, _: &pseudo::Context, id: &pseudo::id::Parser) -> TokenStream {
		use pseudo::id::Parser;
		match id {
			Parser::Lexer => quote! { lexer },
			Parser::Stack => quote! { stack },
			Parser::State => quote! { state },
			Parser::SavedState => quote! { saved_state },
			Parser::AnyNodeOpt => quote! { any_node_opt },
			Parser::AnyNodeOptSpanless => quote! { any_node_opt_spanless },
			Parser::AnyItem(i) => {
				let id = quote::format_ident!("any_item{}", i);
				quote! { #id }
			}
			Parser::AnyItemSpanless(i) => {
				let id = quote::format_ident!("item{}_spanless", i);
				quote! { #id }
			}
			Parser::AnyItemSpan(i) => {
				let id = quote::format_ident!("item{}_span", i);
				quote! { #id }
			}
			Parser::Item(i) => {
				let id = quote::format_ident!("item{}", i);
				quote! { #id }
			}
			Parser::AnyNode => quote! { any_node },
			Parser::Node => quote! { node },
			Parser::AnyTokenOpt => quote! { any_token_opt },
			Parser::AnyTokenOptSpanless => quote! { any_token_opt_spanless },
			Parser::Token => quote! { token },
			Parser::Position => quote! { position },
			Parser::Span => quote! { span },
			Parser::Result => quote! { result },
			Parser::Unexpected => quote! { unexpected },
		}
	}
}
