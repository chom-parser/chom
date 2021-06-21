use super::{ast, lexer, ExternModule, SourceSpanCrate, StdCrate};
use crate::{
	lexing::Token,
	mono::{ty, Grammar, Index, Terminal, Type},
	parsing, util,
};
use quote::quote;
use std::collections::{HashMap, HashSet};

pub struct Module {
	inner: rust_codegen::module::Ref,
}

impl Module {
	pub fn new(
		context: &rust_codegen::Context,
		std_crate: &StdCrate,
		source_span_crate: &SourceSpanCrate,
		extern_mod: &ExternModule,
		lexer_mod: &lexer::Module,
		ast_mod: &ast::Module,
		grammar: &Grammar,
		table: &parsing::table::LR0,
		path: &[String],
	) -> Self {
		let module_ref = super::declare_module(context, path);

		let error_type_ref = {
			let mut module = module_ref.borrow_mut();
			let error_type_ref = module.add_enum("Error");

			{
				let mut error_type = error_type_ref.borrow_mut();
				error_type.set_public();

				let mut lexer_variant = rust_codegen::enm::Variant::new("Lexer");
				lexer_variant.add_param(extern_mod.error_type());
				error_type.add(lexer_variant);

				let mut unexpected_token_variant =
					rust_codegen::enm::Variant::new("UnexpectedToken");
				unexpected_token_variant.add_param(lexer_mod.token_ty().instanciate());
				error_type.add(unexpected_token_variant);

				let unexpected_eof_variant = rust_codegen::enm::Variant::new("UnexpectedEof");
				error_type.add(unexpected_eof_variant);
			}

			let from_impl = module.add_impl(
				Some(
					std_crate
						.from_trait
						.instanciate_with([extern_mod.error_type()]),
				),
				error_type_ref.instanciate(),
			);
			let mut sig = rust_codegen::func::Signature::new("from", error_type_ref.instanciate());
			sig.add_arg("e", extern_mod.error_type());
			from_impl.add_function(
				sig,
				quote! {
					Self::Lexer(e)
				},
			);

			error_type_ref
		};

		let scope = rust_codegen::Scope::new(module_ref.clone());

		let loc_token_type = source_span_crate
			.loc_struct
			.instanciate_with([lexer_mod.token_ty().instanciate()]);
		let lexer_item_type = std_crate.result_enum.instanciate_with([
			loc_token_type,
			source_span_crate
				.loc_struct
				.instanciate_with([error_type_ref.instanciate()]),
		]);
		let mut lexer_ty_param = rust_codegen::ty::param::Definition::new("L");
		lexer_ty_param.add_bound(
			std_crate
				.iterator_trait
				.instanciate_with([rust_codegen::ty::Param::named("Item", lexer_item_type)]),
		);

		let mut functions = Vec::new();
		for (ty_index, q) in table.entries() {
			let ty = grammar.ty(ty_index).unwrap();

			let ast_ty = ast_mod.ty_instance(ty_index).unwrap();

			let name = format!("parse_{}", ty_snake_case_name(grammar, &ty));
			let mut sig = rust_codegen::func::Signature::new(
				name,
				std_crate.result_enum.instanciate_with([
					source_span_crate.loc_struct.instanciate_with([ast_ty]),
					source_span_crate
						.loc_struct
						.instanciate_with([error_type_ref.instanciate()]),
				]),
			);
			sig.add_type_param(lexer_ty_param.clone());
			sig.add_arg(
				"lexer",
				rust_codegen::ty::Instance::ref_mut(
					rust_codegen::Lifetime::Anonymous,
					lexer_ty_param.instanciate(),
				),
			);
			functions.push((
				sig,
				generate_from_state(lexer_mod, ast_mod, grammar, &scope, table, q),
			));
		}

		{
			let mut module = module_ref.borrow_mut();
			for (sig, body) in functions {
				module.add_public_function(sig, body);
			}
		}

		Self { inner: module_ref }
	}

	pub fn inner(&self) -> rust_codegen::module::Inner {
		self.inner.inner()
	}

	pub fn write<W: std::io::Write>(&self, out: &mut W) -> std::io::Result<()> {
		let inner = self.inner();
		write!(out, "{}", quote::quote! { #inner })
	}
}

pub fn generate_from_state(
	lexer_mod: &lexer::Module,
	ast_mod: &ast::Module,
	grammar: &Grammar,
	scope: &rust_codegen::Scope,
	table: &parsing::table::LR0,
	q: u32,
) -> proc_macro2::TokenStream {
	let mut nt_map = HashMap::new();
	let mut states = Vec::new();

	let mut stack = vec![q];
	let mut visited = HashSet::new();
	while let Some(q) = stack.pop() {
		if visited.insert(q) {
			let body = generate_state(ast_mod, grammar, scope, &mut nt_map, table, &mut stack, q);
			states.push(quote! {
				#q => { #body }
			})
		}
	}

	let non_terminal_variants = nt_map.into_iter().map(|(_, variant)| {
		let id = variant.id;
		let ty = variant.ty_path;
		quote! { #id ( #ty ) }
	});

	let lexer_path = lexer_mod.path(scope);
	quote! {
		use #lexer_path as lexer;

		pub enum NonTerminal {
			#(#non_terminal_variants),*
		}

		pub enum Item {
			Terminal(lexer::Token),
			NonTerminal(NonTerminal)
		}

		let mut stack = Vec::new();
		let mut non_terminal = None;
		let mut result_span = ::source_span::Span::default();
		let mut state = #q;
		loop {
			state = match state {
				#(#states)*
				_ => unreachable!()
			}
		}
	}
}

pub struct NonTerminalVariant {
	id: proc_macro2::Ident,
	ty_path: rust_codegen::ScopedPath,
}

impl NonTerminalVariant {
	pub fn pattern(&self, arg: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
		let id = &self.id;
		quote! { NonTerminal :: #id ( #arg ) }
	}
}

fn non_terminal_variant_name(grammar: &Grammar, ty: &Type) -> String {
	ty_caml_case_name(grammar, ty)
}

fn non_terminal_variant<'a>(
	ast_mod: &ast::Module,
	grammar: &Grammar,
	scope: &rust_codegen::Scope,
	map: &'a mut HashMap<Index, NonTerminalVariant>,
	index: Index,
) -> &'a NonTerminalVariant {
	use std::collections::hash_map::Entry;
	match map.entry(index) {
		Entry::Occupied(entry) => entry.into_mut(),
		Entry::Vacant(entry) => {
			let ty = grammar.ty(index).unwrap();
			let rust_ty = ast_mod.ty_instance(index).unwrap();

			use rust_codegen::Instance;
			entry.insert(NonTerminalVariant {
				id: quote::format_ident!("{}", non_terminal_variant_name(grammar, &ty)),
				ty_path: rust_ty.path(scope),
			})
		}
	}
}

pub fn generate_state(
	ast_mod: &ast::Module,
	grammar: &Grammar,
	scope: &rust_codegen::Scope,
	nt_map: &mut HashMap<Index, NonTerminalVariant>,
	table: &parsing::table::LR0,
	stack: &mut Vec<u32>,
	q: u32,
) -> proc_macro2::TokenStream {
	use parsing::table::lr0::State;
	let state = table.state(q).unwrap();

	let lexer_path = quote! { lexer };

	match state {
		State::Reduce(rule) => match rule {
			parsing::table::Rule::Initial(ty_index) => {
				let variant = non_terminal_variant(ast_mod, grammar, scope, nt_map, *ty_index);
				let pattern = variant.pattern(quote! { result });

				quote! {
					let result = if let Some((Item::NonTerminal(#pattern), _)) = stack.pop() { result } else { unreachable!() };
					break Ok(::source_span::Loc::new(result, result_span))
				}
			}
			parsing::table::Rule::Function(f_index) => {
				let f = grammar.function(*f_index).unwrap();

				let mut args_pop = Vec::new();
				let mut args = Vec::new();
				let mut first = true;
				for a in f.arguments() {
					match a {
						ty::Expr::Terminal(index) => {
							let t = grammar.terminal(*index).unwrap();
							let token = t.token().unwrap();
							if token.has_parameter() {
								let pattern =
									lexer::token_pattern(&lexer_path, &token, quote! { value });
								let arg = quote::format_ident!("a{}", args.len());
								let state_pat = if first {
									quote! { next_state }
								} else {
									quote! { _ }
								};
								args_pop.push(quote! {
									let (#arg, #state_pat) = if let Some((Item::Terminal(#pattern), q)) = stack.pop() { (value, q) } else { unreachable!() };
								});
								args.push(quote! { #arg })
							} else {
								args_pop.push(if first {
									quote! { let (_, next_state) = stack.pop().unwrap() }
								} else {
									quote! { stack.pop() }
								})
							}
						}
						ty::Expr::Type(index) => {
							let arg = quote::format_ident!("a{}", args.len());
							let variant =
								non_terminal_variant(ast_mod, grammar, scope, nt_map, *index);
							let pattern = variant.pattern(quote! { value });
							let state_pat = if first {
								quote! { next_state }
							} else {
								quote! { _ }
							};

							args_pop.push(quote! {
								let (#arg, #state_pat) = if let Some((Item::NonTerminal(#pattern), q)) = stack.pop() { (value, q) } else { unreachable!() };
							});

							args.push(if a.depends_on(grammar, ty::Expr::Type(f.return_ty())) {
								quote! { Box::new(#arg) }
							} else {
								quote! { #arg }
							})
						}
					}

					first = false
				}

				args_pop.reverse();
				let args = if args.is_empty() {
					None
				} else {
					Some(quote! { ( #(#args),* ) })
				};

				let nt_variant =
					non_terminal_variant(ast_mod, grammar, scope, nt_map, f.return_ty());
				let return_ty = &nt_variant.ty_path;
				let variant = quote::format_ident!("{}", ast::variant_name(f.poly()));
				let expr = nt_variant.pattern(quote! { #return_ty :: #variant #args });

				quote! {
					#(#args_pop);*
					non_terminal = Some(#expr);
					next_state
				}
			}
		},
		State::Shift { action, goto } => {
			let mut actions = Vec::new();
			for (symbol, q) in action {
				match symbol {
					Some(t) => {
						let terminal = grammar.terminal(*t).unwrap();
						let token = terminal.token().unwrap();
						let pattern = lexer::token_pattern(&lexer_path, token, quote! { value });

						actions.push(quote! {
							Some(#pattern) => {
								stack.push((Item::Terminal(#pattern), state));
								#q
							}
						});

						stack.push(*q)
					}
					None => {
						actions.push(quote! {
							None => { #q }
						});
						stack.push(*q)
					}
				}
			}

			let mut gotos = Vec::new();
			for (nt, q) in goto {
				let variant = non_terminal_variant(ast_mod, grammar, scope, nt_map, *nt);
				let pattern = variant.pattern(quote! { value });
				gotos.push(quote! {
					#pattern => {
						stack.push((Item::NonTerminal(#pattern), state));
						#q
					}
				});

				stack.push(*q)
			}

			quote! {
				match non_terminal.take() {
					Some(nt) => {
						match nt {
							#(#gotos),*
							_ => unreachable!()
						}
					},
					None => {
						let (token, span) = ::source_span::Loc::transposed(
							lexer.next().transpose().map_err(|e| e.inner_into())?,
							result_span.end().into()
						).into_raw_parts();
						result_span.append(span);
						match token {
							#(#actions),*
							unexpected => match unexpected {
								Some(token) => {
									break Err(::source_span::Loc::new(Error::UnexpectedToken(token), span))
								},
								None => break Err(::source_span::Loc::new(Error::UnexpectedEof, result_span.end().into()))
							}
						}
					}
				}
			}
		}
	}
}

pub fn ty_caml_case_name(grammar: &Grammar, ty: &Type) -> String {
	let mut name = String::new();

	for p in ty.parameters() {
		if let ty::Expr::Type(index) = p {
			let p_ty = grammar.ty(*index).unwrap();

			name.push_str(&ty_caml_case_name(grammar, &p_ty));
		}
	}

	name.push_str(&util::to_caml_case(ty.id().name()));

	name
}

pub fn ty_snake_case_name(grammar: &Grammar, ty: &Type) -> String {
	let mut name = String::new();

	for p in ty.parameters() {
		if let ty::Expr::Type(index) = p {
			let p_ty = grammar.ty(*index).unwrap();

			if !name.is_empty() {
				name.push_str("_")
			}

			name.push_str(&ty_snake_case_name(grammar, &p_ty));
		}
	}

	if !name.is_empty() {
		name.push_str("_")
	}

	name.push_str(&util::to_snake_case(ty.id().name()));

	name
}
