use super::{ExternModule, SourceSpanCrate, StdCrate};
use crate::{
	lexing::{token, DetAutomaton, DetState, Table, Token},
	mono::{terminal, Grammar},
	util,
};
use btree_range_map::util::PartialEnum;
use proc_macro2::TokenStream;
use quote::quote;
use std::{
	collections::{BTreeMap, BTreeSet, HashMap},
	ops::{Bound, RangeBounds},
};

pub struct Module {
	inner: rust_codegen::module::Ref,
	token_enum: rust_codegen::enm::Ref,
}

impl Module {
	pub fn new(
		context: &rust_codegen::Context,
		grammar: &Grammar,
		table: &Table,
		std_crate: &StdCrate,
		source_span_crate: &SourceSpanCrate,
		extern_mod: &ExternModule,
		path: &[String],
	) -> Self {
		let module_ref = super::declare_module(context, path);

		let token_enum_ref;
		let self_ty;
		let mut error_param;
		let mut chars_param;
		let mut metrics_param;
		let char_primitive;

		{
			let mut module = module_ref.borrow_mut();
			token_enum_ref = generate_tokens(grammar, extern_mod, &mut module);

			let lexer_struct_ref = module.add_struct("Lexer");
			let mut lexer_struct = lexer_struct_ref.borrow_mut();
			lexer_struct.set_public();
			chars_param = rust_codegen::ty::param::Definition::new("I");
			// let char_primitive = rust_codegen::ty::Instance::Primitive(rust_codegen::ty::Primitive::Char);
			chars_param.add_bound(std_crate.iterator_trait.instanciate());
			// chars_param.add_bound(std_crate.iterator_trait.instanciate_with([rust_codegen::ty::Param::named("Item", char_primitive)]));
			metrics_param = rust_codegen::ty::param::Definition::new("M");
			lexer_struct.add_param(chars_param.clone());
			lexer_struct.add_param(metrics_param.clone());
			lexer_struct.add_field(
				"chars",
				std_crate
					.peekable_struct
					.instanciate_with([chars_param.instanciate()]),
			);
			lexer_struct.add_field("buffer", std_crate.string_struct.instanciate());
			lexer_struct.add_field("metrics", metrics_param.instanciate());
			lexer_struct.add_field("span", source_span_crate.span_struct.instanciate());

			error_param = rust_codegen::ty::param::Definition::new("E");
			error_param.add_bound(
				std_crate
					.into_trait
					.instanciate_with([extern_mod.error_type()]),
			);
			char_primitive =
				rust_codegen::ty::Instance::Primitive(rust_codegen::ty::Primitive::Char);
			chars_param = rust_codegen::ty::param::Definition::new("I");
			chars_param.add_bound(
				std_crate
					.iterator_trait
					.instanciate_with([rust_codegen::ty::Param::named(
						"Item",
						std_crate
							.result_enum
							.instanciate_with([char_primitive.clone(), error_param.instanciate()]),
					)]),
			);
			metrics_param = rust_codegen::ty::param::Definition::new("M");
			metrics_param.add_bound(source_span_crate.metrics_trait.instanciate());

			self_ty = lexer_struct_ref
				.instanciate_with([chars_param.instanciate(), metrics_param.instanciate()]);
		}

		let loc_error_type = source_span_crate
			.loc_struct
			.instanciate_with([extern_mod.error_type()]);
		let loc_token_type = source_span_crate
			.loc_struct
			.instanciate_with([token_enum_ref.instanciate()]);
		let next_token_func_sig = rust_codegen::func::Signature::method_mut(
			"next_token",
			std_crate.result_enum.instanciate_with([
				std_crate
					.option_enum
					.instanciate_with([loc_token_type.clone()]),
				loc_error_type.clone(),
			]),
		);

		let loc_path;
		let next_token_func_body = {
			let scope = rust_codegen::Scope::new(module_ref.clone());
			use rust_codegen::Instance;
			loc_path = source_span_crate.loc_struct.path(&scope);
			generate_next_token_function(
				source_span_crate,
				extern_mod,
				&scope,
				grammar,
				table,
				table.root(),
				false,
			)
		};

		{
			let mut module = module_ref.borrow_mut();
			let implem = module.add_impl(None, self_ty.clone());
			implem.add_type_param(error_param.clone());
			implem.add_type_param(chars_param.clone());
			implem.add_type_param(metrics_param.clone());

			let sig = rust_codegen::func::Signature::method_mut(
				"peek_char",
				std_crate.result_enum.instanciate_with([
					std_crate.option_enum.instanciate_with([char_primitive]),
					loc_error_type.clone(),
				]),
			);
			implem.add_function(
				sig,
				quote! {
					match self.chars.peek() {
						Some(Ok(c)) => {
							Ok(Some(*c))
						},
						Some(Err(_)) => Err(self.consume_char().unwrap_err()),
						None => Ok(None)
					}
				},
			);
			let sig = rust_codegen::func::Signature::method_mut(
				"consume_char",
				std_crate
					.result_enum
					.instanciate_with([rust_codegen::ty::Instance::unit(), loc_error_type.clone()]),
			);
			implem.add_function(
				sig,
				quote! {
					match self.chars.next() {
						Some(Ok(c)) => {
							self.buffer.push(c);
							self.span.push(c, &self.metrics);
							Ok(())
						},
						Some(Err(e)) => Err(#loc_path::new(e.into(), self.span.end().into())),
						None => Ok(())
					}
				},
			);

			implem.add_function(next_token_func_sig, next_token_func_body);

			let iterator_impl =
				module.add_impl(Some(std_crate.iterator_trait.instanciate()), self_ty);
			iterator_impl.add_type_param(error_param.clone());
			iterator_impl.add_type_param(chars_param.clone());
			iterator_impl.add_type_param(metrics_param.clone());
			let item_ty = std_crate
				.result_enum
				.instanciate_with([loc_token_type, loc_error_type]);
			iterator_impl.add_associated_type("Item", item_ty.clone());
			let sig = rust_codegen::func::Signature::method_mut(
				"next",
				std_crate.option_enum.instanciate_with([item_ty]),
			);
			iterator_impl.add_function(
				sig,
				quote! {
					self.next_token().transpose()
				},
			)
		}

		Self {
			inner: module_ref,
			token_enum: token_enum_ref,
		}
	}

	pub fn token_ty(&self) -> &rust_codegen::enm::Ref {
		&self.token_enum
	}

	pub fn inner(&self) -> rust_codegen::module::Inner {
		self.inner.inner()
	}

	pub fn path(&self, scope: &rust_codegen::Scope) -> rust_codegen::ScopedPath {
		use rust_codegen::Instance;
		self.inner.path(scope)
	}

	pub fn write<W: std::io::Write>(&self, out: &mut W) -> std::io::Result<()> {
		let inner = self.inner();
		write!(out, "{}", quote::quote! { #inner })
	}
}

/// Generate a Rust pattern matching the given token.
pub fn token_pattern(
	lexer: &proc_macro2::TokenStream,
	token: &Token,
	param: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
	let id = token_ident(token);

	match token {
		Token::Keyword(_) => quote! { #lexer::Token::Keyword(#lexer::Keyword::#id) },
		Token::Operator(_) => quote! { #lexer::Token::Operator(#lexer::Operator::#id) },
		Token::Punct(_) => quote! { #lexer::Token::Punct(#lexer::Punct::#id) },
		Token::Begin(_) => quote! { #lexer::Token::Begin(#lexer::Delimiter::#id) },
		Token::End(_) => quote! { #lexer::Token::End(#lexer::Delimiter::#id) },
		Token::Anonymous(_, c) | Token::Composed(_, c) | Token::Named(_, c) => {
			if c.is_some() {
				quote! { #lexer :: Token :: #id (#param) }
			} else {
				quote! { #lexer :: Token :: #id }
			}
		}
	}
}

fn generate_next_token_function(
	source_span_crate: &SourceSpanCrate,
	extern_mod: &ExternModule,
	scope: &rust_codegen::Scope,
	grammar: &Grammar,
	table: &Table,
	automaton: &DetAutomaton<DetState>,
	default_token: bool,
) -> TokenStream {
	use rust_codegen::Instance;
	let loc_path = source_span_crate.loc_struct.path(scope);

	// let automaton = table.root();
	let mut id_table = HashMap::new();

	let init_id = state_id(&mut id_table, automaton.initial_state());

	let consume_char = if default_token {
		None
	} else {
		Some(quote! { self.consume_char()?; })
	};

	let states = automaton.transitions().iter().map(|(q, transitions)| {
		let mut inverse: BTreeMap<
			&crate::lexing::DetState,
			BTreeSet<btree_range_map::AnyRange<char>>,
		> = BTreeMap::new();

		for (range, target) in transitions {
			if inverse.contains_key(target) {
				let ranges = inverse.get_mut(target).unwrap();
				ranges.insert(range.clone());
			} else {
				let mut ranges = BTreeSet::new();
				ranges.insert(range.clone());
				inverse.insert(target, ranges);
			}
		}

		let id = state_id(&mut id_table, q);

		let cases = inverse.into_iter().map(|(target, ranges)| {
			let rust_ranges = rust_ranges(&ranges);
			let target_id = state_id(&mut id_table, target);

			quote! {
				#rust_ranges => {
					#consume_char
					state = #target_id;
					continue
				}
			}
		});

		let default_case = match q {
			DetState::Final(token_id, _) => {
				let loc_path = source_span_crate.loc_struct.path(scope);
				let terminal = grammar.terminal(*token_id).unwrap();
				let output_token = match terminal.desc() {
					terminal::Desc::Whitespace(_) => {
						quote! {
							continue 'next_token
						}
					}
					terminal::Desc::RegExp(_) => {
						let token = terminal.token().unwrap();
						let path = token_path(
							source_span_crate,
							extern_mod,
							scope,
							token,
							quote! { self.buffer.as_str() },
						);

						quote! {
							return Ok(Some(#loc_path::new(#path, self.span)))
						}
					}
				};

				match table.sub_automaton(*token_id) {
					Some(sub_automaton) => {
						let body = generate_next_token_function(
							source_span_crate,
							extern_mod,
							scope,
							grammar,
							table,
							sub_automaton,
							true,
						);
						quote! {
							#body
							#output_token
						}
					}
					None => output_token,
				}
			}
			_ => {
				if default_token {
					quote! { break }
				} else {
					let extern_mod_path = extern_mod.path(scope);
					quote! {
						return Err(#loc_path::new(#extern_mod_path::unexpected(next_c), self.span.last().into()))
					}
				}
			}
		};

		let eos_case = match q {
			DetState::Initial => quote! { { return Ok(None) } },
			_ => quote! { () },
		};

		let next_char = if default_token {
			quote! { chars.next() }
		} else {
			quote! { self.peek_char()? }
		};

		quote! {
			#id => {
				let next_c = #next_char;
				match next_c {
					Some(c) => match c {
						#(#cases)*
						_ => ()
					}
					None => #eos_case
				}
				#default_case
			}
		}
	});

	let automaton_loop = quote! {
		let mut state = #init_id;

		loop {
			match state {
				#(#states)*,
				_ => unreachable!()
			}
		}
	};

	if default_token {
		quote! {
			let mut chars = self.buffer.chars();
			#automaton_loop
		}
	} else {
		quote! {
			'next_token: loop {
				self.buffer.clear();
				self.span.clear();
				#automaton_loop
			}
		}
	}
}

fn token_class_name(class: &token::Class) -> String {
	match class {
		token::Class::Anonymous(i, _) => {
			format!("Token{}", i)
		}
		token::Class::Composed(names, _) => {
			let mut name = String::new();
			for n in names {
				name.push_str(&util::upcase_to_caml_case(n))
			}
			name
		}
		token::Class::Named(name, _) => util::upcase_to_caml_case(name.as_str()),
		token::Class::Keyword => "Keyword".to_string(),
		token::Class::Operator => "Operator".to_string(),
		token::Class::Punct => "Punct".to_string(),
		token::Class::Begin => "Begin".to_string(),
		token::Class::End => "End".to_string(),
	}
}

// fn token_class_ident(class: &token::Class) -> proc_macro2::Ident {
// 	proc_macro2::Ident::new(&token_class_name(class), proc_macro2::Span::call_site())
// }

fn conversion_target_type(
	extern_mod: &ExternModule,
	c: Option<&token::Convertion>,
) -> Option<rust_codegen::ty::Instance> {
	c.map(|c| extern_mod.extern_type(c.target).unwrap().instanciate())
}

fn token_class_parameter(
	extern_mod: &ExternModule,
	keyword_enum: &Option<rust_codegen::enm::Ref>,
	operator_enum: &Option<rust_codegen::enm::Ref>,
	punct_enum: &Option<rust_codegen::enm::Ref>,
	delimiter_enum: &Option<rust_codegen::enm::Ref>,
	class: &token::Class,
) -> Option<rust_codegen::ty::Instance> {
	match class {
		token::Class::Anonymous(_, c) => conversion_target_type(extern_mod, c.as_ref()),
		token::Class::Composed(_, c) => conversion_target_type(extern_mod, c.as_ref()),
		token::Class::Named(_, c) => conversion_target_type(extern_mod, c.as_ref()),
		token::Class::Keyword => keyword_enum.as_ref().map(|e| e.instanciate()),
		token::Class::Operator => operator_enum.as_ref().map(|e| e.instanciate()),
		token::Class::Punct => punct_enum.as_ref().map(|e| e.instanciate()),
		token::Class::Begin => delimiter_enum.as_ref().map(|e| e.instanciate()),
		token::Class::End => delimiter_enum.as_ref().map(|e| e.instanciate()),
	}
}

// fn token_class_variant(class: &token::Class) -> proc_macro2::TokenStream {
// 	let ident = token_class_ident(class);
// 	let param = token_class_parameter(class).map(|p| quote!{ (#p) });
// 	quote! {
// 		#ident #param
// 	}
// }

fn token_name(token: &Token) -> String {
	match token {
		Token::Named(name, _) => util::upcase_to_caml_case(name.as_str()),
		Token::Keyword(kw) => util::upcase_to_caml_case(kw),
		Token::Begin(b) => util::upcase_to_caml_case(b.name()),
		Token::End(e) => util::upcase_to_caml_case(e.name()),
		Token::Operator(o) => util::upcase_to_caml_case(o.name()),
		Token::Punct(p) => util::upcase_to_caml_case(p.name()),
		Token::Composed(names, _) => {
			let mut name = String::new();
			for n in names {
				name.push_str(&util::upcase_to_caml_case(n))
			}
			name
		}
		Token::Anonymous(i, _) => {
			format!("Token{}", i)
		}
	}
}

fn token_ident(token: &Token) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&token_name(token), proc_macro2::Span::call_site())
}

fn token_path(
	source_span_crate: &SourceSpanCrate,
	extern_mod: &ExternModule,
	scope: &rust_codegen::Scope,
	token: &Token,
	parameter: TokenStream,
) -> TokenStream {
	match token {
		Token::Keyword(_) => {
			let id = token_ident(token);
			quote! { Token::Keyword(Keyword::#id) }
		}
		Token::Punct(_) => {
			let id = token_ident(token);
			quote! { Token::Punct(Punct::#id) }
		}
		Token::Operator(_) => {
			let id = token_ident(token);
			quote! { Token::Operator(Operator::#id) }
		}
		Token::Begin(_) => {
			let id = token_ident(token);
			quote! { Token::Begin(Delimiter::#id) }
		}
		Token::End(_) => {
			let id = token_ident(token);
			quote! { Token::End(Delimiter::#id) }
		}
		Token::Anonymous(_, c) | Token::Named(_, c) | Token::Composed(_, c) => {
			let parameter = c.as_ref().map(|c| {
				let converter = extern_mod.converter_path(scope, c);
				use rust_codegen::Instance;
				let loc_path = source_span_crate.loc_struct.path(scope);
				quote! { ( #converter ( #parameter ).map_err(|e| #loc_path::new(e, self.span))? ) }
			});
			let id = token_ident(token);
			quote! { Token::#id #parameter }
		}
	}
}

// /// Rust token reference.
// pub enum TokenRef {
// 	/// Operator token.
// 	///
// 	/// The parameter gives the index of the variant in the `Operator` enum.
// 	Operator(usize),

// 	/// Punctuation token.
// 	///
// 	/// The parameter gives the index of the variant in the `Punct` enum.
// 	Punct(usize),

// 	/// Group begin token.
// 	///
// 	/// The parameter gives the index of the variant in the `Delimiter` enum.
// 	Begin(usize),

// 	/// Group end token.
// 	///
// 	/// The parameter gives the index of the variant in the `Delimiter` enum.
// 	End(usize),

// 	/// Other token.
// 	///
// 	/// The parameter gives the index of the variant in the `Token` enum.
// 	Other(usize, bool)
// }

fn generate_tokens(
	grammar: &Grammar,
	extern_mod: &ExternModule,
	module: &mut rust_codegen::Module,
) -> rust_codegen::enm::Ref {
	let mut token_keywords = BTreeSet::new();
	let mut token_operators = BTreeSet::new();
	let mut token_puncts = BTreeSet::new();
	let mut token_delimiters = BTreeSet::new();
	let mut token_classes = BTreeSet::new();

	// First we collect the different token classes.
	for (terminal, _) in grammar.terminals() {
		if let Some(token) = terminal.token() {
			match token {
				Token::Keyword(k) => {
					token_keywords.insert(k.clone());
				}
				Token::Operator(o) => {
					token_operators.insert(*o);
				}
				Token::Punct(p) => {
					token_puncts.insert(*p);
				}
				Token::Begin(d) => {
					token_delimiters.insert(*d);
				}
				Token::End(d) => {
					token_delimiters.insert(*d);
				}
				_ => (),
			}

			token_classes.insert(token.class());
		}
	}

	// Create the `Keyword` enum if necessary.
	let mut keyword_map = HashMap::new();
	let keyword_enum_ref = if token_keywords.is_empty() {
		None
	} else {
		let enm_ref = module.add_enum("Keyword");
		{
			let mut enm = enm_ref.borrow_mut();
			enm.set_public();
			for k in token_keywords.into_iter() {
				let name = util::to_caml_case(k.as_str());
				let variant = rust_codegen::enm::Variant::new(name);
				keyword_map.insert(k, keyword_map.len());
				enm.add(variant)
			}
		}
		Some(enm_ref)
	};

	// Create the `Operator` enum if necessary.
	let mut operator_map = HashMap::new();
	let operator_enum_ref = if token_operators.is_empty() {
		None
	} else {
		let enm_ref = module.add_enum("Operator");
		{
			let mut enm = enm_ref.borrow_mut();
			enm.set_public();
			for o in token_operators.into_iter() {
				let variant = rust_codegen::enm::Variant::new(o.name());
				operator_map.insert(o, operator_map.len());
				enm.add(variant)
			}
		}
		Some(enm_ref)
	};

	// Create the `Punct` enum if necessary.
	let mut punct_map = HashMap::new();
	let punct_enum_ref = if token_puncts.is_empty() {
		None
	} else {
		let enm_ref = module.add_enum("Punct");
		{
			let mut enm = enm_ref.borrow_mut();
			enm.set_public();
			for p in token_puncts.into_iter() {
				let variant = rust_codegen::enm::Variant::new(p.name());
				punct_map.insert(p, punct_map.len());
				enm.add(variant)
			}
		}
		Some(enm_ref)
	};

	// Create the `Delimiter` enum if necessary.
	let mut delimiter_map = HashMap::new();
	let delimiter_enum_ref = if token_delimiters.is_empty() {
		None
	} else {
		let enm_ref = module.add_enum("Delimiter");
		{
			let mut enm = enm_ref.borrow_mut();
			enm.set_public();
			for d in token_delimiters.into_iter() {
				let variant = rust_codegen::enm::Variant::new(d.name());
				delimiter_map.insert(d, delimiter_map.len());
				enm.add(variant)
			}
		}
		Some(enm_ref)
	};

	// Create the `Token` enum.
	let token_enum_ref = module.add_enum("Token");

	{
		let mut token_enum = token_enum_ref.borrow_mut();
		token_enum.set_public();
		let mut class_map = HashMap::new();
		for c in token_classes.into_iter() {
			let name = token_class_name(&c);
			let mut variant = rust_codegen::enm::Variant::new(name);

			if let Some(p) = token_class_parameter(
				extern_mod,
				&keyword_enum_ref,
				&operator_enum_ref,
				&punct_enum_ref,
				&delimiter_enum_ref,
				&c,
			) {
				variant.add_param(p)
			}

			class_map.insert(c, class_map.len());
			token_enum.add(variant)
		}

		// // Map each token to a Rust expression.
		// let mut token_map = HashMap::new();
		// for (i, (terminal, _)) in grammar.terminals().iter().enumerate() {
		// 	let i = i as u32;

		// 	if let Some(token) = terminal.token() {
		// 		let token_ref = match token {
		// 			Token::Operator(o) => {
		// 				let index = operator_map.get(o).unwrap();
		// 				TokenRef::Operator(*index)
		// 			},
		// 			Token::Punct(p) => {
		// 				let index = punct_map.get(p).unwrap();
		// 				TokenRef::Punct(*index)
		// 			},
		// 			Token::Begin(d) => {
		// 				let index = delimiter_map.get(d).unwrap();
		// 				TokenRef::Begin(*index)
		// 			},
		// 			Token::End(d) => {
		// 				let index = delimiter_map.get(d).unwrap();
		// 				TokenRef::End(*index)
		// 			},
		// 			_ => {
		// 				let c = token.class();
		// 				let index = class_map.get(&c).unwrap();
		// 				TokenRef::Other(*index, c.has_parameter())
		// 			}
		// 		};

		// 		token_map.insert(i, token_ref);
		// 	}
		// }
	}

	token_enum_ref
}

fn state_id<'a>(table: &mut HashMap<&'a DetState, usize>, q: &'a DetState) -> usize {
	match table.get(&q) {
		Some(id) => *id,
		None => {
			let id = table.len();
			table.insert(q, id);
			id
		}
	}
}

fn included_start_bound(bound: Bound<&char>) -> char {
	match bound {
		Bound::Included(a) => *a,
		Bound::Excluded(a) => a.succ().unwrap(),
		Bound::Unbounded => PartialEnum::MIN,
	}
}

fn included_end_bound(bound: Bound<&char>) -> char {
	match bound {
		Bound::Included(a) => *a,
		Bound::Excluded(a) => a.pred().unwrap(),
		Bound::Unbounded => PartialEnum::MAX,
	}
}

fn rust_ranges(ranges: &BTreeSet<btree_range_map::AnyRange<char>>) -> TokenStream {
	let mut tokens = Vec::new();

	for range in ranges {
		tokens.push(rust_range(range))
	}

	quote! { #(#tokens)|* }
}

fn rust_range(range: &btree_range_map::AnyRange<char>) -> TokenStream {
	let a = included_start_bound(range.start_bound());
	let b = included_end_bound(range.end_bound());

	if a == b {
		quote! { #a }
	} else {
		quote! { #a ..= #b }
	}
}
