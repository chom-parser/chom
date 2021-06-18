use proc_macro2::TokenStream;
use quote::quote;
use btree_range_map::util::PartialEnum;
use std::{
	collections::{
		HashMap,
		BTreeSet,
		BTreeMap
	},
	ops::{
		Bound,
		RangeBounds
	}
};
use crate::{
	util,
	mono::{
		Grammar,
		terminal,
		ExternalType
	},
	lexing::{
		token,
		Token,
		DetState,
		Table,
	}
};
use super::ExternModule;

pub struct Module {
	inner: rust_codegen::module::Ref,
	token_enum: rust_codegen::enm::Ref
}

impl Module {
	pub fn new(
		grammar: &Grammar,
		extern_mod: &ExternModule,
		module_ref: rust_codegen::module::Ref
	) -> Self {
		let token_enum;

		{
			let mut module = module_ref.borrow_mut();
			token_enum = module.add_enum("Token");
		}

		Self {
			inner: module_ref,
			token_enum
		}
	}

	pub fn token_ty(&self) -> &rust_codegen::enm::Ref {
		&self.token_enum
	}
}

fn token_class_name(class: &token::Class) -> String {
	match class {
		token::Class::Anonymous(i, _) => {
			format!("Token{}", i)
		},
		token::Class::Composed(names, _) => {
			let mut name = String::new();
			for n in names {
				name.push_str(&util::upcase_to_caml_case(n))
			}
			name
		},
		token::Class::Named(name, _) => util::upcase_to_caml_case(name.as_str()),
		token::Class::Keyword => "Keyword".to_string(),
		token::Class::Operator => "Operator".to_string(),
		token::Class::Punct => "Punct".to_string(),
		token::Class::Begin => "Begin".to_string(),
		token::Class::End => "End".to_string()
	}
}

fn token_class_ident(class: &token::Class) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&token_class_name(class), proc_macro2::Span::call_site())
}

fn external_type(ty: &ExternalType) -> Option<TokenStream> {
	match ty {
		ExternalType::Unit => None,
		ExternalType::Custom(name) => {
			let id = proc_macro2::Ident::new(&util::to_caml_case(name.as_str()), proc_macro2::Span::call_site());
			Some(quote! { #id })
		}
	}
}

fn conversion_target_type(c: Option<&token::Convertion>) -> Option<TokenStream> {
	c.map(|c| external_type(&c.target).map(|ty| quote! { T::#ty })).flatten()
}

fn token_class_parameter(class: &token::Class) -> Option<proc_macro2::TokenStream> {
	match class {
		token::Class::Anonymous(_, c) => conversion_target_type(c.as_ref()),
		token::Class::Composed(_, c) => conversion_target_type(c.as_ref()),
		token::Class::Named(_, c) => conversion_target_type(c.as_ref()),
		token::Class::Keyword => Some(quote! { Keyword }),
		token::Class::Operator => Some(quote! { Operator }),
		token::Class::Punct => Some(quote! { Punct }),
		token::Class::Begin => Some(quote! { Separator }),
		token::Class::End => Some(quote! { Separator })
	}
}

fn token_class_variant(class: &token::Class) -> proc_macro2::TokenStream {
	let ident = token_class_ident(class);
	let param = token_class_parameter(class).map(|p| quote!{ (#p) });
	quote! {
		#ident #param
	}
}

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
		},
		Token::Anonymous(i, _) => {
			format!("Token{}", i)
		}
	}
}

fn token_ident(token: &Token) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&token_name(token), proc_macro2::Span::call_site())
}

fn converter_name(c: &token::Convertion) -> String {
	util::upcase_to_snake_case(c.from.as_str())
}

fn converter_ident(c: &token::Convertion) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&converter_name(c), proc_macro2::Span::call_site())
}

fn converter_path(c: &token::Convertion) -> TokenStream {
	let id = converter_ident(c);
	quote!{ T::#id }
}

fn token_path(token: &Token, parameter: TokenStream) -> TokenStream {
	match token {
		Token::Keyword(_) => {
			let id = token_ident(token);
			quote! { Token::Keyword(Keyword::#id) }
		},
		Token::Punct(_) => {
			let id = token_ident(token);
			quote! { Token::Punct(Punct::#id) }
		},
		Token::Operator(_) => {
			let id = token_ident(token);
			quote! { Token::Operator(Operator::#id) }
		},
		Token::Begin(_) => {
			let id = token_ident(token);
			quote! { Token::Begin(Delimiter::#id) }
		},
		Token::End(_) => {
			let id = token_ident(token);
			quote! { Token::End(Delimiter::#id) }
		},
		Token::Anonymous(_, c) | Token::Named(_, c) | Token::Composed(_, c) => {
			let parameter = c.as_ref().map(|c| {
				let converter = converter_path(c);
				quote! { ( #converter ( #parameter ).map_err(|e| Loc::new(e, self.span))? ) }
			});
			let id = token_ident(token);
			quote! { Token::#id #parameter }
		}
	}
}

pub fn generate(grammar: &Grammar, table: &Table) -> TokenStream {
	let mut token_operators = BTreeSet::new();
	let mut token_puncts = BTreeSet::new();
	let mut token_delimiters = BTreeSet::new();
	let mut token_classes = BTreeSet::new();
	for (terminal, _) in grammar.terminals() {
		if let Some(token) = terminal.token() {
			match token {
				Token::Operator(o) => { token_operators.insert(*o); },
				Token::Punct(p) => { token_puncts.insert(*p); },
				Token::Begin(d) => { token_delimiters.insert(*d); },
				Token::End(d) => { token_delimiters.insert(*d); },
				_ => ()
			}
	
			token_classes.insert(token.class());
		}
	}

	let mut variants: Vec<TokenStream> = Vec::new();
	for class in &token_classes {
		variants.push(token_class_variant(class))
	}

	let mut external_types = Vec::new();
	for (ty, _) in grammar.extern_types() {
		if *ty != ExternalType::Unit {
			let id = external_type(ty);
			external_types.push(quote! {
				type #id;
			})
		}
	}

	let (token_type_param, token_type_param_def) = if external_types.is_empty() {
		(None, None)
	} else {
		(Some(quote!{ <T> }), Some(quote!{ <T: Interface> }))
	};

	let mut converters: Vec<TokenStream> = Vec::new();
	for (def, _) in grammar.regexps() {
		let ty = grammar.extern_type(def.ty).unwrap();
		if let Some(c) = token::Convertion::new_opt(&def.id, ty) {
			let id = converter_ident(&c);
			let target_ty = external_type(&c.target);
			converters.push(quote! {
				fn #id (token: &str) -> Result<Self::#target_ty, Self::Error>;
			})
		}
	}

	let automaton = table.automaton();
	let mut id_table = HashMap::new();

	let init_id = state_id(&mut id_table, automaton.initial_state());

	let states = automaton.transitions().iter().map(|(q, transitions)| {

		let mut inverse: BTreeMap<&crate::lexing::DetState, BTreeSet<btree_range_map::AnyRange<char>>> = BTreeMap::new();

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
					state = #target_id
				}
			}
		});

		let default_case = match q {
			DetState::Final(token_id, _) => {
				let terminal = &grammar.terminals()[*token_id as usize].0;

				match terminal.desc() {
					terminal::Desc::Whitespace => {
						quote! {
							state = #init_id;
							self.buffer.clear();
							self.span.clear();
						}
					},
					terminal::Desc::RegExp(_) => {
						let token = terminal.token().unwrap();
						let path = token_path(token, quote!{ self.buffer.as_str() });

						quote! {
							break Ok(Some(Loc::new(#path, self.span)))
						}
					}
				}
			},
			_ => {
				quote! {
					break Err(Loc::new(T::unexpected(c), self.span.last().into()))
				}
			}
		};

		let eos_case = match q {
			DetState::Initial => {
				quote! { break Ok(None) }
			},
			_ => default_case.clone()
		};

		quote! {
			#id => {
				match self.next_char()? {
					Some(c) => match c {
						#(#cases)*
						_ => {
							#default_case
						}
					},
					None => {
						#eos_case
					}
				}
			}
		}
	});

	let enum_operator = if token_operators.is_empty() {
		None
	} else {
		Some(quote! {
			pub enum Operator {
				#(#token_operators),*
			}
		})
	};

	let enum_delimiter = if token_delimiters.is_empty() {
		None
	} else {
		Some(quote! {
			pub enum Delimiter {
				#(#token_delimiters),*
			}
		})
	};

	let enum_punct = if token_puncts.is_empty() {
		None
	} else {
		Some(quote! {
			pub enum Punct {
				#(#token_puncts),*
			}
		})
	};

	quote! {
		use core::{
			iter::Peekable,
			marker::PhantomData
		};
		use source_span::{
			Metrics,
			Span,
			Loc
		};

		#enum_operator
		#enum_delimiter
		#enum_punct

		pub enum Token #token_type_param_def {
			#(#variants),*
		}

		pub trait Interface {
			type Error;

			fn unexpected(c: Option<char>) -> Self::Error;

			#(#external_types)*

			#(#converters)*
		}

		pub struct Lexer<I: Iterator, M, T: Interface> {
			/// Char stream.
			chars: Peekable<I>,

			/// Token data buffer.
			buffer: String,

			/// Character metrics.
			metrics: M,

			/// Token span.
			span: Span,

			/// Glue.
			t: PhantomData<T>
		}

		impl<E, I: Iterator<Item=Result<char, E>>, M: Metrics, T: Interface> Lexer<I, M, T> where E: Into<T::Error> {
			fn next_char(&mut self) -> Result<Option<char>, Loc<T::Error>> {
				match self.chars.next() {
					Some(Ok(c)) => {
						self.buffer.push(c);
						self.span.push(c, &self.metrics);
						Ok(Some(c))
					},
					Some(Err(e)) => Err(Loc::new(e.into(), self.span.end().into())),
					None => Ok(None)
				}
			}

			pub fn next_token(&mut self) -> Result<Option<Loc<Token #token_type_param>>, Loc<T::Error>> {
				let mut state = #init_id;
				self.buffer.clear();
				self.span.clear();
				
				loop {
					match state {
						#(#states)*,
						_ => unreachable!()
					}
				}
			}
		}

		impl<E, I: Iterator<Item=Result<char, E>>, M: Metrics, T: Interface> Iterator for Lexer<I, M, T> where E: Into<T::Error> {
			type Item = Result<Loc<Token #token_type_param>, Loc<T::Error>>;

			fn next(&mut self) -> Option<Self::Item> {
				self.next_token().transpose()
			}
		}
	}
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
		Bound::Unbounded => PartialEnum::MIN
	}
}

fn included_end_bound(bound: Bound<&char>) -> char {
	match bound {
		Bound::Included(a) => *a,
		Bound::Excluded(a) => a.pred().unwrap(),
		Bound::Unbounded => PartialEnum::MAX
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