use proc_macro2::TokenStream;
use quote::quote;
use range_map::PartialEnum;
use std::{
	collections::HashMap,
	ops::{
		Bound,
		RangeBounds
	}
};
use crate::lexing::{
	State,
	DetState,
	Table
};

pub fn generate(table: &Table) -> TokenStream {
	let variants: Vec<TokenStream> = Vec::new();

	let automaton = table.automaton().determinize();
	let mut id_table = HashMap::new();

	let init_id = state_id(&mut id_table, automaton.initial_state());

	let states = automaton.transitions().iter().map(|(q, transitions)| {
		let id = state_id(&mut id_table, q);

		let cases = transitions.iter().map(|(range, target)| {
			let a = included_start_bound(range.start_bound());
			let b = included_end_bound(range.end_bound());
			let target_id = state_id(&mut id_table, target);

			quote! {
				#a ..= #b => {
					state = #target_id
				}
			}
		});

		let default_case = if let Some(State::Final(token_id)) = q.iter().find(|q| q.is_final()) {
			let variant_id = proc_macro2::Ident::new(&to_caml_case(token_id.as_str()), proc_macro2::Span::call_site());

			quote! {
				return Some(Ok(Token::#variant_id))
			}
		} else {
			quote! {
				return Some(Err(Error::Unexpected(c)))
			}
		};

		quote! {
			#id => {
				match self.peek_char()? {
					Some(c) => match c {
						#(#cases)*,
						_ => {
							#default_case
						}
					},
					_ => {
						#default_case
					}
				}
			}
		}
	});

	quote! {
		pub enum Token {
			#(#variants),*
		}

		struct Lexer<I> {
			chars: Peekable<I>,
			buffer: String
		}

		impl<E, I: Iterator<Item=Result<char, E>>> Lexer<I> {
			fn next_char(&mut self) -> Result<Option<char>, E> {
				match self.chars.next() {
					Some(Ok(c)) => Ok(Some(c)),
					Some(Err(e)) => Err(e),
					None => Ok(None)
				}
			}

			fn peek_char(&mut self) -> Result<Option<char>, E> {
				match self.chars.peek() {
					Some(Ok(c)) => Ok(Some(*c)),
					Some(Err(_)) => self.next_char(),
					None => None
				}
			}
		}

		impl<E, I: Iterator<Item=Result<char, E>>> Iterator for Lexer<I> {
			type Item = Result<Loc<Token>, E>;

			fn next(&mut self) -> Option<Result<Loc<Token>, E>> {
				let state = #init_id;
				
				loop {
					match state {
						#(#states)*
					}
				}
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
		Bound::Unbounded => range_map::PartialEnum::MIN
	}
}

fn included_end_bound(bound: Bound<&char>) -> char {
	match bound {
		Bound::Included(a) => *a,
		Bound::Excluded(a) => a.pred().unwrap(),
		Bound::Unbounded => range_map::PartialEnum::MAX
	}
}

pub fn to_caml_case(s: &str) -> String {
	let mut id = String::new();
	let mut up = true;

	for c in s.chars() {
		match c {
			'_' | ' ' | '-' => {
				up = true;
			},
			c if up => {
				up = false;
				id.push(c.to_uppercase().next().unwrap());
			},
			c => {
				id.push(c);
			}
		}
	}

	id
}

// pub fn to_snake_case(s: &str) -> String {
// 	let mut id = String::new();
// 	for c in s.chars() {
// 		match c {
// 			' ' | '-' => {
// 				if !id.is_empty() {
// 					id.push('_');
// 				}
// 			},
// 			c if c.is_uppercase() => {
// 				if !id.is_empty() {
// 					id.push('_');
// 				}

// 				id.push(c.to_lowercase().next().unwrap());
// 			},
// 			c => {
// 				id.push(c)
// 			}
// 		}
// 	}

// 	id
// }