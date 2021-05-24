use proc_macro2::TokenStream;
use quote::quote;
use crate::{
	util,
	mono::{
		Grammar,
		ty,
		Type,
		Function
	}
};
use super::external_type;

pub fn generate(grammar: &Grammar) -> TokenStream {
	let mut tokens = TokenStream::new();
	
	for ty in grammar.types() {
		tokens.extend(generate_type(grammar, &ty))
	}

	tokens
}

fn type_name(ty: &Type) -> String {
	util::to_caml_case(ty.id().name())
}

fn type_ident(ty: &Type) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&type_name(ty), proc_macro2::Span::call_site())
}

fn variant_name(f: &Function) -> String {
	util::to_caml_case(f.id().as_str())
}

fn variant_ident(f: &Function) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&variant_name(f), proc_macro2::Span::call_site())
}

fn generate_type(grammar: &Grammar, ty: &Type) -> TokenStream {
	let id = type_ident(ty);
	let mut variants = Vec::new();

	for c in ty.constructors() {
		let f = grammar.function(c).unwrap();
		let f_id = variant_ident(f.as_ref());
		let mut args = Vec::new();

		for a in f.arguments() {
			match a {
				ty::Expr::Terminal(index) => {
					let t = grammar.terminal(*index).unwrap();
					if let Some(exp_index) = t.as_regexp_ref() {
						let exp = grammar.regexp(exp_index).unwrap();
						if let Some(tokens) = external_type(&exp.ty) {
							args.push(tokens)
						}
					}
				},
				ty::Expr::Type(index) => {
					let arg_ty = *grammar.ty(*index).unwrap().as_ref();
					let arg_ty_id = type_ident(&arg_ty);

					args.push(if std::ptr::eq(arg_ty, ty) {
						quote! { Box<#arg_ty_id> }
					} else {
						quote! { Box<#arg_ty_id> }
					})
				}
			}
		}

		let args = if !args.is_empty() {
			Some(quote! { (#(#args),*) })
		} else {
			None
		};

		variants.push(quote! {
			#f_id #args
		})
	}

	quote! {
		pub enum #id {
			#(#variants),*
		}
	}
}