use proc_macro2::TokenStream;
use quote::quote;
use crate::{
	util,
	mono::{
		ExternalType
	}
};

pub mod lexer;
pub mod ast;
pub mod lr0;

fn external_type(ty: &ExternalType) -> Option<TokenStream> {
	match ty {
		ExternalType::Unit => None,
		ExternalType::Custom(name) => {
			let id = proc_macro2::Ident::new(&util::to_caml_case(name.as_str()), proc_macro2::Span::call_site());
			Some(quote! { #id })
		}
	}
}