use proc_macro2::TokenStream;
use quote::quote;
use crate::gen::pseudo;
use super::{
	Target,
	Generate
};

/// Rust target.
pub struct Rust;

impl Target for Rust {
	type Output = TokenStream;
}

impl Generate<pseudo::module::Id> for Rust {
	fn generate(&self, context: &pseudo::Context, id: &pseudo::module::Id) -> TokenStream {
		match id {
			pseudo::module::Id::Root => quote! { crate },
			pseudo::module::Id::Named(name) => {
				let id = quote::format_ident!("{}", crate::util::to_snake_case(&name));
				quote! { #id }
			}
		}
	}
}

impl Generate<pseudo::Module> for Rust {
	fn generate(&self, context: &pseudo::Context, module: &pseudo::Module) -> TokenStream {
		let mut tokens = TokenStream::new();
		
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
			#(#submodules)*
		}
	}
}