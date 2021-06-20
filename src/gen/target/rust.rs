use std::collections::HashMap;
use crate::{
	util,
	mono::{
		Grammar,
		ExternalType
	},
	lexing
};

pub mod lexer;
pub mod ast;
pub mod lr0;

pub struct Target {
	context: rust_codegen::Context,
	std_crate: StdCrate,
	source_span_crate: SourceSpanCrate,
	extern_mod: ExternModule
}

impl Target {
	pub fn new(grammar: &Grammar, extern_module_path: &[String]) -> Self {
		let mut context = rust_codegen::Context::new();

		let std_crate = StdCrate::new(&mut context);
		let source_span_crate = SourceSpanCrate::new(&mut context);
		let extern_mod = ExternModule::new(&context, grammar, extern_module_path);

		Self {
			context,
			std_crate,
			source_span_crate,
			extern_mod
		}
	}

	pub fn generate_ast(&self, grammar: &Grammar, path: &[String]) -> ast::Module {
		ast::Module::new(&self.context, grammar, &self.extern_mod, path)
	}

	pub fn generate_lexer(&self, grammar: &Grammar, lexing_table: &lexing::Table, path: &[String]) -> lexer::Module {
		lexer::Module::new(&self.context, grammar, lexing_table, &self.std_crate, &self.source_span_crate, &self.extern_mod, path)
	}

	pub fn generate_lr0_parser(&self, grammar: &Grammar, ast_mod: &ast::Module, lexer_mod: &lexer::Module, table: &crate::parsing::table::LR0, path: &[String]) -> lr0::Module {
		lr0::Module::new(&self.context, &self.std_crate, &self.source_span_crate, lexer_mod, ast_mod, grammar, table, path)
	}
}

fn declare_module(context: &rust_codegen::Context, path: &[String]) -> rust_codegen::module::Ref {
	let mut module_ref: Option<rust_codegen::module::Ref> = None;

	for id in path {
		module_ref = Some(match module_ref.take() {
			Some(parent) => parent.borrow_mut().add_submodule(id.as_str()),
			None => {
				let crte = context.root_crate();
				let mut crte = crte.borrow_mut();
				crte.add_module(id.as_str())
			}
		});
	}

	module_ref.unwrap()
}

pub struct StdCrate {
	pub into_trait: rust_codegen::tr::Ref,
	pub iterator_trait: rust_codegen::tr::Ref,
	pub peekable_struct: rust_codegen::strct::Ref,
	pub option_enum: rust_codegen::enm::Ref,
	pub result_enum: rust_codegen::enm::Ref,
	pub string_struct: rust_codegen::strct::Ref,
}

impl StdCrate {
	fn new(context: &mut rust_codegen::Context) -> Self {
		let into_trait;
		let iterator_trait;
		let peekable_struct;
		let result_enum;
		let option_enum;
		let string_struct;
		let crte_ref = context.add_extern_crate("std");
		
		{
			let mut crte = crte_ref.borrow_mut();

			let std_convert_mod_ref = crte.add_module("convert");
			let mut std_convert_mod = std_convert_mod_ref.borrow_mut();
			into_trait = std_convert_mod.add_trait("Into");

			let std_iter_mod_ref = crte.add_module("iter");
			let mut std_iter_mod = std_iter_mod_ref.borrow_mut();
			iterator_trait = std_iter_mod.add_trait("Iterator");
			let mut iterator_trait = iterator_trait.borrow_mut();
			iterator_trait.add_associated_type("Item");
			peekable_struct = std_iter_mod.add_struct("Peekable");

			let std_option_mod_ref = crte.add_module("option");
			let mut std_option_mod = std_option_mod_ref.borrow_mut();
			option_enum = std_option_mod.add_enum("Option");

			let std_result_mod_ref = crte.add_module("result");
			let mut std_result_mod = std_result_mod_ref.borrow_mut();
			result_enum = std_result_mod.add_enum("Result");

			let std_string_mod_ref = crte.add_module("string");
			let mut std_string_mod = std_string_mod_ref.borrow_mut();
			string_struct = std_string_mod.add_struct("String");
		}

		Self {
			into_trait,
			iterator_trait,
			peekable_struct,
			result_enum,
			option_enum,
			string_struct
		}
	}
}

pub struct SourceSpanCrate {
	pub span_struct: rust_codegen::strct::Ref,
	pub loc_struct: rust_codegen::strct::Ref,
	pub metrics_trait: rust_codegen::tr::Ref
}

impl SourceSpanCrate {
	fn new(context: &mut rust_codegen::Context) -> Self {
		let span_struct;
		let loc_struct;
		let metrics_trait;
		let crte_ref = context.add_extern_crate("source_span");
		
		{
			let mut crte = crte_ref.borrow_mut();
			span_struct = crte.add_struct("Span");
			loc_struct = crte.add_struct("Loc");
			metrics_trait = crte.add_trait("Metrics");
		}

		Self {
			span_struct,
			loc_struct,
			metrics_trait
		}
	}
}

/// Rust module defining external types.
pub struct ExternModule {
	inner: rust_codegen::module::Ref,
	types: HashMap<u32, rust_codegen::strct::Ref>,
	error_type: rust_codegen::strct::Ref
}

fn converter_name(c: &lexing::token::Convertion) -> String {
	util::upcase_to_snake_case(c.from.as_str())
}

fn converter_ident(c: &lexing::token::Convertion) -> proc_macro2::Ident {
	proc_macro2::Ident::new(&converter_name(c), proc_macro2::Span::call_site())
}

impl ExternModule {
	pub fn new(context: &rust_codegen::Context, grammar: &Grammar, path: &[String]) -> Self {
		let module_ref = declare_module(context, path);

		let mut types = HashMap::new();
		let error_type;

		{
			let mut module = module_ref.borrow_mut();

			for (i, (ty, _)) in grammar.extern_types().iter().enumerate() {
				match ty {
					ExternalType::Unit => (),
					ExternalType::Custom(name) => {
						let id = util::to_caml_case(name.as_str());
						types.insert(i as u32, module.add_struct(id));
					}
				}
			}

			for (def, _) in grammar.regexps() {
				if let Some(c) = lexing::token::Convertion::new_opt(grammar.poly(), &def.id, def.ty) {
					let id = converter_name(&c);
					let ty = types.get(&def.ty).unwrap();
					let sig = rust_codegen::func::Signature::new(id, ty.instanciate());
					module.add_function(sig, quote::quote! { unimplemented!() });
				}
			}

			error_type = module.add_struct("Error");
		}

		Self {
			inner: module_ref,
			types,
			error_type
		}
	}

	pub fn extern_type(&self, i: u32) -> Option<rust_codegen::strct::Ref> {
		self.types.get(&i).cloned()
	}

	pub fn error_type(&self) -> rust_codegen::ty::Instance {
		self.error_type.instanciate()
	}

	pub fn path(&self, scope: &rust_codegen::Scope) -> rust_codegen::ScopedPath {
		use rust_codegen::Instance;
		self.inner.path(scope)
	}

	pub fn converter_path(&self, scope: &rust_codegen::Scope, c: &lexing::token::Convertion) -> proc_macro2::TokenStream {
		let path = self.path(scope);
		let id = converter_ident(c);
		quote::quote!{ #path::#id }
	}
}