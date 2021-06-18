use std::collections::HashMap;
use crate::{
	util,
	mono::{
		Grammar,
		ExternalType
	}
};

pub mod lexer;
pub mod ast;
pub mod lr0;

impl<'a> Env<'a> {
	pub fn new(
		context: &'a mut rust_codegen::Context,
		grammar: &Grammar,
		extern_module_path: &[String],
		ast_module_path: &[String],
		lexer_module_path: &[String]
	) -> Self {
		let std_crate = StdCrate::new(context);
		let source_span_crate = SourceSpanCrate::new(context);
		
		let extern_mod = ExternModule::new(grammar, declare_module(context, extern_module_path));
		let ast_mod = ast::Module::new(grammar, &extern_mod, declare_module(context, ast_module_path));
		let lexer_mod = lexer::Module::new(grammar, &extern_mod, declare_module(context, lexer_module_path));
	
		// // Token type.
		// env.token_enum = Some(env.lexer_mod.as_ref().unwrap().borrow_mut().add_enum("Token"));
	
		Self {
			context,
			std_crate,
			source_span_crate,
			extern_mod,
			ast_mod,
			lexer_mod
		}
	}

	fn loc_token(&self) -> rust_codegen::ty::Instance {
		self.source_span_crate.loc_struct.instanciate_with([self.lexer_mod.token_ty().instanciate()])
	}
}

fn declare_module(context: &mut rust_codegen::Context, path: &[String]) -> rust_codegen::module::Ref {
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
	pub iterator_trait: rust_codegen::tr::Ref
}

impl StdCrate {
	fn new(context: &mut rust_codegen::Context) -> Self {
		let iterator_trait;
		let crte_ref = context.add_extern_crate("std");
		
		{
			let mut crte = crte_ref.borrow_mut();
			let std_iter_mod_ref = crte.add_module("iter");
			let mut std_iter_mod = std_iter_mod_ref.borrow_mut();
			
			iterator_trait = std_iter_mod.add_trait("Iterator");
			let mut iterator_trait = iterator_trait.borrow_mut();
			iterator_trait.add_associated_type("Item");
		}

		Self {
			iterator_trait
		}
	}
}

pub struct SourceSpanCrate {
	pub loc_struct: rust_codegen::strct::Ref
}

impl SourceSpanCrate {
	fn new(context: &mut rust_codegen::Context) -> Self {
		let loc_struct;
		let crte_ref = context.add_extern_crate("source_span");
		
		{
			let mut crte = crte_ref.borrow_mut();
			loc_struct = crte.add_struct("Loc");
		}

		Self {
			loc_struct
		}
	}
}

pub struct Env<'a> {
	/// Code generation context.
	pub context: &'a mut rust_codegen::Context,
	
	/// `std` crate interface.
	pub std_crate: StdCrate,

	/// `source-span` crate interface.
	pub source_span_crate: SourceSpanCrate,

	/// External types module interface.
	pub extern_mod: ExternModule,

	/// AST module.
	pub ast_mod: ast::Module,

	/// Lexer module.
	pub lexer_mod: lexer::Module
}

/// Rust module defining external types.
pub struct ExternModule {
	types: HashMap<u32, rust_codegen::strct::Ref>
}

impl ExternModule {
	pub fn new(grammar: &Grammar, module_ref: rust_codegen::module::Ref) -> Self {
		let mut types = HashMap::new();

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
		}

		Self {
			types
		}
	}

	pub fn extern_type(&self, i: u32) -> Option<rust_codegen::strct::Ref> {
		self.types.get(&i).cloned()
	}
}