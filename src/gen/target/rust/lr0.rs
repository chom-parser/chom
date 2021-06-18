use rust_codegen::{
	ty,
	func
};
use crate::{
	mono::Grammar,
	parsing
};
use super::Env;

pub fn generate(env: &Env, grammar: &Grammar, table: &parsing::table::LR0) -> rust_codegen::module::Ref {
	let crte = env.context.root_crate();
	let mut crte = crte.borrow_mut();

	let module_ref = crte.add_module("parser");

	let mut lexer_ty_param = rust_codegen::ty::param::Definition::new("L");
	lexer_ty_param.add_bound(env.std_crate.iterator_trait.instanciate_with([ty::Param::named("Item", env.loc_token())]));

	{
		let mut module = module_ref.borrow_mut();

		for (ty_index, q) in table.entries() {
			let ty = grammar.ty(ty_index).unwrap();

			let ast_ty = env.ast_mod.ty_instance(ty_index).unwrap();

			let name = format!("parse_{}", ty.id().name());
			let mut sig = func::Signature::new(name, ast_ty);
			sig.add_type_param(lexer_ty_param.clone());
			sig.add_arg("lexer", lexer_ty_param.instanciate());
			module.add_public_function(sig, quote::quote! {
				panic!("TODO")
			});
		}
	}

	module_ref
}