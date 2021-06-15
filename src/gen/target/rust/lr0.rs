pub fn generate(crte: &mut rust_codegen::Crate) -> rust_codegen::module::Ref {
	let module_ref = crte.add_module("parser");

	{
		let mut module = module_ref.borrow_mut();
		let ty_ref = module.add_struct("Parser");
		let mut ty = ty_ref.borrow_mut();
		ty.add_field("lexer", lexer_ty);
	}

	module_ref
}