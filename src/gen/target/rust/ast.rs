use std::collections::HashMap;
use crate::{
	util,
	poly,
	mono::{
		self,
		Index,
		Grammar,
	}
};
use super::ExternModule;

/// AST Rust module.
pub struct Module {
	inner: rust_codegen::module::Ref,
	types: HashMap<u32, rust_codegen::enm::Ref>,
	instances: HashMap<Index, rust_codegen::ty::Instance>
}

fn type_name(ty: &poly::Type) -> String {
	util::to_caml_case(ty.id().name())
}

pub fn variant_name(f: &poly::Function) -> String {
	util::to_caml_case(f.id().as_str())
}

impl Module {
	pub fn ty(&self, i: u32) ->  Option<rust_codegen::enm::Ref> {
		self.types.get(&i).cloned()
	}

	pub fn ty_instance(&self, i: Index) ->  Option<rust_codegen::ty::Instance> {
		self.instances.get(&i).cloned()
	}

	pub fn write<W: std::io::Write>(&self, out: &mut W) -> std::io::Result<()> {
		let inner = self.inner();
		write!(out, "{}", quote::quote!{ #inner })
	}
	
	pub fn new(
		context: &rust_codegen::Context,
		grammar: &Grammar,
		extern_mod: &ExternModule,
		path: &[String]
	) -> Self {
		let module_ref = super::declare_module(context, path);

		let mut types = HashMap::new();
		let mut instances = HashMap::new();

		{
			let mut module = module_ref.borrow_mut();

			fn rust_ty(
				grammar: &Grammar,
				extern_mod: &ExternModule,
				module: &mut rust_codegen::Module,
				types: &mut HashMap<u32, rust_codegen::enm::Ref>,
				i: u32
			) -> rust_codegen::enm::Ref {
				fn poly_instance(
					grammar: &Grammar,
					extern_mod: &ExternModule,
					module: &mut rust_codegen::Module,
					types: &mut HashMap<u32, rust_codegen::enm::Ref>,
					ty_args: &[rust_codegen::ty::param::Definition],
					e: &poly::ty::Expr
				) -> Option<rust_codegen::ty::Instance> {
					match e {
						poly::ty::Expr::Terminal(index) => {
							let t = grammar.terminal(*index).unwrap();
							if let Some(exp_index) = t.as_regexp_ref() {
								let exp = grammar.regexp(exp_index).unwrap();
								if let Some(ty) = extern_mod.extern_type(exp.ty) {
									Some(ty.instanciate())
								} else {
									None
								}
							} else {
								None
							}
						},
						poly::ty::Expr::Type(nt, args) => {
							let ty = rust_ty(
								grammar,
								extern_mod,
								module,
								types,
								*nt
							);

							let mut rust_args = Vec::new();
							for a in args {
								if let Some(rust_a) = poly_instance(grammar, extern_mod, module, types, ty_args, a) {
									rust_args.push(rust_a)
								}
							}

							Some(ty.instanciate_with(rust_args))
						},
						poly::ty::Expr::Var(t) => {
							Some(ty_args[*t as usize].instanciate())
						}
					}
				}

				if !types.contains_key(&i) {
					let ty = grammar.poly().ty(i).unwrap();
					let enum_ty_ref = module.add_enum(type_name(ty));
					types.insert(i as u32, enum_ty_ref.clone());

					{
						let mut enum_ty = enum_ty_ref.borrow_mut();
					
						for p in ty.parameters() {
							if let poly::ty::Parameter::NonTerminal(id) = p {
								enum_ty.add_param(id.as_str());
							}
						}

						for &c in ty.constructors() {
							let f = grammar.poly().function(c).unwrap();
							let mut variant = rust_codegen::enm::Variant::new(variant_name(f.as_ref()));

							for a in f.arguments() {
								if let Some(rust_a) = poly_instance(grammar, extern_mod, module, types, enum_ty.parameters(), a) {
									variant.add_param(rust_a)
								}
							}

							enum_ty.add(variant);
						}
					}
				}

				types.get(&i).unwrap().clone()
			}

			// Declare types.
			for (i, _) in grammar.poly().types().iter().enumerate() {
				rust_ty(grammar, extern_mod, &mut module, &mut types, i as u32);
			}

			fn instance(
				grammar: &Grammar,
				types: &HashMap<u32, rust_codegen::enm::Ref>,
				instances: &mut HashMap<Index, rust_codegen::ty::Instance>,
				index: Index
			) -> rust_codegen::ty::Instance {
				if !instances.contains_key(&index) {
					let mono_ty = grammar.ty(index).unwrap();
					let ty_index = index.0;
					let ty = types.get(&ty_index).unwrap();

					let mut params = Vec::new();
					for i in 0..mono_ty.poly().parameters().len() {
						if let mono::ty::Expr::Type(p_index) = mono_ty.parameter(i as u32).unwrap() {
							params.push(instance(grammar, types, instances, *p_index))
						}
					}

					instances.insert(index, ty.instanciate_with(params));
				}

				instances.get(&index).unwrap().clone()
			}

			// Declare types instances.
			for (index, _) in grammar.enumerate_types() {
				instance(grammar, &types, &mut instances, index);
			}
		}

		Self {
			inner: module_ref,
			types,
			instances
		}
	}

	pub fn inner(&self) -> rust_codegen::module::Inner {
		self.inner.inner()
	}
}

impl quote::ToTokens for Module {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		self.inner.inner().to_tokens(tokens);
	}
}