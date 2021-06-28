use std::collections::HashMap;
use crate::{
	poly,
	mono
};
pub use crate::lexing::token::{
	Delimiter,
	Operator,
	Punct
};

pub mod built_in;
pub mod ty;
pub mod module;
pub mod constant;
pub mod id;
pub mod expr;
pub mod pattern;
pub mod routine;

pub use ty::Type;
pub use module::Module;
pub use constant::Constant;
pub use id::Id;
pub use expr::Expr;
pub use pattern::Pattern;
pub use routine::Routine;

/// Pseudo-code context.
pub struct Context<'a, 'p> {
	grammar: &'a mono::Grammar<'p>,

	/// List of modules.
	modules: Vec<Module>,

	/// List of defined types.
	types: Vec<Type>,

	/// Built-in types.
	built_in: built_in::Types,

	/// Index of the extern module.
	extern_module: u32,

	/// Index of the AST module.
	ast_module: u32,

	/// Index of the Lexer module.
	lexer_module: u32,

	/// Index of the parser module.
	parser_module: u32,

	/// Maps each grammar extern type to a pseudo-code type (in the `types` field).
	/// 
	/// The unit external type is the only one not mapped.
	grammar_extern_type: HashMap<u32, u32>,

	/// Maps each grammar polymorphic type to a pseudo-code type (in the `types` field).
	grammar_type: HashMap<u32, u32>,

	/// Function variants.
	/// 
	/// Maps each function to its associated variant (if any) in its return type.
	function_variants: HashMap<u32, u32>
}

impl<'a, 'p> Context<'a, 'p> {
	pub fn module(&self, index: u32) -> Option<&Module> {
		self.modules.get(index as usize)
	}

	/// Creates a expression that calls the given constructor function.
	pub fn constructor_expr<F>(&self, index: mono::Index, build_arg: F) -> Expr where F: Fn(u32) -> Expr {
		let f = self.grammar.function(index).unwrap();
		let ty_index = f.return_ty();
		let grammar_ty = self.grammar.poly().ty(ty_index.0).unwrap();
		let ty = *self.grammar_type.get(&ty_index.0).unwrap();

		let args = if f.poly().is_fully_labeled(self.grammar.poly(), grammar_ty) {
			let bindings = f.poly().arguments().iter().enumerate().filter_map(|(i, labeled_a)| {
				let i = i as u32;
				if labeled_a.is_typed(self.grammar.poly(), grammar_ty) {
					let label = labeled_a.label().unwrap().clone();
					Some(expr::Binding {
						name: label,
						expr: build_arg(i)
					})
				} else {
					None
				}
			});
			expr::BuildArgs::Struct(bindings.collect())
		} else {
			let args = f.poly().arguments().iter().enumerate().filter_map(|(i, labeled_a)| {
				let i = i as u32;
				if labeled_a.is_typed(self.grammar.poly(), grammar_ty) {
					Some(build_arg(i))
				} else {
					None
				}
			});
			expr::BuildArgs::Tuple(args.collect())
		};

		match self.function_variants.get(&index.0) {
			Some(&v) => {
				Expr::Cons(ty::Ref::Defined(ty), v, args)
			},
			None => {
				Expr::New(ty::Ref::Defined(ty), args)
			}
		}
	}

	pub fn new(
		grammar: &'a mono::Grammar<'p>,
		extern_module_path: &[String],
		ast_module_path: &[String],
		lexer_module_path: &[String],
		parser_module_path: &[String]
	) -> Self {
		let mut modules = vec![Module::root()];
		let mut submodules = vec![HashMap::new()];

		fn declare_module(modules: &mut Vec<Module>, submodules: &mut Vec<HashMap<String, u32>>, path: &[String], role: module::Role) -> u32 {
			use std::collections::hash_map::Entry;
			match path.split_last() {
				Some((name, parent_path)) => {
					let parent_index = declare_module(modules, submodules, parent_path, role);
					match submodules[parent_index as usize].entry(name.to_string()) {
						Entry::Vacant(entry) => {
							let index = modules.len() as u32;
							modules.push(Module::new(parent_index, name.to_string()));
							modules[parent_index as usize].add_module(index);
							entry.insert(index);
							index
						},
						Entry::Occupied(entry) => {
							*entry.get()
						}
					}
				},
				None => 0
			}
		}

		let extern_module = declare_module(&mut modules, &mut submodules, extern_module_path, module::Role::Extern);
		let ast_module = declare_module(&mut modules, &mut submodules, ast_module_path, module::Role::Ast);
		let lexer_module = declare_module(&mut modules, &mut submodules, lexer_module_path, module::Role::Lexer);
		let parser_module = declare_module(&mut modules, &mut submodules, parser_module_path, module::Role::ParserRoot);

		let mut types = Vec::new();

		fn declare_type(modules: &mut [Module], types: &mut Vec<Type>, ty: Type) -> u32 {
			let index = types.len() as u32;
			modules[ty.module() as usize].add_type(ty::Ref::Defined(index));
			types.push(ty);
			index
		}

		let mut grammar_extern_type = HashMap::new();
		for (i, (id, _)) in grammar.extern_types().iter().enumerate() {
			let index = i as u32;
			let i = declare_type(&mut modules, &mut types, Type::opaque(extern_module, id, index));
			grammar_extern_type.insert(index, i);
		}

		struct DefinitionEnv<'a> {
			ast_module: u32,
			modules: &'a mut [Module],
			types: &'a mut Vec<Type>,
			grammar: &'a poly::Grammar,
			grammar_extern_type: &'a HashMap<u32, u32>,
			grammar_type: &'a mut HashMap<u32, u32>,
			function_variants: &'a mut HashMap<u32, u32>,
		}

		fn type_expr(
			env: &mut DefinitionEnv,
			context: &poly::Type,
			expr: &poly::ty::Expr
		) -> Option<ty::Expr> {
			match expr {
				poly::ty::Expr::Type(index, params) => {
					let ty = define_type(env, *index);
					let params = params.iter().filter_map(|e| type_expr(env, context, e)).collect();
					Some(ty::Expr::Defined(ty, params))
				},
				poly::ty::Expr::Terminal(index) => {
					let t = env.grammar.terminal(*index).unwrap();
					t.extern_type(env.grammar).map(|i| {
						let ty = *env.grammar_extern_type.get(&i).unwrap();
						ty::Expr::Defined(ty, Vec::new())
					})
				},
				poly::ty::Expr::Var(x) => {
					match context.parameter(*x).unwrap() {
						poly::ty::Parameter::Terminal(_) => None,
						poly::ty::Parameter::Type(i) => Some(ty::Expr::Var(i))
					}
				}
			}
		}

		fn define_type(
			env: &mut DefinitionEnv,
			index: u32
		) -> u32 {
			match env.grammar_type.get(&index).cloned() {
				Some(i) => i,
				None => {
					let ty = env.grammar.ty(index).unwrap();
					let id = ty.id().as_defined();
					
					let desc = if ty.constructor_count() == 1 {
						// Single constructor type.
						let f_index = ty.constructors()[0];
						let f = env.grammar.function(f_index).unwrap();

						if f.is_fully_labeled(env.grammar, ty) {
							// Structure type.
							let mut strct = ty::Struct::new();
							for p in f.arguments() {
								if let Some(p_ty) = type_expr(env, ty, p.expr()) {
									strct.add_field(p.label().unwrap().clone(), p_ty);
								}
							}
							ty::Desc::Struct(strct)
						} else {
							// Tuple struct type.
							let args = f.arguments().iter().filter_map(|p| type_expr(env, ty, p.expr()));
							ty::Desc::TupleStruct(args.collect())
						}
					} else {
						let mut enm = ty::Enum::new();
						for &f_index in ty.constructors() {
							let f = env.grammar.function(f_index).unwrap();

							let desc = if f.is_fully_labeled(env.grammar, ty) {
								// Struct variant.
								let mut strct = ty::Struct::new();
								for p in f.arguments() {
									if let Some(p_ty) = type_expr(env, ty, p.expr()) {
										strct.add_field(p.label().unwrap().clone(), p_ty);
									}
								}
								ty::VariantDesc::Struct(strct)
							} else {
								// Regular vartiant.
								let args = f.arguments().iter().filter_map(|p| type_expr(env, ty, p.expr()));
								ty::VariantDesc::Tuple(args.collect())
							};

							let variant = ty::Variant::Defined(f.id().as_defined().clone(), desc);
							env.function_variants.insert(f_index, enm.add_variant(variant));
						}

						ty::Desc::Enum(enm)
					};

					let i = declare_type(env.modules, env.types, Type::new(env.ast_module, id.clone(), Some(index), desc));
					env.grammar_type.insert(index, i);
					i
				}
			}
		}

		let mut grammar_type = HashMap::new();
		let mut function_variants = HashMap::new();
		let poly_grammar = grammar.poly();
		let mut env = DefinitionEnv {
			ast_module,
			modules: &mut modules,
			types: &mut types,
			grammar: poly_grammar,
			grammar_extern_type: &grammar_extern_type,
			grammar_type: &mut grammar_type,
			function_variants: &mut function_variants,
		};
		for (index, _) in poly_grammar.types().iter().enumerate() {
			define_type(&mut env, index as u32);
		}

		let built_in = built_in::Types::new(
			grammar,
			lexer_module,
			parser_module,
			&grammar_extern_type
		);

		Self {
			grammar,
			modules,
			types,
			built_in,
			extern_module,
			ast_module,
			lexer_module,
			parser_module,
			grammar_extern_type,
			grammar_type,
			function_variants
		}
	}

	pub fn ty(&self, r: ty::Ref) -> Option<&Type> {
		match r {
			ty::Ref::BuiltIn(b) => match b {
				built_in::Type::Token => Some(&self.built_in.tokens),
				built_in::Type::Keyword => self.built_in.keywords.as_ref(),
				built_in::Type::Operator => self.built_in.operators.as_ref(),
				built_in::Type::Delimiter => self.built_in.delimiters.as_ref(),
				built_in::Type::Punct => self.built_in.puncts.as_ref(),
				built_in::Type::Node => Some(&self.built_in.nodes),
				built_in::Type::Item => Some(&self.built_in.items)
			},
			ty::Ref::Defined(index) => self.types.get(index as usize)
		}
	}
}