use std::collections::HashMap;
use chom_ir::{
	expr,
	ty,
	Function,
	function
};
use crate::{
	Ident,
	mono,
	poly,
	parsing,
	lexing
};
use super::{
	Pattern,
	Expr,
	Module,
	Type,
	TypeExpr,
	Path
};

pub mod id;
mod namespace;
pub mod provided;

pub use id::Id;
pub use namespace::*;

/// Pseudo-code context.
pub struct Context<'a, 'p> {
	/// Generation configuration.
	config: super::Config,

	/// Grammar.
	grammar: &'a mono::Grammar<'p>,

	/// Intermediate representation context.
	ir: chom_ir::Context<Namespace<'p>>,

	/// Provided types.
	provided: provided::Types<'p>,

	/// Index of the extern module.
	extern_module: u32,

	/// Index of the AST module.
	ast_module: u32,

	/// Index of the Lexer module.
	lexer_module: u32,

	/// Index of the parser module.
	parser_module: u32,

	/// Maps each grammar extern type to an IR type.
	///
	/// The unit external type is the only one not mapped.
	grammar_extern_type: HashMap<u32, u32>,

	/// Extern exing error IR type index.
	lexing_error_ty: u32,

	/// Unexpected char error function.
	lexing_error_fn: u32,

	/// Maps each grammar polymorphic type to an IR type.
	grammar_type: HashMap<u32, u32>,

	/// Function variants.
	///
	/// Maps each function to its associated variant (if any) in its return type.
	function_variants: HashMap<u32, u32>,

	/// Keeps track of the name of each submodules.
	submodules: Vec<HashMap<String, u32>>
}

impl<'a, 'p> Context<'a, 'p> {
	pub fn config(&self) -> &super::Config {
		&self.config
	}

	pub fn grammar(&self) -> &'a mono::Grammar<'p> {
		self.grammar
	}

	pub fn namespace(&self) -> &Namespace<'p> {
		self.ir.id()
	}

	pub fn ir_context(&self) -> &chom_ir::Context<Namespace<'p>> {
		&self.ir
	}

	pub fn into_ir_context(self) -> chom_ir::Context<Namespace<'p>> {
		self.ir
	}

	pub fn module(&self, index: u32) -> Option<&Module<'p>> {
		self.ir.module(index)
	}

	// pub fn module_mut(&mut self, index: u32) -> Option<&mut Module> {
	// 	self.modules.get_mut(index as usize)
	// }

	pub fn root_module(&self) -> &Module<'p> {
		self.module(0).unwrap()
	}

	pub fn extern_module(&self) -> &Module<'p> {
		self.module(self.extern_module).unwrap()
	}

	pub fn ast_module(&self) -> &Module<'p> {
		self.module(self.ast_module).unwrap()
	}

	pub fn lexer_module(&self) -> &Module<'p> {
		self.module(self.lexer_module).unwrap()
	}

	pub fn parser_module(&self) -> &Module<'p> {
		self.module(self.parser_module).unwrap()
	}

	pub fn module_path(&self, index: u32) -> Option<Path<'_, 'p>> {
		self.ir.module_path(index)
	}

	pub fn extern_module_path(&self) -> Path<'_, 'p> {
		self.module_path(self.extern_module).unwrap()
	}

	pub fn lexing_error_type_expr(&self) -> TypeExpr<'p> {
		TypeExpr::Instance(ty::Ref::Defined(self.lexing_error_ty), Vec::new())
	}

	pub fn lexing_error_function(&self) -> u32 {
		self.lexing_error_fn
	}

	pub fn ast_module_path(&self) -> Path<'_, 'p> {
		self.module_path(self.ast_module).unwrap()
	}

	pub fn lexer_module_path(&self) -> Path<'_, 'p> {
		self.module_path(self.lexer_module).unwrap()
	}

	pub fn parser_module_path(&self) -> Path<'_, 'p> {
		self.module_path(self.parser_module).unwrap()
	}

	pub fn provided(&self) -> &provided::Types<'p> {
		&self.provided
	}

	pub fn module_index_from_path(&self, path: &[String]) -> Option<u32> {
		match path.split_last() {
			Some((name, parent_path)) => {
				self.module_index_from_path(parent_path).map(|parent_index| {
					self.submodules[parent_index as usize].get(name).cloned()
				}).flatten()
			}
			None => Some(0),
		}
	}

	// pub fn ty(&self, r: ty::Ref) -> Option<&Type> {
	// 	match r {
	// 		ty::Ref::BuiltIn(b) => match b {
	// 			built_in::Type::Token => Some(&self.built_in.tokens),
	// 			built_in::Type::Keyword => self.built_in.keywords.as_ref(),
	// 			built_in::Type::Operator => self.built_in.operators.as_ref(),
	// 			built_in::Type::Delimiter => self.built_in.delimiters.as_ref(),
	// 			built_in::Type::Punct => self.built_in.puncts.as_ref(),
	// 			built_in::Type::Node => Some(&self.built_in.nodes),
	// 			built_in::Type::Item => Some(&self.built_in.items),
	// 		},
	// 		ty::Ref::Defined(index) => self.defined_type(index),
	// 	}
	// }

	// pub fn defined_type(&self, i: u32) -> Option<&Type> {
	// 	self.types.get(i as usize)
	// }

	/// Creates a expression that calls the given constructor function.
	pub fn constructor_expr<F>(&self, index: mono::Index, build_arg: F) -> Expr<'p>
	where
		F: Fn(u32) -> Expr<'p>,
	{
		let f = self.grammar.function(index).unwrap();
		let ty_index = f.return_ty();
		let grammar_ty = self.grammar.poly().ty(ty_index.0).unwrap();
		let ir_ty = *self.grammar_type.get(&ty_index.0).unwrap();

		let args = f
			.poly()
			.arguments()
			.iter()
			.enumerate()
			.filter_map(|(i, labeled_a)| {
				let i = i as u32;
				if labeled_a.is_typed(self.grammar.poly(), grammar_ty) {
					Some(build_arg(i))
				} else {
					None
				}
			})
			.collect();

		match self.function_variants.get(&index.0) {
			Some(&v) => Expr::Cons(ty::Ref::Defined(ir_ty), v, args),
			None => Expr::New(ty::Ref::Defined(ir_ty), args),
		}
	}

	pub fn type_expr(&self, index: mono::Index) -> TypeExpr<'p> {
		let ty = self.grammar.ty(index).unwrap();
		let params = ty
			.parameters()
			.iter()
			.filter_map(|p| match p {
				mono::ty::Expr::Terminal(index) => {
					let t = self.grammar.terminal(*index).unwrap();
					t.extern_type(self.grammar.poly()).map(|i| {
						let ir_ty = *self.grammar_extern_type.get(&i).unwrap();
						ty::Expr::Instance(ty::Ref::Defined(ir_ty), Vec::new())
					})
				}
				mono::ty::Expr::Type(index) => Some(self.type_expr(*index)),
			})
			.collect();
		let ir_ty = *self.grammar_type.get(&index.0).unwrap();
		ty::Expr::Instance(ty::Ref::Defined(ir_ty), params)
	}

	pub fn new(
		config: super::Config,
		grammar: &'a mono::Grammar<'p>,
		extern_module_path: &[String],
		ast_module_path: &[String],
		lexer_module_path: &[String],
		lexing_table: &lexing::Table,
		parser_module_path: &[String],
		parsing_table: &parsing::Table,
	) -> Self {
		let mut ir = chom_ir::Context::new(Namespace::new(grammar.poly()));
		let mut submodules = vec![HashMap::new()];

		fn declare_module<'p>(
			ir: &mut chom_ir::Context<Namespace<'p>>,
			submodules: &mut Vec<HashMap<String, u32>>,
			path: &[String],
			is_extern: bool
		) -> u32 {
			use std::collections::hash_map::Entry;
			let index = match path.split_last() {
				Some((name, parent_path)) => {
					let parent_index = declare_module(ir, submodules, parent_path, false);
					match submodules[parent_index as usize].entry(name.to_string()) {
						Entry::Vacant(entry) => {
							let id = ir.id_mut().new_module_id(Ident::new(name).unwrap());
							let index = ir.add_module(Module::new(parent_index, id));
							entry.insert(index);
							submodules.push(HashMap::new());
							index
						}
						Entry::Occupied(entry) => *entry.get(),
					}
				}
				None => 0,
			};

			if is_extern {
				ir.module_mut(index).unwrap().set_extern(true);
			}

			index
		}

		let extern_module = declare_module(
			&mut ir,
			&mut submodules,
			extern_module_path,
			true,
		);
		let ast_module = declare_module(
			&mut ir,
			&mut submodules,
			ast_module_path,
			false,
		);
		let lexer_module = declare_module(
			&mut ir,
			&mut submodules,
			lexer_module_path,
			false,
		);
		let parser_module = declare_module(
			&mut ir,
			&mut submodules,
			parser_module_path,
			false,
		);

		log::info!("IR: declaring extern types...");
		let mut grammar_extern_type = HashMap::new();
		for (i, _) in grammar.extern_types().iter().enumerate() {
			let index = i as u32;
			let i = ir.add_type(Type::opaque(extern_module, TypeId::Extern(index)));
			grammar_extern_type.insert(index, i);
		}

		let lexing_error_ty = ir.add_type(Type::opaque(extern_module, TypeId::ExternError));
		let lexing_error_fn = ir.add_function(Function::new(function::Owner::Module(extern_module), function::Signature::UndefinedChar(
			ty::Expr::Instance(ty::Ref::Defined(lexing_error_ty), Vec::new())
		), None));

		struct DefinitionEnv<'a, 'p> {
			config: &'a super::Config,
			ir: &'a mut chom_ir::Context<Namespace<'p>>,
			ast_module: u32,
			grammar_extern_type: &'a HashMap<u32, u32>,
			grammar_type: &'a mut HashMap<u32, u32>,
			function_variants: &'a mut HashMap<u32, u32>,
		}

		fn sub_type_expr<'a, 'p>(
			env: &mut DefinitionEnv<'a, 'p>,
			context_ty: &poly::Type,
			expr: &poly::ty::Expr,
		) -> Option<TypeExpr<'p>> {
			log::info!("IR: sub type expr...");
			match expr {
				poly::ty::Expr::Type(index, params) => {
					let ir_ty = define_type(env, *index);
					let params = params
						.iter()
						.filter_map(|e| sub_type_expr(env, context_ty, e))
						.collect();
					Some(ty::Expr::Instance(ty::Ref::Defined(ir_ty), params))
				}
				poly::ty::Expr::Terminal(index) => {
					let grammar = env.ir.id().grammar();
					let t = grammar.terminal(*index).unwrap();
					t.extern_type(grammar).map(|i| {
						let ir_ty = *env.grammar_extern_type.get(&i).unwrap();
						ty::Expr::Instance(ty::Ref::Defined(ir_ty), Vec::new())
					})
				}
				poly::ty::Expr::Var(x) => match context_ty.parameter(*x).unwrap() {
					poly::ty::Parameter::Terminal(_) => None,
					poly::ty::Parameter::Type(i) => Some(ty::Expr::Var(ty::Param::Defined(i))),
				},
			}
		}

		fn type_expr<'a, 'p>(
			env: &mut DefinitionEnv<'a, 'p>,
			context_ty_index: u32,
			expr: &poly::ty::Expr,
		) -> Option<TypeExpr<'p>> {
			let grammar = env.ir.id().grammar();
			let context_ty = grammar.ty(context_ty_index).unwrap();
			sub_type_expr(env, context_ty, expr).map(|mut p| {
				if env.config.locate {
					p = ty::Expr::locate(p)
				}

				if expr.depends_on(grammar, context_ty_index) {
					p = ty::Expr::heap(p)
				}

				p
			})
		}

		fn define_type<'a, 'p>(env: &mut DefinitionEnv<'a, 'p>, index: u32) -> u32 {
			match env.grammar_type.get(&index).cloned() {
				Some(ir_ty) => ir_ty,
				None => {
					let grammar = env.ir.id().grammar();
					let ty = grammar.ty(index).unwrap();
					let id = TypeId::Grammar(index);
					match ty.id() {
						poly::ty::Id::Primitive(p) => {
							panic!("primitive type definition")
						}
						poly::ty::Id::Defined(ident) => {
							log::info!("IR: defining type `{}` ({})", ident, index);
							let ir_ty = env.ir.add_type(Type::new(env.ast_module, id, ty::Desc::Opaque));
							env.grammar_type.insert(index, ir_ty);

							let desc = if ty.constructor_count() == 1 {
								// Single constructor type.
								let f_index = ty.constructors()[0];
								let f = grammar.function(f_index).unwrap();

								if f.is_fully_labeled(grammar, ty) {
									// Structure type.
									let mut strct = ty::Struct::new();
									for (p_index, p) in f.arguments().iter().enumerate() {
										if let Some(p_ty) = type_expr(env, index, p.expr()) {
											strct.add_field(FieldId(f_index, p_index as u32), p_ty);
										}
									}
									ty::Desc::Struct(strct)
								} else {
									// Tuple struct type.
									let args = f
										.arguments()
										.iter()
										.filter_map(|p| type_expr(env, index, p.expr()));
									ty::Desc::TupleStruct(args.collect())
								}
							} else {
								let mut enm = ty::Enum::new();
								for &f_index in ty.constructors() {
									let f = grammar.function(f_index).unwrap();

									let desc = if f.is_fully_labeled(grammar, ty) {
										// Struct variant.
										let mut strct = ty::Struct::new();
										for (p_index, p) in f.arguments().iter().enumerate() {
											if let Some(p_ty) = type_expr(env, index, p.expr()) {
												strct.add_field(FieldId(f_index, p_index as u32), p_ty);
											}
										}
										ty::VariantDesc::Struct(strct)
									} else {
										// Regular vartiant.
										let args = f
											.arguments()
											.iter()
											.filter_map(|p| type_expr(env, index, p.expr()));
										ty::VariantDesc::Tuple(args.collect())
									};

									let variant = ty::Variant::Defined(VariantId::Function(f_index), desc);
									env.function_variants
										.insert(f_index, enm.add_variant(variant));
								}

								ty::Desc::Enum(enm)
							};

							env.ir.ty_mut(ty::Ref::Defined(ir_ty)).unwrap().set_desc(desc);
							ir_ty
						}
					}
				}
			}
		}

		let mut grammar_type = HashMap::new();
		let mut function_variants = HashMap::new();
		let poly_grammar = grammar.poly();
		let mut env = DefinitionEnv {
			config: &config,
			ir: &mut ir,
			ast_module,
			grammar_extern_type: &grammar_extern_type,
			grammar_type: &mut grammar_type,
			function_variants: &mut function_variants,
		};
		log::info!("IR: defining intern types...");
		for (index, _) in poly_grammar.types().iter().enumerate() {
			define_type(&mut env, index as u32);
		}

		let provided = provided::Types::new(
			grammar,
			&mut ir,
			extern_module,
			lexer_module,
			parser_module,
			&grammar_extern_type,
			&grammar_type,
		);

		let mut context = Self {
			config,
			grammar,
			ir,
			provided,
			extern_module,
			ast_module,
			lexer_module,
			parser_module,
			grammar_extern_type,
			lexing_error_ty,
			lexing_error_fn,
			grammar_type,
			function_variants,
			submodules
		};

		// log::info!("IR: generating debug formatting routines...");
		// for (i, _) in grammar.poly().types().iter().enumerate() {
		// 	let ty_index = i as u32;
		// 	let ir_ty = *context.grammar_type.get(&ty_index).unwrap();
		// 	let ty_ref = ty::Ref::Defined(ir_ty);
		// 	let debug_format =
		// 		Routine::Format(ty_ref, routine::format::generate_debug(&context, ty_ref));
		// 	context
		// 		.module_mut(context.ast_module)
		// 		.unwrap()
		// 		.add_routine(debug_format);
		// }

		log::info!("IR: lexer function...");
		let lexer = super::lexer::generate(&context, lexing_table);
		context.ir.add_function(Function::new(
			function::Owner::Type(context.provided().lexer_type()),
			function::Signature::Lexer(context.provided().token_type_expr(), context.lexing_error_type_expr()),
			Some(lexer)
		));

		match parsing_table {
			parsing::Table::LR0(table) => {
				log::info!("IR: LR0 parsing functions...");
				for (q, ty_index) in table.initial_states() {
					let parser = super::parser::lr0::generate(&context, table, q);
					let ir_type = context.type_expr(ty_index);
					context.ir.add_function(Function::new(
						function::Owner::Module(parser_module),
						function::Signature::Parser(context.provided().token_type_expr(), ir_type),
						Some(parser)
					));
				}
			}
			parsing::Table::LALR1(table) => {
				unimplemented!()
			}
		}

		context
	}
}
