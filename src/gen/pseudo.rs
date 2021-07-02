pub use crate::{
	lexing::{
		self,
		token::{Delimiter, Operator, Punct},
	},
	parsing,
};
use crate::{mono, poly};
use std::collections::HashMap;

pub mod built_in;
pub mod constant;
pub mod expr;
pub mod id;
pub mod module;
pub mod pattern;
pub mod routine;
pub mod ty;

pub use constant::Constant;
pub use expr::Expr;
pub use id::Id;
pub use module::Module;
pub use pattern::Pattern;
pub use routine::Routine;
pub use ty::Type;

/// Pseudo-code context.
pub struct Context<'a, 'p> {
	/// Generation configuration.
	config: super::Config,

	/// Grammar.
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
	function_variants: HashMap<u32, u32>,
}

impl<'a, 'p> Context<'a, 'p> {
	pub fn config(&self) -> &super::Config {
		&self.config
	}

	pub fn grammar(&self) -> &'a mono::Grammar<'p> {
		self.grammar
	}

	pub fn module(&self, index: u32) -> Option<&Module> {
		self.modules.get(index as usize)
	}

	pub fn module_mut(&mut self, index: u32) -> Option<&mut Module> {
		self.modules.get_mut(index as usize)
	}

	pub fn extern_module(&self) -> &Module {
		self.module(self.extern_module).unwrap()
	}

	pub fn ast_module(&self) -> &Module {
		self.module(self.ast_module).unwrap()
	}

	pub fn lexer_module(&self) -> &Module {
		self.module(self.lexer_module).unwrap()
	}

	pub fn parser_module(&self) -> &Module {
		self.module(self.parser_module).unwrap()
	}

	pub fn module_path(&self, index: u32) -> Option<module::Path> {
		self.module(index)
			.map(|m| module::Path::new(m.parent().map(|p| self.module_path(p).unwrap()), m.id()))
	}

	pub fn extern_module_path(&self) -> module::Path {
		self.module_path(self.extern_module).unwrap()
	}

	pub fn ast_module_path(&self) -> module::Path {
		self.module_path(self.ast_module).unwrap()
	}

	pub fn lexer_module_path(&self) -> module::Path {
		self.module_path(self.lexer_module).unwrap()
	}

	pub fn parser_module_path(&self) -> module::Path {
		self.module_path(self.parser_module).unwrap()
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
				built_in::Type::Item => Some(&self.built_in.items),
			},
			ty::Ref::Defined(index) => self.defined_type(index),
		}
	}

	pub fn defined_type(&self, i: u32) -> Option<&Type> {
		self.types.get(i as usize)
	}

	/// Creates a expression that calls the given constructor function.
	pub fn constructor_expr<F>(&self, index: mono::Index, build_arg: F) -> Expr
	where
		F: Fn(u32) -> Expr,
	{
		let f = self.grammar.function(index).unwrap();
		let ty_index = f.return_ty();
		let grammar_ty = self.grammar.poly().ty(ty_index.0).unwrap();
		let ty = *self.grammar_type.get(&ty_index.0).unwrap();

		let args = if f.poly().is_fully_labeled(self.grammar.poly(), grammar_ty) {
			let bindings = f
				.poly()
				.arguments()
				.iter()
				.enumerate()
				.filter_map(|(i, labeled_a)| {
					let i = i as u32;
					if labeled_a.is_typed(self.grammar.poly(), grammar_ty) {
						let label = labeled_a.label().unwrap().clone();
						Some(expr::Binding {
							name: label,
							expr: build_arg(i),
						})
					} else {
						None
					}
				});
			expr::BuildArgs::Struct(bindings.collect())
		} else {
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
				});
			expr::BuildArgs::Tuple(args.collect())
		};

		match self.function_variants.get(&index.0) {
			Some(&v) => Expr::Cons(ty::Ref::Defined(ty), v, args),
			None => Expr::New(ty::Ref::Defined(ty), args),
		}
	}

	pub fn type_expr(&self, index: mono::Index) -> ty::Expr {
		let ty = self.grammar.ty(index).unwrap();
		let params = ty
			.parameters()
			.iter()
			.filter_map(|p| match p {
				mono::ty::Expr::Terminal(index) => {
					let t = self.grammar.terminal(*index).unwrap();
					t.extern_type(self.grammar.poly()).map(|i| {
						let ty = *self.grammar_extern_type.get(&i).unwrap();
						ty::Expr::Defined(ty, Vec::new())
					})
				}
				mono::ty::Expr::Type(index) => Some(self.type_expr(*index)),
			})
			.collect();
		let generated_index = *self.grammar_type.get(&index.0).unwrap();
		ty::Expr::Defined(generated_index, params)
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
		let mut modules = vec![Module::root()];
		let mut submodules = vec![HashMap::new()];

		fn declare_module(
			modules: &mut Vec<Module>,
			submodules: &mut Vec<HashMap<String, u32>>,
			path: &[String],
			role: Option<module::Role>,
		) -> u32 {
			use std::collections::hash_map::Entry;
			let index = match path.split_last() {
				Some((name, parent_path)) => {
					let parent_index = declare_module(modules, submodules, parent_path, None);
					match submodules[parent_index as usize].entry(name.to_string()) {
						Entry::Vacant(entry) => {
							let index = modules.len() as u32;
							modules.push(Module::new(index, parent_index, name.to_string()));
							modules[parent_index as usize].add_module(index);
							entry.insert(index);
							submodules.push(HashMap::new());
							index
						}
						Entry::Occupied(entry) => *entry.get(),
					}
				}
				None => 0,
			};

			if let Some(role) = role {
				modules[index as usize].add_role(role);
			}

			index
		}

		let extern_module = declare_module(
			&mut modules,
			&mut submodules,
			extern_module_path,
			Some(module::Role::Extern),
		);
		let ast_module = declare_module(
			&mut modules,
			&mut submodules,
			ast_module_path,
			Some(module::Role::Ast),
		);
		let lexer_module = declare_module(
			&mut modules,
			&mut submodules,
			lexer_module_path,
			Some(module::Role::Lexer),
		);
		let parser_module = declare_module(
			&mut modules,
			&mut submodules,
			parser_module_path,
			Some(module::Role::ParserRoot),
		);

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
			let i = declare_type(
				&mut modules,
				&mut types,
				Type::opaque(extern_module, id, index),
			);
			grammar_extern_type.insert(index, i);
		}

		struct DefinitionEnv<'a> {
			config: &'a super::Config,
			ast_module: u32,
			modules: &'a mut [Module],
			types: &'a mut Vec<Type>,
			grammar: &'a poly::Grammar,
			grammar_extern_type: &'a HashMap<u32, u32>,
			grammar_type: &'a mut HashMap<u32, u32>,
			function_variants: &'a mut HashMap<u32, u32>,
		}

		fn sub_type_expr(
			env: &mut DefinitionEnv,
			context_ty: &poly::Type,
			expr: &poly::ty::Expr,
		) -> Option<ty::Expr> {
			match expr {
				poly::ty::Expr::Type(index, params) => {
					let ty = define_type(env, *index);
					let params = params
						.iter()
						.filter_map(|e| sub_type_expr(env, context_ty, e))
						.collect();
					Some(ty::Expr::Defined(ty, params))
				}
				poly::ty::Expr::Terminal(index) => {
					let t = env.grammar.terminal(*index).unwrap();
					t.extern_type(env.grammar).map(|i| {
						let ty = *env.grammar_extern_type.get(&i).unwrap();
						ty::Expr::Defined(ty, Vec::new())
					})
				}
				poly::ty::Expr::Var(x) => match context_ty.parameter(*x).unwrap() {
					poly::ty::Parameter::Terminal(_) => None,
					poly::ty::Parameter::Type(i) => Some(ty::Expr::Var(i)),
				},
			}
		}

		fn type_expr(
			env: &mut DefinitionEnv,
			context_ty_index: u32,
			expr: &poly::ty::Expr,
		) -> Option<ty::Expr> {
			let context_ty = env.grammar.ty(context_ty_index).unwrap();
			sub_type_expr(env, context_ty, expr).map(|mut p| {
				if env.config.locate {
					p = ty::Expr::Loc(Box::new(p))
				}

				if expr.depends_on(env.grammar, context_ty_index) {
					p = ty::Expr::Heap(Box::new(p))
				}

				p
			})
		}

		fn define_type(env: &mut DefinitionEnv, index: u32) -> u32 {
			match env.grammar_type.get(&index).cloned() {
				Some(i) => i,
				None => {
					let ty = env.grammar.ty(index).unwrap();
					let id = ty.id().as_defined();
					let i = declare_type(
						env.modules,
						env.types,
						Type::new(env.ast_module, id.clone(), Some(index), ty::Desc::Opaque),
					);
					env.grammar_type.insert(index, i);

					let desc = if ty.constructor_count() == 1 {
						// Single constructor type.
						let f_index = ty.constructors()[0];
						let f = env.grammar.function(f_index).unwrap();

						if f.is_fully_labeled(env.grammar, ty) {
							// Structure type.
							let mut strct = ty::Struct::new();
							for p in f.arguments() {
								if let Some(p_ty) = type_expr(env, index, p.expr()) {
									strct.add_field(p.label().unwrap().clone(), p_ty);
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
							let f = env.grammar.function(f_index).unwrap();

							let desc = if f.is_fully_labeled(env.grammar, ty) {
								// Struct variant.
								let mut strct = ty::Struct::new();
								for p in f.arguments() {
									if let Some(p_ty) = type_expr(env, index, p.expr()) {
										strct.add_field(p.label().unwrap().clone(), p_ty);
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

							let variant = ty::Variant::Defined(f.id().as_defined().clone(), desc);
							env.function_variants
								.insert(f_index, enm.add_variant(variant));
						}

						ty::Desc::Enum(enm)
					};

					env.types[i as usize].set_desc(desc);
					i
				}
			}
		}

		let mut grammar_type = HashMap::new();
		let mut function_variants = HashMap::new();
		let poly_grammar = grammar.poly();
		let mut env = DefinitionEnv {
			config: &config,
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
			&mut modules,
			lexer_module,
			parser_module,
			&grammar_extern_type,
			&grammar_type,
		);

		let mut context = Self {
			config,
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
			function_variants,
		};

		for (i, _) in grammar.poly().types().iter().enumerate() {
			let ty_index = i as u32;
			let local_index = *context.grammar_type.get(&ty_index).unwrap();
			let ty_ref = ty::Ref::Defined(local_index);
			let debug_format =
				Routine::Format(ty_ref, routine::format::generate_debug(&context, ty_ref));
			context
				.module_mut(context.ast_module)
				.unwrap()
				.add_routine(debug_format);
		}

		let lexer = Routine::Lexer(routine::lexer::generate(&context, lexing_table));
		context
			.module_mut(context.lexer_module)
			.unwrap()
			.add_routine(lexer);

		match parsing_table {
			parsing::Table::LR0(table) => {
				for (q, ty_index) in table.initial_states() {
					let parser = Routine::Parser(
						ty_index,
						super::Parser::LR0(routine::lr0::generate(&context, table, q)),
					);
					context
						.module_mut(context.parser_module)
						.unwrap()
						.add_routine(parser);
				}
			}
			parsing::Table::LALR1(table) => {
				unimplemented!()
			}
		}

		context
	}
}
