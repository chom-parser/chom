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

pub use ty::Type;
pub use module::Module;

/// Pseudo-code context.
pub struct Context {
	/// List of modules.
	modules: Vec<Module>,

	/// List of defined types.
	types: Vec<Type>,

	/// Built-in types.
	build_in: built_in::Types,

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

	/// Maps each terminal to a token pattern.
	grammar_terminal: HashMap<u32, Pattern>,
}

impl Context {
	pub fn new(
		grammar: &mono::Grammar,
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

		let (build_in, grammar_terminal) = built_in::Types::new(grammar, parser_module, &grammar_extern_type);

		let mut grammar_type = HashMap::new();
		let poly_grammar = grammar.poly();
		for (index, ty) in poly_grammar.types().iter().enumerate() {
			let index = index as u32;
			if let poly::ty::Id::Defined(id) = ty.id() {
				let type_parameters: Vec<_> = ty.parameters().iter().filter_map(|p| match p {
					poly::ty::Parameter::Type(id) => Some(id.clone()),
					poly::ty::Parameter::Terminal(_) => None
				}).collect();

				if ty.constructor_count() == 1 {
					// Single constructor type.
					let f_index = ty.constructors()[0];
					let f = poly_grammar.function(f_index).unwrap();
					if f.is_fully_labeled(poly_grammar) {
						// ...
					} else {
						// ...
					}
				} else {
					for &f_index in ty.constructors() {
						let f = poly_grammar.function(f_index).unwrap();
						if f.is_fully_labeled(poly_grammar) {
							// ...
						} else {
							// ...
						}
					}
				}
			}
		}

		Self {
			modules,
			types,
			build_in,
			extern_module,
			ast_module,
			lexer_module,
			parser_module,
			grammar_extern_type,
			grammar_type,
			grammar_terminal
		}
	}

	pub fn ty(&self, r: ty::Ref) -> Option<&Type> {
		match r {
			ty::Ref::BuiltIn(b) => match b {
				built_in::Type::Token => Some(&self.build_in.tokens),
				built_in::Type::Keyword => self.build_in.keywords.as_ref(),
				built_in::Type::Operator => self.build_in.operators.as_ref(),
				built_in::Type::Delimiter => self.build_in.delimiters.as_ref(),
				built_in::Type::Punct => self.build_in.puncts.as_ref()
			},
			ty::Ref::Defined(index) => self.types.get(index as usize)
		}
	}
}

/// Pattern.
pub enum Pattern {
	/// Pattern payload.
	Payload,

	/// Enum variant.
	/// 
	/// The first parameter is the type id.
	/// The second is the variant index.
	/// The third is the inner pattern if any.
	Variant(ty::Ref, u32, Option<Box<Pattern>>)
}