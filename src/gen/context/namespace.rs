use crate::{
	Ident,
	poly,
	mono
};
use super::{
	Id,
	provided
};

#[derive(Clone, Copy)]
pub struct ModuleId(u32);

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeId {
	/// Extern type.
	Extern(u32),

	/// Extern lexing error type.
	ExternError,

	/// Intern type.
	Grammar(u32),

	/// Provided type.
	Provided(provided::Type),
}

#[derive(Clone, Copy)]
pub enum VariantId {
	/// Function variant.
	/// 
	/// It corresponds to the index of the defining function
	/// in the polymorphic grammar.
	Function(u32),

	/// Provided variant.
	Provided(provided::Variant)
}

/// References a field in a structure.
/// 
/// The first parameter is the index of the polymorphic function/constructor in the grammar
/// that defined the structure.
/// The second parameter is the index of the labeled parameter in the function that serves as a field.
#[derive(Clone, Copy)]
pub struct FieldId(pub u32, pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Label {
	Lexer(u32),
	Parser
}

impl Label {
	pub fn to_ident(&self) -> Ident {
		match self {
			Self::Lexer(i) => Ident::new(format!("lexer{}", i)).unwrap(),
			Self::Parser => Ident::new("parser").unwrap()
		}
	}
}

#[derive(Clone, Copy)]
pub enum FunctionId {
	/// Undefined character error constructor.
	UndefinedChar,

	/// Extern parser.
	/// 
	/// The given parameter is the index of the target
	/// extern type in the grammar.
	ExternParser(u32),

	/// Lexer.
	Lexer,

	/// Parser.
	/// 
	/// The given parameter is the index of the target
	/// type in the grammar.
	Parser(mono::Index),

	/// Debug formatter.
	DebugFormat
}

pub struct Namespace<'a, 'p> {
	/// Grammar.
	grammar: &'a mono::Grammar<'p>,

	/// The name of each module.
	modules: Vec<Ident>,

	/// Lists each named variant in `tokens`,
	/// with its name and associated type if any.
	named_variants: Vec<Ident>,

	/// Lists each keyword in `keywords`.
	keyword_variants: Vec<Ident>,

	/// Lists each node variants.
	node_variants: Vec<Ident>,
}

impl<'a, 'p> Namespace<'a, 'p> {
	pub fn new(grammar: &'a mono::Grammar<'p>) -> Self {
		Self {
			grammar,
			modules: Vec::new(),
			named_variants: Vec::new(),
			keyword_variants: Vec::new(),
			node_variants: Vec::new()
		}
	}

	pub fn grammar(&self) -> &'a mono::Grammar<'p> {
		self.grammar
	}

	pub(crate) fn new_module_id(&mut self, id: Ident) -> ModuleId {
		let i = self.modules.len() as u32;
		self.modules.push(id);
		ModuleId(i)
	}

	pub(crate) fn new_named_variant(&mut self, id: Ident) -> u32 {
		let i = self.named_variants.len() as u32;
		self.named_variants.push(id);
		i
	}

	pub(crate) fn new_keyword_variant(&mut self, id: Ident) -> u32 {
		let i = self.keyword_variants.len() as u32;
		self.keyword_variants.push(id);
		i
	}

	pub(crate) fn new_node_variant(&mut self, id: Ident) -> u32 {
		let i = self.node_variants.len() as u32;
		self.node_variants.push(id);
		i
	}
}

impl<'a, 'p> chom_ir::Namespace for Namespace<'a, 'p> {
	type Var = Id;

	type Module = ModuleId;

	type Type = TypeId;

	type Param = u32;

	type Variant = VariantId;

	type Field = FieldId;

	type Label = Label;

	type Function = FunctionId;

	fn var_ident(&self, v: Self::Var) -> Ident {
		v.to_ident()
	}

	fn module_ident(&self, m: Self::Module) -> Ident {
		self.modules.get(m.0 as usize).cloned().unwrap()
	}

	fn type_ident(&self, t: Self::Type) -> Ident {
		match t {
			TypeId::Extern(t) => {
				self.grammar().extern_type(t).unwrap().clone()
			},
			TypeId::ExternError => {
				// Note: because of this, `error` is the only reserved extern type name.
				Ident::new("error").unwrap() // TODO: test for reserved name error.
			}
			TypeId::Grammar(t) => {
				let ty = self.grammar.poly().ty(t).unwrap();
				ty.id().as_defined().expect("expected defined grammar type").clone()
			},
			TypeId::Provided(p) => p.ident()
		}
	}

	fn param_ident(&self, p: Self::Param) -> Ident {
		Ident::new(format!("T{}", p)).unwrap()
	}

	fn variant_ident(&self, v: Self::Variant) -> Ident {
		match v {
			VariantId::Function(i) => {
				self.grammar().poly().function(i).unwrap().id().as_defined().clone()
			}
			VariantId::Provided(p) => {
				use provided::{
					Variant,
					TokenVariant
				};
				match p {
					Variant::Token(t) => {
						match t {
							TokenVariant::Named(i) => self.named_variants[i as usize].clone(),
							TokenVariant::Keyword => Ident::new("Keyword").unwrap(),
							TokenVariant::Operator => Ident::new("Operator").unwrap(),
							TokenVariant::Begin => Ident::new("Begin").unwrap(),
							TokenVariant::End => Ident::new("End").unwrap(),
							TokenVariant::Punct => Ident::new("Punct").unwrap()
						}
					}
					Variant::Keyword(i) => self.keyword_variants[i as usize].clone(),
					Variant::Delimiter(d) => d.ident(),
					Variant::Operator(o) => o.ident(),
					Variant::Punct(p) => p.ident(),
					Variant::Node(i) => self.node_variants[i as usize].clone(),
					Variant::Item(i) => i.ident()
				}
			}
		}
	}

	fn field_ident(&self, f: Self::Field) -> Ident {
		self.grammar.poly().function(f.0).unwrap().argument(f.1).unwrap().label().unwrap().clone()
	}

	fn label_ident(&self, l: Self::Label) -> Ident {
		l.to_ident()
	}

	fn function_ident(&self, f: Self::Function) -> Ident {
		match f {
			FunctionId::UndefinedChar => Ident::new("undefined-char").unwrap(),
			FunctionId::ExternParser(index) => {
				let ty = self.grammar.extern_type(index).unwrap();
				Ident::new(format!("parse-{}", ty)).unwrap()
			}
			FunctionId::Lexer => Ident::new("lexer").unwrap(),
			FunctionId::Parser(index) => {
				let ty = self.grammar.ty(index).unwrap();
				let id = ty.composed_id(self.grammar);
				Ident::new(format!("parse-{}", id)).unwrap()
			}
			FunctionId::DebugFormat => Ident::new("debug-format").unwrap()
		}
	}
}