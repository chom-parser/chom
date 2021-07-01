use super::built_in;
use crate::Ident;

pub struct Type {
	/// Index of the containing module.
	module: u32,

	/// Identifier.
	id: Id,

	/// Associated grammar type.
	grammar_ty: Option<GrammarType>,

	/// Parameters.
	parameters: Vec<Ident>,

	/// Description.
	desc: Desc,
}

impl Type {
	/// Creates a new opaque type from a grammar extern type.
	pub fn opaque(module: u32, id: &Ident, grammar_extern_ty: u32) -> Self {
		Self {
			module,
			id: Id::Defined(id.clone()),
			grammar_ty: Some(GrammarType::Extern(grammar_extern_ty)),
			parameters: Vec::new(),
			desc: Desc::Opaque,
		}
	}

	pub fn new(module: u32, id: Ident, grammar_ty: Option<u32>, desc: Desc) -> Self {
		Self {
			module,
			id: Id::Defined(id),
			grammar_ty: grammar_ty.map(GrammarType::Intern),
			parameters: Vec::new(),
			desc,
		}
	}

	pub fn module(&self) -> u32 {
		self.module
	}

	pub fn id(&self) -> &Id {
		&self.id
	}

	pub fn parameters(&self) -> &[Ident] {
		&self.parameters
	}

	pub fn grammar_type(&self) -> Option<GrammarType> {
		self.grammar_ty
	}

	pub fn desc(&self) -> &Desc {
		&self.desc
	}

	pub(crate) fn set_desc(&mut self, desc: Desc) {
		self.desc = desc
	}

	pub fn as_enum(&self) -> &Enum {
		match &self.desc {
			Desc::Enum(enm) => enm,
			_ => panic!("type is not an enum"),
		}
	}
}

#[derive(Clone, Copy)]
pub enum GrammarType {
	Extern(u32),
	Intern(u32),
}

#[derive(Clone)]
pub enum Id {
	BuiltIn(built_in::Type),
	Defined(Ident),
}

#[derive(Clone, Copy)]
pub enum Ref {
	BuiltIn(built_in::Type),
	Defined(u32),
}

pub enum Desc {
	Opaque,
	Enum(Enum),
	Struct(Struct),
	TupleStruct(Vec<Expr>),
}

/// Enumerator type.
pub struct Enum {
	variants: Vec<Variant>,
}

impl Enum {
	pub fn new() -> Self {
		Self {
			variants: Vec::new(),
		}
	}

	pub fn len(&self) -> u32 {
		self.variants.len() as u32
	}

	pub fn is_empty(&self) -> bool {
		self.variants.is_empty()
	}

	pub fn variant(&self, index: u32) -> Option<&Variant> {
		self.variants.get(index as usize)
	}

	pub fn variants(&self) -> &[Variant] {
		&self.variants
	}

	pub fn add_variant(&mut self, v: Variant) -> u32 {
		let i = self.variants.len() as u32;
		self.variants.push(v);
		i
	}

	pub fn not_empty(self) -> Option<Self> {
		if self.is_empty() {
			None
		} else {
			Some(self)
		}
	}
}

/// Enum variant.
pub enum Variant {
	/// Built-in variant.
	BuiltIn(built_in::Variant),

	/// Defined variant.
	Defined(Ident, VariantDesc),
}

impl Variant {
	pub fn is_empty(&self) -> bool {
		match self {
			Self::BuiltIn(t) => t.parameter().is_none(),
			Self::Defined(_, desc) => desc.is_empty(),
		}
	}

	pub fn tuple_parameters(&self) -> Option<&[Expr]> {
		match self {
			Self::BuiltIn(t) => t.parameter().map(|p| std::slice::from_ref(p)),
			Self::Defined(_, VariantDesc::Tuple(params)) => Some(params.as_ref()),
			_ => None,
		}
	}

	pub fn struct_parameter(&self) -> Option<&Struct> {
		match self {
			Self::Defined(_, VariantDesc::Struct(s)) => Some(s),
			_ => None,
		}
	}
}

pub enum VariantDesc {
	/// The variant contains untagged parameters.
	///
	/// The given list is not empty.
	Tuple(Vec<Expr>),

	/// The variant contains only tagged parameters,
	/// defining a structure.
	Struct(Struct),
}

impl VariantDesc {
	pub fn len(&self) -> u32 {
		match self {
			Self::Tuple(args) => args.len() as u32,
			Self::Struct(s) => s.len(),
		}
	}

	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}
}

/// Structure type.
pub struct Struct {
	fields: Vec<Field>,
}

impl Struct {
	pub fn new() -> Self {
		Self { fields: Vec::new() }
	}

	pub fn len(&self) -> u32 {
		self.fields.len() as u32
	}

	pub fn is_empty(&self) -> bool {
		self.fields.is_empty()
	}

	pub fn fields(&self) -> &[Field] {
		&self.fields
	}

	pub fn add_field(&mut self, id: Ident, ty: Expr) -> u32 {
		let field = Field { id, ty };
		let i = self.fields.len() as u32;
		self.fields.push(field);
		i
	}
}

pub struct Field {
	pub id: Ident,
	pub ty: Expr,
}

/// Type expression.
#[derive(Clone)]
pub enum Expr {
	/// Type variable.
	Var(u32),

	/// Unit type.
	Unit,

	/// Built-in type.
	BuiltIn(built_in::Type),

	/// Defined type.
	///
	/// The first parameter is the index of the type definition in `Context`.
	/// The second parameter is the list of type parameters.
	Defined(u32, Vec<Expr>),

	/// Located type.
	///
	/// In Rust, this will generally be translated into `::source_span::Loc<T>`.
	Loc(Box<Expr>),

	/// On-heap type.
	///
	/// In Rust, this will generally be translated into `Box`.
	Heap(Box<Expr>),

	/// Optional type.
	Option(Box<Expr>),

	/// List type.
	///
	/// In Rust, this will generally be translated into `Vec`.
	List(Box<Expr>),
}

impl Expr {
	pub fn as_defined(&self) -> Option<(u32, &[Expr])> {
		match self {
			Self::Defined(i, args) => Some((*i, args.as_ref())),
			_ => None,
		}
	}
}
