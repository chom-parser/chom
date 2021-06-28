use super::Grammar;
use crate::Ident;
use std::fmt;

/// Polymorphic type identifier.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Id {
	Primitive(Primitive),
	Defined(Ident),
}

impl Id {
	pub fn name(&self) -> &str {
		match self {
			Self::Primitive(p) => p.name(),
			Self::Defined(id) => id.as_str(),
		}
	}

	pub fn as_defined(&self) -> &Ident {
		match self {
			Self::Defined(id) => id,
			_ => panic!("invalid type id")
		}
	}
}

/// Primitive type id.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Primitive {
	Option,
	List,
}

impl Primitive {
	pub fn name(&self) -> &str {
		match self {
			Self::Option => "option",
			Self::List => "list",
		}
	}
}

pub struct Parameters {
	/// Type parameters.
	type_parameters: Vec<Ident>,

	/// Terminal parameters.
	terminal_parameters: Vec<Ident>,

	/// List.
	list: Vec<Parameter>
}

impl Parameters {
	pub fn new() -> Self {
		Self {
			type_parameters: Vec::new(),
			terminal_parameters: Vec::new(),
			list: Vec::new()
		}
	}

	pub fn single_type_parameter() -> Self {
		Self {
			type_parameters: vec![Ident::new("t").unwrap()],
			terminal_parameters: Vec::new(),
			list: vec![Parameter::Type(0)]
		}
	}

	pub fn len(&self) -> u32 {
		self.list.len() as u32
	}

	pub fn is_empty(&self) -> bool {
		self.list.is_empty()
	}
	
	pub fn iter(&self) -> impl '_ + Iterator<Item=Parameter> {
		self.list.iter().cloned()
	}

	pub fn get(&self, i: u32) -> Option<Parameter> {
		self.list.get(i as usize).cloned()
	}

	pub fn type_parameter(&self, i: u32) -> Option<&Ident> {
		self.type_parameters.get(i as usize)
	}

	pub fn type_parameters(&self) -> impl '_ + Iterator<Item=&Ident> {
		self.type_parameters.iter()
	}

	pub fn terminal_parameter(&self, i: u32) -> Option<&Ident> {
		self.terminal_parameters.get(i as usize)
	}

	pub fn terminal_parameters(&self) -> impl '_ + Iterator<Item=&Ident> {
		self.terminal_parameters.iter()
	}

	pub fn add_type_parameter(&mut self, id: Ident) {
		let i = self.type_parameters.len() as u32;
		self.type_parameters.push(id);
		self.list.push(Parameter::Type(i))
	}

	pub fn add_terminal_parameter(&mut self, id: Ident) {
		let i = self.terminal_parameters.len() as u32;
		self.terminal_parameters.push(id);
		self.list.push(Parameter::Terminal(i))
	}
}

/// Type parameter.
#[derive(Clone)]
pub enum Parameter {
	/// Type.
	/// 
	/// Provides its index in `Parameters::type_parameters`.
	Type(u32),

	/// Terminal.
	/// 
	/// Provides its index in `Parameters::terminal_parameters`.
	Terminal(u32)
}

impl Parameter {
	pub fn display<'s, 'p>(&'s self, parameters: &'p Parameters) -> DisplayParameter<'s, 'p> {
		DisplayParameter(self, parameters)
	}
}

pub struct DisplayParameter<'s, 'p>(&'s Parameter, &'p Parameters);

impl<'s, 'p> fmt::Display for DisplayParameter<'s, 'p> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.0 {
			Parameter::Type(i) => {
				let id = self.1.type_parameter(*i).unwrap();
				write!(f, "<{}>", id)
			},
			Parameter::Terminal(i) => {
				let id = self.1.terminal_parameter(*i).unwrap();
				id.fmt(f)
			}
		}
	}
}

/// Polymorphic type.
pub struct Type {
	/// Identifier.
	id: Id,

	/// Parameters.
	parameters: Parameters,

	/// Constructors.
	constructors: Vec<u32>,
}

impl Type {
	pub fn new(id: Id) -> Self {
		Self {
			id,
			parameters: Parameters::new(),
			constructors: Vec::new(),
		}
	}

	pub fn defined(id: Ident) -> Self {
		Self::new(Id::Defined(id))
	}

	pub fn primitive_option() -> Self {
		Self {
			id: Id::Primitive(Primitive::Option),
			parameters: Parameters::single_type_parameter(),
			constructors: Vec::new(),
		}
	}

	pub fn primitive_list() -> Self {
		Self {
			id: Id::Primitive(Primitive::List),
			parameters: Parameters::single_type_parameter(),
			constructors: Vec::new(),
		}
	}

	pub fn id(&self) -> &Id {
		&self.id
	}

	pub fn name(&self) -> &str {
		self.id().name()
	}

	pub fn defined_id(&self) -> Option<&Ident> {
		match &self.id {
			Id::Defined(id) => Some(id),
			_ => None,
		}
	}

	pub fn parameters(&self) -> &Parameters {
		&self.parameters
	}

	pub fn parameter(&self, i: u32) -> Option<Parameter> {
		self.parameters.get(i)
	}

	pub fn constructor_count(&self) -> u32 {
		self.constructors.len() as u32
	}

	pub fn constructors(&self) -> &[u32] {
		&self.constructors
	}

	pub fn add_constructor(&mut self, rule: u32) {
		self.constructors.push(rule)
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.id.name().fmt(f)?;

		for p in self.parameters.iter() {
			write!(f, " {}", p.display(&self.parameters))?;
		}

		Ok(())
	}
}

// pub enum GrammarType {
// 	Extern(u32),
// 	Intern(u32, Vec<GrammarType>),
// 	Var(u32)
// }

/// Labeled type expression.
pub struct LabeledExpr {
	label: Option<Ident>,
	expr: Expr,
}

impl LabeledExpr {
	pub fn new(label: Option<Ident>, expr: Expr) -> Self {
		Self { label, expr }
	}

	pub fn label(&self) -> Option<&Ident> {
		self.label.as_ref()
	}

	pub fn expr(&self) -> &Expr {
		&self.expr
	}

	pub fn is_labeled(&self) -> bool {
		self.label.is_some()
	}

	pub fn is_typed(&self, grammar: &Grammar, context: &Type) -> bool {
		self.expr.is_typed(grammar, context)
	}
}

/// Expression.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expr {
	Var(u32),
	Terminal(u32),
	Type(u32, Vec<Expr>),
}

impl Expr {
	pub fn depends_on(&self, other: &Self) -> bool {
		if self == other {
			true
		} else if let Self::Type(_, args) = self {
			args.iter().any(|a| a.depends_on(other))
		} else {
			false
		}
	}

	/// Checks if this expression is typed (with an intern or extern type).
	pub fn is_typed(&self, grammar: &Grammar, context: &Type) -> bool {
		match self {
			Self::Var(x) => match context.parameter(*x).unwrap() {
				Parameter::Terminal(_) => false,
				Parameter::Type(_) => true
			},
			Self::Terminal(t) => {
				grammar.terminal(*t).unwrap().extern_type(grammar).is_some()
			},
			Self::Type(_, _) => true
		}
	}

	// pub fn ty(&self, grammar: &Grammar, context: &Type) -> Option<GrammarType> {
	// 	match self {
	// 		Self::Var(x) => match context.parameter(*x).unwrap() {
	// 			Parameter::Terminal(t) => None,
	// 			Parameter::Type(_, i) => Some(GrammarType::Var(*i))
	// 		},
	// 		Self::Terminal(t) => {
	// 			grammar.terminal(t).unwrap().ty().map(GrammarType::Extern)
	// 			// context.parameter(index: u32)
	// 		},
	// 		Self::Type(t, _) => {
	// 			// ...
	// 		}
	// 	}
	// }

	pub fn format<'g>(&self, context: &'g Type, grammar: &'g Grammar) -> FormattedExpr<'g, '_> {
		FormattedExpr(grammar, context, self)
	}
}

pub struct FormattedExpr<'g, 'e>(&'g Grammar, &'g Type, &'e Expr);

impl<'g, 'e> fmt::Display for FormattedExpr<'g, 'e> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.2 {
			Expr::Var(x) => self.1.parameter(*x).unwrap().display(self.1.parameters()).fmt(f),
			Expr::Terminal(t) => {
				let t = self.0.terminal(*t).unwrap();
				t.format(self.0).fmt(f)
			}
			Expr::Type(nt, _args) => {
				let ty = self.0.ty(*nt).unwrap();
				ty.name().fmt(f)
			}
		}
	}
}
