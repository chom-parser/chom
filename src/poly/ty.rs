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

/// Polymorphic type.
pub struct Type {
	/// Identifier.
	id: Id,

	/// Parameters.
	parameters: Vec<Parameter>,

	/// Constructors.
	constructors: Vec<u32>,
}

/// Type parameter.
pub enum Parameter {
	/// Type.
	Type(Ident),

	/// Terminal.
	Terminal(Ident),
}

impl fmt::Display for Parameter {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Type(id) => write!(f, "<{}>", id),
			Self::Terminal(id) => id.fmt(f),
		}
	}
}

impl Type {
	pub fn new(id: Id) -> Self {
		Self {
			id,
			parameters: Vec::new(),
			constructors: Vec::new(),
		}
	}

	pub fn defined(id: Ident) -> Self {
		Self::new(Id::Defined(id))
	}

	pub fn primitive_option() -> Self {
		Self {
			id: Id::Primitive(Primitive::Option),
			parameters: vec![Parameter::Type(Ident::new("t".to_string()).unwrap())],
			constructors: Vec::new(),
		}
	}

	pub fn primitive_list() -> Self {
		Self {
			id: Id::Primitive(Primitive::List),
			parameters: vec![Parameter::Type(Ident::new("t".to_string()).unwrap())],
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

	pub fn parameters(&self) -> &[Parameter] {
		&self.parameters
	}

	pub fn parameter(&self, index: u32) -> Option<&Parameter> {
		self.parameters.get(index as usize)
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

		for p in &self.parameters {
			write!(f, " {}", p)?;
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

	pub fn is_typed(&self, grammar: &Grammar) -> bool {
		self.expr.is_typed(grammar)
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
			Expr::Var(x) => self.1.parameter(*x).unwrap().fmt(f),
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
