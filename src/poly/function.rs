use super::{ty, Grammar, Type};
use crate::Ident;

/// Primitive type constructor.
pub enum Primitive {
	None,
	Some,
	Nil,
	Cons,
}

impl Primitive {
	pub fn as_str(&self) -> &str {
		match self {
			Self::None => "none",
			Self::Some => "some",
			Self::Nil => "nil",
			Self::Cons => "cons",
		}
	}
}

/// Function identifier.
pub enum Id {
	Defined(Ident),
	Primitive(Primitive),
	Cast,
}

impl Id {
	pub fn as_str(&self) -> &str {
		match self {
			Self::Defined(id) => id.as_str(),
			Self::Primitive(p) => p.as_str(),
			Self::Cast => "cast",
		}
	}

	pub fn as_defined(&self) -> &Ident {
		match self {
			Self::Defined(id) => id,
			_ => panic!("invalid function id"),
		}
	}
}

/// Polymorphic function.
pub struct Function {
	/// Identifier.
	id: Id,

	/// Return type.
	return_ty: u32,

	/// Arguments.
	args: Vec<ty::LabeledExpr>,
}

impl Function {
	pub fn new(id: Id, return_ty: u32, args: Vec<ty::LabeledExpr>) -> Self {
		Function {
			id,
			return_ty,
			args,
		}
	}

	pub fn id(&self) -> &Id {
		&self.id
	}

	pub fn return_ty(&self) -> u32 {
		self.return_ty
	}

	pub fn arguments(&self) -> &[ty::LabeledExpr] {
		&self.args
	}

	pub fn argument(&self, offset: u32) -> Option<&ty::LabeledExpr> {
		self.args.get(offset as usize)
	}

	/// Returns an iterator over the typed arguments.
	pub fn typed_arguments<'a>(&'a self, grammar: &'a Grammar, context: &'a Type) -> impl 'a + Iterator<Item=&'a ty::LabeledExpr> {
		self.args.iter().filter(move |a| a.is_typed(grammar, context))
	}

	/// Checks if the function is "fully labeled".
	///
	/// That is when it is not a constant (it has some arguments)
	/// and every typed (non unit) argument is labeled.
	pub fn is_fully_labeled(&self, grammar: &Grammar, context: &Type) -> bool {
		!self.args.is_empty()
			&& self
				.args
				.iter()
				.all(|a| a.is_labeled() || !a.is_typed(grammar, context))
	}

	/// Returns an iterator over the fields of the structure defined by this function, if any.
	pub fn fields<'a>(&'a self, grammar: &'a Grammar, context: &'a Type) -> Option<impl 'a + Iterator<Item=&'a ty::LabeledExpr>> {
		if self.is_fully_labeled(grammar, context) {
			Some(self.typed_arguments(grammar, context))
		} else {
			None
		}
	}
}

// pub struct FormattedExpr<'g, 'e>(&'g Grammar, &'e Expr);

// impl<'g, 'e> fmt::Display for FormattedExpr<'g, 'e> {
// 	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
// 		match self.1 {
// 			Expr::Terminal(t) => {
// 				let t = self.0.terminal(*t).unwrap();
// 				t.format(self.0).fmt(f)
// 			},
// 			Expr::Type(nt, args) => {
// 				let ty = self.0.ty(*nt).unwrap();
// 				ty.name().fmt(f)
// 			}
// 		}
// 	}
// }
