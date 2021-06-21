use super::{ty, Grammar};
use crate::syntax::Ident;
use source_span::Loc;
use std::fmt;

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
}

/// Polymorphic function.
pub struct Function {
	/// Identifier.
	id: Id,

	/// Return type.
	return_ty: u32,

	/// Arguments.
	args: Vec<ty::Expr>,
}

impl Function {
	pub fn new(id: Id, return_ty: u32, args: Vec<ty::Expr>) -> Self {
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

	pub fn arguments(&self) -> &[ty::Expr] {
		&self.args
	}

	pub fn argument(&self, offset: usize) -> Option<&ty::Expr> {
		self.args.get(offset)
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
