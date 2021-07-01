use super::{ty, Grammar};
use crate::poly;
pub use poly::function::Id;

/// Polymorphic function.
pub struct Function<'a> {
	/// Polymorphic function.
	poly: &'a poly::Function,

	/// Instance id.
	instance: ty::Instance,

	/// Arguments.
	args: Vec<ty::Expr>,
}

impl<'a> Function<'a> {
	pub(crate) fn new(
		poly: &'a poly::Function,
		instance: ty::Instance,
		args: Vec<ty::Expr>,
	) -> Self {
		Self {
			poly,
			instance,
			args,
		}
	}

	pub fn poly(&self) -> &'a poly::Function {
		self.poly
	}

	pub fn id(&self) -> &Id {
		self.poly.id()
	}

	pub fn return_ty(&self) -> (u32, ty::Instance) {
		(self.poly.return_ty(), self.instance)
	}

	pub fn arity(&self) -> u32 {
		self.args.len() as u32
	}

	pub fn arguments(&self) -> &[ty::Expr] {
		&self.args
	}

	pub fn argument(&self, i: u32) -> Option<ty::Expr> {
		self.args.get(i as usize).cloned()
	}

	pub fn put_argument_on_heap(
		&self,
		grammar: &Grammar,
		context_ty: super::Index,
		i: u32,
	) -> bool {
		self.poly
			.argument(i)
			.map(|a| a.expr().depends_on(grammar.poly(), context_ty.0))
			.unwrap_or(false)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		let mut string = String::new();

		for a in &self.args {
			string.extend(a.instance(grammar).chars())
		}

		string
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
