use super::{expr, ty, Constant, Expr, Id};
use crate::Ident;

/// Pattern.
#[derive(Clone)]
pub enum Pattern {
	/// Matches any value.
	Any,

	/// Matches any value and bind to the given ident.
	Bind(Id),

	/// `Some` variant of the option type.
	Some(Box<Pattern>),

	/// `None` variant of the option type.
	None,

	/// Enum variant.
	///
	/// The first parameter is the type id.
	/// The second is the variant index.
	/// The third is the inner pattern if any.
	Cons(ty::Ref, u32, ConsArgs),

	/// Constant value.
	Constant(Constant),

	/// Union.
	Or(Vec<Pattern>),
}

#[derive(Clone)]
pub enum ConsArgs {
	Tuple(Vec<Pattern>),
	Struct(Vec<Binding>),
}

impl ConsArgs {
	pub fn is_empty(&self) -> bool {
		match self {
			Self::Tuple(args) => args.is_empty(),
			Self::Struct(bindings) => bindings.is_empty(),
		}
	}
}

#[derive(Clone)]
pub struct Binding {
	pub name: Ident,
	pub pattern: Pattern,
}

impl Pattern {
	pub fn is_union(&self) -> bool {
		match self {
			Self::Or(_) => true,
			_ => false,
		}
	}

	pub fn is_bound(&self) -> bool {
		match self {
			Self::Any => false,
			Self::Bind(_) => true,
			Self::Some(p) => p.is_bound(),
			Self::None => false,
			Self::Cons(_, _, args) => match args {
				ConsArgs::Tuple(args) => args.iter().any(|a| a.is_bound()),
				ConsArgs::Struct(bindings) => bindings.iter().any(|b| b.pattern.is_bound()),
			},
			Self::Constant(_) => false,
			Self::Or(_) => false,
		}
	}

	pub fn bind_any<F>(&self, f: F) -> Self
	where
		F: Copy + Fn() -> Id,
	{
		match self {
			Self::Any => Self::Bind(f()),
			Self::Bind(id) => Self::Bind(*id),
			Self::Some(p) => Self::Some(Box::new(p.bind_any(f))),
			Self::None => Self::None,
			Self::Cons(ty, v, args) => {
				let args = match args {
					ConsArgs::Tuple(args) => {
						ConsArgs::Tuple(args.iter().map(|p| p.bind_any(f)).collect())
					}
					ConsArgs::Struct(bindings) => ConsArgs::Struct(
						bindings
							.iter()
							.map(|b| Binding {
								name: b.name.clone(),
								pattern: b.pattern.bind_any(f),
							})
							.collect(),
					),
				};
				Self::Cons(*ty, *v, args)
			}
			Self::Constant(c) => Self::Constant(*c),
			Self::Or(patterns) => Self::Or(patterns.iter().map(|p| p.bind_any(f)).collect()),
		}
	}

	pub fn as_expr<F>(&self, f: F) -> Expr
	where
		F: Copy + Fn(Option<Id>) -> Expr,
	{
		match self {
			Self::Any => f(None),
			Self::Bind(id) => f(Some(*id)),
			Self::Some(p) => Expr::Some(Box::new(p.as_expr(f))),
			Self::None => Expr::None,
			Self::Cons(ty, v, args) => {
				let args = match args {
					ConsArgs::Tuple(args) => {
						expr::BuildArgs::Tuple(args.iter().map(|p| p.as_expr(f)).collect())
					}
					ConsArgs::Struct(bindings) => expr::BuildArgs::Struct(
						bindings
							.iter()
							.map(|b| expr::Binding {
								name: b.name.clone(),
								expr: b.pattern.as_expr(f),
							})
							.collect(),
					),
				};
				Expr::Cons(*ty, *v, args)
			}
			Self::Constant(c) => Expr::Constant(*c),
			Self::Or(_) => panic!("union pattern cannot be made into an expression"),
		}
	}
}
