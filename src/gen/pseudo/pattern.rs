use super::{expr, ty, Constant, Expr, Id};

/// Pattern.
#[derive(Clone)]
pub enum Pattern {
	/// Matches any value.
	Any,

	/// Matches any value and bind to the given ident.
	BindAny(Id),

	/// Matches the given pattern and bind to the given ident.
	Bind(Id, Box<Pattern>),

	/// `Some` variant of the option type.
	Some(Box<Pattern>),

	/// `None` variant of the option type.
	None,

	/// Enum variant.
	///
	/// The first parameter is the type id.
	/// The second is the variant index.
	/// The third is the inner pattern if any.
	Cons(ty::Ref, u32, Vec<Pattern>),

	/// Constant value.
	Constant(Constant),

	/// Union.
	Or(Vec<Pattern>),
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
			Self::BindAny(_) => true,
			Self::Bind(_, _) => true,
			Self::Some(p) => p.is_bound(),
			Self::None => false,
			Self::Cons(_, _, args) => args.iter().any(|a| a.is_bound()),
			Self::Constant(_) => false,
			Self::Or(_) => false,
		}
	}

	pub fn bind_any<F>(&self, f: F) -> Self
	where
		F: Copy + Fn() -> Id,
	{
		match self {
			Self::Any => Self::BindAny(f()),
			Self::BindAny(id) => Self::BindAny(*id),
			Self::Bind(id, p) => Self::Bind(*id, Box::new(p.bind_any(f))),
			Self::Some(p) => Self::Some(Box::new(p.bind_any(f))),
			Self::None => Self::None,
			Self::Cons(ty, v, args) => {
				let args = args.iter().map(|p| p.bind_any(f)).collect();
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
			Self::BindAny(id) => f(Some(*id)),
			Self::Bind(_, p) => p.as_expr(f),
			Self::Some(p) => Expr::Some(Box::new(p.as_expr(f))),
			Self::None => Expr::None,
			Self::Cons(ty, v, args) => {
				let args = args.iter().map(|p| p.as_expr(f)).collect();
				Expr::Cons(*ty, *v, expr::BuildArgs::Tuple(args))
			}
			Self::Constant(c) => Expr::Constant(*c),
			Self::Or(_) => panic!("union pattern cannot be made into an expression"),
		}
	}
}