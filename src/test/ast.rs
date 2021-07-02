pub enum Expr {
	Term(::source_span::Loc<crate::test::ast::Term>),
	Add(
		Box<::source_span::Loc<crate::test::ast::Expr>>,
		::source_span::Loc<crate::test::ast::Term>,
	),
}
pub struct Term(pub ::source_span::Loc<crate::test::glue::Int>);
impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			crate::test::ast::Expr::Term(arg0) => {
				write!(f, "expr.term({:?})", arg0)
			}
			crate::test::ast::Expr::Add(arg0, arg1) => {
				write!(f, "expr.add({:?}, {:?})", arg0, arg1)
			}
		}
	}
}
impl std::fmt::Debug for Term {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "term({:?})", self.0)
	}
}
