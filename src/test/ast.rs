pub enum Expr {
	Term(::source_span::Loc<crate::test::ast::Term>),
	Add(
		Box<::source_span::Loc<crate::test::ast::Expr>>,
		::source_span::Loc<crate::test::ast::Term>,
	),
}
pub struct Term(pub ::source_span::Loc<crate::test::glue::Int>);
