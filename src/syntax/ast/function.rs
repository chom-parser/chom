use super::ty;
use crate::Ident;
use source_span::Loc;

pub struct Function {
	pub id: Option<Loc<Ident>>,
	pub args: Vec<Loc<ty::LabeledExpr>>,
}
