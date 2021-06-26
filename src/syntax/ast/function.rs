use crate::Ident;
use super::ty;
use source_span::Loc;

pub struct Function {
	pub id: Option<Loc<Ident>>,
	pub args: Vec<Loc<ty::LabeledExpr>>,
}
