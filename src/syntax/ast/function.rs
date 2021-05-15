use source_span::Loc;
use super::{
	Ident,
	ty
};

pub struct Function {
	pub id: Option<Loc<Ident>>,
	pub args: Vec<Loc<ty::Expr>>
}