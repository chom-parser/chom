pub mod pseudo;
pub mod target;

pub use target::{
	Target,
	Generate
};

pub enum Parser {
	LR0(pseudo::Expr),
	LALR1(pseudo::Expr)
}