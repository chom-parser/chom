use crate::mono;

pub mod lexer;
pub mod lr0;

pub enum Routine {
	Lexer(super::Expr),
	Parser(mono::Index, crate::gen::Parser)
}