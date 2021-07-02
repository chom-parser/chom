use crate::mono;

pub mod format;
pub mod lexer;
pub mod lr0;

pub enum Routine {
	/// Formatting.
	Format(super::ty::Ref, super::Expr),

	/// Lexer.
	Lexer(super::Expr),

	/// Parser.
	Parser(mono::Index, crate::gen::Parser),
}
