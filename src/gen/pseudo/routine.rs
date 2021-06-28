pub mod lexer;
pub mod lr0;

#[derive(Clone, Copy)]
pub enum Routine {
	Lexer,
	Parser(crate::gen::ParserType)
}