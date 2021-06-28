pub mod pseudo;
pub mod target;

#[derive(Clone, Copy)]
pub enum ParserType {
	LR0,
	LALR1
}