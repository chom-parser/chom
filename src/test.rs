pub mod glue {
	pub enum Error {
		// ...
	}

	pub type Int = i32;

	pub fn integer(s: &str) -> Result<Int, Error> {
		unimplemented!()
	}

	pub fn unexpected(c: Option<char>) -> Error {
		unimplemented!()
	}
}

pub mod ast;
pub mod lexer;
pub mod parser;
