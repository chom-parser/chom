pub mod glue {
	pub enum Error {
		// ...
	}

	pub type Int = i32;
	pub type String = std::string::String;

	pub fn integer(_s: &str) -> Result<Int, Error> {
		unimplemented!()
	}

	pub fn ident(_s: &str) -> Result<String, Error> {
		unimplemented!()
	}

	pub fn unexpected(_c: Option<char>) -> Error {
		unimplemented!()
	}
}

pub mod ast;
pub mod lexer;
pub mod parser;
