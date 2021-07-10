#![feature(arbitrary_self_types)]
extern crate source_span;

pub mod charset;
pub mod gen;
mod ident;
pub mod lexing;
pub mod mono;
pub mod out;
pub mod parsing;
pub mod poly;
pub mod syntax;
pub mod util;

pub use charset::CharSet;
pub use ident::Ident;

pub enum ParseAndCompileError {
	Syntax(syntax::Error),
	Compile(poly::Error)
}

/// Helpful function to parse and compile a grammar at once.
pub fn parse_and_compile<I: Iterator<Item=std::io::Result<char>>, M: source_span::Metrics>(buffer: &source_span::SourceBuffer<std::io::Error, I, M>, metrics: M) -> Result<mono::OwnedGrammar, source_span::Loc<ParseAndCompileError>> {
	use syntax::Parsable;
	let mut lexer = syntax::Lexer::new(buffer.iter(), metrics).peekable();
	let ast = syntax::Grammar::parse(&mut lexer).map_err(|e| e.map(ParseAndCompileError::Syntax))?;
	let grammar = ast.compile().map_err(|e| e.map(ParseAndCompileError::Compile))?;
	Ok(mono::OwnedGrammar::new(grammar.into_inner()))
}