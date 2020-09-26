#[macro_use]
extern crate clap;
extern crate grammar;

use std::io::{
	self,
	Read,
	Write
};
use utf8_decode::UnsafeDecoder;

use grammar::{
	syntax::{
		self,
		Lexer,
		Parsable
	}
};

fn main() -> io::Result<()> {
	// Parse options.
	let yaml = load_yaml!("cli.yml");
	let matches = clap::App::from_yaml(yaml).get_matches();

	// Init logger.
	let verbosity = matches.occurrences_of("verbose") as usize;
	stderrlog::new().verbosity(verbosity).init().unwrap();

	let input = io::stdin();
	let mut output = io::stdout();

	let utf8_input = UnsafeDecoder::new(input.lock().bytes());
	let mut lexer = syntax::Lexer::new(utf8_input, source_span::DEFAULT_METRICS).peekable();

	Ok(())
}
