#[macro_use]
extern crate clap;
extern crate grammar;

use std::{
	fs::File,
	io::{
		self,
		Read,
		BufReader,
		Write
	}
};
use utf8_decode::UnsafeDecoder;
use source_span::{
	Position,
	fmt::{
		Formatter,
		Style
	}
};

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

	let filename = matches.value_of("FILE").unwrap();
	let file = File::open(filename)?;
	let input = BufReader::new(file);

	let metrics = source_span::DefaultMetrics::with_tab_stop(4);
	let utf8_input = UnsafeDecoder::new(input.bytes());
	let buffer = source_span::SourceBuffer::new(utf8_input, Position::default(), metrics);
	let mut lexer = syntax::Lexer::new(buffer.iter(), metrics).peekable();

	match syntax::Grammar::parse(&mut lexer) {
		Ok(ast) => {
			match ast.compile() {
				Ok(grammar) => {
					// ...
				},
				Err(e) => {
					let mut fmt = Formatter::new();
					fmt.add(e.span(), Some(format!("{}", e)), Style::Error);
					let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics)?;
					println!("{}", formatted);
				}
			}
		},
		Err(e) => {
			let mut fmt = Formatter::new();
			fmt.add(e.span(), Some(format!("{}", e)), Style::Error);
			let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics)?;
			println!("{}", formatted);
		}
	}

	Ok(())
}
