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
	out,
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
					match grammar::lexing::Table::new(&grammar) {
						Ok(lexing_table) => {
							// let stdout = std::io::stdout();
							// let mut out = stdout.lock();
							// lexing_table.automaton().dot_write(&grammar, &mut out).unwrap();

							// let code = grammar::gen::lexer::generate(&grammar, &lexing_table);
							// println!("{}", code)
						},
						Err(e) => {
							let mut block = out::Block::new(out::Type::Error, e.title());
							block.highlights_mut().add(e.span(), None, Style::Error);
							e.fill_block(&grammar, &mut block);
							let span = e.span().aligned();
							let formatted = block.render(buffer.iter_span(span), span, &metrics)?;
							eprintln!("{}", formatted);
						}
					}
				},
				Err(e) => {
					let mut fmt = Formatter::new();
					fmt.add(e.span(), Some(format!("{}", e)), Style::Error);
					e.format_notes(&mut fmt, Style::Note);
					let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics)?;
					eprintln!("{}", formatted);
				}
			}
		},
		Err(e) => {
			let mut fmt = Formatter::new();
			fmt.add(e.span(), Some(format!("{}", e)), Style::Error);
			let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics)?;
			eprintln!("{}", formatted);
		}
	}

	Ok(())
}
