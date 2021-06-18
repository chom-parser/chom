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
	},
	mono
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
							let formatted = block.render(buffer.iter_span(block.span()), &metrics)?;
							eprintln!("{}", formatted);
						}
					}

					let mono_grammar = mono::Grammar::new(&grammar);

					// Generate the lexer
					// let stdout = std::io::stdout();
					// let mut out = stdout.lock();
					// let tokens = grammar::gen::target::rust::ast::generate(&mono_grammar);
					// write!(out, "{}", tokens)?;

					let parsing_table = grammar::parsing::table::NonDeterministic::new(&mono_grammar);

					// let stdout = std::io::stdout();
					// let mut out = stdout.lock();
					// parsing_table.dot_write(&grammar, &mut out).unwrap();

					match grammar::parsing::table::LR0::from_non_deterministic(&mono_grammar, &parsing_table) {
						Ok(lr0_table) => {
							use grammar::gen::target::rust;

							let extern_mod_path = ["glue".to_string()];
							let ast_mod_path = ["ast".to_string()];
							let lexer_mod_path = ["lexer".to_string()];

							let mut context = rust_codegen::Context::new();
							let env = rust::Env::new(
								&mut context,
								&mono_grammar,
								&extern_mod_path,
								&ast_mod_path,
								&lexer_mod_path
							);
							
							let parser_mod = rust::lr0::generate(&env, &mono_grammar, &lr0_table);
							let parser_mod_inner = parser_mod.inner();

							let stdout = std::io::stdout();
							let mut out = stdout.lock();
							write!(out, "{}", quote::quote! { #parser_mod_inner })?;
						},
						Err(e) => {
							let mut block = out::Block::new(out::Type::Error, e.title());
							block.highlights_mut().add(e.span(), e.label(), Style::Error);
							e.fill_block(&mono_grammar, &mut block);
							let formatted = block.render(buffer.iter_span(block.span()), &metrics)?;
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
