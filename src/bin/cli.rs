#[macro_use]
extern crate clap;

use source_span::{
	fmt::{Formatter, Style},
	Loc, Position,
};
use std::{
	fs::File,
	io::{self, BufReader, Read},
	path::{Path, PathBuf},
};
use utf8_decode::UnsafeDecoder;

use chom::{
	mono, out, poly,
	syntax::{self, Parsable},
	gen::{
		Target as GenTarget,
		Generate
	}
};

fn is_path_separator(c: char) -> bool {
	c == '/' || c == '.' || c == ':'
}

fn parse_path(s: &str) -> Vec<String> {
	s.split(is_path_separator)
		.filter(|segment| !segment.is_empty())
		.map(|s| s.to_string())
		.collect()
}

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

	log::info!("parsing grammar...");
	match syntax::Grammar::parse(&mut lexer) {
		Ok(ast) => {
			log::info!("compiling grammar...");
			match ast.compile() {
				Ok(grammar) => {
					let mono_grammar = mono::Grammar::new(&grammar);
					if let Err(e) = run_subcommand(&matches, &mono_grammar) {
						e.format(&buffer, &metrics)?;
						std::process::exit(1)
					}
				}
				Err(e) => {
					let mut fmt = Formatter::new();
					fmt.add(e.span(), Some(format!("{}", e)), Style::Error);
					e.format_notes(&mut fmt, Style::Note);
					let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics)?;
					eprintln!("{}", formatted);
					std::process::exit(1)
				}
			}
		}
		Err(e) => {
			let mut fmt = Formatter::new();
			fmt.add(e.span(), Some(format!("{}", e)), Style::Error);
			let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics)?;
			eprintln!("{}", formatted);
			std::process::exit(1)
		}
	}

	Ok(())
}

fn run_subcommand<'a, 'p>(
	matches: &clap::ArgMatches,
	grammar: &'a mono::Grammar<'p>,
) -> Result<(), Error<'a, 'p>> {
	match matches.subcommand() {
		("table", Some(_m)) => generate_parse_table(grammar.poly(), &mut Output::std()?),
		("parser", Some(m)) => {
			let extern_mod_path = parse_path(m.value_of("EXTERN").unwrap());
			let ast_mod_path = parse_path(m.value_of("AST").unwrap());
			let lexer_mod_path = parse_path(m.value_of("LEXER").unwrap());
			let parser_mod_path = parse_path(m.value_of("PARSER").unwrap());

			let context = generate_pseudo_context(
				grammar,
				&extern_mod_path,
				&ast_mod_path,
				&lexer_mod_path,
				&parser_mod_path
			)?;

			let mut lexer_output = None;
			let mut ast_output = None;
			let mut parser_output = None;

			let target = Target::rust();

			match m.value_of("std-output") {
				None => {
					let root = m
						.value_of("root")
						.map(|d| Path::new(d))
						.unwrap_or(Path::new(""));
					let create_parents = m.is_present("create-parents");
					ast_output = Some(Output::file(
						target.module_filename(root, context.ast_module_path()),
						create_parents,
					)?);
					lexer_output = Some(Output::file(
						target.module_filename(root, context.lexer_module_path()),
						create_parents,
					)?);
					parser_output = Some(Output::file(
						target.module_filename(root, context.parser_module_path()),
						create_parents,
					)?);
				}
				Some(component) => match component {
					"lexer" => lexer_output = Some(Output::std()?),
					"ast" => ast_output = Some(Output::std()?),
					"parser" => parser_output = Some(Output::std()?),
					_ => {
						log::error!("unknown component `{}`", component);
						std::process::exit(1)
					}
				},
			}

			Ok(generate_parser(
				context,
				target,
				lexer_output.as_mut(),
				ast_output.as_mut(),
				parser_output.as_mut(),
			)?)
		}
		(name, _) => Err(Error::UnknownCommand(name.to_string())),
	}
}

enum Error<'a, 'p> {
	UnknownCommand(String),
	IO(io::Error),
	Lexing(&'p poly::Grammar, Loc<chom::lexing::Error>),
	LR0Ambiguity(&'a mono::Grammar<'p>, Loc<chom::parsing::table::lr0::Ambiguity>),
}

impl<'a, 'p> Error<'a, 'p> {
	fn format<E, I: Iterator<Item = Result<char, E>>, M: source_span::Metrics>(
		&self,
		buffer: &source_span::SourceBuffer<E, I, M>,
		metrics: &M,
	) -> Result<(), E> {
		match self {
			Self::UnknownCommand(name) => {
				eprintln!("unknown command `{}`", name)
			}
			Self::IO(e) => {
				eprintln!("{}", e)
			}
			Self::Lexing(grammar, e) => {
				let mut block = out::Block::new(out::Type::Error, e.title());
				block.highlights_mut().add(e.span(), None, Style::Error);
				e.fill_block(&grammar, &mut block);
				let formatted = block.render(buffer.iter_span(block.span()), metrics)?;
				eprintln!("{}", formatted);
			}
			Self::LR0Ambiguity(mono_grammar, e) => {
				let mut block = out::Block::new(out::Type::Error, e.title());
				block
					.highlights_mut()
					.add(e.span(), e.label(), Style::Error);
				e.fill_block(&mono_grammar, &mut block);
				let formatted = block.render(buffer.iter_span(block.span()), metrics)?;
				eprintln!("{}", formatted);
			}
		}

		Ok(())
	}
}

impl<'a, 'p> From<io::Error> for Error<'a, 'p> {
	fn from(e: io::Error) -> Self {
		Self::IO(e)
	}
}

fn generate_parse_table<'a, 'p>(
	grammar: &'p poly::Grammar,
	output: &mut Output,
) -> Result<(), Error<'a, 'p>> {
	let mono_grammar = mono::Grammar::new(&grammar);
	let parsing_table = chom::parsing::table::NonDeterministic::new(&mono_grammar);
	parsing_table.dot_write(&mono_grammar, &mut output.lock())?;
	Ok(())

	// match chom::parsing::table::LR0::from_non_deterministic(&mono_grammar, &parsing_table) {
	// 	Ok(lr0_table) => {
	// 		// ...

	// 		Ok(())
	// 	},
	// 	Err(e) => Err(Error::LR0Ambiguity(mono_grammar, e))
	// }
}

fn generate_pseudo_context<'a, 'g>(
	grammar: &'a mono::Grammar<'g>,
	extern_module_path: &[String],
	ast_module_path: &[String],
	lexer_module_path: &[String],
	parser_module_path: &[String]
) -> Result<chom::gen::pseudo::Context<'a, 'g>, Error<'a, 'g>> {
	match chom::lexing::Table::new(grammar.poly()) {
		Ok(lexing_table) => {
			let parsing_table = chom::parsing::table::NonDeterministic::new(grammar);

			match chom::parsing::table::LR0::from_non_deterministic(grammar, &parsing_table) {
				Ok(lr0_table) => {
					Ok(chom::gen::pseudo::Context::new(
						grammar,
						extern_module_path,
						ast_module_path,
						lexer_module_path,
						&lexing_table,
						parser_module_path,
						&chom::parsing::Table::LR0(lr0_table)
					))
				}
				Err(e) => Err(Error::LR0Ambiguity(grammar, e)),
			}
		}
		Err(e) => Err(Error::Lexing(grammar.poly(), e)),
	}
}

fn generate_parser<'a, 'p>(
	context: chom::gen::pseudo::Context<'a, 'p>,
	target: Target,
	lexer_output: Option<&mut Output>,
	ast_output: Option<&mut Output>,
	parser_output: Option<&mut Output>,
) -> io::Result<()> {
	if let Some(ast_output) = ast_output {
		// Generate the AST
		log::info!("writing AST module");
		target.write_module(&mut ast_output.lock(), &context, context.ast_module())?
	}

	if let Some(lexer_output) = lexer_output {
		// Generate the lexer
		log::info!("writing lexer module");
		target.write_module(&mut lexer_output.lock(), &context, context.lexer_module())?
	}

	if let Some(parser_output) = parser_output {
		// Generate the parser
		log::info!("writing parser module");
		target.write_module(&mut parser_output.lock(), &context, context.parser_module())?
	}

	Ok(())
}

pub enum Target {
	Rust(chom::gen::target::Rust),
}

impl Target {
	fn rust() -> Self {
		Self::Rust(chom::gen::target::Rust)
	}

	fn module_filename<P: AsRef<Path>>(&self, root: P, path: chom::gen::pseudo::module::Path) -> PathBuf {
		let filename = match self {
			Self::Rust(target) => target.module_filename(root, path)
		};

		log::info!("will write to `{}`", filename.to_string_lossy());
		filename
	}

	fn write_module<O: io::Write>(&self, out: &mut O, context: &chom::gen::pseudo::Context, module: &chom::gen::pseudo::Module) -> io::Result<()> {
		match self {
			Self::Rust(target) => write!(out, "{}", target.generate(context, module))
		}
	}
}

pub enum Output {
	Std(std::io::Stdout),
	File(std::fs::File),
}

impl Output {
	fn std() -> io::Result<Self> {
		Ok(Self::Std(std::io::stdout()))
	}

	fn file<P: AsRef<Path>>(path: P, create_parents: bool) -> io::Result<Self> {
		if create_parents {
			if let Some(parent) = path.as_ref().parent() {
				std::fs::create_dir_all(parent)?
			}
		}

		Ok(Self::File(File::create(path)?))
	}

	fn lock<'a>(&'a mut self) -> LockedOutput<'a> {
		match self {
			Output::Std(stdout) => LockedOutput::Std(stdout.lock()),
			Output::File(ref mut file) => LockedOutput::File(file),
		}
	}
}

pub enum LockedOutput<'a> {
	Std(std::io::StdoutLock<'a>),
	File(&'a mut std::fs::File),
}

impl<'a> io::Write for LockedOutput<'a> {
	fn write(&mut self, data: &[u8]) -> io::Result<usize> {
		match self {
			Self::Std(o) => o.write(data),
			Self::File(o) => o.write(data),
		}
	}

	fn flush(&mut self) -> io::Result<()> {
		match self {
			Self::Std(o) => o.flush(),
			Self::File(o) => o.flush(),
		}
	}
}
