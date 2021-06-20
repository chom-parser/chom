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
	},
	path::{
		Path,
		PathBuf
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

fn is_path_separator(c: char) -> bool {
	c == '/' || c == '.' || c == ':'
}

fn parse_path(s: &str) -> Vec<String> {
	s.split(is_path_separator).filter(|segment| !segment.is_empty()).map(|s| s.to_string()).collect()
}

fn main() -> io::Result<()> {
	// Parse options.
	let yaml = load_yaml!("cli.yml");
	let matches = clap::App::from_yaml(yaml).get_matches();

	// Init logger.
	let verbosity = matches.occurrences_of("verbose") as usize;
	stderrlog::new().verbosity(verbosity).init().unwrap();

	let filename = matches.value_of("FILE").unwrap();

	match matches.subcommand() {
		("table", Some(m)) => {
			// ...
		},
		("parser", Some(m)) => {
			let extern_mod_path = parse_path(m.value_of("EXTERN").unwrap());
			let lexer_mod_path = parse_path(m.value_of("LEXER").unwrap());
			let ast_mod_path = parse_path(m.value_of("AST").unwrap());
			let parser_mod_path = parse_path(m.value_of("PARSER").unwrap());

			let mut lexer_output = None;
			let mut ast_output = None;
			let mut parser_output = None;

			let target = Target::Rust;

			match m.value_of("std-output") {
				None => {
					lexer_output = Some(Output::file(target.path_into_filename(&lexer_mod_path))?);
					ast_output = Some(Output::file(target.path_into_filename(&ast_mod_path))?);
					parser_output = Some(Output::file(target.path_into_filename(&parser_mod_path))?);
				},
				Some(component) => {
					match component {
						"lexer" => lexer_output = Some(Output::std()?),
						"ast" => ast_output = Some(Output::std()?),
						"parser" => parser_output = Some(Output::std()?),
						_ => {
							log::error!("unknown component `{}`", component);
							std::process::exit(1)
						}
					}
				}
			}

			let file = File::open(filename)?;
			let mut input = BufReader::new(file);

			generate_parser(
				&mut input,
				target,
				&extern_mod_path,
				&lexer_mod_path,
				&ast_mod_path,
				&parser_mod_path,
				lexer_output.as_mut(),
				ast_output.as_mut(),
				parser_output.as_mut()
			)?
		},
		(name, _) => {
			log::error!("unknown command `{}`", name);
			std::process::exit(1)
		}
	}
	
	Ok(())
}

fn generate_parser<R: std::io::BufRead>(
	input: &mut R,
	target: Target,
	extern_mod_path: &[String],
	lexer_mod_path: &[String],
	ast_mod_path: &[String],
	parser_mod_path: &[String],
	lexer_output: Option<&mut Output>,
	ast_output: Option<&mut Output>,
	parser_output: Option<&mut Output>
) -> io::Result<()> {
	let metrics = source_span::DefaultMetrics::with_tab_stop(4);
	let utf8_input = UnsafeDecoder::new(input.bytes());
	let buffer = source_span::SourceBuffer::new(utf8_input, Position::default(), metrics);
	let mut lexer = syntax::Lexer::new(buffer.iter(), metrics).peekable();

	match syntax::Grammar::parse(&mut lexer) {
		Ok(ast) => {
			match ast.compile() {
				Ok(grammar) => {
					let mono_grammar = mono::Grammar::new(&grammar);
					let env = target.env(&mono_grammar, extern_mod_path);
					let ast_module = env.generate_ast(&mono_grammar, ast_mod_path);

					if let Some(ast_output) = ast_output {
						// Generate the AST
						log::info!("writing AST module");
						ast_module.write(&mut ast_output.lock())?
					}

					match grammar::lexing::Table::new(&grammar) {
						Ok(lexing_table) => {
							let lexer_module = env.generate_lexer(&mono_grammar, &lexing_table, lexer_mod_path);

							if let Some(lexer_output) = lexer_output {
								// Generate the lexer
								log::info!("writing lexer module");
								lexer_module.write(&mut lexer_output.lock())?
							}

							let parsing_table = grammar::parsing::table::NonDeterministic::new(&mono_grammar);

							// let stdout = std::io::stdout();
							// let mut out = stdout.lock();
							// parsing_table.dot_write(&grammar, &mut out).unwrap();

							match grammar::parsing::table::LR0::from_non_deterministic(&mono_grammar, &parsing_table) {
								Ok(lr0_table) => {
									let parser_module = env.generate_lr0_parser(&mono_grammar, &ast_module, &lexer_module, &lr0_table, parser_mod_path);

									// let stdout = std::io::stdout();
									// let mut out = stdout.lock();
									// write!(out, "{}", quote::quote! { #parser_mod_inner })?;
									if let Some(parser_output) = parser_output {
										// Generate the parser
										log::info!("writing parser module");
										parser_module.write(&mut parser_output.lock())?
									}
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
							let mut block = out::Block::new(out::Type::Error, e.title());
							block.highlights_mut().add(e.span(), None, Style::Error);
							e.fill_block(&grammar, &mut block);
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

pub enum Target {
	Rust
}

impl Target {
	fn path_into_filename(&self, path: &[String]) -> PathBuf {
		let mut filename = PathBuf::new();

		match self {
			Self::Rust => {
				for segment in path {
					filename.push(grammar::util::to_snake_case(segment.as_str()))
				}

				filename.set_extension("rs");
			}
		}

		log::info!("will write to `{}`", filename.to_string_lossy());
		filename
	}

	fn env(&self, grammar: &mono::Grammar, extern_module_path: &[String]) -> TargetEnv {
		match self {
			Self::Rust => TargetEnv::rust(grammar, extern_module_path)
		}
	}
}

pub enum TargetEnv {
	Rust(grammar::gen::target::rust::Target)
}

impl TargetEnv {
	fn rust(grammar: &mono::Grammar, extern_module_path: &[String]) -> Self {
		Self::Rust(grammar::gen::target::rust::Target::new(grammar, extern_module_path))
	}

	fn generate_ast(&self, grammar: &mono::Grammar, path: &[String]) -> AstModule {
		match self {
			Self::Rust(target) => AstModule::Rust(target.generate_ast(grammar, path))
		}
	}

	fn generate_lexer(&self, grammar: &mono::Grammar, table: &grammar::lexing::Table, path: &[String]) -> LexerModule {
		match self {
			Self::Rust(target) => LexerModule::Rust(target.generate_lexer(grammar, table, path))
		}
	}

	fn generate_lr0_parser(&self, grammar: &mono::Grammar, ast_mod: &AstModule, lexer_mod: &LexerModule, table: &grammar::parsing::table::LR0, path: &[String]) -> LR0Module {
		match self {
			Self::Rust(target) => LR0Module::Rust(target.generate_lr0_parser(grammar, ast_mod.as_rust(), lexer_mod.as_rust(), table, path))
		}
	}
}

pub enum AstModule {
	Rust(grammar::gen::target::rust::ast::Module)
}

impl AstModule {
	fn write<W: std::io::Write>(&self, out: &mut W) -> io::Result<()> {
		match self {
			Self::Rust(m) => m.write(out)
		}
	}

	fn as_rust(&self) -> &grammar::gen::target::rust::ast::Module {
		match self {
			Self::Rust(m) => m
		}
	}
}

pub enum LexerModule {
	Rust(grammar::gen::target::rust::lexer::Module)
}

impl LexerModule {
	fn write<W: std::io::Write>(&self, out: &mut W) -> io::Result<()> {
		match self {
			Self::Rust(m) => m.write(out)
		}
	}

	fn as_rust(&self) -> &grammar::gen::target::rust::lexer::Module {
		match self {
			Self::Rust(m) => m
		}
	}
}

pub enum LR0Module {
	Rust(grammar::gen::target::rust::lr0::Module)
}

impl LR0Module {
	fn write<W: std::io::Write>(&self, out: &mut W) -> io::Result<()> {
		match self {
			Self::Rust(m) => m.write(out)
		}
	}
}

pub enum Output {
	Std(std::io::Stdout),
	File(std::fs::File)
}

impl Output {
	fn std() -> io::Result<Self> {
		Ok(Self::Std(std::io::stdout()))
	}
	
	fn file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
		Ok(Self::File(File::create(path)?))
	}

	fn lock<'a>(&'a mut self) -> LockedOutput<'a> {
		match self {
			Output::Std(stdout) => LockedOutput::Std(stdout.lock()),
			Output::File(ref mut file) => LockedOutput::File(file)
		}
	}
}

pub enum LockedOutput<'a> {
	Std(std::io::StdoutLock<'a>),
	File(&'a mut std::fs::File)
}

impl<'a> io::Write for LockedOutput<'a> {
	fn write(&mut self, data: &[u8]) -> io::Result<usize> {
		match self {
			Self::Std(o) => o.write(data),
			Self::File(o) => o.write(data)
		}
	}

	fn flush(&mut self) -> io::Result<()> {
		match self {
			Self::Std(o) => o.flush(),
			Self::File(o) => o.flush()
		}
	}
}