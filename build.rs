use proc_macro2::TokenStream;
use quote::quote;
use std::{
	fs::{self, File},
	io::{self, BufReader, Read, Write},
	path::{Path, PathBuf},
};

#[derive(Debug)]
pub enum Error {
	IO(io::Error),
	Toml(toml::de::Error),
	Data(DataError),
}

impl From<io::Error> for Error {
	fn from(e: io::Error) -> Self {
		Self::IO(e)
	}
}

impl From<toml::de::Error> for Error {
	fn from(e: toml::de::Error) -> Self {
		Self::Toml(e)
	}
}

impl From<DataError> for Error {
	fn from(e: DataError) -> Self {
		Self::Data(e)
	}
}

#[derive(Debug)]
pub enum DataError {
	DocumentNotATable,
	GrammarNotString,
	ChallengeNotArray,
	ChallengeNotTable,
	ChallengeNoName,
	NameNotString,
	ChallengeNoInput,
	InputNotString,
	ChallengeNoType,
	TypeNotString,
	ChallengeNoResult,
	SuccessNotString,
}

fn main() -> Result<(), Error> {
	foreach_toml_file("tests", |path| {
		let stem = path.file_stem().unwrap().to_string_lossy();
		let mut output = path.parent().unwrap().to_path_buf();
		output.push(&*stem);
		output.set_extension("rs");

		if should_generate(&output, &path)? {
			let contents = read_file(&path)?;
			let toml: toml::Value = contents.parse()?;
			let test = Test::from_toml(&stem, toml)?;
			let code = test.generate("tests");
			write_file(output, format!("{}", code))?
		}

		Ok(())
	})
}

fn should_generate(output: &Path, source: &Path) -> io::Result<bool> {
	if output.exists() {
		let source_metadata = fs::metadata(source)?;
		let output_metadata = fs::metadata(output)?;
		let source_modified = source_metadata.modified()?;
		let output_modified = output_metadata.modified()?;
		Ok(source_modified > output_modified)
	} else {
		Ok(true)
	}
}

fn foreach_toml_file<P: AsRef<Path>, F>(dir: P, f: F) -> Result<(), Error>
where
	F: Fn(PathBuf) -> Result<(), Error>,
{
	for entry in fs::read_dir(dir)? {
		let entry = entry?;
		let path = entry.path();
		if path.is_file() {
			if let Some(ext) = path.extension() {
				if ext == "toml" {
					f(path)?
				}
			}
		}
	}

	Ok(())
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
	let file = fs::File::open(path)?;
	let mut buf_reader = BufReader::new(file);
	let mut contents = String::new();
	buf_reader.read_to_string(&mut contents)?;
	Ok(contents)
}

fn write_file<P: AsRef<Path>, C: AsRef<[u8]>>(path: P, contents: C) -> io::Result<()> {
	// eprintln!("write to {}", path.as_ref().display());
	let mut file = File::create(path)?;
	file.write_all(contents.as_ref())
}

/// Parser type.
pub enum Parser {
	LR0,
}

/// Test description.
pub struct Test {
	/// Grammar file basename.
	grammar: String,

	/// Parser type.
	parser: Parser,

	/// Challenges.
	challenges: Vec<Challenge>,
}

impl Test {
	fn from_toml(stem: &str, toml: toml::Value) -> Result<Self, DataError> {
		let table = toml.as_table().ok_or(DataError::DocumentNotATable)?;
		let grammar = table
			.get("grammar")
			.map(|n| n.as_str().ok_or(DataError::GrammarNotString))
			.transpose()?
			.unwrap_or(stem);

		let mut challenges = Vec::new();
		if let Some(value) = table.get("challenge") {
			let values = value.as_array().ok_or(DataError::ChallengeNotArray)?;
			for challenge in values {
				challenges.push(Challenge::from_toml(challenge)?)
			}
		}

		Ok(Self {
			grammar: grammar.to_string(),
			parser: Parser::LR0,
			challenges,
		})
	}

	fn generate<P: AsRef<Path>>(&self, root: P) -> TokenStream {
		let mut grammar_path = root.as_ref().to_path_buf();
		grammar_path.push(&self.grammar);
		grammar_path.set_extension("chom");
		let grammar_path_string = grammar_path.to_string_lossy();

		let challenges = self.challenges.iter().map(|c| c.generate());

		quote! {
			use std::{
				io::{
					self,
					BufReader,
					Read
				},
				fs::File
			};
			use source_span::Position;
			use utf8_decode::UnsafeDecoder;
			use chom::gen;
			use chom_ir::eval;

			lazy_static::lazy_static! {
				static ref GRAMMAR: chom::mono::OwnedGrammar = {
					let file = File::open(#grammar_path_string).unwrap();
					let input = BufReader::new(file);
					let metrics = source_span::DefaultMetrics::with_tab_stop(4);
					let utf8_input = UnsafeDecoder::new(input.bytes());
					let buffer = source_span::SourceBuffer::new(utf8_input, Position::default(), metrics);
					match chom::parse_and_compile(&buffer, metrics) {
						Ok(grammar) => grammar,
						Err(e) => panic!("error")
					}
				};

				static ref CONTEXT: chom::gen::Context<'static, 'static> = {
					let config = chom::gen::Config { locate: true };
					match gen::context_from_grammar(
						config,
						GRAMMAR.mono(),
						&["glue"],
						&["ast"],
						&["lexer"],
						&["parser"],
					) {
						Ok(context) => context,
						Err(e) => {
							panic!("error")
						}
					}
				};
			}

			#(#challenges)*
		}
	}
}

/// Challenge result.
pub enum ChallengeResult {
	Success(String),
}

/// Test challenge.
pub struct Challenge {
	/// Challenge name.
	name: String,

	/// Input phrase.
	input: String,

	/// Output AST type.
	ty: String,

	/// Expected outcome.
	result: ChallengeResult,
}

impl Challenge {
	fn from_toml(toml: &toml::Value) -> Result<Self, DataError> {
		let table = toml.as_table().ok_or(DataError::ChallengeNotTable)?;
		let name = table
			.get("name")
			.ok_or(DataError::ChallengeNoName)?
			.as_str()
			.ok_or(DataError::NameNotString)?;
		let input = table
			.get("input")
			.ok_or(DataError::ChallengeNoInput)?
			.as_str()
			.ok_or(DataError::InputNotString)?;
		let ty = table
			.get("type")
			.ok_or(DataError::ChallengeNoType)?
			.as_str()
			.ok_or(DataError::TypeNotString)?;
		let success = table
			.get("success")
			.ok_or(DataError::ChallengeNoResult)?
			.as_str()
			.ok_or(DataError::SuccessNotString)?;

		Ok(Self {
			name: name.to_string(),
			input: input.to_string(),
			ty: ty.to_string(),
			result: ChallengeResult::Success(success.to_string()),
		})
	}

	fn generate(&self) -> TokenStream {
		let id = quote::format_ident!("{}", self.name);
		let input = &self.input;
		let ty = &self.ty;

		match &self.result {
			ChallengeResult::Success(success) => {
				quote! {
					#[test]
					fn #id() -> io::Result<()> {
						let context = &CONTEXT;

						let phrase = #input;
						let target_ty = #ty;
						let expected = #success;

						let mut source = phrase.chars().map(|c| Ok(c));
						let mut eval = eval::Evaluator::new(context.ir_context());

						let target_ty_expr = eval.context().parse_ty_expr(&target_ty).expect("invalid target type expr").expect("target type not found");
						let lexer = eval.instanciate_lexer(&mut source).expect("no lexer");
						let parser = eval.context().parsers_for(&target_ty_expr).next().expect("no parser defined for target type");

						match eval.parse(parser, lexer) {
							Ok(result) => {
								match result {
									Ok(value) => {
										match eval.debug_format(value) {
											Ok(result) => {
												assert_eq!(result, expected);
												Ok(())
											}
											Err(e) => panic!("failed while formatting with: {}", e)
										}
									},
									Err(e) => panic!("failed with: {}", e.as_ref().name())
								}
							},
							Err(e) => {
								panic!("fatal error during parsing: {}", e)
							}
						}
					}
				}
			}
		}
	}
}
