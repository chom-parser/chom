use source_span::Loc;
use crate::{
	poly,
	mono,
	lexing,
	parsing
};

mod context;
pub mod lexer;
pub mod parser;

pub use context::*;

pub type Pattern<'p> = chom_ir::Pattern<Namespace<'p>>;
pub type Expr<'p> = chom_ir::Expr<Namespace<'p>>;
pub type Module<'p> = chom_ir::Module<Namespace<'p>>;
pub type Type<'p> = chom_ir::Type<Namespace<'p>>;
pub type TypeExpr<'p> = chom_ir::ty::Expr<Namespace<'p>>;
pub type Path<'a, 'p> = chom_ir::Path<'a, Namespace<'p>>;

// pub mod pseudo;
// pub mod target;

// pub use target::{Generate, Target};

/// Generation configuration.
pub struct Config {
	/// Generate and AST and parser that locates every node in the source file.
	///
	/// In Rust this is done using the
	/// [`source-span`](https://crates.io/crates/source-span) crate.
	pub locate: bool,
}

// pub enum Parser {
// 	LR0(pseudo::Expr),
// 	LALR1(pseudo::Expr),
// }

pub enum Error<'a, 'p> {
	Lexing(&'p poly::Grammar, Loc<lexing::Error>),
	LR0Ambiguity(
		&'a mono::Grammar<'p>,
		Loc<parsing::table::lr0::Ambiguity>,
	)
}

pub fn context_from_grammar<'a, 'p, S>(
	config: Config,
	grammar: &'a mono::Grammar<'p>,
	extern_mod_path: &[S],
	ast_mod_path: &[S],
	lexer_mod_path: &[S],
	parser_mod_path: &[S],
) -> Result<Context<'a, 'p>, Error<'a, 'p>> where S: AsRef<str> {
	match lexing::Table::new(grammar.poly()) {
		Ok(lexing_table) => {
			let parsing_table = parsing::table::NonDeterministic::new(grammar);
			match parsing::table::LR0::from_non_deterministic(grammar, &parsing_table) {
				Ok(lr0_table) => {
					Ok(Context::new(
						config,
						grammar,
						extern_mod_path,
						ast_mod_path,
						lexer_mod_path,
						&lexing_table,
						parser_mod_path,
						&parsing::Table::LR0(lr0_table),
					))
				},
				Err(e) => Err(Error::LR0Ambiguity(grammar, e)),
			}
		}
		Err(e) => Err(Error::Lexing(grammar.poly(), e)),
	}
}