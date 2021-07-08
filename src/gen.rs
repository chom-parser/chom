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
