pub mod pseudo;
pub mod target;

pub use target::{Generate, Target};

/// Generation configuration.
pub struct Config {
	/// Generate and AST and parser that locates every node in the source file.
	///
	/// In Rust this is done using the
	/// [`source-span`](https://crates.io/crates/source-span) crate.
	pub locate: bool,
}

pub enum Parser {
	LR0(pseudo::Expr),
	LALR1(pseudo::Expr),
}
