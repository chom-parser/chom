use std::collections::HashSet;
use source_span::{
	Loc,
	Span
};
use crate::{
	syntax,
	lexing::{
		RegExp,
		regexp
	}
};

mod external;
mod ty;
pub mod terminal;
pub mod rule;
pub mod non_terminal;
mod compile;

pub use external::ExternalType;
pub use ty::Type;
pub use terminal::Terminal;
pub use rule::Rule;
pub use non_terminal::NonTerminal;
pub use compile::Error;

pub struct Grammar {
	/// Extern types.
	externs: Vec<(ExternalType, Option<Span>)>,

	/// Regular expressions.
	regexps: Vec<(regexp::Definition, Loc<syntax::RegExpDefinition>)>,
	
	/// Terminals.
	terminals: Vec<(Terminal, HashSet<Loc<syntax::Terminal>>)>,

	/// Types.
	types: Vec<Loc<Type>>,

	/// Non terminal.
	non_terminals: Vec<(NonTerminal, HashSet<Span>)>,

	/// Grammar rules.
	rules: Vec<Loc<Rule>>
}

impl Grammar {
	pub(crate) fn from_raw_parts(
		externs: Vec<(ExternalType, Option<Span>)>,
		regexps: Vec<(regexp::Definition, Loc<syntax::RegExpDefinition>)>,
		terminals: Vec<(Terminal, HashSet<Loc<syntax::Terminal>>)>,
		types: Vec<Loc<Type>>,
		non_terminals: Vec<(NonTerminal, HashSet<Span>)>,
		rules: Vec<Loc<Rule>>
	) -> Self {
		let g = Self {
			externs,
			regexps,
			terminals,
			types,
			non_terminals,
			rules
		};

		for (terminal, _) in &g.terminals {
			terminal.init_token(&g)
		}

		g
	}

	pub fn extern_types(&self) -> &[(ExternalType, Option<Span>)] {
		&self.externs
	}

	pub fn extern_type(&self, index: u32) -> Option<&ExternalType> {
		self.externs.get(index as usize).map(|p| &p.0)
	}

	pub fn regexps(&self) -> &[(regexp::Definition, Loc<syntax::RegExpDefinition>)] {
		&self.regexps
	}

	pub fn regexp(&self, index: u32) -> Option<&regexp::Definition> {
		self.regexps.get(index as usize).map(|p| &p.0)
	}

	pub fn types(&self) -> &[Loc<Type>] {
		&self.types
	}

	pub fn ty(&self, index: u32) -> Option<&Loc<Type>> {
		self.types.get(index as usize)
	}

	pub fn terminals(&self) -> &[(Terminal, HashSet<Loc<syntax::Terminal>>)] {
		&self.terminals
	}

	pub fn terminal(&self, index: u32) -> Option<&Terminal> {
		self.terminals.get(index as usize).map(|t| &t.0)
	}

	pub fn non_terminals(&self) -> &[(NonTerminal, HashSet<Span>)] {
		&self.non_terminals
	}

	pub fn non_terminal(&self, index: u32) -> Option<&NonTerminal> {
		self.non_terminals.get(index as usize).map(|(nt, _)| nt)
	}

	pub fn rules(&self) -> &[Loc<Rule>] {
		&self.rules
	}

	pub fn rule(&self, index: u32) -> Option<&Loc<Rule>> {
		self.rules.get(index as usize)
	}
}