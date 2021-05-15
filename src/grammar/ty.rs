use std::{
	fmt,
	collections::HashSet
};
use source_span::{
	Loc,
	Span
};
use crate::{
	Grammar,
	syntax::Ident
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Desc {
	Nammed(Loc<Ident>),
	Repeat(u32, usize, usize, Option<Separator>),
}

pub struct Type {
	desc: Desc,
	rules: Vec<u32>,
	usages: HashSet<Span>
}

impl Type {
	pub fn new(desc: Desc) -> Self {
		Self {
			desc,
			rules: Vec::new(),
			usages: HashSet::new()
		}
	}

	pub fn nammed(id: Loc<Ident>) -> Self {
		Self::new(Desc::Nammed(id))
	}

	pub fn desc(&self) -> &Desc {
		&self.desc
	}

	pub fn span(&self) -> Span {
		match &self.desc {
			Desc::Nammed(id) => id.span(),
			_ => self.usages.iter().next().cloned().expect("type is never used")
		}
	}

	pub fn id(&self) -> Option<&Loc<Ident>> {
		match &self.desc {
			Desc::Nammed(id) => Some(id),
			_ => None
		}
	}

	pub fn rules(&self) -> &[u32] {
		&self.rules
	}

	pub fn add_usage(&mut self, span: Span) {
		self.usages.insert(span);
	}

	pub fn add_rule(&mut self, rule: u32) {
		self.rules.push(rule)
	}

	pub fn format<'g>(&self, grammar: &'g Grammar) -> FormattedType<'g, '_> {
		FormattedType(grammar, self)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		let rule = grammar.rule(*self.rules.first().unwrap()).unwrap();
		rule.instance(grammar)
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Separator {
	/// If true, then every item of the list must be followed by a separator.
	pub strong: bool,

	/// Identifier of the terminal used as separator.
	pub terminal: u32
}

impl fmt::Display for Separator {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.strong {
			write!(f, "!")?;
		}

		self.terminal.fmt(f)
	}
}

pub struct FormattedType<'g, 't>(&'g Grammar, &'t Type);

impl<'g, 't> fmt::Display for FormattedType<'g, 't> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self.1.desc {
			Desc::Nammed(id) => id.as_ref().fmt(f),
			Desc::Repeat(inner_ty, min, max, sep) => {
				write!(f, "<")?;

				self.0.ty(*inner_ty).unwrap().format(self.0).fmt(f)?;
				
				if let Some(sep) = sep {
					sep.fmt(f)?
				}

				match (*min, *max) {
					(0, 1) => write!(f, "?")?,
					(0, usize::MAX) => write!(f, "*")?,
					(1, 1) => (),
					(1, usize::MAX) => write!(f, "+")?,
					(min, usize::MAX) => write!(f, "{{{},*}}", min)?,
					(min, max) => write!(f, "{{{},{}}}", min, max)?
				}

				write!(f, ">")
			}
		}
	}
}