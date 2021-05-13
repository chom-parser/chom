use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum NonTerminal {
	Type(u32),
	Repeat(u32, usize, usize, Option<Separator>),
}

impl fmt::Display for NonTerminal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Type(ident) => write!(f, "<{}>", ident),
			Self::Repeat(ident, min, max, sep) => {
				match sep {
					Some(sep) => {
						match (*min, *max) {
							(0, usize::MAX) => write!(f, "<{}*{}>", ident, sep),
							(1, usize::MAX) => write!(f, "<{}+{}>", ident, sep),
							_ => unimplemented!()
						}
					},
					None => {
						match (*min, *max) {
							(0, usize::MAX) => write!(f, "<{}*>", ident),
							(1, usize::MAX) => write!(f, "<{}+>", ident),
							(0, 1) => write!(f, "<{}?>", ident),
							_ => unimplemented!()
						}
					}
				}
			}
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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