use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Ident(pub String);

impl Ident {
	pub fn as_str(&self) -> &str {
		&self.0
	}
}

impl fmt::Display for Ident {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.0.fmt(f)
	}
}
