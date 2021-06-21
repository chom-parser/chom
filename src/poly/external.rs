use crate::syntax::Ident;
use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ExternalType {
	Unit,
	Custom(Ident),
}

impl Default for ExternalType {
	fn default() -> ExternalType {
		Self::Unit
	}
}

impl ExternalType {
	pub fn from_ident(ident: &Ident) -> Self {
		Self::Custom(ident.clone())
	}

	pub fn from_opt_ident(ident: Option<&Ident>) -> Self {
		ident.map(Self::from_ident).unwrap_or_default()
	}

	pub fn name(&self) -> &str {
		match self {
			Self::Unit => "unit",
			Self::Custom(id) => id.as_str(),
		}
	}
}

impl fmt::Display for ExternalType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.name().fmt(f)
	}
}
