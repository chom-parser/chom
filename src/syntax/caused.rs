use source_span::Span;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy)]
pub enum Source {
	None,
	Explicit(Span),
	Implicit(Span),
}

impl Source {
	pub fn span(&self) -> Option<Span> {
		match self {
			Self::Explicit(span) => Some(*span),
			Self::Implicit(span) => Some(*span),
			Self::None => None,
		}
	}
}

pub struct Caused<T>(T, Source);

impl<T> Caused<T> {
	pub fn new(t: T, source: Source) -> Self {
		Self(t, source)
	}

	pub fn explicit(t: T, span: Span) -> Self {
		Self(t, Source::Explicit(span))
	}

	pub fn implicit(t: T, span: Span) -> Self {
		Self(t, Source::Implicit(span))
	}

	pub fn source(&self) -> &Source {
		&self.1
	}

	pub fn span(&self) -> Option<Span> {
		self.1.span()
	}
}

impl<T> AsRef<T> for Caused<T> {
	fn as_ref(&self) -> &T {
		&self.0
	}
}

impl<T> AsMut<T> for Caused<T> {
	fn as_mut(&mut self) -> &mut T {
		&mut self.0
	}
}

impl<T> Deref for Caused<T> {
	type Target = T;

	fn deref(&self) -> &T {
		&self.0
	}
}

impl<T> DerefMut for Caused<T> {
	fn deref_mut(&mut self) -> &mut T {
		&mut self.0
	}
}
