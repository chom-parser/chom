use crate::syntax::Ident;
use source_span::{Loc, Span};
use std::fmt;

#[derive(Debug)]
pub enum Error {
	UndefinedExternalType(Ident),
	AlreadyDefinedExternalType(Ident, Span),
	UndefinedRegExp(Ident),
	AlreadyDefinedRegExp(Ident, Span),
	UndefinedType(Ident),
	AlreadyDefinedType(Ident, Span),
	RegExpTypeMissmatch(Loc<Ident>),
}

impl Error {
	pub fn format_notes(
		&self,
		fmt: &mut source_span::fmt::Formatter,
		style: source_span::fmt::Style,
	) {
		match self {
			Error::AlreadyDefinedExternalType(_, span) => {
				fmt.add(*span, Some("first declaration".to_string()), style)
			}
			Error::AlreadyDefinedRegExp(_, span) => {
				fmt.add(*span, Some("first definition".to_string()), style)
			}
			Error::AlreadyDefinedType(_, span) => {
				fmt.add(*span, Some("first definition".to_string()), style)
			}
			Error::RegExpTypeMissmatch(ty) => fmt.add(
				ty.span(),
				Some("the regexp type is defined here".to_string()),
				style,
			),
			_ => (),
		}
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Error::UndefinedExternalType(id) => write!(f, "undefined extern type `{}`", id),
			Error::AlreadyDefinedExternalType(id, _) => {
				write!(f, "already declared extern type `{}`", id)
			}
			Error::UndefinedRegExp(id) => write!(f, "undefined regular expression `{}`", id),
			Error::AlreadyDefinedRegExp(id, _) => {
				write!(f, "already defined regular expression `{}`", id)
			}
			Error::UndefinedType(id) => write!(f, "undefined type `{}`", id),
			Error::AlreadyDefinedType(id, _) => write!(f, "already defined type `{}`", id),
			Error::RegExpTypeMissmatch(ty) => {
				write!(f, "expected regexp type `unit`, found `{}`", ty)
			}
		}
	}
}
