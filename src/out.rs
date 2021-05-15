use std::fmt;
use source_span::{
	Metrics,
	Span
};
use yansi::Paint;

mod ambiguity;

pub use ambiguity::*;

pub const WARNING: source_span::fmt::Style = source_span::fmt::Style::Warning;

pub enum Type {
	Warning,
	Error
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Warning => write!(f, "{}", Paint::yellow("warning").bold()),
			Self::Error => write!(f, "{}", Paint::red("error").bold())
		}
	}
}

pub struct Block {
	ty: Type,
	title: String,
	source: Option<String>,
	highlights: source_span::fmt::Formatter,
	notes: Vec<Note>
}

impl Block {
	pub fn new<S: ToString>(ty: Type, title: S) -> Block {
		Block {
			ty,
			title: title.to_string(),
			source: None,
			highlights: source_span::fmt::Formatter::new(),
			notes: Vec::new()
		}
	}

	pub fn source(&self) -> Option<&str> {
		self.source.as_ref().map(|s| s.as_str())
	}

	pub fn set_source<S: ToString>(&mut self, source: S) {
		self.source = Some(source.to_string())
	}

	pub fn highlights_mut(&mut self) -> &mut source_span::fmt::Formatter {
		&mut self.highlights
	}

	pub fn add_note<S: ToString>(&mut self, ty: NoteType, content: S) {
		self.notes.push(Note {
			ty,
			content: content.to_string()
		})
	}

	pub fn render<E, I: Iterator<Item = Result<char, E>>, M: Metrics>(&self, input: I, span: Span, metrics: &M) -> Result<Formatted, E> {
		let margin_len = self.highlights.margin_len(&span);
		
		Ok(Formatted {
			block: self,
			margin_len: if margin_len >= 2 { margin_len - 2 } else { 0 },
			highlights: self.highlights.render(input, span, metrics)?
		})
	}
}

pub struct Formatted<'a> {
	block: &'a Block,
	margin_len: usize,
	highlights: source_span::fmt::Formatted
}

impl<'a> fmt::Display for Formatted<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut tab = String::with_capacity(self.margin_len);
		for _ in 0..self.margin_len {
			tab.push(' ')
		}

		write!(f, "{}{} {}\n", self.block.ty, Paint::new(':').bold(), Paint::new(&self.block.title).bold())?;

		if let Some(source) = &self.block.source {
			write!(f, "{}--> {}", tab, source)?
		}

		write!(f, "{}{}", tab, Paint::blue('|').bold())?;
		self.highlights.fmt(f)?;
		write!(f, "{}{}\n", tab, Paint::blue('|').bold())?;

		for note in &self.block.notes {
			for (i, line) in note.content.lines().enumerate() {
				if i == 0 {
					write!(f, "{}= {}: {}\n", tab, note.ty, line)?;
				} else {
					write!(f, "{}  {}\n", tab, line)?
				}
			}
		}

		Ok(())
	}
}

pub enum NoteType {
	Note,
	Help
}

impl fmt::Display for NoteType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Note => write!(f, "{}", Paint::new("note").bold()),
			Self::Help => write!(f, "{}", Paint::green("help").bold())
		}
	}
}

pub struct Note {
	ty: NoteType,
	content: String
}