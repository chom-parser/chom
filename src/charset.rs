use std::{
	fmt,
	ops::{
		Deref,
		DerefMut
	}
};
use btree_range_map::{
	RangeSet,
	AnyRange
};

pub struct DisplayChar(pub char);

impl fmt::Display for DisplayChar {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let c = self.0;
		match c {
			'\\' => write!(f, "\\\\"),
			'\r' => write!(f, "\\r"),
			'\n' => write!(f, "\\n"),
			' ' => write!(f, "\\s"),
			'\t' => write!(f, "\\t"),
			_ if c.is_control() => {
				let d = c as u32;
				if d <= 0xff {
					write!(f, "\\x{:02x}", d)
				} else if d <= 0xffff {
					write!(f, "\\u{:04x}", d)
				} else {
					write!(f, "\\U{:08x}", d)
				}
			},
			_ => c.fmt(f)
		}
	}
}

pub struct DisplayString<'a>(pub &'a str);

impl<'a> fmt::Display for DisplayString<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for c in self.0.chars() {
			DisplayChar(c).fmt(f)?;
		}

		Ok(())
	}
}

pub struct DisplayCharRange<'a>(pub &'a AnyRange<char>);

impl<'a> fmt::Display for DisplayCharRange<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(first) = self.0.first() {
			let last = self.0.last().unwrap();
	
			if first == last {
				fmt::Display::fmt(&DisplayChar(first), f)
			} else if first as u32 + 1 == last as u32 { // Note: no risk of overflowing here with `char`.
				write!(f, "{}{}", DisplayChar(first), DisplayChar(last))
			} else {
				write!(f, "{}-{}", DisplayChar(first), DisplayChar(last))
			}
		} else {
			Ok(())
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CharSet(RangeSet<char>);

impl CharSet {
	pub fn new() -> CharSet {
		CharSet(RangeSet::new())
	}

	/// Creates the charset of whitespaces.
	/// 
	/// TODO: for now, only ASCII whitespaces are included.
	pub fn whitespace() -> CharSet {
		let mut set = RangeSet::new();
		set.insert(' ');
		set.insert('\x09'..='\x0d');
		CharSet(set)
	}

	pub fn len(&self) -> btree_range_map::util::Saturating<u32> {
		self.0.len()
	}

	pub fn from_char(c: char, case_sensitive: bool) -> CharSet {
		let mut set = CharSet::new();
		
		if case_sensitive {
			set.insert(c)
		} else {
			for c in c.to_uppercase() {
				set.insert(c)
			}
			for c in c.to_lowercase() {
				set.insert(c)
			}
		}

		set
	}

	pub fn ranges(&self) -> impl Iterator<Item=&btree_range_map::AnyRange<char>> {
		self.0.iter()
	}

	pub fn negated(&self) -> CharSet {
		let mut set = CharSet::new();
		set.insert('\u{0}'..='\u{d7ff}');
		set.insert('\u{e000}'..='\u{10ffff}');

		for range in self.iter() {
			set.remove(range.clone())
		}

		set
	}

	pub fn first(&self) -> Option<char> {
		self.0.iter().next().map(|range| range.first()).flatten()
	}
}

impl fmt::Display for CharSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for range in &self.0 {
			DisplayCharRange(range).fmt(f)?;
		}

		Ok(())
	}
}

impl Deref for CharSet {
	type Target = RangeSet<char>;

	fn deref(&self) -> &RangeSet<char> {
		&self.0
	}
}

impl DerefMut for CharSet {
	fn deref_mut(&mut self) -> &mut RangeSet<char> {
		&mut self.0
	}
}