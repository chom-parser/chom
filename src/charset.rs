use std::{
	fmt,
	ops::{
		Deref,
		DerefMut
	}
};
use range_map::{
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

fn fmt_char_range(range: &AnyRange<char>, f: &mut fmt::Formatter) -> fmt::Result {
	if let Some(first) = range.first() {
		let last = range.last().unwrap();

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

#[derive(Clone)]
pub struct CharSet(RangeSet<char>);

impl CharSet {
	pub fn new() -> CharSet {
		CharSet(RangeSet::new())
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

	pub fn negated(&self) -> CharSet {
		let mut set = CharSet::new();
		set.insert('\u{0}'..='\u{d7ff}');
		set.insert('\u{e000}'..='\u{10ffff}');

		for range in self.iter() {
			set.remove(range.clone())
		}

		set
	}
}

impl fmt::Display for CharSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for range in &self.0 {
			fmt_char_range(range, f)?;
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