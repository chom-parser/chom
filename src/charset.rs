use std::fmt;
use std::convert::TryInto;

#[derive(Clone, Copy)]
pub struct Range {
	first: u32,
	last: u32
}

impl Range {
	pub fn new(first: char, last: char) -> Range {
		Range {
			first: first as u32,
			last: last as u32
		}
	}

	pub fn first(&self) -> char {
		self.first.try_into().unwrap()
	}

	pub fn last(&self) -> char {
		self.last.try_into().unwrap()
	}

	pub fn is_empty(&self) -> bool {
		self.first <= self.last
	}

	pub fn non_empty(self) -> Option<Range> {
		if self.is_empty() {
			None
		} else {
			Some(self)
		}
	}

	pub fn inter_split(&self, other: &Range) -> (Option<Range>, Option<Range>, Option<Range>, Option<Range>, Option<Range>) {
		let before = Range {
			first: other.first,
			last: self.first
		};

		let left_before = Range {
			first: self.first,
			last: other.first
		};

		let inter = Range {
			first: std::cmp::max(self.first, other.first),
			last: std::cmp::min(self.last, other.last)
		};

		let left_after = Range {
			first: other.last,
			last: self.last
		};

		let after = Range {
			first: self.last,
			last: other.last
		};

		(before.non_empty(), left_before.non_empty(), inter.non_empty(), left_after.non_empty(), after.non_empty())
	}
}

impl From<char> for Range {
	fn from(c: char) -> Range {
		Range {
			first: c as u32,
			last: c as u32
		}
	}
}

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

impl fmt::Display for Range {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.first == self.last {
			DisplayChar(self.first()).fmt(f)
		} else if self.first + 1 == self.last {
			write!(f, "{}{}", DisplayChar(self.first()), DisplayChar(self.last()))
		} else {
			write!(f, "{}-{}", DisplayChar(self.first()), DisplayChar(self.last()))
		}
	}
}

#[derive(Clone)]
struct LinkedRange {
	range: Range,
	tail: Option<Box<LinkedRange>>
}

impl LinkedRange {
	pub fn tail(&self) -> Option<&LinkedRange> {
		match &self.tail {
			Some(tail) => Some(&*tail),
			None => None
		}
	}

	pub fn remove_tail(&mut self) -> Option<Box<LinkedRange>> {
		let mut tail = None;
		std::mem::swap(&mut self.tail, &mut tail);
		tail
	}

	pub fn remove(&mut self) {
		let mut tail = self.remove_tail().unwrap();
		std::mem::swap(self, &mut tail);
	}

	pub fn add(&mut self, range: Range) {
		if range.last + 1 < self.range.first {
			let mut new_tail: LinkedRange = self.range.into();
			std::mem::swap(&mut new_tail.tail, &mut self.tail);
			self.range = range;
			self.tail = Some(Box::new(new_tail));
		} else {
			if range.first <= self.range.last + 1 {
				match &mut self.tail {
					Some(tail) if range.last + 1 >= tail.range.first => {
						tail.add(range);
						self.remove();
					},
					_ => {
						self.range.first = std::cmp::min(self.range.first, range.first);
						self.range.last = std::cmp::max(self.range.last, range.last);
					}
				}
			} else {
				match &mut self.tail {
					Some(tail) => tail.add(range),
					None => {
						self.tail = Some(Box::new(range.into()))
					}
				}
			}
		}
	}
}

impl From<Range> for LinkedRange {
	fn from(r: Range) -> LinkedRange {
		LinkedRange {
			range: r,
			tail: None
		}
	}
}

#[derive(Clone)]
pub struct CharSet {
	head: Option<LinkedRange>
}

impl CharSet {
	pub fn new() -> CharSet {
		CharSet {
			head: None
		}
	}

	pub fn add(&mut self, range: Range) {
		match &mut self.head {
			Some(head) => head.add(range),
			None => {
				self.head = Some(range.into())
			}
		}
	}

	pub fn add_char(&mut self, c: char) {
		self.add(c.into())
	}

	pub fn iter(&self) -> Iter {
		Iter {
			next: self.head.as_ref()
		}
	}
}

pub struct Iter<'a> {
	next: Option<&'a LinkedRange>
}

impl<'a> Iterator for Iter<'a> {
	type Item = Range;

	fn next(&mut self) -> Option<Range> {
		match self.next {
			Some(r) => {
				let range = r.range;
				self.next = r.tail();
				Some(range)
			},
			None => None
		}
	}
}

impl fmt::Display for CharSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for range in self.iter() {
			range.fmt(f)?;
		}

		Ok(())
	}
}
