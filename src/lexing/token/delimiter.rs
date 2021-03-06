macro_rules! delimiter {
	($($a:literal, $b:literal : $name:ident),*) => {
		#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
		pub enum Delimiter {
			$(
				$name
			),*
		}

		impl Delimiter {
			pub fn from_opening_char(c: char) -> Option<Self> {
				Some(match c {
					$(
						$a => Self::$name,
					)*
					_ => return None
				})
			}

			pub fn from_closing_char(c: char) -> Option<Self> {
				Some(match c {
					$(
						$b => Self::$name,
					)*
					_ => return None
				})
			}

			pub fn name(&self) -> &'static str {
				match self {
					$(
						Self::$name => stringify!($name)
					),*
				}
			}

			pub fn ident(&self) -> crate::Ident {
				crate::Ident::new(self.name()).unwrap()
			}
		}
	};
}

delimiter! {
	'(', ')': Parenthesis,
	'[', ']': Bracket,
	'{', '}': Braces
}
