macro_rules! operator {
	($($p:literal : $name:ident),*) => {
		#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
		pub enum Operator {
			$(
				$name
			),*
		}

		impl Operator {
			pub fn from_char(c: char) -> Option<Self> {
				Some(match c {
					$(
						$p => Self::$name,
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

operator! {
	'+': Plus,
	'-': Minus,
	'/': Div,
	'*': Mul,
	'%': Percent
}
