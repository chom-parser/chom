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
		}

		impl quote::ToTokens for Operator {
			fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
				match self {
					$(
						Self::$name => tokens.extend(quote::quote!{ $name })
					),*
				}
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
