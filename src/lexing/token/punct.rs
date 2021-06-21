macro_rules! punct {
	($($p:literal : $name:ident),*) => {
		#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
		pub enum Punct {
			$(
				$name
			),*
		}

		impl Punct {
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

		impl quote::ToTokens for Punct {
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

punct! {
	',': Comma,
	':': Colon,
	';': Semicolon,
	'!': Exclamation,
	'?': Question,
	'.': Period
}
