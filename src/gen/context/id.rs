use crate::Ident;

/// Variable identifier.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
	Lexer(Lexer),
	Parser(Parser),
	Format(Format),
}

impl Id {
	pub fn to_ident(&self) -> Ident {
		match self {
			Self::Lexer(id) => id.to_ident(),
			Self::Parser(id) => id.to_ident(),
			Self::Format(id) => id.to_ident()
		}
	}
}

/// Variables occuring in the lexer.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lexer {
	// /// Buffer member.
	// Buffer,

	// /// Location member.
	// Span,

	/// Current state.
	State,

	/// An iterator over the characters of the buffer.
	BufferChars,

	/// An optional character.
	CharOpt,

	/// A sub-token emitted from a sub-automaton.
	SubToken,

	/// Unexpected character.
	Unexpected,
}

impl Lexer {
	pub fn to_ident(&self) -> Ident {
		match self {
			Self::State => Ident::new("state").unwrap(),
			Self::BufferChars => Ident::new("chars").unwrap(),
			Self::CharOpt => Ident::new("char_opt").unwrap(),
			Self::SubToken => Ident::new("sub_token").unwrap(),
			Self::Unexpected => Ident::new("unexpected").unwrap()
		}
	}
}

/// Variable occuring in the parser.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Parser {
	/// Lexer.
	Lexer,

	/// Item stack.
	Stack,

	/// Current state.
	State,

	/// State popped on the stack.
	SavedState,

	/// Any optional AST node.
	AnyNodeOpt,

	/// Any optional AST node, without span information.
	AnyNodeOptSpanless,

	/// Any item popped from the stack.
	AnyItem(u32),

	/// Item without span information.
	AnyItemSpanless(u32),

	/// Item span information.
	AnyItemSpan(u32),

	/// Unwrapped item.
	Item(u32),

	/// Any AST node.
	AnyNode,

	/// AST node.
	Node,

	/// Any optional token.
	AnyTokenOpt,

	/// Any optional token, without span information.
	AnyTokenOptSpanless,

	/// Token.
	Token,

	/// Current position.
	Position,

	/// Last item location.
	Span,

	/// Parsed AST.
	Result,

	/// Unexpected token.
	Unexpected,
}

impl Parser {
	pub fn to_ident(&self) -> Ident {
		match self {
			Self::Lexer => Ident::new("lexer").unwrap(),
			Self::Stack => Ident::new("stack").unwrap(),
			Self::State => Ident::new("state").unwrap(),
			Self::SavedState => Ident::new("saved_state").unwrap(),
			Self::AnyNodeOpt => Ident::new("any_node_opt").unwrap(),
			Self::AnyNodeOptSpanless => Ident::new("any_node_opt_spanless").unwrap(),
			Self::AnyItem(i) => Ident::new(format!("any_item{}", i)).unwrap(),
			Self::AnyItemSpanless(i) => Ident::new(format!("any_item{}_spanless", i)).unwrap(),
			Self::AnyItemSpan(i) => Ident::new(format!("item{}_span", i)).unwrap(),
			Self::Item(i) => Ident::new(format!("item{}", i)).unwrap(),
			Self::AnyNode => Ident::new("any_node").unwrap(),
			Self::Node => Ident::new("node").unwrap(),
			Self::AnyTokenOpt => Ident::new("any_token_opt").unwrap(),
			Self::AnyTokenOptSpanless => Ident::new("any_token_opt_spanless").unwrap(),
			Self::Token => Ident::new("token").unwrap(),
			Self::Position => Ident::new("position").unwrap(),
			Self::Span => Ident::new("span").unwrap(),
			Self::Result => Ident::new("result").unwrap(),
			Self::Unexpected => Ident::new("unexpected").unwrap()
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Format {
	/// Argument.
	Arg(u32),
}

impl Format {
	pub fn to_ident(&self) -> Ident {
		match self {
			Self::Arg(i) => Ident::new(format!("a{}", i)).unwrap(),
		}
	}
}