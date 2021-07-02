/// Variable identifier.
#[derive(Clone, Copy)]
pub enum Id {
	Lexer(Lexer),
	Parser(Parser),
	Format(Format),
}

/// Variables occuring in the lexer.
#[derive(Clone, Copy)]
pub enum Lexer {
	/// The lexer itself.
	Itself,

	/// Buffer member.
	Buffer,

	/// Location member.
	Span,

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

/// Variable occuring in the parser.
#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
pub enum Format {
	/// The value beeing printed.
	Itself,

	/// Argument.
	Arg(u32),
}
