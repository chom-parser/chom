/// Variable identifier.
#[derive(Clone, Copy)]
pub enum Id {
	/// Source character stream.
	Source,

	/// Optional character taken from the source.
	CharOpt,

	/// State identifier.
	State,

	/// Lexer buffer.
	Buffer,

	/// lexer buffer characters iterator.
	BufferChars,

	/// Sub token.
	SubToken,

	/// Lexer.
	Lexer,

	/// Any optional token emitted by the parser.
	AnyTokenOpt,

	/// Token emitted by the parser.
	Token,

	/// Parsing stack.
	Stack,

	/// Any optional AST node.
	AnyNodeOpt,

	/// Any AST node.
	AnyNode,

	/// AST node.
	Node,

	/// Popped stack item.
	Item(u32),

	/// State popped from the stack.
	/// 
	/// It is set by the [`StackPop`](super::expr::ParserOperation::StackPop) operation.
	SavedState,

	/// Unexpected value.
	Unexpected,

	/// Parser result.
	Result
}