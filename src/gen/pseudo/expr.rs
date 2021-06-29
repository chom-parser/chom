use crate::Ident;
use super::{
	Pattern,
	Constant,
	Id,
	ty
};

/// Labels.
pub enum Label {
	/// Lexer loop label.
	/// 
	/// The given index is used to uniquely identify the lexer loop.
	/// There may be nested lexing loops.
	Lexer(u32),

	/// Parser loop.
	Parser
}

/// Lexer operation.
pub enum LexerOperation {
	/// Peek a character from the source char stream.
	SourcePeek,

	/// Consume a character from the source and evaluate the given expression.
	SourceConsume(Box<Expr>),

	/// Push the last read character and evaluate the given expression.
	BufferPush(Box<Expr>),

	/// Clear the buffer and evaluate the given expression.
	BufferClear(Box<Expr>),

	/// Parse the buffer content using the given token parser (given the index of the grammar terminal).
	BufferParse(u32),

	/// Creates an iterator from the characters of the buffer.
	BufferIter,

	/// Get the next char from the buffer characters iterator [`Id::BufferChars`],
	/// put it in [`Id::CharOpt`] and evaluate the given expression.
	/// Also set [`Id::BufferChars`] to its next state.
	BufferCharsNext(Box<Expr>),
}

/// Parser operation.
pub enum ParserOperation {
	/// Push a value on the stack and evaluate the given expression.
	StackPush(Box<Expr>, Box<Expr>),

	/// Pop some values from the stack and evaluate the given expression.
	/// 
	/// If the stack is `(value_n, state_n)::...::(value_1, state_1)::rest` then
	/// this corresponds to the following OCaml-like bit of code: 
	/// 
	/// ```ocaml
	/// let stack = [rest] in
	/// let ((pattern_1, saved_state), ..., (pattern_n, _)) = ((value_n, state_n), ..., (value_1, state_1)) in
	/// expr
	/// ```
	/// where `pattern_1, ..., pattern_n` are given as first parameter and are
	/// guaranteed to match the associated values.
	/// The special variable `saved_state` is identified by [`Id::SavedState`].
	StackPop(Vec<Pattern>, Box<Expr>),

	/// Get the next token from the parser and evaluate the given expression.
	/// 
	/// This corresponds to the following Ocaml-like bit of code:
	/// ```
	/// let (lexer, any_token_opt) = lexer.next in ...
	/// ```
	/// The variable [`Id::AnyTokenOpt`] is set.
	/// The variable [`Id::Lexer`] is set.
	LexerNext(Box<Expr>)
}

pub enum Error {
	UnexpectedChar(Box<Expr>),
	UnexpectedToken(Box<Expr>),
	UnexpectedNode(Box<Expr>)
}

/// Expression.
pub enum Expr {
	Constant(Constant),

	/// Get the value of the given variable.
	Get(Id),

	/// Set the value of the given variable.
	Set(Id, Box<Expr>, Box<Expr>),

	/// Lexer operation.
	Lexer(LexerOperation),

	/// Parser operation.
	Parser(ParserOperation),

	/// Create a new instance of the given type using the given arguments.
	New(ty::Ref, BuildArgs),

	/// Construct an enum variant.
	/// 
	/// The first parameter is the enum type index,
	/// the second the variant index.
	Cons(ty::Ref, u32, BuildArgs),

	/// Option `Some`.
	Some(Box<Expr>),

	/// Option `None`.
	None,

	/// Result `Ok`.
	Ok(Box<Expr>),

	/// Result `Error`.
	Err(Error),

	/// Pattern matching.
	Match {
		expr: Box<Expr>,
		cases: Vec<MatchCase>
	},

	/// Tail recursion.
	/// 
	/// This may be compiled into a while loop (e.g. in Rust):
	/// ```
	/// while condition {
	/// 	body
	/// }
	/// ```
	/// 
	/// This may also be compiled into a recursive function call (e.g. in OCaml):
	/// ```ocaml
	/// let rec f args = body in f args
	/// ```
	TailRecursion {
		label: Label,
		args: Vec<Id>,
		body: Box<Expr>
	},

	/// Recurse on the given tail recursion loop.
	/// 
	/// The list of argument must match the list of the `TailRecursion` arguments.
	Recurse(Label, Vec<Id>),

	/// Unreachable expression.
	Unreachable
}

pub struct MatchCase {
	pub pattern: Pattern,
	pub expr: Expr
}

pub enum BuildArgs {
	Tuple(Vec<Expr>),
	Struct(Vec<Binding>)
}

pub struct Binding {
	pub name: Ident,
	pub expr: Expr
}