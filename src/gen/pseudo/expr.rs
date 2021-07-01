use super::{ty, Constant, Id, Pattern};
use crate::Ident;

/// Labels.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Label {
	/// Lexer loop label.
	///
	/// The given index is used to uniquely identify the lexer loop.
	/// There may be nested lexing loops.
	Lexer(u32),

	/// Parser loop.
	Parser,
}

/// Lexer operation.
pub enum LexerOperation {
	/// Peek a character from the source char stream.
	PeekChar,

	/// Parse the buffer content using the given token parser (given the index of the grammar terminal).
	BufferParse(u32),

	/// Creates an iterator from the characters of the buffer.
	BufferIter,

	/// Get the next char from the buffer characters iterator [`Id::BufferChars`],
	/// put it in [`Id::CharOpt`] and evaluate the given expression.
	/// Also set [`Id::BufferChars`] to its next state.
	BufferCharsNext(Box<Expr>),

	/// Clear the lexer.
	///
	/// Clear the buffer and optionally set the location to the last position
	/// if the given boolean is true.
	Clear(bool, Box<Expr>),

	/// Set [`Id::Lexer`] by consuming the next character from the source,
	/// putting it in the buffer and optionally extending the token location
	/// if the given boolean is `true`.
	/// Then the given expression is evaluated.
	ConsumeChar(bool, Box<Expr>),
}

impl LexerOperation {
	/// Checks if the operation is "continued",
	/// that is when an expression is evaluated after the operation.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::BufferCharsNext(_) | Self::Clear(_, _) | Self::ConsumeChar(_, _) => true,
			_ => false,
		}
	}
}

/// Parser operation.
pub enum ParserOperation {
	/// New empty stack.
	StackNew,

	/// Push a value on the stack and evaluate the given expression.
	StackPush(Box<Expr>, Box<Expr>),

	/// Pop one value from the stack and evaluate the given expression.
	///
	/// `StackPop(a, b, e)` is equivalent to the following bit of Ocaml-like code:
	/// ```ocaml
	/// let (stack, a, b) = pop stack in e
	/// ```
	StackPop(Option<Id>, Option<Id>, Box<Expr>),

	/// Unwrap the value matching the given pattern and evaluate the next expression.
	///
	/// `PatternUnwrap(p, v, e)` is equivalent to the following bit of Ocaml-like code:
	/// ```ocaml
	/// let p = v in e
	/// ```
	///
	/// The value is guaranteed to match the input pattern.
	PatternUnwrap(Pattern, Box<Expr>, Box<Expr>),

	/// Get the next token from the parser and evaluate the given expression.
	///
	/// This corresponds to the following Ocaml-like bit of code:
	/// ```
	/// let (lexer, any_token_opt) = lexer.next in ...
	/// ```
	/// The variable [`Id::AnyTokenOpt`] is set.
	/// The variable [`Id::Lexer`] is set.
	LexerNext(Box<Expr>),

	/// New default position.
	PositionNew,

	/// Create a new span from a position.
	PositionToSpan(Box<Expr>),

	/// Transpose an optional located value into a located optional value.
	///
	/// The second expression gives the default span to apply when
	/// the value is `none`.
	LocOptTranspose(Box<Expr>, Box<Expr>),

	/// Unwrap a located value into a pair (value, span),
	/// and then evaluate an expression.
	///
	/// `UnwrapOptLoc(a, b, c, d)` is equivalent to the
	/// following Ocaml-like bit of code:
	/// ```
	/// let (a, b) = Loc.into_pair c in d
	/// ```
	LocUnwrap(Option<Id>, Option<Id>, Box<Expr>, Box<Expr>),

	/// Compute the smallest span that includes both input span parameters.
	LocUnion(Box<Expr>, Box<Expr>),
}

impl ParserOperation {
	/// Checks if the operation is "continued",
	/// that is when an expression is evaluated after the operation.
	pub fn is_continued(&self) -> bool {
		true
	}
}

pub enum Error {
	UnexpectedChar(Box<Expr>),
	UnexpectedToken(Box<Expr>),
	UnexpectedNode(Box<Expr>),
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
	Err(Box<Expr>),

	/// Error expression.
	Error(Error),

	/// Wrap the first expression with the location given by the second.
	Locate(Box<Expr>, Box<Expr>),

	/// Put the value on the heap.
	Heap(Box<Expr>),

	/// Pattern matching.
	Match {
		expr: Box<Expr>,
		cases: Vec<MatchCase>,
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
		body: Box<Expr>,
	},

	/// Recurse on the given tail recursion loop.
	///
	/// The list of argument must match the list of the `TailRecursion` arguments.
	Recurse(Label, Vec<Id>),

	/// Unreachable expression.
	Unreachable,
}

impl Expr {
	/// Checks if the expression is "continued",
	/// that is when another expression is evaluated after the operation.
	/// Such expression can be generated with a preceding `return` or `break` statement
	/// in non-functional target languages.
	///
	/// The `Recurse` and `Unreachable` expressions are also considered to be continued.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Set(_, _, _) | Self::Match { .. } | Self::Recurse(_, _) | Self::Unreachable => {
				true
			}
			Self::Lexer(op) => op.is_continued(),
			Self::Parser(op) => op.is_continued(),
			_ => false,
		}
	}
}

pub struct MatchCase {
	pub pattern: Pattern,
	pub expr: Expr,
}

pub enum BuildArgs {
	Tuple(Vec<Expr>),
	Struct(Vec<Binding>),
}

pub struct Binding {
	pub name: Ident,
	pub expr: Expr,
}
