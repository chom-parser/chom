use std::iter::Peekable;
pub use source_span::{
	Span,
	Position,
	Loc
};

mod error;
mod ast;
pub mod lexer;

pub use error::{
	Error,
	Result
};
pub use ast::*;
pub use lexer::Lexer;
use lexer::{Delimiter};

pub trait Parsable: Sized {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>>;
}

fn peek<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Option<Loc<lexer::Token>>> {
	match lexer.peek() {
		Some(Ok(token)) => Ok(Some(token.clone())),
		Some(Err(e)) => {
			let mut dummy_span = Span::default();
			consume(lexer, &mut dummy_span)
		},
		None => Ok(None)
	}
}

fn peek_token<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>, span: &Span) -> Result<Loc<lexer::Token>> {
	if let Some(token) = peek(lexer)? {
		Ok(token)
	} else {
		Err(Loc::new(Error::UnexpectedEos, span.end().into()))
	}
}

fn peek_pipe<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>, span: &Span) -> Result<bool> {
	let token = peek_token(lexer, span)?;
	if let lexer::Token::Punct('|') = token.as_ref() {
		Ok(true)
	} else {
		Ok(false)
	}
}

fn consume<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Option<Loc<lexer::Token>>> {
	match lexer.next() {
		Some(Ok(token)) => {
			if span.is_empty() {
				*span = token.span();
			} else {
				span.append(token.span());
			}
			Ok(Some(token.clone()))
		},
		Some(Err(e)) => Err(e.inner_into()),
		None => Ok(None)
	}
}

fn expect<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Loc<lexer::Token>> {
	if let Some(token) = consume(lexer, span)? {
		Ok(token)
	} else {
		Err(Loc::new(Error::UnexpectedEos, span.end().into()))
	}
}

fn expect_punct<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Loc<char>> {
	let token = expect(lexer, span)?;

	if let lexer::Token::Punct(p) = token.as_ref() {
		Ok(Loc::new(*p, token.span()))
	} else {
		Err(Loc::new(Error::UnexpectedToken(token.as_ref().clone()), token.span()))
	}
}

impl Parsable for Grammar {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let mut externs = Vec::new();
		let mut regexps = Vec::new();
		let mut types = Vec::new();

		while let Some(token) = consume(lexer, &mut span)? {
			span.append(token.span());
			match token.as_ref() {
				lexer::Token::Keyword(kw) => {
					match kw {
						lexer::Keyword::Extern => {
							let id = Ident::parse(lexer)?;
							externs.push(id);
						},
						lexer::Keyword::RegExp => {
							let id = Ident::parse(lexer)?;
							let mut ty = None;
							let punct = expect_punct(lexer, &mut span)?;
							match punct.as_ref() {
								':' => {
									ty = Some(Ident::parse(lexer)?);
									let punct = expect_punct(lexer, &mut span)?;
									match punct.as_ref() {
										'=' => (),
										_ => return Err(Loc::new(Error::UnexpectedToken(lexer::Token::Punct(*punct.as_ref())), punct.span()))
									}
								},
								'=' => (),
								_ => return Err(Loc::new(Error::UnexpectedToken(lexer::Token::Punct(*punct.as_ref())), punct.span()))
							}
							let exp = RegExp::parse(lexer)?;
							let regexp_span = id.span().union(exp.span());
							regexps.push(Loc::new(RegExpDefinition {
								id, exp: TypedRegExp {
									ty, exp
								}
							}, regexp_span))
						},
						lexer::Keyword::Type => {
							let id = Ident::parse(lexer)?;
							let mut rules = Vec::new();
							let punct = expect_punct(lexer, &mut span)?;
							match punct.as_ref() {
								'=' => (),
								_ => return Err(Loc::new(Error::UnexpectedToken(lexer::Token::Punct(*punct.as_ref())), punct.span()))
							}
							let token = peek_token(lexer, &span)?;
							match token.as_ref() {
								lexer::Token::Punct('|') => {
									while peek_pipe(lexer, &span)? {
										consume(lexer, &mut span)?;
										let rule = Rule::parse(lexer)?;
										span.append(rule.span());
										rules.push(rule);
									}
								},
								_ => {
									let tokens = Vec::<Loc<ast::Token>>::parse(lexer)?;
									let tokens_span = tokens.span();
									span.append(tokens_span);
									rules.push(Loc::new(Rule {
										id: None,
										tokens: tokens.into_inner()
									}, tokens_span));
								}
							}

							types.push(Loc::new(Type {
								id: id,
								rules: rules
							}, span))
						}
					}
				},
				_ => return Err(Loc::new(Error::UnexpectedToken(token.as_ref().clone()), token.span()))
			}
		}

		Ok(Loc::new(Grammar {
			externs,
			regexps,
			types
		}, span))
	}
}

impl Parsable for Ident {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let token = expect(lexer, &mut span)?;
		match token.as_ref() {
			lexer::Token::Ident(name) => Ok(Loc::new(Ident(name.clone()), token.span())),
			_ => return Err(Loc::new(Error::UnexpectedToken(token.as_ref().clone()), token.span()))
		}
	}
}

impl Parsable for Rule {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let id = Ident::parse(lexer)?;
		let punct = expect_punct(lexer, &mut span)?;
		match punct.as_ref() {
			':' => (),
			_ => return Err(Loc::new(Error::UnexpectedToken(lexer::Token::Punct(*punct.as_ref())), punct.span()))
		}
		let tokens = Vec::<Loc<ast::Token>>::parse(lexer)?;
		span.append(tokens.span());
		Ok(Loc::new(Rule { id: Some(id), tokens: tokens.into_inner() }, span))
	}
}

fn is_repeater(c: char) -> bool {
	match c {
		'+' | '*' | '?' => true,
		_ => false
	}
}

impl Parsable for ast::Token {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let token = expect(lexer, &mut span)?;
		let token_span = token.span().clone();
		let ast = match token.into_inner() {
			lexer::Token::Ident(id) => {
				let exp = TypedRegExp {
					ty: None,
					exp: Loc::new(RegExp(vec![Loc::new(RegExpAtom::Ident(Ident(id)), token_span)]), token_span)
				};
				ast::Token::Terminal(Terminal::RegExp(exp))
			},
			lexer::Token::String(s, case_sensitive) => {
				let exp = TypedRegExp {
					ty: None,
					exp: Loc::new(RegExp(vec![Loc::new(RegExpAtom::Literal(s, case_sensitive), token_span)]), token_span)
				};
				ast::Token::Terminal(Terminal::RegExp(exp))
			},
			lexer::Token::Group(Delimiter::Brace, tokens) => {
				let mut lexer = tokens.into_iter().map(safe_token).peekable();
				let exp = TypedRegExp {
					ty: None,
					exp: RegExp::parse(&mut lexer)?
				};
				ast::Token::Terminal(Terminal::RegExp(exp))
			},
			lexer::Token::Group(Delimiter::Angle, tokens) => {
				let mut lexer = tokens.into_iter().map(safe_token).peekable();
				let token = expect(&mut lexer, &mut span)?;
				let token_span = token.span().clone();
				let id = match token.into_inner() {
					lexer::Token::Ident(id) => Loc::new(Ident(id), token_span),
					unexpected => {
						return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
					}
				};

				let nt = if let Some(token) = peek(&mut lexer)? {
					consume(&mut lexer, &mut span)?;
					let token_span = token.span().clone();
					let (min, max) = match token.into_inner() {
						lexer::Token::Punct('*') => (0, std::usize::MAX),
						lexer::Token::Punct('+') => (1, std::usize::MAX),
						lexer::Token::Punct('?') => (0, 1),
						unexpected => {
							return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
						}
					};

					let sep = if let Some(mut token) = peek(&mut lexer)? {
						consume(&mut lexer, &mut span)?;
						let mut token_span = token.span().clone();
						let mut sep_span = token_span;

						let strong = if let lexer::Token::Punct('!') = token.as_ref() {
							token = expect(&mut lexer, &mut span)?;
							token_span = token.span().clone();
							sep_span.append(token_span);
							true
						} else {
							false
						};

						let sep = match token.into_inner() {
							lexer::Token::String(s, case_sensitive) => {
								let exp = TypedRegExp {
									ty: None,
									exp: Loc::new(RegExp(vec![Loc::new(RegExpAtom::Literal(s, case_sensitive), token_span)]), token_span)
								};
								Terminal::RegExp(exp)
							},
							lexer::Token::Group(Delimiter::Brace, tokens) => {
								let mut lexer = tokens.into_iter().map(safe_token).peekable();
								let exp = TypedRegExp {
									ty: None,
									exp: RegExp::parse(&mut lexer)?
								};
								Terminal::RegExp(exp)
							},
							unexpected => {
								return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
							}
						};

						Some(Loc::new(Separator { strong, terminal: Loc::new(sep, token_span) }, sep_span))
					} else {
						None
					};

					NonTerminal::Repeat(id, min, max, sep)
				} else {
					NonTerminal::Type(id)
				};

				ast::Token::NonTerminal(nt)
			},
			unexpected => {
				return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
			}
		};

		Ok(Loc::new(ast, span))
	}
}

impl Parsable for Vec<Loc<ast::Token>> {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let mut tokens = Vec::new();
		loop {
			if let Some(token) = peek(lexer)? {
				match token.as_ref() {
					lexer::Token::Punct('|') | lexer::Token::Keyword(_) => {
						break
					},
					_ => {
						let token = ast::Token::parse(lexer)?;
						span.append(token.span());
						tokens.push(token);
					}
				}
			} else {
				break
			}
		}

		Ok(Loc::new(tokens, span))
	}
}

fn safe_token(token: Loc<lexer::Token>) -> lexer::Result<Loc<lexer::Token>> {
	Ok(token)
}

impl Parsable for RegExp {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(lexer: &mut Peekable<L>) -> Result<Loc<Self>> {
		let mut span = Span::default();

		let mut disjunction = Vec::new();
		let mut atoms = Vec::new();
		loop {
			if let Some(token) = peek(lexer)? {
				let token_span = token.span().clone();
				match token.into_inner() {
					lexer::Token::Ident(id) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(RegExpAtom::Ident(Ident(id.clone())), token_span));
					},
					lexer::Token::CharSet(set, negate) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(RegExpAtom::CharSet(set.clone(), negate), token_span));
					},
					lexer::Token::String(str, case_sensitive) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(RegExpAtom::Literal(str.clone(), case_sensitive), token_span));
					},
					lexer::Token::Group(Delimiter::Brace, tokens) => {
						consume(lexer, &mut span)?;
						let mut lexer = tokens.into_iter().map(safe_token).peekable();
						let exp = RegExp::parse(&mut lexer)?;
						let exp_span = exp.span();
						span.append(exp_span);
						atoms.push(Loc::new(RegExpAtom::Capture(exp.into_inner()), exp_span));
					},
					lexer::Token::Group(Delimiter::Parenthesis, tokens) => {
						consume(lexer, &mut span)?;
						let mut lexer = tokens.into_iter().map(safe_token).peekable();
						let mut exp = RegExp::parse(&mut lexer)?;
						let exp_span = exp.span();
						span.append(exp_span);
						atoms.push(Loc::new(RegExpAtom::Group(exp.into_inner()), exp_span));
					},
					lexer::Token::Punct(c) if is_repeater(c) => {
						consume(lexer, &mut span)?;
						if let Some(atom) = atoms.pop() {
							let (min, max) = match c {
								'*' => (0, std::usize::MAX),
								'+' => (1, std::usize::MAX),
								'?' => (0, 1),
								_ => unreachable!()
							};
							let atom_span = atom.span().clone();
							atoms.push(Loc::new(RegExpAtom::Repeat(Box::new(Loc::new(atom.into_inner(), atom_span)), 0, std::usize::MAX), atom_span))
						} else {
							return Err(Loc::new(Error::UnexpectedToken(lexer::Token::Punct(c)), token_span))
						}
					},
					lexer::Token::Punct('|') => {
						consume(lexer, &mut span)?;
						disjunction.push(atoms);
						atoms = Vec::new();
					},
					_ => break
				}
			} else {
				break
			}
		}

		if !atoms.is_empty() {
			disjunction.push(atoms);
		}

		let exp = match disjunction.len() {
			0 => return Err(Loc::new(Error::EmptyRegExp, span)),
			1 => RegExp(disjunction.pop().unwrap()),
			_ => {
				let exps = disjunction.into_iter().map(|atoms| {
					let span = atoms.first().unwrap().span().union(atoms.last().unwrap().span());
					Loc::new(RegExp(atoms), span)
				}).collect();
				RegExp(vec![Loc::new(RegExpAtom::Or(exps), span)])
			}
		};

		Ok(Loc::new(exp, span))
	}
}
