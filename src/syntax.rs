pub use source_span::{Loc, Position, Span};
use std::iter::Peekable;

mod ast;
mod caused;
mod error;
pub mod lexer;

pub use ast::*;
pub use caused::Caused;
pub use error::{Error, Result};
use lexer::Delimiter;
pub use lexer::Lexer;

pub trait Parsable: Sized {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>>;

	fn parse_only<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let value = Self::parse(lexer)?;

		if let Some(unexpected) = lexer.next().transpose().map_err(|e| e.inner_into())? {
			let (unexpected, span) = unexpected.into_raw_parts();
			return Err(Loc::new(Error::UnexpectedToken(unexpected), span));
		}

		Ok(value)
	}
}

fn peek<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
	lexer: &mut Peekable<L>,
) -> Result<Option<Loc<lexer::Token>>> {
	match lexer.peek() {
		Some(Ok(token)) => Ok(Some(token.clone())),
		Some(Err(_)) => {
			let mut dummy_span = Span::default();
			consume(lexer, &mut dummy_span)
		}
		None => Ok(None),
	}
}

fn peek_token<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
	lexer: &mut Peekable<L>,
	span: &Span,
) -> Result<Loc<lexer::Token>> {
	if let Some(token) = peek(lexer)? {
		Ok(token)
	} else {
		Err(Loc::new(Error::UnexpectedEos, span.end().into()))
	}
}

fn try_peek_pipe<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
	lexer: &mut Peekable<L>,
	_span: &Span,
) -> Result<bool> {
	Ok(match peek(lexer)? {
		Some(token) => {
			if let lexer::Token::Punct('|') = token.as_ref() {
				true
			} else {
				false
			}
		}
		None => false,
	})
}

fn consume<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
	lexer: &mut Peekable<L>,
	span: &mut Span,
) -> Result<Option<Loc<lexer::Token>>> {
	match lexer.next() {
		Some(Ok(token)) => {
			if span.is_empty() {
				*span = token.span();
			} else {
				span.append(token.span());
			}
			Ok(Some(token.clone()))
		}
		Some(Err(e)) => Err(e.inner_into()),
		None => Ok(None),
	}
}

fn expect<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
	lexer: &mut Peekable<L>,
	span: &mut Span,
) -> Result<Loc<lexer::Token>> {
	if let Some(token) = consume(lexer, span)? {
		Ok(token)
	} else {
		Err(Loc::new(Error::UnexpectedEos, span.end().into()))
	}
}

fn expect_punct<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
	lexer: &mut Peekable<L>,
	span: &mut Span,
) -> Result<Loc<char>> {
	let token = expect(lexer, span)?;

	if let lexer::Token::Punct(p) = token.as_ref() {
		Ok(Loc::new(*p, token.span()))
	} else {
		Err(Loc::new(
			Error::UnexpectedToken(token.as_ref().clone()),
			token.span(),
		))
	}
}

impl Parsable for Grammar {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
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
						}
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
										_ => {
											return Err(Loc::new(
												Error::UnexpectedToken(lexer::Token::Punct(
													*punct.as_ref(),
												)),
												punct.span(),
											))
										}
									}
								}
								'=' => (),
								_ => {
									return Err(Loc::new(
										Error::UnexpectedToken(lexer::Token::Punct(
											*punct.as_ref(),
										)),
										punct.span(),
									))
								}
							}
							let exp = RegExp::parse(lexer)?;
							let regexp_span = id.span().union(exp.span());
							regexps.push(Loc::new(regexp::Definition { id, ty, exp }, regexp_span))
						}
						lexer::Keyword::Type => {
							let id = Ident::parse(lexer)?;
							let mut rules = Vec::new();
							let punct = expect_punct(lexer, &mut span)?;
							match punct.as_ref() {
								'=' => (),
								_ => {
									return Err(Loc::new(
										Error::UnexpectedToken(lexer::Token::Punct(
											*punct.as_ref(),
										)),
										punct.span(),
									))
								}
							}
							let token = peek_token(lexer, &span)?;
							match token.as_ref() {
								lexer::Token::Punct('|') => {
									while try_peek_pipe(lexer, &span)? {
										consume(lexer, &mut span)?;
										let rule = Function::parse(lexer)?;
										span.append(rule.span());
										rules.push(rule);
									}
								}
								_ => {
									// Alias.
									let items = Vec::<Loc<ty::LabeledExpr>>::parse(lexer)?;
									let tokens_span = items.span();
									span.append(tokens_span);
									rules.push(Loc::new(
										Function {
											id: None,
											args: items.into_inner(),
										},
										tokens_span,
									));
								}
							}

							types.push(Loc::new(
								ty::Definition {
									id: id,
									parameters: Vec::new(), // TODO
									constructors: rules,
								},
								span,
							))
						}
					}
				}
				_ => {
					return Err(Loc::new(
						Error::UnexpectedToken(token.as_ref().clone()),
						token.span(),
					))
				}
			}
		}

		Ok(Loc::new(
			Grammar {
				externs,
				regexps,
				types,
			},
			span,
		))
	}
}

impl Parsable for Ident {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let token = expect(lexer, &mut span)?;
		match token.as_ref() {
			lexer::Token::Ident(name) => Ok(Loc::new(Ident(name.clone()), token.span())),
			_ => {
				return Err(Loc::new(
					Error::UnexpectedToken(token.as_ref().clone()),
					token.span(),
				))
			}
		}
	}
}

impl Parsable for Function {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let id = Ident::parse(lexer)?;
		let mut span = id.span();
		let punct = expect_punct(lexer, &mut span)?;
		match punct.as_ref() {
			':' => (),
			_ => {
				return Err(Loc::new(
					Error::UnexpectedToken(lexer::Token::Punct(*punct.as_ref())),
					punct.span(),
				))
			}
		}
		let items = Vec::<Loc<ast::ty::LabeledExpr>>::parse(lexer)?;
		span.append(items.span());
		Ok(Loc::new(
			Function {
				id: Some(id),
				args: items.into_inner(),
			},
			span,
		))
	}
}

fn is_repeater(c: char) -> bool {
	match c {
		'+' | '*' | '?' => true,
		_ => false,
	}
}

impl Parsable for ast::ty::LabeledExpr {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let label = match peek_token(lexer, &span)?.as_ref() {
			lexer::Token::Punct('@') => {
				consume(lexer, &mut span)?;
				let (token, token_span) = expect(lexer, &mut span)?.into_raw_parts();
				match token {
					lexer::Token::Ident(id) => {
						Some(Loc::new(Ident(id), token_span))
					},
					token => {
						return Err(Loc::new(
							Error::UnexpectedToken(token),
							token_span,
						))
					}
				}
			},
			_ => None
		};

		let expr = ast::ty::Expr::parse(lexer)?;
		let span = span.union(expr.span());

		Ok(Loc::new(ast::ty::LabeledExpr {
			label,
			expr
		}, span))
	}
}

impl Parsable for ast::ty::Expr {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let token = expect(lexer, &mut span)?;
		let token_span = token.span().clone();
		let ast = match token.into_inner() {
			lexer::Token::Ident(id) => {
				let exp = RegExp(vec![Loc::new(regexp::Atom::Ref(Ident(id)), token_span)]);
				ast::ty::Expr::Terminal(exp)
			}
			lexer::Token::String(s, case_sensitive) => {
				let exp = RegExp(vec![Loc::new(
					regexp::Atom::Literal(s, case_sensitive),
					token_span,
				)]);
				ast::ty::Expr::Terminal(exp)
			}
			lexer::Token::Group(Delimiter::Parenthesis, items) => {
				let mut lexer = items.into_iter().map(safe_token).peekable();
				let exp = RegExp::parse_only(&mut lexer)?;
				ast::ty::Expr::Terminal(exp.into_inner())
			}
			lexer::Token::Group(Delimiter::Angle, items) => {
				let mut lexer = items.into_iter().map(safe_token).peekable();
				let token = expect(&mut lexer, &mut span)?;
				let token_span = token.span().clone();
				let id = match token.into_inner() {
					lexer::Token::Ident(id) => Loc::new(Ident(id), token_span),
					unexpected => {
						return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
					}
				};

				let mut args = Vec::new();
				while peek(&mut lexer)?.is_some() {
					args.push(ast::ty::Expr::parse(&mut lexer)?);
				}

				// let nt = if let Some(token) = peek(&mut lexer)? {
				// 	consume(&mut lexer, &mut span)?;
				// 	let token_span = token.span().clone();
				// 	let (min, max) = match token.into_inner() {
				// 		lexer::Token::Punct('*') => (0, std::usize::MAX),
				// 		lexer::Token::Punct('+') => (1, std::usize::MAX),
				// 		lexer::Token::Punct('?') => (0, 1),
				// 		unexpected => {
				// 			return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
				// 		}
				// 	};

				// 	let sep = if let Some(mut token) = peek(&mut lexer)? {
				// 		consume(&mut lexer, &mut span)?;
				// 		let mut token_span = token.span().clone();
				// 		let mut sep_span = token_span;

				// 		let strong = if let lexer::Token::Punct('!') = token.as_ref() {
				// 			token = expect(&mut lexer, &mut span)?;
				// 			token_span = token.span().clone();
				// 			sep_span.append(token_span);
				// 			true
				// 		} else {
				// 			false
				// 		};

				// 		let sep = match token.into_inner() {
				// 			lexer::Token::String(s, case_sensitive) => {
				// 				let exp = RegExp(vec![Loc::new(RegExpAtom::Literal(s, case_sensitive), token_span)]);
				// 				Terminal::RegExp(exp)
				// 			},
				// 			lexer::Token::Group(Delimiter::Parenthesis, items) => {
				// 				let mut lexer = items.into_iter().map(safe_token).peekable();
				// 				let exp = RegExp::parse_only(&mut lexer)?;
				// 				Terminal::RegExp(exp.into_inner())
				// 			},
				// 			unexpected => {
				// 				return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
				// 			}
				// 		};

				// 		Some(Loc::new(Separator { strong, terminal: Loc::new(sep, token_span) }, sep_span))
				// 	} else {
				// 		None
				// 	};

				// 	NonTerminal::Repeat(id, min, max, sep)
				// } else {
				// 	NonTerminal::Type(id)
				// };

				ast::ty::Expr::NonTerminal(id, args)
			}
			unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span)),
		};

		Ok(Loc::new(ast, span))
	}
}

impl Parsable for Vec<Loc<ast::ty::LabeledExpr>> {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let mut span = Span::default();
		let mut items = Vec::new();
		loop {
			if let Some(token) = peek(lexer)? {
				match token.as_ref() {
					lexer::Token::Punct('|') | lexer::Token::Keyword(_) => break,
					_ => {
						let token = ast::ty::LabeledExpr::parse(lexer)?;
						span.append(token.span());
						items.push(token);
					}
				}
			} else {
				break;
			}
		}

		Ok(Loc::new(items, span))
	}
}

fn safe_token(token: Loc<lexer::Token>) -> lexer::Result<Loc<lexer::Token>> {
	Ok(token)
}

impl Parsable for RegExp {
	fn parse<L: Iterator<Item = lexer::Result<Loc<lexer::Token>>>>(
		lexer: &mut Peekable<L>,
	) -> Result<Loc<Self>> {
		let mut span = Span::default();
		// let mut inner_span = span;

		let mut disjunction = Vec::new();
		let mut atoms = Vec::new();
		// let mut ty = None;
		loop {
			if let Some(token) = peek(lexer)? {
				let token_span = token.span().clone();
				match token.into_inner() {
					lexer::Token::Punct('.') => {
						atoms.push(Loc::new(regexp::Atom::Any, token_span))
					},
					lexer::Token::Ident(id) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(regexp::Atom::Ref(Ident(id.clone())), token_span));
					}
					lexer::Token::CharSet(set, false) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(regexp::Atom::CharSet(set.clone()), token_span));
					}
					lexer::Token::CharSet(set, true) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(regexp::Atom::CharSet(set.negated()), token_span));
					}
					lexer::Token::String(str, case_sensitive) => {
						consume(lexer, &mut span)?;
						atoms.push(Loc::new(
							regexp::Atom::Literal(str.clone(), case_sensitive),
							token_span,
						));
					}
					lexer::Token::Group(Delimiter::Parenthesis, items) => {
						consume(lexer, &mut span)?;
						let mut lexer = items.into_iter().map(safe_token).peekable();
						let exp = RegExp::parse_only(&mut lexer)?;
						let exp_span = exp.span();
						span.append(exp_span);
						atoms.push(Loc::new(regexp::Atom::Group(exp), exp_span));
					}
					lexer::Token::Punct(c) if is_repeater(c) => {
						consume(lexer, &mut span)?;
						if let Some(atom) = atoms.pop() {
							let (min, max) = match c {
								'*' => (0, std::usize::MAX),
								'+' => (1, std::usize::MAX),
								'?' => (0, 1),
								_ => unreachable!(),
							};
							let atom_span = atom.span().clone();
							atoms.push(Loc::new(
								regexp::Atom::Repeat(
									Box::new(Loc::new(atom.into_inner(), atom_span)),
									min,
									max,
								),
								atom_span,
							))
						} else {
							return Err(Loc::new(
								Error::UnexpectedToken(lexer::Token::Punct(c)),
								token_span,
							));
						}
					}
					lexer::Token::Punct('|') => {
						consume(lexer, &mut span)?;
						disjunction.push(atoms);
						atoms = Vec::new();
					}
					lexer::Token::Keyword(_) => break,
					unexpected => {
						return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
					}
				}
			} else {
				break;
			}
		}

		if !atoms.is_empty() {
			disjunction.push(atoms);
		}

		let exp = match disjunction.len() {
			0 => return Err(Loc::new(Error::EmptyRegExp, span)),
			1 => RegExp(disjunction.pop().unwrap()),
			_ => {
				let exps = disjunction
					.into_iter()
					.map(|atoms| {
						let span = atoms
							.first()
							.unwrap()
							.span()
							.union(atoms.last().unwrap().span());
						Loc::new(RegExp(atoms), span)
					})
					.collect();
				RegExp(vec![Loc::new(regexp::Atom::Or(exps), span)])
			}
		};

		Ok(Loc::new(exp, span))
	}
}
