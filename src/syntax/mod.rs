use std::iter::Peekable;
pub use source_span::{Span, Position};

mod error;
mod ast;
mod location;
pub mod lexer;

pub use error::{Error, Result};
pub use ast::*;
pub use location::Located;
pub use lexer::Lexer;
use lexer::{Token, Delimiter};

pub trait Parsable: Sized {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>>;
}

fn peek<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>) -> Result<Option<Located<Token>>> {
    match lexer.peek() {
        Some(Ok(token)) => Ok(Some(token.clone())),
        Some(Err(e)) => {
            let mut dummy_span = Span::default();
            consume(lexer, &mut dummy_span)
        },
        None => Ok(None)
    }
}

fn peek_token<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, span: &Span) -> Result<Located<Token>> {
    if let Some(token) = peek(lexer)? {
        Ok(token)
    } else {
        Err(Located::new(Error::UnexpectedEos, span.end().into()))
    }
}

fn peek_pipe<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, span: &Span) -> Result<bool> {
    let token = peek_token(lexer, span)?;
    if let Token::Punct('|') = token.as_ref() {
        Ok(true)
    } else {
        Ok(false)
    }
}

fn consume<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Option<Located<Token>>> {
    match lexer.next() {
        Some(Ok(token)) => {
            if span.is_empty() {
                *span = token.span();
            } else {
                span.append(token.span());
            }
            Ok(Some(token.clone()))
        },
        Some(Err(e)) => Err(e.into()),
        None => Ok(None)
    }
}

fn expect<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Located<Token>> {
    if let Some(token) = consume(lexer, span)? {
        Ok(token)
    } else {
        Err(Located::new(Error::UnexpectedEos, span.end().into()))
    }
}

fn expect_punct<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Located<char>> {
    let token = expect(lexer, span)?;

    if let Token::Punct(p) = token.as_ref() {
        Ok(Located::new(*p, token.span()))
    } else {
        Err(Located::new(Error::UnexpectedToken(token.as_ref().clone()), token.span()))
    }
}

impl Parsable for Grammar {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>> {
        let mut span: Span = pos.into();
        let mut externs = Vec::new();
        let mut regexps = Vec::new();
        let mut types = Vec::new();

        while let Some(token) = consume(lexer, &mut span)? {
            span.append(token.span());
            match token.as_ref() {
                Token::Keyword(kw) => {
                    match kw {
                        lexer::Keyword::Extern => {
                            let id = Ident::parse(lexer, span.end())?;
                            externs.push(id);
                        },
                        lexer::Keyword::RegExp => {
                            let id = Ident::parse(lexer, span.end())?;
                            let mut ty = None;
                            let punct = expect_punct(lexer, &mut span)?;
                            match punct.as_ref() {
                                ':' => {
                                    ty = Some(Ident::parse(lexer, span.end())?);
                                    let punct = expect_punct(lexer, &mut span)?;
                                    match punct.as_ref() {
                                        '=' => (),
                                        _ => return Err(Located::new(Error::UnexpectedToken(Token::Punct(*punct.as_ref())), punct.span()))
                                    }
                                },
                                '=' => (),
                                _ => return Err(Located::new(Error::UnexpectedToken(Token::Punct(*punct.as_ref())), punct.span()))
                            }
                            let exp = RegExp::parse(lexer, span.end())?;
                            let regexp_span = id.span().union(exp.span());
                            regexps.push(Located::new(RegExpDefinition {
                                id, ty, exp
                            }, regexp_span))
                        },
                        lexer::Keyword::Type => {
                            let id = Ident::parse(lexer, span.end())?;
                            let mut rules = Vec::new();
                            let punct = expect_punct(lexer, &mut span)?;
                            match punct.as_ref() {
                                '=' => (),
                                _ => return Err(Located::new(Error::UnexpectedToken(Token::Punct(*punct.as_ref())), punct.span()))
                            }
                            let token = peek_token(lexer, &span)?;
                            match token.as_ref() {
                                Token::Punct('|') => {
                                    while peek_pipe(lexer, &span)? {
                                        consume(lexer, &mut span)?;
                                        let rule = Rule::parse(lexer, span.end())?;
                                        span.append(rule.span());
                                        rules.push(rule);
                                    }
                                },
                                _ => {
                                    let tokens = Vec::<Located<ast::Token>>::parse(lexer, span.end())?;
                                    span.append(tokens.span());
                                    types.push(Located::new(Type::Alias(tokens.into_inner()), span));
                                    continue
                                }
                            }
                        }
                    }
                },
                _ => return Err(Located::new(Error::UnexpectedToken(token.as_ref().clone()), token.span()))
            }
        }

        Ok(Located::new(Grammar {
            externs,
            regexps,
            types
        }, span))
    }
}

impl Parsable for Ident {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>> {
        let mut span = pos.into();
        let token = expect(lexer, &mut span)?;
        match token.as_ref() {
            Token::Ident(name) => Ok(Located::new(Ident(name.clone()), token.span())),
            _ => return Err(Located::new(Error::UnexpectedToken(token.as_ref().clone()), token.span()))
        }
    }
}

impl Parsable for Rule {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>> {
        let mut span = pos.into();
        let id = Ident::parse(lexer, pos)?;
        let punct = expect_punct(lexer, &mut span)?;
        match punct.as_ref() {
            ':' => (),
            _ => return Err(Located::new(Error::UnexpectedToken(Token::Punct(*punct.as_ref())), punct.span()))
        }
        let tokens = Vec::<Located<ast::Token>>::parse(lexer, span.end())?;
        span.append(tokens.span());
        Ok(Located::new(Rule { id, tokens: tokens.into_inner() }, span))
    }
}

fn is_repeater(c: char) -> bool {
    match c {
        '+' | '*' | '?' => true,
        _ => false
    }
}

impl Parsable for ast::Token {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>> {
        let mut span: Span = pos.into();
        let token = expect(lexer, &mut span)?;
        let token_span = token.span().clone();
        let ast = match token.into_inner() {
            Token::Ident(id) => {
                let exp = TypedRegExp {
                    ty: None,
                    exp: Located::new(RegExp(vec![Located::new(RegExpAtom::Ident(id), token_span)]), token_span)
                };
                ast::Token::Terminal(Terminal::RegExp(exp))
            },
            Token::String(s, case_sensitive) => {
                let exp = TypedRegExp {
                    ty: None,
                    exp: Located::new(RegExp(vec![Located::new(RegExpAtom::Literal(s, case_sensitive), token_span)]), token_span)
                };
                ast::Token::Terminal(Terminal::RegExp(exp))
            },
            Token::Group(Delimiter::Brace, tokens) => {
                let mut lexer = tokens.into_iter().map(safe_token).peekable();
                let exp = TypedRegExp {
                    ty: None,
                    exp: RegExp::parse(&mut lexer, span.end())?
                };
                ast::Token::Terminal(Terminal::RegExp(exp))
            },
            Token::Group(Delimiter::Angle, tokens) => {
                let mut lexer = tokens.into_iter().map(safe_token).peekable();
                let token = expect(&mut lexer, &mut span)?;
                let token_span = token.span().clone();
                let id = match token.into_inner() {
                    Token::Ident(id) => Located::new(id, token_span),
                    unexpected => {
                        return Err(Located::new(Error::UnexpectedToken(unexpected), token_span))
                    }
                };

                let nt = if let Some(token) = peek(&mut lexer)? {
                    consume(&mut lexer, &mut span)?;
                    let token_span = token.span().clone();
                    let (min, max) = match token.into_inner() {
                        Token::Punct('*') => (0, std::usize::MAX),
                        Token::Punct('+') => (1, std::usize::MAX),
                        Token::Punct('?') => (0, 1),
                        unexpected => {
                            return Err(Located::new(Error::UnexpectedToken(unexpected), token_span))
                        }
                    };

                    let sep = if let Some(mut token) = peek(&mut lexer)? {
                        consume(&mut lexer, &mut span)?;
                        let mut token_span = token.span().clone();
                        let mut sep_span = token_span;

                        let strong = if let Token::Punct('!') = token.as_ref() {
                            token = expect(&mut lexer, &mut span)?;
                            token_span = token.span().clone();
                            sep_span.append(token_span);
                            true
                        } else {
                            false
                        };

                        let sep = match token.into_inner() {
                            Token::String(s, case_sensitive) => {
                                let exp = TypedRegExp {
                                    ty: None,
                                    exp: Located::new(RegExp(vec![Located::new(RegExpAtom::Literal(s, case_sensitive), token_span)]), token_span)
                                };
                                Terminal::RegExp(exp)
                            },
                            Token::Group(Delimiter::Brace, tokens) => {
                                let mut lexer = tokens.into_iter().map(safe_token).peekable();
                                let exp = TypedRegExp {
                                    ty: None,
                                    exp: RegExp::parse(&mut lexer, span.end())?
                                };
                                Terminal::RegExp(exp)
                            },
                            unexpected => {
                                return Err(Located::new(Error::UnexpectedToken(unexpected), token_span))
                            }
                        };

                        Some(Located::new(Separator { strong, terminal: Located::new(sep, token_span) }, sep_span))
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
                return Err(Located::new(Error::UnexpectedToken(unexpected), token_span))
            }
        };

        Ok(Located::new(ast, span))
    }
}

impl Parsable for Vec<Located<ast::Token>> {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>> {
        let mut span: Span = pos.into();
        let mut tokens = Vec::new();
        loop {
            if let Some(token) = peek(lexer)? {
                match token.as_ref() {
                    Token::Punct('|') | Token::Keyword(_) => {
                        break
                    },
                    _ => {
                        let token = ast::Token::parse(lexer, span.end())?;
                        span.append(token.span());
                        tokens.push(token);
                    }
                }
            } else {
                break
            }
        }

        Ok(Located::new(tokens, span))
    }
}

fn safe_token(token: Located<Token>) -> lexer::Result<Located<Token>> {
    Ok(token)
}

impl Parsable for RegExp {
    fn parse<L: Iterator<Item = lexer::Result<Located<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Located<Self>> {
        let mut span: Span = pos.into();

        let mut disjunction = Vec::new();
        let mut atoms = Vec::new();
        loop {
            if let Some(token) = peek(lexer)? {
                let token_span = token.span().clone();
                match token.into_inner() {
                    Token::Ident(id) => {
                        consume(lexer, &mut span)?;
                        atoms.push(Located::new(RegExpAtom::Ident(id.clone()), token_span));
                    },
                    Token::CharSet(set, negate) => {
                        consume(lexer, &mut span)?;
                        atoms.push(Located::new(RegExpAtom::CharSet(set.clone(), negate), token_span));
                    },
                    Token::String(str, case_sensitive) => {
                        consume(lexer, &mut span)?;
                        atoms.push(Located::new(RegExpAtom::Literal(str.clone(), case_sensitive), token_span));
                    },
                    Token::Group(Delimiter::Brace, tokens) => {
                        consume(lexer, &mut span)?;
                        let mut lexer = tokens.into_iter().map(safe_token).peekable();
                        let exp = RegExp::parse(&mut lexer, span.end())?;
                        let exp_span = exp.span();
                        span.append(exp_span);
                        atoms.push(Located::new(RegExpAtom::Capture(exp.into_inner()), exp_span));
                    },
                    Token::Group(Delimiter::Parenthesis, tokens) => {
                        consume(lexer, &mut span)?;
                        let mut lexer = tokens.into_iter().map(safe_token).peekable();
                        let mut exp = RegExp::parse(&mut lexer, span.end())?;
                        let exp_span = exp.span();
                        span.append(exp_span);
                        atoms.push(Located::new(RegExpAtom::Group(exp.into_inner()), exp_span));
                    },
                    Token::Punct(c) if is_repeater(c) => {
                        consume(lexer, &mut span)?;
                        if let Some(atom) = atoms.pop() {
                            let (min, max) = match c {
                                '*' => (0, std::usize::MAX),
                                '+' => (1, std::usize::MAX),
                                '?' => (0, 1),
                                _ => unreachable!()
                            };
                            let atom_span = atom.span().clone();
                            atoms.push(Located::new(RegExpAtom::Repeat(Box::new(Located::new(atom.into_inner(), atom_span)), 0, std::usize::MAX), atom_span))
                        } else {
                            return Err(Located::new(Error::UnexpectedToken(Token::Punct(c)), token_span))
                        }
                    },
                    Token::Punct('|') => {
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
            0 => return Err(Located::new(Error::EmptyRegExp, span)),
            1 => RegExp(disjunction.pop().unwrap()),
            _ => {
                let exps = disjunction.into_iter().map(|atoms| {
                    let span = atoms.first().unwrap().span().union(atoms.last().unwrap().span());
                    Located::new(RegExp(atoms), span)
                }).collect();
                RegExp(vec![Located::new(RegExpAtom::Or(exps), span)])
            }
        };

        Ok(Located::new(exp, span))
    }
}
