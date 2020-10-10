use std::collections::HashMap;
use std::rc::Rc;
use std::convert::{
	TryFrom,
	TryInto
};
use std::fmt;
use once_cell::unsync::OnceCell;
use source_span::{
	Span,
	Loc
};
use crate::{
	CharSet,
	Ident
};
use crate::syntax;

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: HashMap<Ident, OnceCell<Rc<Loc<RegExpDefinition>>>>,
	pub types: HashMap<Ident, OnceCell<Rc<Loc<Type>>>>
}

impl Grammar {
	fn regexp_defined(&self, id: &Loc<Ident>) -> Result<(), Loc<Error>> {
		self.regexps.get(id.as_ref()).ok_or(Loc::new(Error::UndefinedRegExp(id.as_ref().clone()), id.span()))?;
		Ok(())
	}

	fn type_defined(&self, id: &Loc<Ident>) -> Result<(), Loc<Error>> {
		self.types.get(id.as_ref()).ok_or(Loc::new(Error::UndefinedType(id.as_ref().clone()), id.span()))?;
		Ok(())
	}
}

pub struct Type {
	id: Loc<Ident>,
	rules: Vec<Loc<Rule>>
}

pub struct Rule {
	pub id: Option<Loc<Ident>>,
	pub tokens: Vec<Loc<Token>>
}

pub enum Token {
	Terminal(Terminal),
	NonTerminal(NonTerminal)
}

pub enum Terminal {
	RegExp(TypedRegExp)
}

pub enum NonTerminal {
	Type(Loc<Ident>),
	Repeat(Loc<Ident>, usize, usize, Option<Loc<Separator>>),
}

pub struct Separator {
	pub strong: bool,
	pub terminal: Loc<Terminal>
}

pub struct RegExpDefinition {
	pub id: Loc<Ident>,
	pub exp: TypedRegExp
}

pub struct TypedRegExp {
	pub ty: Option<Loc<Ident>>,
	pub exp: Loc<RegExp>
}

pub struct RegExp(pub Vec<Loc<RegExpAtom>>);

pub enum RegExpAtom {
	Ref(Ident),
	CharSet(CharSet),
	Literal(String, bool),
	Repeat(Box<Loc<RegExpAtom>>, usize, usize),
	Or(Vec<Loc<RegExp>>),
	Capture(RegExp),
	Group(RegExp),
}

#[derive(Debug)]
pub enum Error {
	UndefinedRegExp(Ident),
	UndefinedType(Ident)
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Error::UndefinedRegExp(id) => write!(f, "undefined regular expression `{}`", id),
			Error::UndefinedType(id) => write!(f, "undefined type `{}`", id)
		}
	}
}

fn compile_Loc_regexp(g: &Grammar, ast: Loc<syntax::RegExp>) -> Result<Loc<RegExp>, Loc<Error>> {
	let span = ast.span();
	Ok(Loc::new(compile_regexp(g, ast.into_inner(), span)?, span))
}

fn compile_regexp(g: &Grammar, ast: syntax::RegExp, span: Span) -> Result<RegExp, Loc<Error>> {
	let mut atoms = Vec::new();
	for ast in ast.0.into_iter() {
		atoms.push(compile_regexp_atom(g, ast)?);
	}

	Ok(RegExp(atoms))
}

fn compile_regexp_atom(g: &Grammar, ast: Loc<syntax::RegExpAtom>) -> Result<Loc<RegExpAtom>, Loc<Error>> {
	let span = ast.span();
	let exp = match ast.into_inner() {
		syntax::RegExpAtom::Ident(id) => {
			g.regexp_defined(&Loc::new(id.clone(), span))?;
			RegExpAtom::Ref(id)
		},
		syntax::RegExpAtom::CharSet(set, false) => RegExpAtom::CharSet(set),
		syntax::RegExpAtom::CharSet(set, true) => RegExpAtom::CharSet(set.negated()),
		syntax::RegExpAtom::Literal(str, case_sensitive) => RegExpAtom::Literal(str, case_sensitive),
		syntax::RegExpAtom::Repeat(atom, min, max) => {
			RegExpAtom::Repeat(Box::new(compile_regexp_atom(g, *atom)?), min, max)
		},
		syntax::RegExpAtom::Or(exps) => {
			let mut compiled_exps = Vec::new();
			for e in exps.into_iter() {
				compiled_exps.push(compile_Loc_regexp(g, e)?);
			}
			RegExpAtom::Or(compiled_exps)
		},
		syntax::RegExpAtom::Capture(exp) => {
			RegExpAtom::Capture(compile_regexp(g, exp, span)?)
		},
		syntax::RegExpAtom::Group(exp) => {
			RegExpAtom::Group(compile_regexp(g, exp, span)?)
		}
	};

	Ok(Loc::new(exp, span))
}

fn compile_terminal(g: &Grammar, ast: syntax::Terminal) -> Result<Terminal, Loc<Error>> {
	let t = match ast {
		syntax::Terminal::RegExp(exp) => {
			Terminal::RegExp(TypedRegExp {
				ty: exp.ty,
				exp: compile_Loc_regexp(&g, exp.exp)?
			})
		}
	};

	Ok(t)
}

fn compile_Loc_terminal(g: &Grammar, ast: Loc<syntax::Terminal>) -> Result<Loc<Terminal>, Loc<Error>> {
	let span = ast.span();
	Ok(Loc::new(compile_terminal(g, ast.into_inner())?, span))
}

fn compile_token(g: &Grammar, ast: Loc<syntax::Token>) -> Result<Loc<Token>, Loc<Error>> {
	let span = ast.span();
	let t = match ast.into_inner() {
		syntax::Token::Terminal(t) => {
			Token::Terminal(compile_terminal(g, t)?)
		},
		syntax::Token::NonTerminal(nt) => {
			let nt = match nt {
				syntax::NonTerminal::Type(id) => {
					g.type_defined(&id)?;
					NonTerminal::Type(id)
				},
				syntax::NonTerminal::Repeat(id, min, max, sep) => {
					g.type_defined(&id)?;
					let sep = match sep {
						Some(s) => {
							let span = s.span();
							let s = s.into_inner();
							Some(Loc::new(Separator {
								strong: s.strong,
								terminal: compile_Loc_terminal(g, s.terminal)?
							}, span))
						},
						None => None
					};
					NonTerminal::Repeat(id, min, max, sep)
				}
			};
			Token::NonTerminal(nt)
		}
	};
	Ok(Loc::new(t, span))
}

fn compile_rule(g: &Grammar, ast: Loc<syntax::Rule>) -> Result<Loc<Rule>, Loc<Error>> {
	let span = ast.span();
	let ast = ast.into_inner();
	let mut tokens = Vec::new();
	for token in ast.tokens.into_iter() {
		tokens.push(compile_token(g, token)?);
	}
	Ok(Loc::new(Rule {
		id: ast.id,
		tokens
	}, span))
}

impl syntax::Grammar {
	pub fn compile(self: Loc<Self>) -> Result<Loc<Grammar>, Loc<Error>> {
		let span = self.span();
		let ast = self.into_inner();
		let externs = ast.externs;
		let mut regexps = HashMap::new();
		let mut types = HashMap::new();

		for def in &ast.regexps {
			regexps.insert(def.id.as_ref().clone(), OnceCell::new());
		}

		for ty in &ast.types {
			types.insert(ty.id.as_ref().clone(), OnceCell::new());
		}

		let mut g = Grammar {
			externs,
			regexps,
			types
		};

		for ast in ast.regexps.into_iter() {
			let id = ast.id.clone();
			let span = ast.span();
			g.regexp_defined(&id);
			let ast = ast.into_inner();
			g.regexps.get(&id).unwrap().set(
				Rc::new(Loc::new(RegExpDefinition {
					id: ast.id,
					exp: TypedRegExp {
						ty: ast.exp.ty,
						exp: compile_Loc_regexp(&g, ast.exp.exp)?
					}
				}, span))
			);
		}

		for ast in ast.types.into_iter() {
			let id = ast.id.clone();
			let span = ast.span();
			g.type_defined(&id);
			let ast = ast.into_inner();

			let mut rules = Vec::new();
			for rule in ast.rules {
				rules.push(compile_rule(&g, rule)?);
			}

			g.types.get(&id).unwrap().set(
				Rc::new(Loc::new(Type {
					id: ast.id,
					rules: rules
				}, span))
			);
		}

		Ok(Loc::new(g, span))
	}
}
