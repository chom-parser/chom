use std::collections::{
	HashMap,
	HashSet
};
use std::rc::Rc;
use std::fmt;
use once_cell::unsync::OnceCell;
use source_span::{
	Span,
	Loc
};
use crate::{
	Ident,
	syntax,
	lexing::{
		RegExp,
		RegExpAtom,
		TypedRegExp
	}
};

pub struct Grammar {
	pub externs: Vec<Loc<Ident>>,
	pub regexps: HashMap<Ident, OnceCell<Rc<Loc<RegExpDefinition>>>>,
	pub types: HashMap<Ident, OnceCell<Rc<Loc<Type>>>>
}

impl Grammar {
	pub fn regexp(&self, id: &Loc<Ident>) -> Result<&Loc<RegExpDefinition>, Loc<Error>> {
		self.regexps.get(id.as_ref()).map(|e| e.get()).flatten().map(|e| e.as_ref()).ok_or_else(|| Loc::new(Error::UndefinedRegExp(id.as_ref().clone()), id.span()))
	}
	
	fn regexp_defined(&self, id: &Loc<Ident>) -> Result<(), Loc<Error>> {
		self.regexps.get(id.as_ref()).ok_or_else(|| Loc::new(Error::UndefinedRegExp(id.as_ref().clone()), id.span()))?;
		Ok(())
	}

	fn type_defined(&self, id: &Loc<Ident>) -> Result<(), Loc<Error>> {
		self.types.get(id.as_ref()).ok_or_else(|| Loc::new(Error::UndefinedType(id.as_ref().clone()), id.span()))?;
		Ok(())
	}

	pub fn terminals(&self) -> HashMap<Terminal, HashSet<Span>> {
		let mut terminals = HashSet::new();
		
		for (id, ty) in &self.types {
			let def = ty.get().unwrap();
			for rule in &def.rules {
				for item in &rule.items {
					match item.as_ref() {
						Item::Terminal(terminal) => {
							match terminals.get(&terminal) {
								Some(spans) => (),
								None => ()
							}
						},
						_ => ()
					}
				}
			}
		}

		terminals
	}
}

pub struct Type {
	pub id: Loc<Ident>,
	pub rules: Vec<Loc<Rule>>
}

pub struct Rule {
	pub id: Option<Loc<Ident>>,
	pub items: Vec<Loc<Item>>
}

pub enum Item {
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

#[derive(Debug)]
pub enum Error {
	UndefinedRegExp(Ident),
	AlreadyDefinedRegExp(Ident),
	UndefinedType(Ident),
	AlreadyDefinedType(Ident)
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Error::UndefinedRegExp(id) => write!(f, "undefined regular expression `{}`", id),
			Error::AlreadyDefinedRegExp(id) => write!(f, "already defined regular expression `{}`", id),
			Error::UndefinedType(id) => write!(f, "undefined type `{}`", id),
			Error::AlreadyDefinedType(id) => write!(f, "already defined type `{}`", id)
		}
	}
}

fn compile_loc_regexp(g: &Grammar, ast: Loc<syntax::RegExp>) -> Result<RegExp, Loc<Error>> {
	let span = ast.span();
	Ok(compile_regexp(g, ast.into_inner(), span)?)
}

fn compile_regexp(g: &Grammar, ast: syntax::RegExp, _span: Span) -> Result<RegExp, Loc<Error>> {
	let mut atoms = Vec::new();
	for ast in ast.0.into_iter() {
		atoms.push(compile_regexp_atom(g, ast)?);
	}

	Ok(RegExp(atoms))
}

fn compile_regexp_atom(g: &Grammar, ast: Loc<syntax::RegExpAtom>) -> Result<RegExpAtom, Loc<Error>> {
	let span = ast.span();
	let exp = match ast.into_inner() {
		syntax::RegExpAtom::Ident(id) => {
			g.regexp_defined(&id)?;
			RegExpAtom::Ref(id.into_inner())
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
				compiled_exps.push(compile_loc_regexp(g, e)?);
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

	Ok(exp)
}

fn compile_terminal(g: &Grammar, ast: syntax::Terminal) -> Result<Terminal, Loc<Error>> {
	let t = match ast {
		syntax::Terminal::RegExp(exp) => {
			Terminal::RegExp(TypedRegExp {
				ty: exp.ty.map(|ty| ty.into_inner()),
				exp: compile_loc_regexp(&g, exp.exp)?
			})
		}
	};

	Ok(t)
}

fn compile_loc_terminal(g: &Grammar, ast: Loc<syntax::Terminal>) -> Result<Loc<Terminal>, Loc<Error>> {
	let span = ast.span();
	Ok(Loc::new(compile_terminal(g, ast.into_inner())?, span))
}

fn compile_item(g: &Grammar, ast: Loc<syntax::Item>) -> Result<Loc<Item>, Loc<Error>> {
	let span = ast.span();
	let t = match ast.into_inner() {
		syntax::Item::Terminal(t) => {
			Item::Terminal(compile_terminal(g, t)?)
		},
		syntax::Item::NonTerminal(nt) => {
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
								terminal: compile_loc_terminal(g, s.terminal)?
							}, span))
						},
						None => None
					};
					NonTerminal::Repeat(id, min, max, sep)
				}
			};
			Item::NonTerminal(nt)
		}
	};
	Ok(Loc::new(t, span))
}

fn compile_rule(g: &Grammar, ast: Loc<syntax::Rule>) -> Result<Loc<Rule>, Loc<Error>> {
	let span = ast.span();
	let ast = ast.into_inner();
	let mut items = Vec::new();
	for item in ast.items.into_iter() {
		items.push(compile_item(g, item)?);
	}
	Ok(Loc::new(Rule {
		id: ast.id,
		items
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

		let g = Grammar {
			externs,
			regexps,
			types
		};

		for ast in ast.regexps.into_iter() {
			let id = ast.id.clone();
			let span = ast.span();
			g.regexp_defined(&id)?;
			let ast = ast.into_inner();
			g.regexps.get(&id).unwrap().set(
				Rc::new(Loc::new(RegExpDefinition {
					id: ast.id,
					exp: TypedRegExp {
						ty: ast.exp.ty.map(|ty| ty.into_inner()),
						exp: compile_loc_regexp(&g, ast.exp.exp)?
					}
				}, span))
			).or_else(|_| Err(Loc::new(Error::AlreadyDefinedRegExp(id.as_ref().clone()), id.span())))?;
		}

		for ast in ast.types.into_iter() {
			let id = ast.id.clone();
			let span = ast.span();
			g.type_defined(&id)?;
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
			).or_else(|_| Err(Loc::new(Error::AlreadyDefinedType(id.as_ref().clone()), id.span())))?;
		}

		Ok(Loc::new(g, span))
	}
}
