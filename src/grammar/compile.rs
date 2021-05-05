use std::{
	fmt,
	collections::{
		HashSet,
		HashMap
	}
};
use source_span::{
	Span,
	Loc
};
use crate::syntax;
use super::{
	Ident,
	Terminal,
	LocTerminal,
	NonTerminal,
	Item,
	Rule,
	Grammar,
	Type,
	RegExpDefinition,
	LocRegExp,
	LocRegExpAtom,
	Separator
};

struct RegExps {
	regexps: HashMap<Ident, Option<Loc<RegExpDefinition>>>,
}

impl RegExps {
	pub fn new(ast: &[Loc<syntax::RegExpDefinition>]) -> Self {
		let mut regexps = HashMap::new();

		for def in ast {
			regexps.insert(def.id.as_ref().clone(), None);
		}

		Self {
			regexps
		}
	}

	pub fn into_map(self) -> HashMap<Ident, Loc<RegExpDefinition>> {
		let mut map = HashMap::new();

		for (id, def) in self.regexps {
			map.insert(id, def.unwrap());
		}

		map
	}

	fn assert_defined(&self, id: &Ident, span: Span) -> Result<(), Loc<Error>> {
		self.get(id, span)?;
		Ok(())
	}

	fn get(&self, id: &Ident, span: Span) -> Result<Option<&Loc<RegExpDefinition>>, Loc<Error>> {
		Ok(self.regexps.get(id).ok_or_else(|| Loc::new(Error::UndefinedRegExp(id.clone()), span))?.as_ref())
	}

	fn get_mut(&mut self, id: &Ident, span: Span) -> Result<&mut Option<Loc<RegExpDefinition>>, Loc<Error>> {
		self.regexps.get_mut(id).ok_or_else(|| Loc::new(Error::UndefinedRegExp(id.clone()), span))
	}

	fn define(&mut self, id: &Ident, span: Span, def: Loc<RegExpDefinition>) -> Result<(), Loc<Error>> {
		let old_def = self.get_mut(id, span)?.replace(def);
		if let Some(old_def) = old_def {
			Err(Loc::new(Error::AlreadyDefinedRegExp(id.clone(), old_def.span()), span))
		} else {
			Ok(())
		}
	}
}

struct Types {
	types: HashMap<Ident, Option<Loc<Type>>>,
}

impl Types {
	pub fn new(ast: &[Loc<syntax::Type>]) -> Self {
		let mut types = HashMap::new();
		
		for def in ast {
			types.insert(def.id.as_ref().clone(), None);
		}

		Self {
			types
		}
	}

	pub fn into_map(self) -> HashMap<Ident, Loc<Type>> {
		let mut map = HashMap::new();

		for (id, def) in self.types {
			map.insert(id, def.unwrap());
		}

		map
	}

	fn assert_defined(&self, id: &Loc<Ident>) -> Result<(), Loc<Error>> {
		self.get(id)?;
		Ok(())
	}

	fn get(&self, id: &Loc<Ident>) -> Result<Option<&Loc<Type>>, Loc<Error>> {
		Ok(self.types.get(id.as_ref()).ok_or_else(|| Loc::new(Error::UndefinedType(id.as_ref().clone()), id.span()))?.as_ref())
	}

	fn get_mut(&mut self, id: &Loc<Ident>) -> Result<&mut Option<Loc<Type>>, Loc<Error>> {
		self.types.get_mut(id.as_ref()).ok_or_else(|| Loc::new(Error::UndefinedType(id.as_ref().clone()), id.span()))
	}

	fn define(&mut self, id: &Loc<Ident>, def: Loc<Type>) -> Result<(), Loc<Error>> {
		let old_def = self.get_mut(id)?.replace(def);
		if let Some(old_def) = old_def {
			Err(Loc::new(Error::AlreadyDefinedType(id.as_ref().clone(), old_def.span()), id.span()))
		} else {
			Ok(())
		}
	}
}

struct Terminals {
	/// Every terminals.
	list: Vec<(Terminal, HashSet<Loc<LocTerminal>>)>,

	/// Associate each terminal to its identifier.
	table: HashMap<Terminal, u32>
}

impl Terminals {
	pub fn new() -> Self {
		Self {
			list: Vec::new(),
			table: HashMap::new()
		}
	}

	pub fn id(&mut self, loc_terminal: Loc<LocTerminal>) -> u32 {
		let terminal = loc_terminal.stripped();
		match self.table.get(&terminal).cloned() {
			Some(id) => {
				self.list[id as usize].1.insert(loc_terminal);
				id
			},
			None => {
				let id = self.list.len() as u32;
				let mut locs = HashSet::new();
				locs.insert(loc_terminal);
				self.list.push((terminal.clone(), locs));
				self.table.insert(terminal, id);
				id
			}
		}
	}

	pub fn into_vec(self) -> Vec<(Terminal, HashSet<Loc<LocTerminal>>)> {
		self.list
	}
}

#[derive(Debug)]
pub enum Error {
	UndefinedRegExp(Ident),
	AlreadyDefinedRegExp(Ident, Span),
	UndefinedType(Ident),
	AlreadyDefinedType(Ident, Span),
	RegExpTypeMissmatch(Loc<Ident>)
}

#[derive(Debug)]
pub enum TerminalAmbiguity {
	ShiftReduce {
		prefix: String,
		next: btree_range_map::AnyRange<char>
	},
	ReduceReduce {
		token: String
	}
}

impl Error {
	pub fn format_notes(&self, fmt: &mut source_span::fmt::Formatter, style: source_span::fmt::Style) {
		match self {
			Error::AlreadyDefinedRegExp(_, span) => {
				fmt.add(*span, Some("first definition".to_string()), style)
			},
			Error::AlreadyDefinedType(_, span) => {
				fmt.add(*span, Some("first definition".to_string()), style)
			},
			Error::RegExpTypeMissmatch(ty) => {
				fmt.add(ty.span(), Some("the regexp type is defined here".to_string()), style)
			},
			_ => ()
		}
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Error::UndefinedRegExp(id) => write!(f, "undefined regular expression `{}`", id),
			Error::AlreadyDefinedRegExp(id, _) => write!(f, "already defined regular expression `{}`", id),
			Error::UndefinedType(id) => write!(f, "undefined type `{}`", id),
			Error::AlreadyDefinedType(id, _) => write!(f, "already defined type `{}`", id),
			Error::RegExpTypeMissmatch(ty) => write!(f, "expected regexp type `unit`, found `{}`", ty)
		}
	}
}

fn compile_loc_regexp(regexps: &RegExps, ast: Loc<syntax::RegExp>) -> Result<Loc<LocRegExp>, Loc<Error>> {
	let (ast, span) = ast.into_raw_parts();
	Ok(Loc::new(compile_regexp(regexps, ast)?, span))
}

fn compile_regexp(regexps: &RegExps, ast: syntax::RegExp) -> Result<LocRegExp, Loc<Error>> {
	let mut atoms = Vec::new();
	for ast in ast.0.into_iter() {
		atoms.push(compile_regexp_atom(regexps, ast)?);
	}

	Ok(LocRegExp(atoms))
}

fn compile_regexp_atom(regexps: &RegExps, ast: Loc<syntax::RegExpAtom>) -> Result<Loc<LocRegExpAtom>, Loc<Error>> {
	let (ast, span) = ast.into_raw_parts();
	let exp = match ast {
		syntax::RegExpAtom::Ident(id) => {
			regexps.assert_defined(&id, span)?;
			LocRegExpAtom::Ref(id)
		},
		syntax::RegExpAtom::CharSet(set, false) => LocRegExpAtom::CharSet(set),
		syntax::RegExpAtom::CharSet(set, true) => LocRegExpAtom::CharSet(set.negated()),
		syntax::RegExpAtom::Literal(str, case_sensitive) => LocRegExpAtom::Literal(str, case_sensitive),
		syntax::RegExpAtom::Repeat(atom, min, max) => {
			LocRegExpAtom::Repeat(Box::new(compile_regexp_atom(regexps, *atom)?), min, max)
		},
		syntax::RegExpAtom::Or(exps) => {
			let mut compiled_exps = Vec::new();
			for e in exps.into_iter() {
				compiled_exps.push(compile_loc_regexp(regexps, e)?);
			}
			LocRegExpAtom::Or(compiled_exps)
		},
		// syntax::RegExpAtom::Capture(exp) => {
		// 	LocRegExpAtom::Capture(compile_loc_regexp(regexps, exp)?)
		// },
		syntax::RegExpAtom::Group(exp) => {
			LocRegExpAtom::Group(compile_loc_regexp(regexps, exp)?)
		},
		// syntax::RegExpAtom::Cast(exp, ty) => {
		// 	LocRegExpAtom::Cast(compile_loc_regexp(regexps, exp)?, ty)
		// }
	};

	Ok(Loc::new(exp, span))
}

fn compile_loc_terminal(regexps: &RegExps, terminals: &mut Terminals, ast: Loc<syntax::Terminal>) -> Result<Loc<u32>, Loc<Error>> {
	let (ast, span) = ast.into_raw_parts();

	let t = match ast {
		syntax::Terminal::RegExp(exp) => {
			LocTerminal::RegExp(compile_regexp(regexps, exp)?)
		}
	};

	let t = Loc::new(t, span);
	let id = terminals.id(t);
	Ok(Loc::new(id, span))
}

fn compile_item(regexps: &RegExps, types: &Types, terminals: &mut Terminals, ast: Loc<syntax::Item>) -> Result<Loc<Item>, Loc<Error>> {
	let span = ast.span();
	let t = match ast.into_inner() {
		syntax::Item::Terminal(t) => {
			Item::Terminal(compile_loc_terminal(regexps, terminals, Loc::new(t, span))?.into_inner())
		},
		syntax::Item::NonTerminal(nt) => {
			let nt = match nt {
				syntax::NonTerminal::Type(id) => {
					types.assert_defined(&id)?;
					NonTerminal::Type(id)
				},
				syntax::NonTerminal::Repeat(id, min, max, sep) => {
					types.assert_defined(&id)?;
					let sep = match sep {
						Some(s) => {
							let span = s.span();
							let s = s.into_inner();
							Some(Loc::new(Separator {
								strong: s.strong,
								terminal: compile_loc_terminal(regexps, terminals, s.terminal)?
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

fn compile_rule(regexps: &RegExps, types: &Types, terminals: &mut Terminals, ast: Loc<syntax::Rule>) -> Result<Loc<Rule>, Loc<Error>> {
	let span = ast.span();
	let ast = ast.into_inner();
	let mut items = Vec::new();
	for item in ast.items.into_iter() {
		items.push(compile_item(regexps, types, terminals, item)?);
	}
	Ok(Loc::new(Rule {
		id: ast.id,
		items
	}, span))
}

fn check_terminal(regexps: &RegExps, terminal: &Loc<LocTerminal>) -> Result<(), Loc<Error>> {
	match terminal.as_ref() {
		LocTerminal::RegExp(exp) => check_regexp(regexps, exp, true)?
	}

	Ok(())
}

fn check_regexp(regexps: &RegExps, exp: &LocRegExp, mut topmost: bool) -> Result<(), Loc<Error>> {
	topmost &= exp.0.len() == 1;
	for atom in &exp.0 {
		check_regexp_atom(regexps, atom, topmost)?
	}

	Ok(())
}

fn check_regexp_atom(regexps: &RegExps, atom: &Loc<LocRegExpAtom>, topmost: bool) -> Result<(), Loc<Error>> {
	match atom.as_ref() {
		LocRegExpAtom::Ref(id) => {
			let target = regexps.get(id, atom.span())?.unwrap();

			if !topmost {
				if let Some(ty) = &target.ty {
					return Err(Loc::new(Error::RegExpTypeMissmatch(ty.clone()), atom.span()))
				}
			}
			
			check_regexp(regexps, target.exp.as_ref(), false)?;
		},
		LocRegExpAtom::CharSet(_) => (),
		LocRegExpAtom::Literal(_, _) => (),
		LocRegExpAtom::Repeat(atom, _, _) => check_regexp_atom(regexps, atom, false)?,
		LocRegExpAtom::Or(list) => {
			for exp in list {
				check_regexp(regexps, exp.as_ref(), false)?
			}
		},
		LocRegExpAtom::Group(exp) => check_regexp(regexps, exp.as_ref(), false)?
	}

	Ok(())
}

impl syntax::Grammar {
	pub fn compile(self: Loc<Self>) -> Result<Loc<Grammar>, Loc<Error>> {
		let span = self.span();
		let ast = self.into_inner();

		let externs = ast.externs;
		let mut regexps = RegExps::new(&ast.regexps);
		let mut types = Types::new(&ast.types);
		let mut terminals = Terminals::new();

		for ast in ast.regexps.into_iter() {
			let id = ast.id.clone();
			let span = ast.span();
			regexps.assert_defined(id.as_ref(), id.span())?;
			let ast = ast.into_inner();
			let def = RegExpDefinition {
				id: ast.id,
				ty: ast.ty,
				exp: compile_loc_regexp(&regexps, ast.exp)?
			};
			regexps.define(&id, id.span(), Loc::new(def, span))?
		}

		for ast in ast.types.into_iter() {
			let id = ast.id.clone();
			let span = ast.span();
			types.assert_defined(&id)?;
			let ast = ast.into_inner();

			let mut rules = Vec::new();
			for rule in ast.rules {
				rules.push(compile_rule(&regexps, &types, &mut terminals, rule)?);
			}

			let def = Type {
				id: ast.id,
				rules: rules
			};

			types.define(&id, Loc::new(def, span))?
		}

		for (_terminal, uses) in &terminals.list {
			let terminal = uses.iter().next().unwrap();
			check_terminal(&regexps, terminal)?
		}

		let g = Grammar::from_raw_parts(
			externs,
			regexps.into_map(),
			types.into_map(),
			terminals.into_vec()
		);

		Ok(Loc::new(g, span))
	}
}
