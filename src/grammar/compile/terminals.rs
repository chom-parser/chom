use std::collections::{
	HashSet,
	BTreeMap
};
use source_span::Loc;
use crate::{
	syntax,
	grammar::{
		terminal,
		Terminal
	}
};
use super::{
	Error,
	RegExps
};

pub struct Terminals {
	/// Every terminals.
	list: Vec<(Terminal, HashSet<Loc<syntax::Terminal>>)>,

	/// Associate each terminal to its identifier.
	table: BTreeMap<terminal::Desc, u32>
}

impl Terminals {
	pub fn new() -> Self {
		Self {
			list: Vec::new(),
			table: BTreeMap::new()
		}
	}

	pub fn id(&mut self, terminal_desc: terminal::Desc, ast: Loc<syntax::Terminal>) -> u32 {
		match self.table.get(&terminal_desc).cloned() {
			Some(id) => {
				self.list[id as usize].1.insert(ast);
				id
			},
			None => {
				let id = self.list.len() as u32;
				let mut locs = HashSet::new();
				locs.insert(ast);
				self.list.push((Terminal::new(terminal_desc.clone()), locs));
				self.table.insert(terminal_desc, id);
				id
			}
		}
	}

	pub fn into_vec(self) -> Vec<(Terminal, HashSet<Loc<syntax::Terminal>>)> {
		self.list
	}

	pub fn check(&self, regexps: &RegExps) -> Result<(), Loc<Error>> {
		for (_terminal, uses) in &self.list {
			let terminal = uses.iter().next().unwrap();
			check_terminal(&regexps, terminal)?
		}

		Ok(())
	}
}

fn check_terminal(regexps: &RegExps, terminal: &Loc<syntax::Terminal>) -> Result<(), Loc<Error>> {
	match terminal.as_ref() {
		syntax::Terminal::RegExp(exp) => check_regexp(regexps, exp, true)?
	}

	Ok(())
}

fn check_regexp(regexps: &RegExps, exp: &syntax::RegExp, mut topmost: bool) -> Result<(), Loc<Error>> {
	topmost &= exp.0.len() == 1;
	for atom in &exp.0 {
		check_regexp_atom(regexps, atom, topmost)?
	}

	Ok(())
}

fn check_regexp_atom(regexps: &RegExps, atom: &Loc<syntax::RegExpAtom>, topmost: bool) -> Result<(), Loc<Error>> {
	match atom.as_ref() {
		syntax::RegExpAtom::Ref(id) => {
			let target = regexps.get(id, atom.span())?;
			let ast = regexps.ast(target);

			if !topmost {
				if let Some(target_ty) = ast.ty.as_ref() {
					return Err(Loc::new(Error::RegExpTypeMissmatch(target_ty.clone()), atom.span()))
				}
			}
			
			check_regexp(regexps, ast.exp.as_ref(), false)?;
		},
		syntax::RegExpAtom::CharSet(_) => (),
		syntax::RegExpAtom::Literal(_, _) => (),
		syntax::RegExpAtom::Repeat(atom, _, _) => check_regexp_atom(regexps, atom, false)?,
		syntax::RegExpAtom::Or(list) => {
			for exp in list {
				check_regexp(regexps, exp.as_ref(), false)?
			}
		},
		syntax::RegExpAtom::Group(exp) => check_regexp(regexps, exp.as_ref(), false)?
	}

	Ok(())
}