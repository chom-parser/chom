use std::collections::{
	HashSet,
	BTreeMap
};
use source_span::Loc;
use crate::{
	syntax,
	poly::{
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
	list: Vec<(Terminal, HashSet<Loc<syntax::RegExp>>)>,

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

	pub fn id(&mut self, terminal_desc: terminal::Desc, ast: Loc<syntax::RegExp>) -> u32 {
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

	pub fn into_vec(self) -> Vec<(Terminal, HashSet<Loc<syntax::RegExp>>)> {
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

fn check_terminal(regexps: &RegExps, terminal: &Loc<syntax::RegExp>) -> Result<(), Loc<Error>> {
	check_regexp(regexps, terminal.as_ref(), true)
}

fn check_regexp(regexps: &RegExps, exp: &syntax::RegExp, mut topmost: bool) -> Result<(), Loc<Error>> {
	topmost &= exp.0.len() == 1;
	for atom in &exp.0 {
		check_regexp_atom(regexps, atom, topmost)?
	}

	Ok(())
}

fn check_regexp_atom(regexps: &RegExps, atom: &Loc<syntax::regexp::Atom>, topmost: bool) -> Result<(), Loc<Error>> {
	match atom.as_ref() {
		syntax::regexp::Atom::Ref(id) => {
			let target = regexps.get(id, atom.span())?;
			let ast = regexps.ast(target);

			if !topmost {
				if let Some(target_ty) = ast.ty.as_ref() {
					return Err(Loc::new(Error::RegExpTypeMissmatch(target_ty.clone()), atom.span()))
				}
			}
			
			check_regexp(regexps, ast.exp.as_ref(), false)?;
		},
		syntax::regexp::Atom::CharSet(_) => (),
		syntax::regexp::Atom::Literal(_, _) => (),
		syntax::regexp::Atom::Repeat(atom, _, _) => check_regexp_atom(regexps, atom, false)?,
		syntax::regexp::Atom::Or(list) => {
			for exp in list {
				check_regexp(regexps, exp.as_ref(), false)?
			}
		},
		syntax::regexp::Atom::Group(exp) => check_regexp(regexps, exp.as_ref(), false)?
	}

	Ok(())
}