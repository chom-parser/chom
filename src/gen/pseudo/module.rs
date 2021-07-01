use super::{ty, Routine};
use std::collections::HashSet;

/// Module.
pub struct Module {
	/// Index of the module in `Context::modules`.
	index: u32,

	/// Parent module.
	parent: Option<u32>,

	/// Module id.
	id: Id,

	/// Module roles.
	roles: HashSet<Role>,

	/// Types defined in the module.
	types: Vec<ty::Ref>,

	/// Sub-modules.
	modules: Vec<u32>,

	/// Defined routines.
	routines: Vec<Routine>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Role {
	Extern,
	Ast,
	Lexer,
	ParserRoot,
	ParserSubmodule,
}

impl Module {
	pub fn root() -> Self {
		Self {
			index: 0,
			parent: None,
			id: Id::Root,
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new(),
			routines: Vec::new(),
		}
	}

	pub fn new(index: u32, parent: u32, name: String) -> Self {
		Self {
			index,
			parent: Some(parent),
			id: Id::Named(name),
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new(),
			routines: Vec::new(),
		}
	}

	pub fn index(&self) -> u32 {
		self.index
	}

	pub fn parent(&self) -> Option<u32> {
		self.parent
	}

	/// Module id.
	pub fn id(&self) -> &Id {
		&self.id
	}

	pub fn roles(&self) -> impl '_ + Iterator<Item = Role> {
		self.roles.iter().cloned()
	}

	pub fn types(&self) -> impl '_ + Iterator<Item = ty::Ref> {
		self.types.iter().cloned()
	}

	pub fn modules(&self) -> impl '_ + Iterator<Item = u32> {
		self.modules.iter().cloned()
	}

	pub fn routines(&self) -> impl '_ + Iterator<Item = &'_ Routine> {
		self.routines.iter()
	}

	pub fn add_role(&mut self, role: Role) {
		self.roles.insert(role);
	}

	pub fn add_type(&mut self, ty: ty::Ref) {
		self.types.push(ty)
	}

	pub fn add_module(&mut self, m: u32) {
		self.modules.push(m)
	}

	pub fn add_routine(&mut self, routine: Routine) {
		self.routines.push(routine)
	}
}

pub enum Id {
	/// Root module.
	///
	/// In Rust this is translated into `crate`.
	Root,

	/// Named module.
	Named(String),
}

pub struct Path<'a> {
	parent: Option<Box<Path<'a>>>,
	id: Option<&'a Id>,
}

impl<'a> Path<'a> {
	pub(crate) fn new(parent: Option<Path<'a>>, id: &'a Id) -> Self {
		Self {
			parent: parent.map(Box::new),
			id: Some(id),
		}
	}
}

impl<'a> Iterator for Path<'a> {
	type Item = &'a Id;

	fn next(&mut self) -> Option<Self::Item> {
		match self.parent.as_mut().map(|p| p.next()).flatten() {
			Some(id) => Some(id),
			None => self.id.take(),
		}
	}
}
