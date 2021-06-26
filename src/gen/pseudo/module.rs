use std::collections::HashSet;
use super::ty;

/// Module.
pub struct Module {
	/// Parent module.
	parent: Option<u32>,

	/// Module id.
	id: Id,

	/// Module roles.
	roles: HashSet<Role>,

	/// Types defined in the module.
	types: Vec<ty::Ref>,

	/// Sub-modules.
	modules: Vec<u32>
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Role {
	Extern,
	Ast,
	Lexer,
	ParserRoot,
	ParserSubmodule
}

impl Module {
	pub fn root() -> Self {
		Self {
			parent: None,
			id: Id::Root,
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new()
		}
	}

	pub fn new(parent: u32, name: String) -> Self {
		Self {
			parent: Some(parent),
			id: Id::Named(name),
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new()
		}
	}

	pub fn add_type(&mut self, ty: ty::Ref) {
		self.types.push(ty)
	}

	pub fn add_module(&mut self, m: u32) {
		self.modules.push(m)
	}
}

pub enum Id {
	/// Root module.
	/// 
	/// In Rust this is translated into `crate`.
	Root,

	/// Named module.
	Named(String)
}