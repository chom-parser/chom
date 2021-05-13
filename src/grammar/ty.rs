use crate::syntax::Ident;

pub struct Type {
	id: Ident,
	rules: Vec<u32>
}

impl Type {
	pub fn new(ident: Ident) -> Self {
		Self {
			id: ident,
			rules: Vec::new()
		}
	}

	pub fn id(&self) -> &Ident {
		&self.id
	}

	pub fn add_rule(&mut self, rule: u32) {
		self.rules.push(rule)
	}
}