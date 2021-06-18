use std::fmt;
use crate::poly;
use super::Grammar;

/// Type instance identifier.
pub type Instance = u32;

pub use poly::ty::Id;

/// Monomorphic type.
pub struct Type<'a> {
	poly: &'a poly::Type,
	instance: Instance,
	parameters_instances: Vec<Expr>
}

impl<'a> Type<'a> {
	pub(crate) fn new(poly: &'a poly::Type, instance: Instance, params: Vec<Expr>) -> Self {
		Self {
			poly,
			instance,
			parameters_instances: params
		}
	}

	pub fn poly(&self) -> &'a poly::Type {
		self.poly
	}

	pub fn id(&self) -> &Id {
		self.poly.id()
	}

	pub fn parameter(&self, i: u32) -> Option<&Expr> {
		self.parameters_instances.get(i as usize)
	}

	pub fn constructors(&self) -> impl '_ + Iterator<Item=(u32, Instance)> {
		self.poly.constructors().iter().map(move |i| (*i, self.instance))
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		let constructor = grammar.function(self.constructors().next().unwrap()).unwrap();
		constructor.instance(grammar)
	}

	pub fn format<'g>(&self, grammar: &'g Grammar<'a>) -> FormattedType<'a, 'g, '_> {
		FormattedType(grammar, self)
	}
}

pub struct FormattedType<'a, 'g, 'e>(&'g Grammar<'a>, &'e Type<'a>);

impl<'a, 'g, 'e> fmt::Display for FormattedType<'a, 'g, 'e> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.1.poly.name().fmt(f)?;
		
		for e in &self.1.parameters_instances {
			write!(f, " {}", e.format(self.0))?;
		}
		
		Ok(())
	}
}

/// Expression.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expr {
	Terminal(u32),
	Type((u32, Instance))
}

impl Expr {
	pub fn format<'a, 'g>(&self, grammar: &'g Grammar<'a>) -> FormattedExpr<'a, 'g, '_> {
		FormattedExpr(grammar, self)
	}

	pub fn instance(&self, grammar: &Grammar) -> String {
		match self {
			Self::Terminal(i) => grammar.terminal(*i).unwrap().instance(grammar.poly()),
			Self::Type(i) => grammar.ty(*i).unwrap().instance(grammar)
		}
	}
}

pub struct FormattedExpr<'a, 'g, 'e>(&'g Grammar<'a>, &'e Expr);

impl<'a, 'g, 'e> fmt::Display for FormattedExpr<'a, 'g, 'e> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.1 {
			Expr::Terminal(t) => {
				let t = self.0.terminal(*t).unwrap();
				t.format(self.0.poly()).fmt(f)
			},
			Expr::Type(nt) => {
				let ty = self.0.ty(*nt).unwrap();
				ty.format(self.0).fmt(f)
			}
		}
	}
}

pub struct Map<T> {
	data: Vec<Vec<T>>
}

impl<T> Map<T> {
	pub fn new<'a, F>(grammar: &Grammar<'a>, f: F) -> Self where F: Fn(((u32, Instance), &Type<'a>)) -> T {
		Self {
			data: grammar.poly().types().iter().enumerate().map(|(ty, _)| {
				grammar.mono_types(ty as u32).unwrap().iter().enumerate().map(|(i, key)| {
					f(((ty as u32, i as Instance), key))
				}).collect()
			}).collect()
		}
	}

	pub fn get(&self, (ty, instance): (u32, Instance)) -> Option<&T> {
		self.data.get(ty as usize).map(|inner| inner.get(instance as usize)).flatten()
	}

	pub fn get_mut(&mut self, (ty, instance): (u32, Instance)) -> Option<&mut T> {
		self.data.get_mut(ty as usize).map(|inner| inner.get_mut(instance as usize)).flatten()
	}

	pub fn enumerate(&self) -> impl '_ + Iterator<Item=((u32, Instance), &T)> {
		self.data.iter().enumerate().map(|(ty, inner)| inner.iter().enumerate().map(move |(i, t)| ((ty as u32, i as Instance), t))).flatten()
	}

	pub fn map<F, U>(&self, f: F) -> Map<U> where F: Fn(((u32, Instance), &T)) -> U {
		Map {
			data: self.data.iter().enumerate().map(|(ty, inner)| {
				inner.iter().enumerate().map(|(i, key)| {
					f(((ty as u32, i as Instance), key))
				}).collect()
			}).collect()
		}
	}
}