use crate::{
	lexing::regexp,
	poly,
	syntax::{self, Caused},
};
use source_span::{Loc, Span};
use std::collections::{HashMap, HashSet};

pub mod function;
pub mod ty;

pub use function::Function;
pub use ty::Type;

pub type Index = (u32, ty::Instance);

pub use poly::{terminal, ExternalType, Terminal};

pub struct Grammar<'a> {
	poly: &'a poly::Grammar,

	/// Types.
	types: Vec<Vec<Type<'a>>>,

	/// Functions.
	functions: Vec<Vec<Function<'a>>>,
}

impl<'a> Grammar<'a> {
	/// Tries to monomorphize the input polymorphic grammar and returns a monomorphic grammar.
	pub fn new(poly: &'a poly::Grammar) -> Self {
		struct Mono<'a> {
			map: HashMap<Vec<ty::Expr>, ty::Instance>,
			types: Vec<Type<'a>>,
		}

		let mut mono = Vec::new();
		mono.resize_with(poly.types().len(), || Mono {
			map: HashMap::new(),
			types: Vec::new(),
		});

		fn monomorphize_ty_expr<'a>(
			poly: &'a poly::Grammar,
			mono: &mut [Mono<'a>],
			context: Option<(u32, ty::Instance)>,
			expr: &poly::ty::Expr,
		) -> ty::Expr {
			match expr {
				poly::ty::Expr::Var(x) => {
					let (ty, i) = context.unwrap();
					let context = &mono[ty as usize].types[i as usize];
					context.parameter(*x).unwrap().clone()
				}
				poly::ty::Expr::Type(ty, params) => {
					let mut mono_params = Vec::with_capacity(params.len());
					for p in params {
						mono_params.push(monomorphize_ty_expr(poly, mono, context, p))
					}

					use std::collections::hash_map::Entry;
					let poly_ty = poly.ty(*ty).unwrap();
					let mono_tys = &mut mono[*ty as usize];
					let (i, inserted) = match mono_tys.map.entry(mono_params) {
						Entry::Vacant(entry) => {
							let i = mono_tys.types.len() as ty::Instance;
							mono_tys
								.types
								.push(Type::new(poly_ty, i, entry.key().clone()));
							entry.insert(i);
							(i, true)
						}
						Entry::Occupied(entry) => (*entry.get(), false),
					};

					if inserted {
						for f_index in poly_ty.constructors() {
							let f = poly.function(*f_index).unwrap();
							for arg in f.arguments() {
								monomorphize_ty_expr(poly, mono, Some((*ty, i)), arg.expr());
							}
						}
					}

					ty::Expr::Type((*ty, i))
				}
				poly::ty::Expr::Terminal(t) => ty::Expr::Terminal(*t),
			}
		}

		for (i, ty) in poly.types().iter().enumerate() {
			if ty.parameters().is_empty() {
				monomorphize_ty_expr(
					poly,
					&mut mono,
					None,
					&poly::ty::Expr::Type(i as u32, Vec::new()),
				);
			}
		}

		fn monomorphized_ty_expr<'a>(
			context: &Type,
			mono: &[Mono<'a>],
			expr: &poly::ty::Expr,
		) -> ty::Expr {
			match expr {
				poly::ty::Expr::Var(x) => context.parameter(*x).unwrap().clone(),
				poly::ty::Expr::Type(t, args) => {
					let mono_args: Vec<_> = args
						.iter()
						.map(|a| monomorphized_ty_expr(context, mono, a))
						.collect();
					let i = mono[*t as usize].map.get(&mono_args).unwrap();
					ty::Expr::Type((*t, *i))
				}
				poly::ty::Expr::Terminal(t) => ty::Expr::Terminal(*t),
			}
		}

		let functions: Vec<Vec<_>> = poly
			.functions()
			.iter()
			.map(|f| {
				let ty = f.return_ty();
				mono[ty as usize]
					.types
					.iter()
					.enumerate()
					.map(|(i, mono_ty)| {
						let mono_args = f
							.arguments()
							.iter()
							.map(|a| monomorphized_ty_expr(mono_ty, &mono, a.expr()))
							.collect();
						Function::new(f, i as ty::Instance, mono_args)
					})
					.collect()
			})
			.collect();

		let types: Vec<Vec<_>> = mono.into_iter().map(|mono_tys| mono_tys.types).collect();

		Self {
			poly,
			types,
			functions,
		}
	}

	pub fn poly(&self) -> &'a poly::Grammar {
		self.poly
	}

	pub fn extern_types(&self) -> &[(ExternalType, Option<Span>)] {
		self.poly().extern_types()
	}

	pub fn extern_type(&self, index: u32) -> Option<&ExternalType> {
		self.poly().extern_type(index)
	}

	pub fn regexps(&self) -> &[(regexp::Definition, Option<Loc<syntax::regexp::Definition>>)] {
		self.poly().regexps()
	}

	pub fn regexp(&self, index: u32) -> Option<&regexp::Definition> {
		self.poly().regexp(index)
	}

	pub fn terminals(&self) -> &[(Terminal, HashSet<Loc<syntax::RegExp>>)] {
		self.poly().terminals()
	}

	pub fn terminal(&self, index: u32) -> Option<&Terminal> {
		self.poly.terminal(index)
	}

	pub fn ty(&self, (index, instance): (u32, ty::Instance)) -> Option<Caused<&Type<'a>>> {
		self.poly
			.ty(index)
			.map(|ty| {
				let source = *ty.source();
				self.types[index as usize]
					.get(instance as usize)
					.map(|i| Caused::new(i, source))
			})
			.flatten()
	}

	pub fn types(&self) -> impl '_ + Iterator<Item = Caused<&Type<'a>>> {
		self.types
			.iter()
			.enumerate()
			.map(move |(index, mono)| {
				let source = *self.poly.ty(index as u32).unwrap().source();
				mono.iter().map(move |f| Caused::new(f, source))
			})
			.flatten()
	}

	pub fn mono_types(&self, i: u32) -> Option<&[Type<'a>]> {
		self.types.get(i as usize).map(|ts| ts.as_ref())
	}

	pub fn enumerate_types(
		&self,
	) -> impl '_ + Iterator<Item = ((u32, ty::Instance), Caused<&Type<'a>>)> {
		self.types
			.iter()
			.enumerate()
			.map(move |(index, mono)| {
				let source = *self.poly.ty(index as u32).unwrap().source();
				mono.iter()
					.enumerate()
					.map(move |(i, f)| ((index as u32, i as ty::Instance), Caused::new(f, source)))
			})
			.flatten()
	}

	pub fn function(
		&self,
		(index, instance): (u32, ty::Instance),
	) -> Option<Caused<&Function<'a>>> {
		self.poly
			.function(index)
			.map(|f| {
				let source = *f.source();
				self.functions[index as usize]
					.get(instance as usize)
					.map(|i| Caused::new(i, source))
			})
			.flatten()
	}

	pub fn mono_functions(&self, i: u32) -> Option<&[Function<'a>]> {
		self.functions.get(i as usize).map(|fs| fs.as_ref())
	}

	pub fn functions(&self) -> impl '_ + Iterator<Item = Caused<&Function<'a>>> {
		self.functions
			.iter()
			.enumerate()
			.map(move |(index, mono)| {
				let source = *self.poly.function(index as u32).unwrap().source();
				mono.iter().map(move |f| Caused::new(f, source))
			})
			.flatten()
	}

	pub fn enumerate_functions(
		&self,
	) -> impl '_ + Iterator<Item = ((u32, ty::Instance), Caused<&Function<'a>>)> {
		self.functions
			.iter()
			.enumerate()
			.map(move |(index, mono)| {
				let source = *self.poly.function(index as u32).unwrap().source();
				mono.iter()
					.enumerate()
					.map(move |(i, f)| ((index as u32, i as ty::Instance), Caused::new(f, source)))
			})
			.flatten()
	}
}
