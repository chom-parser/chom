use std::collections::HashSet;
use source_span::Loc;
use crate::syntax::{
	self,
	Ident,
	Caused
};
use super::{
	terminal,
	Terminal,
	ty,
	function,
	Function,
	Grammar,
	regexp,
	RegExp,
	ExternalType
};

mod error;
mod externals;
mod regexps;
mod terminals;
mod types;
// mod non_terminals;
mod functions;

pub use error::Error;
use externals::*;
use regexps::*;
use terminals::*;
use types::*;
// use non_terminals::*;
use functions::*;

fn compile_loc_regexp(regexps: &RegExps, ast: Loc<syntax::RegExp>) -> Result<RegExp, Loc<Error>> {
	let ast = ast.into_inner();
	Ok(compile_regexp(regexps, ast)?)
}

fn compile_regexp(regexps: &RegExps, ast: syntax::RegExp) -> Result<RegExp, Loc<Error>> {
	let mut atoms = Vec::new();
	for ast in ast.0.into_iter() {
		atoms.push(compile_regexp_atom(regexps, ast)?);
	}

	Ok(RegExp::new(atoms))
}

fn compile_regexp_atom(regexps: &RegExps, ast: Loc<syntax::regexp::Atom>) -> Result<regexp::Atom, Loc<Error>> {
	let (ast, span) = ast.into_raw_parts();
	let exp = match ast {
		syntax::regexp::Atom::Ref(id) => {
			let index = regexps.get(&id, span)?;
			regexp::Atom::Ref(index)
		},
		syntax::regexp::Atom::CharSet(set) => regexp::Atom::CharSet(set),
		syntax::regexp::Atom::Literal(str, case_sensitive) => regexp::Atom::Literal(str, case_sensitive),
		syntax::regexp::Atom::Repeat(atom, min, max) => {
			regexp::Atom::Repeat(Box::new(compile_regexp_atom(regexps, *atom)?), min, max)
		},
		syntax::regexp::Atom::Or(exps) => {
			let mut compiled_exps = Vec::new();
			for e in exps.into_iter() {
				compiled_exps.push(compile_loc_regexp(regexps, e)?);
			}
			regexp::Atom::Or(compiled_exps)
		}
		syntax::regexp::Atom::Group(exp) => {
			regexp::Atom::Group(compile_loc_regexp(regexps, exp)?)
		}
	};

	Ok(exp)
}

fn compile_loc_terminal(regexps: &RegExps, terminals: &mut Terminals, ast: Loc<syntax::RegExp>) -> Result<u32, Loc<Error>> {
	let t_ast = ast.clone().into_inner();
	let desc = terminal::Desc::RegExp(compile_regexp(regexps, t_ast)?);
	Ok(terminals.id(desc, ast))
}

fn compile_ty_expr(regexps: &RegExps, types: &mut Types, terminals: &mut Terminals, ast: Loc<syntax::ty::Expr>) -> Result<ty::Expr, Loc<Error>> {
	let span = ast.span();
	match ast.into_inner() {
		syntax::ty::Expr::Terminal(t) => {
			Ok(ty::Expr::Terminal(compile_loc_terminal(regexps, terminals, Loc::new(t, span))?))
		},
		syntax::ty::Expr::NonTerminal(id, args) => {
			let index = types.get_by_id(&id)?;

			let mut compiled_args = Vec::with_capacity(args.len());
			for a in args {
				compiled_args.push(compile_ty_expr(regexps, types, terminals, a)?)
			}

			Ok(ty::Expr::Type(index, compiled_args))
		}
	}
}

fn compile_function_id(id: Option<Loc<Ident>>) -> Result<function::Id, Loc<Error>> {
	match id {
		Some(id) => Ok(function::Id::Defined(id.into_inner())),
		None => Ok(function::Id::Cast)
	}
}

fn compile_rule(ty: u32, regexps: &RegExps, types: &mut Types, terminals: &mut Terminals, functions: &mut Functions, ast: Loc<syntax::Function>) -> Result<u32, Loc<Error>> {
	let span = ast.span();
	let ast = ast.into_inner();
	let mut args = Vec::new();
	for a in ast.args.into_iter() {
		args.push(compile_ty_expr(regexps, types, terminals, a)?);
	}

	let fun = Function::new(
		compile_function_id(ast.id)?,
		ty,
		args
	);

	let i = functions.len();
	functions.push(Caused::explicit(fun, span));

	Ok(i as u32)
}

impl syntax::Grammar {
	pub fn compile(self: Loc<Self>) -> Result<Loc<Grammar>, Loc<Error>> {
		let span = self.span();
		let ast = self.into_inner();

		// let externs = ast.externs.into_iter().map(|ty| ExternalType::from_ident(&ty.into_inner())).collect();
		let external_types = ExternalTypes::new(ast.externs)?;
		let mut regexps = RegExps::new(&external_types, &ast.regexps)?;
		let mut types = Types::new(&ast.types)?;
		let mut terminals = Terminals::new();
		let mut functions = Functions::new();

		for ast in ast.regexps.into_iter() {
			let id = ast.id.clone();
			regexps.assert_defined(id.as_ref(), id.span())?;
			let def_ast = ast.clone().into_inner();

			let def = regexp::Definition {
				id: def_ast.id.into_inner(),
				ty: external_types.get(def_ast.ty.as_ref())?,
				exp: compile_loc_regexp(&regexps, def_ast.exp)?
			};

			regexps.define(&id, def)
		}

		for ast in ast.types.into_iter() {
			let id = ast.id.clone();
			let i = types.get_by_id(&id).unwrap();

			let ast = ast.into_inner();

			for f in ast.constructors {
				let compiled_f = compile_rule(i, &regexps, &mut types, &mut terminals, &mut functions, f)?;
				types.add_constructor(i, compiled_f)
			}
		}

		// Check terminals well-formedness.
		terminals.check(&regexps)?;

		// Add special terminals.
		let mut terminals = terminals.into_vec();
		terminals.push((Terminal::whitespace(), HashSet::new()));

		let g = Grammar::from_raw_parts(
			external_types.into_vec(),
			regexps.into_vec(),
			terminals,
			types.into_vec(),
			functions
		);

		Ok(Loc::new(g, span))
	}
}
