use std::collections::HashSet;
use source_span::Loc;
use crate::syntax;
use super::{
	terminal,
	Terminal,
	non_terminal,
	NonTerminal,
	rule,
	Rule,
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
mod non_terminals;
mod rules;

pub use error::Error;
use externals::*;
use regexps::*;
use terminals::*;
use types::*;
use non_terminals::*;
use rules::*;

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

fn compile_regexp_atom(regexps: &RegExps, ast: Loc<syntax::RegExpAtom>) -> Result<regexp::Atom, Loc<Error>> {
	let (ast, span) = ast.into_raw_parts();
	let exp = match ast {
		syntax::RegExpAtom::Ref(id) => {
			let index = regexps.get(&id, span)?;
			regexp::Atom::Ref(index)
		},
		syntax::RegExpAtom::CharSet(set) => regexp::Atom::CharSet(set),
		syntax::RegExpAtom::Literal(str, case_sensitive) => regexp::Atom::Literal(str, case_sensitive),
		syntax::RegExpAtom::Repeat(atom, min, max) => {
			regexp::Atom::Repeat(Box::new(compile_regexp_atom(regexps, *atom)?), min, max)
		},
		syntax::RegExpAtom::Or(exps) => {
			let mut compiled_exps = Vec::new();
			for e in exps.into_iter() {
				compiled_exps.push(compile_loc_regexp(regexps, e)?);
			}
			regexp::Atom::Or(compiled_exps)
		}
		syntax::RegExpAtom::Group(exp) => {
			regexp::Atom::Group(compile_loc_regexp(regexps, exp)?)
		}
	};

	Ok(exp)
}

fn compile_loc_terminal(regexps: &RegExps, terminals: &mut Terminals, ast: Loc<syntax::Terminal>) -> Result<u32, Loc<Error>> {
	let t_ast = ast.clone().into_inner();

	let desc = match t_ast {
		syntax::Terminal::RegExp(exp) => {
			terminal::Desc::RegExp(compile_regexp(regexps, exp)?)
		}
	};

	Ok(terminals.id(desc, ast))
}

fn compile_item(regexps: &RegExps, types: &Types, terminals: &mut Terminals, non_terminals: &mut NonTerminals, ast: Loc<syntax::Item>) -> Result<Loc<rule::Item>, Loc<Error>> {
	let span = ast.span();
	let t = match ast.into_inner() {
		syntax::Item::Terminal(t) => {
			rule::Item::Terminal(compile_loc_terminal(regexps, terminals, Loc::new(t, span))?)
		},
		syntax::Item::NonTerminal(nt) => {
			let nt = match nt {
				syntax::NonTerminal::Type(id) => {
					NonTerminal::Type(types.get(&id)?)
				},
				syntax::NonTerminal::Repeat(id, min, max, sep) => {
					let index = types.get(&id)?;
					let sep = match sep {
						Some(s) => {
							let s = s.into_inner();
							Some(non_terminal::Separator {
								strong: s.strong,
								terminal: compile_loc_terminal(regexps, terminals, s.terminal)?
							})
						},
						None => None
					};
					NonTerminal::Repeat(index, min, max, sep)
				}
			};
			rule::Item::NonTerminal(non_terminals.id(nt, span))
		}
	};

	Ok(Loc::new(t, span))
}

fn compile_rule(ty: u32, regexps: &RegExps, types: &Types, terminals: &mut Terminals, non_terminals: &mut NonTerminals, rules: &mut Rules, ast: Loc<syntax::Rule>) -> Result<u32, Loc<Error>> {
	let span = ast.span();
	let ast = ast.into_inner();
	let mut items = Vec::new();
	for item in ast.items.into_iter() {
		items.push(compile_item(regexps, types, terminals, non_terminals, item)?);
	}

	let rule = Rule {
		id: ast.id.clone(),
		ty,
		items
	};

	let i = rules.len();
	rules.push(Loc::new(rule, span));

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
		let mut non_terminals = NonTerminals::new();
		let mut rules = Rules::new();

		for ast in ast.regexps.into_iter() {
			let id = ast.id.clone();
			regexps.assert_defined(id.as_ref(), id.span())?;
			let def_ast = ast.clone().into_inner();
			let def = regexp::Definition {
				id: def_ast.id.into_inner(),
				ty: def_ast.ty.map(|ty| ExternalType::from_ident(ty.as_ref())).unwrap_or_default(),
				exp: compile_loc_regexp(&regexps, def_ast.exp)?
			};

			regexps.define(&id, def)
		}

		for ast in ast.types.into_iter() {
			let id = ast.id.clone();
			let i = types.get(&id).unwrap();

			let ast = ast.into_inner();

			for rule in ast.rules {
				types.add_rule(i, compile_rule(i, &regexps, &types, &mut terminals, &mut non_terminals, &mut rules, rule)?)
			}
		}

		terminals.check(&regexps)?;

		let mut terminals = terminals.into_vec();
		terminals.push((Terminal::whitespace(), HashSet::new()));

		let g = Grammar::from_raw_parts(
			external_types.into_vec(),
			regexps.into_vec(),
			terminals,
			types.into_vec(),
			non_terminals.into_vec(),
			rules
		);

		Ok(Loc::new(g, span))
	}
}
