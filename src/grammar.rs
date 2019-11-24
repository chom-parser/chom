use std::collections::HashMap;
use std::rc::Rc;
use std::convert::TryFrom;
use std::mem::MaybeUninit;
use source_span::Span;
use crate::{CharSet, Ident};
use crate::syntax;
use syntax::Located;

pub struct Grammar {
    pub externs: Vec<Located<Ident>>,
    pub regexps: HashMap<Ident, MaybeUninit<Rc<Located<RegExpDefinition>>>>,
    pub types: HashMap<Ident, MaybeUninit<Rc<Located<Type>>>>
}

impl Grammar {
    fn regexp_defined(&self, id: &Located<Ident>) -> Result<(), Located<Error>> {
        self.regexps.get(id.as_ref()).ok_or(Located::new(Error::UndefinedRegExp(id.as_ref().clone()), id.span()))?;
        Ok(())
    }

    fn type_defined(&self, id: &Located<Ident>) -> Result<(), Located<Error>> {
        self.types.get(id.as_ref()).ok_or(Located::new(Error::UndefinedType(id.as_ref().clone()), id.span()))?;
        Ok(())
    }
}

pub struct Type {
    id: Located<Ident>,
    rules: Vec<Located<Rule>>
}

pub struct Rule {
    pub id: Option<Located<Ident>>,
    pub tokens: Vec<Located<Token>>
}

pub enum Token {
    Terminal(Terminal),
    NonTerminal(NonTerminal)
}

pub enum Terminal {
    RegExp(TypedRegExp)
}

pub enum NonTerminal {
    Type(Located<Ident>),
    Repeat(Located<Ident>, usize, usize, Option<Located<Separator>>),
}

pub struct Separator {
    pub strong: bool,
    pub terminal: Located<Terminal>
}

pub struct RegExpDefinition {
    pub id: Located<Ident>,
    pub exp: TypedRegExp
}

pub struct TypedRegExp {
    pub ty: Option<Located<Ident>>,
    pub exp: Located<RegExp>
}

pub struct RegExp(pub Vec<Located<RegExpAtom>>);

pub enum RegExpAtom {
    Ref(Ident),
    CharSet(CharSet, bool),
    Literal(String, bool),
    Repeat(Box<Located<RegExpAtom>>, usize, usize),
    Or(Vec<Located<RegExp>>),
    Capture(RegExp),
    Group(RegExp),
}

#[derive(Debug)]
pub enum Error {
    UndefinedRegExp(Ident),
    UndefinedType(Ident)
}

fn compile_located_regexp(g: &Grammar, ast: Located<syntax::RegExp>) -> Result<Located<RegExp>, Located<Error>> {
    let span = ast.span();
    Ok(Located::new(compile_regexp(g, ast.into_inner(), span)?, span))
}

fn compile_regexp(g: &Grammar, ast: syntax::RegExp, span: Span) -> Result<RegExp, Located<Error>> {
    let mut atoms = Vec::new();
    for ast in ast.0.into_iter() {
        atoms.push(compile_regexp_atom(g, ast)?);
    }

    Ok(RegExp(atoms))
}

fn compile_regexp_atom(g: &Grammar, ast: Located<syntax::RegExpAtom>) -> Result<Located<RegExpAtom>, Located<Error>> {
    let span = ast.span();
    let exp = match ast.into_inner() {
        syntax::RegExpAtom::Ident(id) => RegExpAtom::Ref(id),
        syntax::RegExpAtom::CharSet(set, negate) => RegExpAtom::CharSet(set, negate),
        syntax::RegExpAtom::Literal(str, case_sensitive) => RegExpAtom::Literal(str, case_sensitive),
        syntax::RegExpAtom::Repeat(atom, min, max) => {
            RegExpAtom::Repeat(Box::new(compile_regexp_atom(g, *atom)?), min, max)
        },
        syntax::RegExpAtom::Or(exps) => {
            let mut compiled_exps = Vec::new();
            for e in exps.into_iter() {
                compiled_exps.push(compile_located_regexp(g, e)?);
            }
            RegExpAtom::Or(compiled_exps)
        },
        syntax::RegExpAtom::Capture(exp) => {
            RegExpAtom::Capture(compile_regexp(g, exp, span)?)
        },
        syntax::RegExpAtom::Group(exp) => {
            RegExpAtom::Group(compile_regexp(g, exp, span)?)
        }
    };

    Ok(Located::new(exp, span))
}

fn compile_terminal(g: &Grammar, ast: syntax::Terminal) -> Result<Terminal, Located<Error>> {
    let t = match ast {
        syntax::Terminal::RegExp(exp) => {
            Terminal::RegExp(TypedRegExp {
                ty: exp.ty,
                exp: compile_located_regexp(&g, exp.exp)?
            })
        }
    };

    Ok(t)
}

fn compile_located_terminal(g: &Grammar, ast: Located<syntax::Terminal>) -> Result<Located<Terminal>, Located<Error>> {
    let span = ast.span();
    Ok(Located::new(compile_terminal(g, ast.into_inner())?, span))
}

fn compile_token(g: &Grammar, ast: Located<syntax::Token>) -> Result<Located<Token>, Located<Error>> {
    let span = ast.span();
    let t = match ast.into_inner() {
        syntax::Token::Terminal(t) => {
            Token::Terminal(compile_terminal(g, t)?)
        },
        syntax::Token::NonTerminal(nt) => {
            let nt = match nt {
                syntax::NonTerminal::Type(id) => NonTerminal::Type(id),
                syntax::NonTerminal::Repeat(id, min, max, sep) => {
                    let sep = match sep {
                        Some(s) => {
                            let span = s.span();
                            let s = s.into_inner();
                            Some(Located::new(Separator {
                                strong: s.strong,
                                terminal: compile_located_terminal(g, s.terminal)?
                            }, span))
                        },
                        None => None
                    };
                    NonTerminal::Repeat(id, min, max, sep)
                }
            };
            Token::NonTerminal(nt)
        }
    };
    Ok(Located::new(t, span))
}

fn compile_rule(g: &Grammar, ast: Located<syntax::Rule>) -> Result<Located<Rule>, Located<Error>> {
    let span = ast.span();
    let ast = ast.into_inner();
    let mut tokens = Vec::new();
    for token in ast.tokens.into_iter() {
        tokens.push(compile_token(g, token)?);
    }
    Ok(Located::new(Rule {
        id: ast.id,
        tokens
    }, span))
}

impl TryFrom<syntax::Grammar> for Grammar {
    type Error = Located<Error>;

    fn try_from(ast: syntax::Grammar) -> Result<Grammar, Located<Error>> {
        let externs = ast.externs;
        let mut regexps = HashMap::new();
        let mut types = HashMap::new();

        for def in &ast.regexps {
            regexps.insert(def.id.as_ref().clone(), unsafe { MaybeUninit::uninit() });
        }

        for ty in &ast.types {
            types.insert(ty.id.as_ref().clone(), unsafe { MaybeUninit::uninit() });
        }

        let mut g = Grammar {
            externs,
            regexps,
            types
        };

        for ast in ast.regexps.into_iter() {
            let id = ast.id.clone();
            let span = ast.span();
            g.regexp_defined(&id);
            let ast = ast.into_inner();
            let mut def = MaybeUninit::new(Rc::new(Located::new(RegExpDefinition {
                id: ast.id,
                exp: TypedRegExp {
                    ty: ast.exp.ty,
                    exp: compile_located_regexp(&g, ast.exp.exp)?
                }
            }, span)));
            let undef = g.regexps.get_mut(&id).unwrap();
            std::mem::swap(&mut def, undef);
            std::mem::forget(def);
        }

        for ast in ast.types.into_iter() {
            let id = ast.id.clone();
            let span = ast.span();
            g.type_defined(&id);
            let ast = ast.into_inner();

            let mut rules = Vec::new();
            for rule in ast.rules {
                rules.push(compile_rule(&g, rule)?);
            }

            let mut def = MaybeUninit::new(Rc::new(Located::new(Type {
                id: ast.id,
                rules: rules
            }, span)));
            let undef = g.types.get_mut(&id).unwrap();
            std::mem::swap(&mut def, undef);
            std::mem::forget(def);
        }

        Ok(g)
    }
}
