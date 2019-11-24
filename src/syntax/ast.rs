use crate::CharSet;
use super::Located;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Ident(pub String);

pub struct Grammar {
    pub externs: Vec<Located<Ident>>,
    pub regexps: Vec<Located<RegExpDefinition>>,
    pub types: Vec<Located<Type>>
}

pub struct Type {
    pub id: Located<Ident>,
    pub rules: Vec<Located<Rule>>
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
    Ident(Ident),
    CharSet(CharSet, bool),
    Literal(String, bool),
    Repeat(Box<Located<RegExpAtom>>, usize, usize),
    Or(Vec<Located<RegExp>>),
    Capture(RegExp),
    Group(RegExp)
}
