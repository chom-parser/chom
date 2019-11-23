use crate::CharSet;
use super::Located;

pub struct Ident(pub String);

pub struct Grammar {
    pub externs: Vec<Located<Ident>>,
    pub regexps: Vec<Located<RegExpDefinition>>,
    pub types: Vec<Located<Type>>
}

pub enum Type {
    Alias(Vec<Located<Token>>),
    NonTerminal {
        id: Located<Ident>,
        rules: Vec<Located<Rule>>
    }
}

pub struct Rule {
    pub id: Located<Ident>,
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
    Type(Located<String>),
    Repeat(Located<String>, usize, usize, Option<Located<Separator>>),
}

pub struct Separator {
    pub strong: bool,
    pub terminal: Located<Terminal>
}

pub struct TypedRegExp {
    pub ty: Option<Located<Ident>>,
    pub exp: Located<RegExp>
}

pub struct RegExpDefinition {
    pub id: Located<Ident>,
    pub ty: Option<Located<Ident>>,
    pub exp: Located<RegExp>
}

pub struct RegExp(pub Vec<Located<RegExpAtom>>);

pub enum RegExpAtom {
    Ident(String),
    CharSet(CharSet, bool),
    Literal(String, bool),
    Repeat(Box<Located<RegExpAtom>>, usize, usize),
    Or(Vec<Located<RegExp>>),
    Capture(RegExp),
    Group(RegExp)
}
