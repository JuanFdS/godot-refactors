use crate::parsing::Rule;
use crate::refactorings::ExtendedPair;
use crate::refactorings::{range_contains, LineCol};
use pest::iterators::Pair;
use std::ops::Range;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program<'a> {
    pub is_tool: bool,
    pub super_class: Option<String>,
    pub declarations: Vec<Declaration<'a>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Declaration<'a> {
    pub kind: DeclarationKind<'a>,
    location: Option<Range<LineCol>>
}

impl <'a> Declaration<'a> {
    pub fn new(pair: Option<Pair<'a, Rule>>, kind: DeclarationKind<'a>) -> Self {
        Declaration {
            kind,
            location: pair.map(|p| p.line_col_range())
        }
    }
    pub fn contains_range(&self, range: &Range<LineCol>) -> bool {
        self.location.as_ref().is_some_and(|l| range_contains(l, range))
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DeclarationKind<'a> {
    // Function(nombre, type, parametros, statements)
    Function(&'a str, Option<String>, Vec<Parameter<'a>>, Vec<Statement<'a>>),
    EmptyLine,
    // Var(identifier, value, anotation)
    Var(String, &'a str, Option<Annotation<'a>>),
    Unknown(&'a str)
}

pub struct Function<'a> {
    pub name: &'a str,
    pub tipe: Option<String>,
    pub parameters: Vec<Parameter<'a>>,
    pub statements: Vec<Statement<'a>>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter<'a> {
    pub pair: Option<Pair<'a, Rule>>,
    pub name: &'a str
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Annotation<'a> {
    pub pair: Option<Pair<'a, Rule>>,
    pub kind: AnnotationKind<'a>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AnnotationKind<'a> {
    Export,
    OnReady,
    ExportToolButton(&'a str)
}

#[derive(Debug, Eq, Clone)]
pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    location: Option<Range<LineCol>>,
}

impl <'a> Statement<'a> {
    pub fn new(pair: Option<Pair<'a, Rule>>, kind: StatementKind<'a>) -> Self {
        Statement {
            kind,
            location: pair.map(|p| p.line_col_range())
        }
    }
    pub fn contains_range(&self, range: &Range<LineCol>) -> bool {
        self.location.as_ref().is_some_and(|l| range_contains(l, range))
    }
}

impl<'a> PartialEq for Statement<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StatementKind<'a> {
    Pass,
    Unknown(String),
    VarDeclaration(&'a str, Expression<'a>),
    Expression(Expression<'a>),
    Return(Option<Expression<'a>>)
}

type RulePair<'a> = Option<Pair<'a, Rule>>;

#[derive(Debug, Eq, PartialEq, Clone)]

pub struct Expression<'a> {
    pub pair: RulePair<'a>,
    pub kind: ExpressionKind<'a>
}

#[derive(Debug, Eq, PartialEq, Clone)]

pub enum ExpressionKind<'a> {
    LiteralInt(usize),
    BinaryOperation(Box<Expression<'a>>, &'a str, Box<Expression<'a>>),
    Unknown(String),
    // (receiver: Expression<'a>, message_name: &'a str, arguments: Vec<Expression<'a>>)
    LiteralSelf,
    MessageSend(Box<Expression<'a>>, &'a str, Vec<Expression<'a>>),
    VariableUsage(&'a str)
}