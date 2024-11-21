use crate::parsing::Rule;
use crate::refactorings::ExtendedPair;
use crate::refactorings::{range_contains, LineCol};
use pest::iterators::Pair;
use std::ops::Range;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program {
    pub is_tool: bool,
    pub super_class: Option<String>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Declaration {
    pub kind: DeclarationKind,
    location: Option<Range<LineCol>>
}

impl <'a> Declaration {
    pub fn new(pair: Option<Pair<'a, Rule>>, kind: DeclarationKind) -> Self {
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
pub enum DeclarationKind {
    EmptyLine,
    // Function(nombre, type, parametros, statements)
    Function(String, Option<String>, Vec<Parameter>, Vec<Statement>),
    // Var(identifier, value, anotation)
    Var(String, String, Option<Annotation>),
    Unknown(String)
}

pub struct Function {
    pub name: String,
    pub tipe: Option<String>,
    pub parameters: Vec<Parameter>,
    pub statements: Vec<Statement>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter {
    pub name: String
}

impl Parameter {
    pub fn new(_pair: Option<Pair<Rule>>, name: &str) -> Self {
        Parameter {
            name: name.into()
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Annotation {
    pub kind: AnnotationKind,
    location: Option<Range<LineCol>>,
}

impl Annotation {
    pub fn new(pair: Option<Pair<Rule>>, kind: AnnotationKind) -> Self {
        Annotation {
            kind,
            location: pair.map(|p| p.line_col_range())
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AnnotationKind {
    Export,
    OnReady,
    ExportToolButton(String)
}

#[derive(Debug, Eq, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    location: Option<Range<LineCol>>,
}

impl Statement {
    pub fn new(pair: Option<Pair<Rule>>, kind: StatementKind) -> Self {
        Statement {
            kind,
            location: pair.map(|p| p.line_col_range())
        }
    }
    pub fn contains_range(&self, range: &Range<LineCol>) -> bool {
        self.location.as_ref().is_some_and(|l| range_contains(l, range))
    }
}

impl<'a> PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StatementKind {
    Pass,
    Unknown(String),
    VarDeclaration(String, Expression),
    Expression(Expression),
    Return(Option<Expression>)
}

type RulePair<'a> = Option<Pair<'a, Rule>>;

#[derive(Debug, Eq, PartialEq, Clone)]

pub struct Expression {
    pub kind: ExpressionKind,
    location: Option<Range<LineCol>>
}

impl Expression {
    pub fn new(pair: Option<Pair<Rule>>, kind: ExpressionKind) -> Self {
        Expression {
            kind,
            location: pair.map(|p| p.line_col_range())
        }
    }
    pub fn contains_range(&self, range: &Range<LineCol>) -> bool {
        self.location.as_ref().is_some_and(|l| range_contains(l, range))
    }

    pub fn line_col_range(&self) -> Range<LineCol> {
        self.location.as_ref().unwrap().clone()
    }

    pub fn line_col(&self) -> LineCol {
        self.line_col_range().start
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]

pub enum ExpressionKind {
    LiteralInt(usize),
    BinaryOperation(Box<Expression>, String, Box<Expression>),
    Unknown(String),
    // (receiver: Expression, message_name: String, arguments: Vec<Expression>)
    LiteralSelf,
    MessageSend(Box<Expression>, String, Vec<Expression>),
    VariableUsage(String)
}