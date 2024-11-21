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

impl <'a, K> AstNode<K> {
    pub fn new(pair: Option<Pair<'a, Rule>>, kind: K) -> Self {
        AstNode {
            kind,
            location: pair.map(|p| p.line_col_range())
        }
    }
    pub fn contains_range(&self, range: &Range<LineCol>) -> bool {
        self.location.as_ref().is_some_and(|l| range_contains(l, range))
    }
}

#[derive(Debug, Eq, Clone)]
pub struct AstNode<K> {
    pub kind: K,
    location: Option<Range<LineCol>>
}

impl<'a, K: PartialEq> PartialEq for AstNode<K> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

pub type Declaration = AstNode<DeclarationKind>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DeclarationKind {
    EmptyLine,
    Function { name: String, return_type: Option<String>, parameters: Vec<Parameter>, statements: Vec<Statement>},
    Var { identifier: String, value: String, annotation: Option<Annotation> },
    Unknown(String)
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

pub type Annotation = AstNode<AnnotationKind>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AnnotationKind {
    Export,
    OnReady,
    ExportToolButton(String)
}

pub type Statement = AstNode<StatementKind>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StatementKind {
    Pass,
    Unknown(String),
    VarDeclaration(String, Expression),
    Expression(Expression),
    Return(Option<Expression>)
}

pub type Expression = AstNode<ExpressionKind>;

impl Expression {
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