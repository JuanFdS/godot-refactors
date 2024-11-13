use pest::iterators::Pair;

use crate::parsing::Rule;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program<'a> {
    pub is_tool: bool,
    pub super_class: Option<String>,
    pub declarations: Vec<Declaration<'a>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Declaration<'a> {
    pub pair: Option<Pair<'a, Rule>>,
    pub kind: DeclarationKind<'a>
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<'a> {
    Pass,
    Unknown(String),
    VarDeclaration(&'a str, String)
}