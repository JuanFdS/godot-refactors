use pest::Parser;
use pest_derive::Parser;
use crate::godot_ast_types::*;

#[derive(Parser)]
#[grammar = "gdscript.pest"]
pub struct GDScriptParser;

use pest::iterators::Pair;

impl GDScriptParser {
    pub fn parse_to_ast<'a, T>(input: &'a str, rule_type: Rule, rule_mapper : fn(Pair<'a, Rule>) -> T) -> T {
        Self::try_parse_to_ast(input, rule_type, rule_mapper).expect("Malio sal el parseo")
    }

    pub fn try_parse_to_ast<'a, T>(
        input: &'a str, rule_type: Rule, rule_mapper : fn(Pair<'a, Rule>) -> T
    ) -> Result<T, pest::error::Error<Rule>> {
        GDScriptParser::parse(rule_type, input)
            .map(|mut pairs| rule_mapper(pairs.next().unwrap()))
    }

    pub fn try_parse_to_program<'a>(input: &'a str) -> Result<Program<'_>, pest::error::Error<Rule>> {
        Self::try_parse_to_ast(input, Rule::program, Self::to_program)
    }

    pub fn parse_to_program<'a>(input: &'a str) -> Program {
        Self::parse_to_ast(input, Rule::program, Self::to_program)
    }

    pub fn parse_to_declaration<'a>(input: &'a str) -> Declaration {
        Self::parse_to_ast(input, Rule::declaration, Self::to_declaration)
    }

    pub fn parse_to_statement<'a>(input: &'a str) -> Statement {
        let to_statement = |x| Self::to_statement(x).unwrap();
        Self::parse_to_ast(input, Rule::statement,  to_statement)
    }

    fn to_annotation(parse_result: Pair<Rule>) -> Annotation {
        let pair = parse_result.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::export => AnnotationKind::Export,
            Rule::onready => AnnotationKind::OnReady,
            Rule::export_tool_button_annotation => AnnotationKind::ExportToolButton(
                pair.into_inner()
                    .find(|pair| pair.as_rule() == Rule::STRING )
                    .unwrap()
                    .into_inner()
                    .find(|pair|pair.as_rule() == Rule::STRING_CONTENT)
                    .unwrap()
                    .as_span()
                    .as_str()
            ),
            _ => panic!()
        }.to_annotation()
    }

    fn is_rule(expected_rule: Rule) -> impl Fn(Pair<Rule>) -> bool {
        move |pair: Pair<Rule>| pair.as_rule() == expected_rule
    }
    

    pub fn to_program(parse_result: Pair<Rule>) -> Program {
        match parse_result.as_rule() {
            Rule::program => {
                let super_class: Option<String> =
                    parse_result.clone().into_inner()
                                .find(|pair| pair.as_rule() == Rule::inheritance)
                                .map(|pair|
                                    pair.into_inner().find(|pair| pair.as_rule() == Rule::identifier)
                                )
                                .flatten()
                                .map(|pair| pair.as_span().as_str().to_string());
                let is_tool: bool = parse_result.clone().into_inner().any(
                    Self::is_rule(Rule::tool_annotation)
                );
                let declarations = parse_result
                    .into_inner()
                    .filter(|pair|
                        pair.as_rule() == Rule::declaration || pair.as_rule() == Rule::empty_line
                    )
                    .map(Self::to_declaration)
                    .collect();
                Program { is_tool, super_class, declarations }
            }
            _ => panic!(),
        }
    }

    fn to_declaration<'a>(parse_result: Pair<'a, Rule>) -> Declaration<'a> {
        match parse_result.as_rule() {
            Rule::function => {
                let mut inner_rules = parse_result.clone().into_inner();
                let function_name = inner_rules.next().unwrap().as_span().as_str();
                let mut next_match = inner_rules.next().unwrap();
                let parameters: Vec<Parameter<'_>>;
                if next_match.as_rule() == Rule::param_list {
                    parameters = next_match.into_inner()
                              .filter(|pair| pair.as_rule() == Rule::parameter)
                              .map(|pair| Parameter::from(pair.as_span().as_str()))
                              .collect();
                    next_match = inner_rules.next().unwrap();
                } else {
                    parameters = vec![];
                }
                let return_type ;
                if next_match.as_rule() == Rule::function_return_type {
                    return_type = Some(next_match.into_inner().last().unwrap().as_span().as_str().to_string());
                    next_match = inner_rules.next().unwrap();
                } else {
                    return_type = None;
                }
                let function_body = next_match
                    .into_inner()
                    .filter_map(Self::to_statement)
                    .collect();
                DeclarationKind::Function(function_name, return_type, parameters, function_body).to_declaration(Some(parse_result))
            },
            Rule::empty_line => DeclarationKind::EmptyLine.to_declaration(Some(parse_result)),
            Rule::var_declaration => {
                let mut inner_rules = parse_result.clone().into_inner();
                // inner_rules.find CONSUMES!!!
                let annotation = inner_rules.clone().find(|p| p.as_rule() == Rule::annotation);
                let identifier = inner_rules.find(
                    |p| p.as_rule() == Rule::identifier).unwrap();
                let expression = inner_rules.find(|p| p.as_rule() == Rule::expression).unwrap();
                DeclarationKind::Var(
                    identifier.as_span().as_str().to_string(),
                    expression.as_span().as_str(),
                    annotation.map(|pair| Self::to_annotation(pair))
                ).to_declaration(Some(parse_result))
            }
            Rule::unknown => {
                DeclarationKind::Unknown(parse_result.as_span().as_str()).to_declaration(Some(parse_result))
            },
            Rule::declaration => Self::to_declaration(parse_result.into_inner().next().unwrap()),
            _ => panic!(),
        }
    }

    fn to_statement(parse_result: Pair<Rule>) -> Option<Statement> {
        match parse_result.as_rule() {
            Rule::unknown => Some(
                Statement { pair: Some(parse_result.clone()), kind: StatementKind::Unknown(parse_result.as_span().as_str().to_owned()) }),
            Rule::pass => Some(
                Statement { pair: Some(parse_result), kind: StatementKind::Pass }
            ),
            Rule::statement => Self::to_statement(parse_result.into_inner().next().unwrap()),
            _ => None,
        }
    }
}