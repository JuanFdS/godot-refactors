use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "gdscript.pest"]
pub struct GDScriptParser;

use pest::iterators::Pair;

impl GDScriptParser {
    fn parse_to_ast<'a, T>(input: &'a str, rule_type: Rule, rule_mapper : fn(Pair<'a, Rule>) -> T) -> T {
        let parse_result = GDScriptParser::parse(rule_type, input)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();

        rule_mapper(parse_result)
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
        match parse_result.into_inner().next().unwrap().as_rule() {
            Rule::export => Annotation::Export,
            Rule::onready => Annotation::OnReady,
            _ => panic!()
        }
    }

    fn is_rule(expected_rule: Rule) -> impl Fn(Pair<Rule>) -> bool {
        move |pair: Pair<Rule>| pair.as_rule() == expected_rule
    }

    fn to_program(parse_result: Pair<Rule>) -> Program {
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

    fn to_declaration(parse_result: Pair<Rule>) -> Declaration {
        match parse_result.as_rule() {
            Rule::function => {
                let mut inner_rules = parse_result.into_inner();
                let function_name = inner_rules.next().unwrap().as_span().as_str();
                let function_body = inner_rules
                    .next()
                    .unwrap()
                    .into_inner()
                    .filter_map(Self::to_statement)
                    .collect();
                Declaration::Function(function_name, function_body)
            },
            Rule::empty_line => Declaration::EmptyLine,
            Rule::var_declaration => {
                let mut inner_rules = parse_result.into_inner();
                // inner_rules.find CONSUMES!!!
                let annotation = inner_rules.clone().find(|p| p.as_rule() == Rule::annotation);
                let identifier = inner_rules.find(
                    |p| p.as_rule() == Rule::identifier).unwrap();
                let expression = inner_rules.find(|p| p.as_rule() == Rule::expression).unwrap();
                Declaration::Var(
                    identifier.as_span().as_str(),
                    expression.as_span().as_str(),
                    annotation.map(|pair| Self::to_annotation(pair))
            )
            }
            Rule::declaration => Self::to_declaration(parse_result.into_inner().next().unwrap()),
            _ => panic!(),
        }
    }

    fn to_statement(parse_result: Pair<Rule>) -> Option<Statement> {
        match parse_result.as_rule() {
            Rule::statement => Some(Statement::Pass),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program<'a> {
    is_tool: bool,
    super_class: Option<String>,
    declarations: Vec<Declaration<'a>>,
}

impl<'a> Program<'a> {
    fn with_declarations(&self, new_declarations: Vec<Declaration<'a>>) -> Program<'a> {
        let mut new_program = self.clone();
        new_program.declarations = new_declarations;
        new_program
    }

    pub fn as_str(&self) -> String {
        let mut code_text = "".to_string();
        if self.is_tool {
            code_text += "@tool\n";
        }
        self.super_class.iter().for_each(|super_class_name| {
            code_text += "extends ";
            code_text += super_class_name;
            code_text += "\n";
        });
        code_text += &self.declarations
            .iter()
            .cloned()
            .map(|declaration| declaration.as_str())
            .collect::<Vec<String>>()
            .join("\n");
        code_text
    }

    pub fn move_declaration_down(&self, declaration: Declaration) -> Program {
        let maybe_idx = self.declarations.iter().position(|x| *x == declaration).filter(|idx| (idx + 1) < self.declarations.len());
        match maybe_idx {
            Some(idx) => {
                let mut updated_declarations = self.declarations.clone();
                updated_declarations.swap(idx, idx + 1);
                self.with_declarations(updated_declarations)
            }
            None => self.clone(),
        }
    }

    pub fn move_declaration_up(&self, declaration: Declaration) -> Program {
        let maybe_idx = self.declarations.iter().position(|x| *x == declaration).filter(|idx| *idx > 0);
        match maybe_idx {
            Some(idx) => {
                let mut updated_declarations = self.declarations.clone();
                updated_declarations.swap(idx, idx - 1);
                self.with_declarations(updated_declarations)
            }
            None => self.clone(),
        }
    }

    pub fn toggle_tool(&self) -> Program<'a> {
        let mut new_program = self.clone();
        new_program.is_tool = !self.is_tool;
        new_program
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Declaration<'a> {
    // Function(nombre, statements)
    Function(&'a str, Vec<Statement>),
    EmptyLine,
    // Var(identifier, value, anotation)
    Var(&'a str, &'a str, Option<Annotation>)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Annotation {
    Export,
    OnReady
}

impl Annotation {
    pub fn as_str(&self) -> String {
        match self {
            Annotation::Export => "@export".to_string(),
            Annotation::OnReady => "@onready".to_string(),
        }
    }
}

impl Declaration<'_> {
    pub fn as_str(&self) -> String {
        match self {
            Declaration::Function(name, statements) => {
                let identation = "\t";
                let function_body = statements
                    .iter()
                    .cloned()
                    .map(|statement| identation.to_owned() + &statement.as_str())
                    .collect::<Vec<String>>()
                    .join("\n");
                format!(
                    "func {fn_name}():\n{body}",
                    fn_name = name,
                    body = function_body
                )
            }
            Declaration::EmptyLine => "".to_string(),
            Declaration::Var(identifier, value, annotation) =>
                format!("{annotation}var {identifier} = {value}", identifier=identifier, value=value,annotation=annotation.clone().map(|x| x.as_str() + " ").unwrap_or("".to_string())),
        }
    }

    pub fn toggle_annotation(&self, annotation: Annotation) -> Declaration {
        match self {
            Declaration::Var(identifier, value, Some(previousAnnotation))
                if *previousAnnotation == annotation => Declaration::Var(identifier, value, None),
            Declaration::Var(identifier, value, _) =>
                Declaration::Var(identifier, value, Some(annotation)),
            _ => self.clone()
        }
    }
}


#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Statement {
    Pass,
}

impl Statement {
    fn as_str(&self) -> String {
        match *self {
            Statement::Pass => "pass".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse_eq(input: &str, expected_result: Program) {
        let parse_result = GDScriptParser::parse(Rule::program, input)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();

        assert_eq!(expected_result, GDScriptParser::to_program(parse_result));
    }

    fn assert_parse_roundtrip(input: &str) {
        let parsed_program: Program<'_> = GDScriptParser::parse_to_program(input);
        let parsed_program_as_string = parsed_program.as_str();

        assert_eq!(input, parsed_program_as_string);
    }

    #[test]
    fn test_program_with_one_function() {
        assert_parse_eq(
            "func foo():\n    pass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![Declaration::Function("foo", vec![Statement::Pass])]
            },
        );
    }

    #[test]
    fn test_program_with_two_functions() {
        assert_parse_eq(
            "func foo():\n    pass\nfunc bar():\n    pass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    Declaration::Function("foo", vec![Statement::Pass]),
                    Declaration::Function("bar", vec![Statement::Pass]),
                ],
            },
        );
    }

    #[test]
    fn test_parse_program_with_two_functions_and_empty_line() {
        assert_parse_eq(
            "func foo():\n\tpass\n\nfunc bar():\n\tpass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    Declaration::Function(
                        "foo", vec![Statement::Pass]),
                    Declaration::EmptyLine,
                    Declaration::Function(
                        "bar", vec![Statement::Pass]),
                ]
            })
    }

    #[test]
    fn tabs_are_valid_identation() {
        assert_parse_eq(
            "func foo():\n\tpass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![Declaration::Function("foo", vec![Statement::Pass])]
            },
        );
    }

    #[test]
    fn test_function_to_string() {
        assert_eq!(
            Declaration::Function("foo", vec![Statement::Pass]).as_str(),
            "func foo():\n\tpass"
        )
    }

    

    #[test]
    fn program_to_string() {
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: 
                    vec![
                        Declaration::Function("foo", vec![Statement::Pass]),
                        Declaration::Function("bar", vec![Statement::Pass])
                    ]
            }.as_str(),
            "func foo():\n\tpass\nfunc bar():\n\tpass"
        )
    }

    #[test]
    fn program_with_newline_to_string() {
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: 
                vec![
                    Declaration::EmptyLine,
                    Declaration::Function("foo", vec![Statement::Pass]),
                    Declaration::Function("bar", vec![Statement::Pass])
                ]
            }.as_str(),
            "\nfunc foo():\n\tpass\nfunc bar():\n\tpass"
        )
    }

    #[test]
    fn move_function_down() {
        let foo = Declaration::Function("foo", vec![Statement::Pass]);
        let bar = Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo.clone(), bar.clone()] }.move_declaration_down(foo.clone()),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![bar.clone(), foo.clone()] }
        )
    }

    #[test]
    fn move_function_down_when_it_is_the_last_function_leaves_the_program_as_it_is() {
        let foo = Declaration::Function("foo", vec![Statement::Pass]);
        let bar = Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo.clone(), bar.clone()] }.move_declaration_down(bar.clone()
            ),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo.clone(), bar.clone()] }
        )
    }

    #[test]
    fn move_function_up_when_it_is_the_first_function_leaves_the_program_as_it_is() {
        let foo = Declaration::Function("foo", vec![Statement::Pass]);
        let bar = Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo.clone(), bar.clone()] }.move_declaration_up(foo.clone()
            ),
            Program {
                is_tool: false,
                declarations: vec![foo.clone(), bar.clone()],
                super_class: None,
            }
        )
    }

    #[test]
    fn move_function_down_when_there_is_an_empty_line_between_functions_moves_the_function_into_the_empty_line() {
        let foo = || Declaration::Function("foo", vec![Statement::Pass]);
        let bar = || Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                declarations: vec![foo(), Declaration::EmptyLine, bar()],
                super_class: None,
            }.move_declaration_down(foo()),
            Program {
                is_tool: false,
                declarations: vec![Declaration::EmptyLine, foo(), bar()],
                super_class: None,
            }
        )
    }

    #[test]
    fn move_function_down_changes_the_code_so_the_declaration_goes_down() {
        let program = GDScriptParser::parse_to_program(
            "func foo():\n    pass\nfunc bar():\n    pass");

        let foo_function = || GDScriptParser::parse_to_declaration("func foo():\n    pass");

        let refactored_program = program.move_declaration_down(foo_function());

        assert_eq!(
            refactored_program.as_str(),
            "func bar():\n\tpass\nfunc foo():\n\tpass"
        )
    }

    #[test]
    fn moving_function_around_when_there_are_multiple_empty_lines() {
        let program = GDScriptParser::parse_to_program(
            "func foo():\n\tpass\n\nfunc bar():\n\tpass"
        );

        let foo_function = || GDScriptParser::parse_to_declaration("func foo():\n\tpass");

        let program_with_foo_one_time_down =
            program.move_declaration_down(foo_function());

        assert_eq!(
            program_with_foo_one_time_down.as_str(),
            "\nfunc foo():\n\tpass\nfunc bar():\n\tpass"
        );

        let program_with_foo_two_times_down = program_with_foo_one_time_down.move_declaration_down(foo_function());
        assert_eq!(
            program_with_foo_two_times_down.as_str(),
            "\nfunc bar():\n\tpass\nfunc foo():\n\tpass"
        )
    }

    #[test]
    fn variable_declaration_parses() {
        assert_parse_eq(
            "var x = 2",
            Program {
                is_tool: false,
                declarations: vec![Declaration::Var("x","2",None)],
                super_class: None
            }
        )
    }

    #[test]
    fn variable_can_start_with_underscore() {
        assert_parse_eq(
            "var _x = 2",
            Program {
                is_tool: false,
                declarations: vec![Declaration::Var("_x","2",None)],
                super_class: None
            }
        )
    }

    #[test]
    fn variable_declaration_parses_even_with_annotations() {
        assert_parse_eq(
            "@export var x = 2",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![Declaration::Var("x","2",Some(Annotation::Export))]
            }
        )
    }

    #[test]
    fn toggle_export_annotation_adds_export_if_var_declaration_doesnt_has_it() {
        assert_eq!(
            Declaration::Var("x","2",None).toggle_annotation(Annotation::Export),
            Declaration::Var("x","2",Some(Annotation::Export))
        )
    }

    #[test]
    fn toggle_export_annotation_changes_annotation_to_export_if_it_is_a_different_one() {
        assert_eq!(
            Declaration::Var("x","2",Some(Annotation::OnReady)).toggle_annotation(Annotation::Export),
            Declaration::Var("x","2",Some(Annotation::Export))
        )
    }

    
    #[test]
    fn toggle_export_annotation_removes_annotation_if_it_has_export() {
        assert_eq!(
            Declaration::Var("x","2",Some(Annotation::Export)).toggle_annotation(Annotation::Export),
            Declaration::Var("x","2",None)
        )
    }

    #[test]
    fn program_can_be_tool() {
        assert_parse_eq(
            "@tool\nfunc foo():\n\tpass\n",
            Program {
                is_tool: true,
                super_class: None,
                declarations:
                    vec![
                        Declaration::Function("foo",vec![Statement::Pass])
                        ]
            },
        )
    }

    #[test]
    fn tool_program_to_string() {
        assert_eq!(
            Program {
                is_tool: true,
                super_class: None,
                declarations:
                    vec![
                        Declaration::Function("foo",vec![Statement::Pass])
                        ]
            }.as_str(),
            "@tool\nfunc foo():\n\tpass"
        )
    }

    #[test]
    fn toggle_tool_on_a_tool_program_makes_it_nontool() {
        assert_eq!(
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![]
            }.toggle_tool(),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![]
            }
        )
    }

    #[test]
    fn toggle_tool_on_a_non_tool_program_makes_it_tool() {
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![]
            }.toggle_tool(),
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![]
            }
        )
    }

    #[test]
    fn a_tool_program_with_inheritance_can_be_parsed() {
        assert_parse_eq(
            "@tool\nextends Node2D\nfunc foo():\n\tpass",
            Program {
                is_tool: true,
                super_class: Some("Node2D".to_owned()),
                declarations: vec![Declaration::Function("foo", vec![Statement::Pass])]
            });
    }

    #[test]
    fn tool_program_with_inheritance_as_string() {
        let program_code = Program {
            is_tool: true,
            super_class: Some("Node2D".to_owned()),
            declarations: vec![Declaration::Function("foo", vec![Statement::Pass])]
        }.as_str();

        assert_eq!(
            program_code,
            "@tool\nextends Node2D\nfunc foo():\n\tpass"
        )
    }

    #[test]
    fn tool_program_with_inheritance_roundtrip() {
        assert_parse_roundtrip("@tool\nextends Node2D\nfunc foo():\n\tpass");
    }
}
