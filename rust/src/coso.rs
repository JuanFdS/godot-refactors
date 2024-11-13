
use std::ops::Range;

use godot::classes::cone_twist_joint_3d::Param;
use pest::Parser;
use pest_derive::Parser;


#[derive(Parser)]
#[grammar = "gdscript.pest"]
pub struct GDScriptParser;

use pest::iterators::Pair;

impl GDScriptParser {
    fn parse_to_ast<'a, T>(input: &'a str, rule_type: Rule, rule_mapper : fn(Pair<'a, Rule>) -> T) -> T {
        Self::try_parse_to_ast(input, rule_type, rule_mapper).expect("Malio sal el parseo")
    }

    fn try_parse_to_ast<'a, T>(
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
            Rule::unknown => Some(Statement::Unknown(parse_result.as_span().as_str().to_owned())),
            Rule::pass => Some(Statement::Pass),
            Rule::statement => Self::to_statement(parse_result.into_inner().next().unwrap()),
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

pub trait Replace<T: Clone> {
    fn replace_mut(&mut self, index: usize, f: impl Fn(&T) -> T) -> T;

    fn replace(&self, index: usize, f: impl Fn(&T) -> T) -> Self;
}

impl<T: Clone> Replace<T> for Vec<T> {
    fn replace_mut(&mut self, index: usize, f: impl Fn(&T) -> T) -> T {
        let maybe_element = self.get(index);
        match maybe_element {
            None => panic!(),
            Some(element) => {
                let new_element = f(&element);
                self.push(new_element);
                return self.swap_remove(index);
            }
        }
    }

    fn replace(&self, index: usize, f: impl Fn(&T) -> T) -> Self {
        let mut new_vector = self.clone();
        match self.get(index) {
            Some(old_value) => {
                let new_value = f(old_value);
                new_vector.push(new_value);
                new_vector.swap_remove(index);
                new_vector
            },
            None => panic!("Index out of bounds"),
        }
    }
}

fn range_contains<T: PartialOrd>(outer: std::ops::Range<T>, inner: std::ops::Range<T>) -> bool {
    outer.start <= inner.start && outer.end >= inner.end
}

type LineCol = (usize, usize);

trait ExtendedPair {
    fn contains_range(&self, range: Range<LineCol>) -> bool;

    fn is_contained_in_range(&self, range: Range<LineCol>) -> bool;

    fn line_col_range(&self) -> Range<LineCol>;
}

impl<'a> ExtendedPair for Pair<'a, Rule> {
    fn contains_range(&self, range: Range<LineCol>) -> bool {
        range_contains(self.line_col_range(), range)
    }

    fn is_contained_in_range(&self, range: Range<LineCol>) -> bool {
        range_contains(range, self.line_col_range())
    }
    
    fn line_col_range(&self) -> Range<LineCol> {
        let start = self.line_col();
        let end = self.as_span().end_pos().line_col();
        Range { start, end }
    }
}

impl<'a> Program<'a> {

    fn transform_declaration(&self, index: usize, f: impl Fn(&Declaration<'a>) -> Declaration<'a>) -> Program {
        let new_declarations = self.declarations.replace(index, f);
        self.with_declarations(new_declarations)
    }

    pub fn extract_variable(
        &self,
        start_line_column: (usize, usize),
        end_line_column: (usize, usize),
        variable_name: &'a str
    ) -> Program {
        let maybe_declaration_idx = self.declarations.iter().position( |declaration|
            {
                let is_function = match declaration.kind {
                    DeclarationKind::Function(_, _, _, _) => true,
                    _ => false
                };
                let declaration_pair = declaration.pair.clone().unwrap();
                is_function && declaration_pair.contains_range(Range { start: start_line_column, end: end_line_column })
            }
        );
        if maybe_declaration_idx.is_none() {
            return self.clone()
        }
        let declaration_idx = maybe_declaration_idx.unwrap();
        self.transform_declaration(declaration_idx, move |declaration: &Declaration<'a>| {
            match declaration.clone().kind {
                DeclarationKind::Function(function_name, function_type, params, ref statements) => {
                    // HACK
                    let statement_idx = start_line_column.0 - declaration.clone().pair.unwrap().line_col().0 - 1;

                    let mut new_statements = statements.clone();
                    let old_expression = new_statements.replace_mut( statement_idx, |_| Statement::Unknown(variable_name.to_owned()));
                    let new_statement = Statement::VarDeclaration(variable_name, old_expression.as_str());
                    new_statements.insert(statement_idx, new_statement);
                    let new_declaration = Declaration {
                        pair: None,
                        kind: DeclarationKind::Function(
                            function_name,
                            function_type,
                            params,
                            new_statements
                        )
                    };
                    new_declaration
                },
                _ => panic!()
            }
        })
    }

    fn without_pairs(&self) -> Program {
        Program {
            is_tool: self.is_tool,
            super_class: self.super_class.clone(),
            declarations: self.declarations.iter().map(|declaration|declaration.without_pairs()).collect::<Vec<Declaration>>()
        }
    }

    pub fn all_errors(&self) -> Vec<&Declaration<'a>> {
        self.declarations.iter().filter(|declaration|
            match declaration.kind {
                DeclarationKind::Unknown(_) => true,
                _ => false
            }
        ).collect::<Vec<&Declaration<'a>>>()
    }

    pub fn first_error(&self) -> Option<Declaration> {
        self.declarations.iter().find(|declaration|
            match declaration.kind {
                DeclarationKind::Unknown(_) => true,
                _ => false
            }
        ).cloned()
    }

    pub fn is_valid(&self) -> bool {
        self.first_error().is_none()
    }

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
        code_text += "\n";
        code_text
    }

    pub fn move_declaration_down(&self, declaration: Declaration) -> Program {
        let maybe_idx = self.declarations.iter().position(|x| x.kind == declaration.kind);
        match maybe_idx {
            Some(idx) if idx + 1 == self.declarations.len() => {
                let mut updated_declarations = self.declarations.clone();
                updated_declarations.insert(idx, DeclarationKind::EmptyLine.to_declaration(None));
                self.with_declarations(updated_declarations)
            },
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
    
    pub fn toggle_tool_button(&self, function: Declaration<'a>) -> Program<'a> {
        match function.kind {
            DeclarationKind::Function(name, _, _, _) => {
                let mut declarations: Vec<Declaration<'a>> = self.declarations.clone();
                let maybe_idx: Option<usize> = declarations.iter().position(
                    |declaration| declaration.kind == function.kind
                );
                match maybe_idx {
                    None => self.clone(),
                    Some(idx) => {
                        let already_defined_toggle_button_var_idx = declarations.clone().into_iter().position(|declaration|
                            match declaration.kind {
                                DeclarationKind::Var(_, f_name, Some(Annotation { pair: None, kind: AnnotationKind::ExportToolButton(_) })) => f_name == name,
                                _ => false
                            }
                        );
                        match already_defined_toggle_button_var_idx {
                            Some(idx) => {
                                declarations.remove(idx);
                                self.with_declarations(declarations)
                            }
                            None => {
                                let var_name: String = format!("_{}", name);
                                let button_variable: Declaration<'a> = Declaration {
                                    pair: None,
                                    kind: DeclarationKind::Var(
                                        var_name,
                                        name,
                                        Some(Annotation { pair: None, kind: AnnotationKind::ExportToolButton(name) })
                                    )
                                };
                                declarations.insert(idx, button_variable);
                                self.with_declarations(declarations)
                            },
                        }
                    }
                }
            }
            _ => self.clone()
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Declaration<'a> {
    pair: Option<Pair<'a, Rule>>,
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
    name: &'a str, tipe: Option<String>, parameters: Vec<Parameter<'a>>, statements: Vec<Statement<'a>>
}

impl<'a> Function<'a> {
    fn transform_statement(&self, index: usize, f: impl Fn(&Statement) -> Statement<'a>) -> Function<'a> {
        let new_statements = self.statements.replace(index, f);
        self.with_statements(new_statements)
    }

    fn with_statements(&self, new_statements: Vec<Statement<'a>>) -> Function<'a> {
        Function { name: self.name, tipe: self.tipe.clone(), parameters: self.parameters.clone(), statements: new_statements }
    }

    fn to_declaration_kind(&self) -> DeclarationKind<'a> {
        DeclarationKind::Function(self.name, self.tipe.clone(), self.parameters.clone(), self.statements.clone())
    }
}


fn dec_function<'a>(function_name: &'a str, function_type: Option<String>, parameters: Vec<Parameter<'a>>, statements: Vec<Statement<'a>>) -> Declaration<'a> {
    DeclarationKind::Function(function_name, function_type, parameters, statements).to_declaration(None)
}

fn dec_empty_line<'a>() -> Declaration<'a> {
    DeclarationKind::EmptyLine.to_declaration(None)
}

fn dec_var<'a>(identifier: String, value: &'a str, annotation: Option<Annotation<'a>>) -> Declaration<'a> {
    DeclarationKind::Var(identifier, value, annotation).to_declaration(None)
}

fn dec_unknown<'a>(content: &'a str) -> Declaration<'a> {
    DeclarationKind::Unknown(content).to_declaration(None)
}


impl<'a> DeclarationKind<'a> {
    fn to_function(&self) -> Option<Function> {
        match self {
            DeclarationKind::Function(name, tipe, parameters, statements) =>
                Some(Function { name, tipe: tipe.clone(), parameters: parameters.to_vec(), statements: statements.to_vec() }),
            _ => None
        }
    }

    fn to_declaration(&self, pair: Option<Pair<'a, Rule>> ) -> Declaration<'a> {
        Declaration { pair, kind: self.clone() }
    }

    pub fn as_str(&self) -> String {
        self.to_declaration(None).as_str()
    }
    
    fn without_pairs(&self) -> DeclarationKind<'_> {
        match self {
            DeclarationKind::Function(_, _, _, _) => self.clone(),
            DeclarationKind::EmptyLine => self.clone(),
            DeclarationKind::Var(name, value, maybe_annotation) =>
                DeclarationKind::Var(name.to_string(), value, maybe_annotation.clone().map(|annotation| {
                    let this = &annotation;
                    Annotation { pair: None, kind: this.kind.clone() }
                })),
            DeclarationKind::Unknown(_) => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter<'a> {
    pair: Option<Pair<'a, Rule>>,
    name: &'a str
}

impl Parameter<'_> {
    pub fn from(name: &'_ str) -> Parameter {
        Parameter { pair: None, name }
    }
}

fn parameter<'a>(name: &'a str) -> Parameter<'a> {
    Parameter { pair: None, name }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Annotation<'a> {
    pair: Option<Pair<'a, Rule>>,
    kind: AnnotationKind<'a>
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AnnotationKind<'a> {
    Export,
    OnReady,
    ExportToolButton(&'a str)
}

fn annotation_export() -> Annotation<'static> {
    AnnotationKind::Export.to_annotation()
}

fn annotation_onready() -> Annotation<'static> {
    AnnotationKind::OnReady.to_annotation()
}

fn annotation_export_tool_button<'a>(text: &'a str) -> Annotation<'a> {
    AnnotationKind::ExportToolButton(text).to_annotation()
}

impl<'a> AnnotationKind<'a> {
    pub fn to_annotation(&self) -> Annotation<'a> {
        Annotation { pair: None, kind: self.clone() }
    }
}

impl Annotation<'_> {
    pub fn as_str(&self) -> String {
        match self.kind {
            AnnotationKind::Export => "@export".to_string(),
            AnnotationKind::OnReady => "@onready".to_string(),
            AnnotationKind::ExportToolButton(some_string) =>
                "@export_tool_button(\"".to_owned() + some_string + "\")",
        }
    }
    
    fn without_pairs(&self) -> Annotation<'_> {
        Annotation { pair: None, kind: self.kind.clone() }
    }
}

impl<'a> Declaration<'a> {
    pub fn as_str(&self) -> String {
        match &self.kind {
            DeclarationKind::Function(name, return_type, parameters, statements) => {
                let identation = "\t";
                let param_list = parameters
                    .iter()
                    .cloned()
                    .map(|param| param.name)
                    .collect::<Vec<&str>>()
                    .join(", ");
                let function_return_type: String =
                    return_type.clone().map_or_else(|| "".to_string(), |a_type| format!(" -> {}", a_type));
                let function_body = statements
                    .iter()
                    .cloned()
                    .map(|statement| identation.to_owned() + &statement.as_str())
                    .collect::<Vec<String>>()
                    .join("\n");
                format!(
                    "func {fn_name}({param_list}){function_return_type}:\n{body}",
                    fn_name = name,
                    body = function_body
                )
            }
            DeclarationKind::EmptyLine => "".to_string(),
            DeclarationKind::Var(identifier, value, annotation) =>
                format!("{annotation}var {identifier} = {value}", identifier=identifier, value=value,annotation=annotation.clone().map(|x| x.as_str() + " ").unwrap_or("".to_string())),
            DeclarationKind::Unknown(string) => string.to_string(),
        }
    }

    pub fn toggle_annotation(&self, annotation: Annotation<'a>) -> Declaration<'a> {
        match self.kind.clone() {
            DeclarationKind::Var(identifier, value, Some(previous_annotation))
                if previous_annotation.kind == annotation.kind => DeclarationKind::Var(
                    identifier.to_string(), value, None).to_declaration(None),
            DeclarationKind::Var(identifier, value, _) =>
                DeclarationKind::Var(identifier.to_string(), value, Some(annotation)).to_declaration(None),
            _ => self.clone()
        }
    }
    
    fn without_pairs(&self) -> Declaration<'a> {
        Declaration {
            pair: None,
            kind: {
                let this = &self.kind;
                match this {
                    DeclarationKind::Function(_, _, _, _) => this.clone(),
                    DeclarationKind::EmptyLine => this.clone(),
                    DeclarationKind::Var(name, value, maybe_annotation) =>
                        DeclarationKind::Var(name.to_string(), value, maybe_annotation.clone().map(|annotation| {
                            let this = &annotation;
                            Annotation { pair: None, kind: this.kind.clone() }
                        })),
                    DeclarationKind::Unknown(_) => this.clone(),
                }
            }
        }
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<'a> {
    Pass,
    Unknown(String),
    VarDeclaration(&'a str, String)
}


impl<'a> Statement<'a> {
    fn as_str(&self) -> String {
        match self {
            Statement::Pass => "pass".to_string(),
            Statement::Unknown(string) => string.to_string(),
            Statement::VarDeclaration(name, expression) => format!("var {name} = {expression}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use godot::classes::web_socket_peer::State;

    use super::*;

    fn assert_parse_eq(input: &str, expected_result: Program) {
        let parse_result = GDScriptParser::parse(Rule::program, input)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();

        assert_eq!(expected_result, GDScriptParser::to_program(parse_result).without_pairs());
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
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])]
            },
        );
    }

    #[test]
    fn test_program_extract_variable() {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+2\n");
        
        let new_program = program.extract_variable((2,1), (2, 4), "coso");

        assert_eq!("func foo():\n\tvar coso = 2+2\n\tcoso\n", new_program.as_str());
    }

    
    #[test]
    fn test_program_extract_variable_when_the_part_to_extract_is_repeated_in_more_than_one_function() {
        let program = GDScriptParser::parse_to_program("func foo():\n\treturn 2+2\nfunc bar():\n\tself.foo()\n");
        
        let new_program = program.extract_variable((4,0), (4, 4), "x");

        assert_eq!("func foo():\n\treturn 2+2\nfunc bar():\n\tvar x = self.foo()\n\tx\n", new_program.as_str());
    }

    #[test]
    fn test_program_extract_variable_when_the_statement_is_at_the_end_of_the_function() {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+2\n\t3+5");
        
        let new_program = program.extract_variable((3,0), (3, 4), "y");

        assert_eq!("func foo():\n\t2+2\n\tvar y = 3+5\n\ty\n", new_program.as_str());
    }


    #[test]
    fn test_program_with_two_functions() {
        assert_parse_eq(
            "func foo():\n    pass\nfunc bar():\n    pass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                    dec_function("bar", None, vec![], vec![Statement::Pass]),
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
                    dec_function(
                        "foo", None, vec![], vec![Statement::Pass]),
                    dec_empty_line(),
                    dec_function(
                        "bar", None, vec![], vec![Statement::Pass]),
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
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])]
            },
        );
    }

    #[test]
    fn test_function_to_string() {
        assert_eq!(
            dec_function("foo", None, vec![], vec![Statement::Pass]).as_str(),
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
                        dec_function("foo", None, vec![], vec![Statement::Pass]),
                        dec_function("bar", None, vec![], vec![Statement::Pass])
                    ]
            }.as_str(),
            "func foo():\n\tpass\nfunc bar():\n\tpass\n"
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
                    dec_empty_line(),
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                    dec_function("bar", None, vec![], vec![Statement::Pass])
                ]
            }.as_str(),
            "\nfunc foo():\n\tpass\nfunc bar():\n\tpass\n"
        )
    }

    #[test]
    fn move_function_down() {
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), bar()] }.move_declaration_down(foo()),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![bar(), foo()] }
        )
    }

    #[test]
    fn move_function_down_when_it_is_the_last_function_adds_a_newline_before_it() {
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), bar()] }.move_declaration_down(bar()
            ),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), dec_empty_line(), bar()] }
        )
    }

    #[test]
    fn move_function_up_when_it_is_the_first_function_leaves_the_program_as_it_is() {
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), bar()] }.move_declaration_up(foo()
            ),
            Program {
                is_tool: false,
                declarations: vec![foo(), bar()],
                super_class: None,
            }
        )
    }

    #[test]
    fn move_function_down_when_there_is_an_empty_line_between_functions_moves_the_function_into_the_empty_line() {
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
        assert_eq!(
            Program {
                is_tool: false,
                declarations: vec![foo(), dec_empty_line(), bar()],
                super_class: None,
            }.move_declaration_down(foo()),
            Program {
                is_tool: false,
                declarations: vec![dec_empty_line(), foo(), bar()],
                super_class: None,
            }
        )
    }

    #[test]
    fn move_function_down_changes_the_code_so_the_declaration_goes_down() {
        let program = GDScriptParser::parse_to_program(
            "func foo():\n    pass\nfunc bar():\n    pass\n");

        let foo_function = || GDScriptParser::parse_to_declaration("func foo():\n    pass");

        let refactored_program = program.move_declaration_down(foo_function());

        assert_eq!(
            refactored_program.as_str(),
            "func bar():\n\tpass\nfunc foo():\n\tpass\n"
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
            "\nfunc foo():\n\tpass\nfunc bar():\n\tpass\n"
        );

        let program_with_foo_two_times_down = program_with_foo_one_time_down.move_declaration_down(foo_function());
        assert_eq!(
            program_with_foo_two_times_down.as_str(),
            "\nfunc bar():\n\tpass\nfunc foo():\n\tpass\n"
        )
    }

    #[test]
    fn variable_declaration_parses() {
        assert_parse_eq(
            "var x = 2",
            Program {
                is_tool: false,
                declarations: vec![dec_var("x".to_string(),"2",None)],
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
                declarations: vec![dec_var("_x".to_string(),"2",None)],
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
                declarations: vec![dec_var("x".to_string(),"2",Some(annotation_export()))]
            }
        )
    }

    #[test]
    fn toggle_export_annotation_adds_export_if_var_declaration_doesnt_has_it() {
        assert_eq!(
            dec_var("x".to_string(),"2",None).toggle_annotation(annotation_export()),
            dec_var("x".to_string(),"2",Some(annotation_export()))
        )
    }

    #[test]
    fn toggle_export_annotation_changes_annotation_to_export_if_it_is_a_different_one() {
        assert_eq!(
            dec_var("x".to_string(),"2",Some(annotation_onready())).toggle_annotation(annotation_export()),
            dec_var("x".to_string(),"2",Some(annotation_export()))
        )
    }

    
    #[test]
    fn toggle_export_annotation_removes_annotation_if_it_has_export() {
        assert_eq!(
            dec_var("x".to_string(),"2",Some(annotation_export())).toggle_annotation(annotation_export()),
            dec_var("x".to_string(),"2",None)
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
                        dec_function("foo", None, vec![], vec![Statement::Pass])
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
                        dec_function("foo", None, vec![], vec![Statement::Pass])
                        ]
            }.as_str(),
            "@tool\nfunc foo():\n\tpass\n"
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
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])]
            });
    }

    #[test]
    fn tool_program_with_inheritance_as_string() {
        let program_code = Program {
            is_tool: true,
            super_class: Some("Node2D".to_owned()),
            declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])]
        }.as_str();

        assert_eq!(
            program_code,
            "@tool\nextends Node2D\nfunc foo():\n\tpass\n"
        )
    }

    #[test]
    fn tool_program_with_inheritance_roundtrip() {
        assert_parse_roundtrip("@tool\nextends Node2D\nfunc foo():\n\tpass\n");
    }

    #[test]
    fn parsing_program_with_export_tool_button_annotation() {
        let input = "@tool\nextends Node\n@export_tool_button(\"Bleh\") var _foo = foo\nfunc foo():\n\tpass".to_string();

        assert_parse_eq(
            &input,
            Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![
                    dec_var(
                        "_foo".to_string(),
                        "foo",
                        Some(annotation_export_tool_button("Bleh"))
                    ),
                    dec_function("foo", None, vec![],  vec![Statement::Pass])
                ]
            }
        );
    }

    #[test]
    fn toggle_tool_button_on_function() {
        let program = Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![
                    dec_function("foo", None, vec![], vec![Statement::Pass])
                ]
            };

        let refactored_program = program.toggle_tool_button(
            dec_function("foo", None, vec![], vec![Statement::Pass])
        );

        assert_eq!(
            refactored_program,
            Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![
                    dec_var("_foo".to_string(),"foo",Some(AnnotationKind::ExportToolButton("foo").to_annotation())),
                    dec_function("foo", None, vec![], vec![Statement::Pass])
                ]
            });
    }

    #[test]
    fn toggle_tool_button_on_function_from_string_to_string() {
        let input =
        "@tool\nextends Node\n\nfunc foo():\n\tpass\n";

        let program = GDScriptParser::parse_to_program(input);
        let foo = GDScriptParser::parse_to_declaration(
            "func foo():\n\tpass"
        );
        let new_program = program.toggle_tool_button(foo);

        assert_eq!(
            new_program.as_str(),
            "@tool\nextends Node\n\n@export_tool_button(\"foo\") var _foo = foo\nfunc foo():\n\tpass\n"
        );
    }

    #[test]
    fn toggle_tool_button_on_function_that_already_had_a_button_removes_it() {
        let program = Program {
            is_tool: true,
            super_class: Some("Node".to_owned()),
            declarations: vec![
                dec_var("_foo".to_string(),"foo",Some(AnnotationKind::ExportToolButton("foo").to_annotation())),
                dec_function("foo", None, vec![], vec![Statement::Pass])
            ]
        };

        let refactored_program = program.toggle_tool_button(
            dec_function("foo", None, vec![], vec![Statement::Pass])
        );

        assert_eq!(
            refactored_program,
            Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![
                    dec_function("foo", None, vec![], vec![Statement::Pass])
                ]
            }
        )
    }

    #[test]
    fn toggle_tool_button_on_function_that_already_had_a_button_removes_it_even_with_underscores() {
        let input = "@export_tool_button(\"f_oo\") var _f_oo = f_oo\nfunc f_oo():\n\tpass\n";

        let program = GDScriptParser::parse_to_program(input);

        let foo_function = GDScriptParser::parse_to_declaration("func f_oo():\n\tpass\n");
        let new_program = program.toggle_tool_button(foo_function);

        assert_eq!(
            new_program.as_str(),
            "func f_oo():\n\tpass\n"
        )
    }

    #[test]
    fn if_there_are_unknown_lines_they_are_parsed_as_unknwowns() {
        let input = "func foo():\n\tpass\n\nsaracatunga = 3 + 7\n$coso.3\n";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                    dec_empty_line(),
                    dec_unknown("saracatunga = 3 + 7"),
                    dec_unknown("$coso.3")
                ]
            });
    }

    #[test]
    fn if_there_are_unknown_lines_they_are_parsed_back_as_they_were() {
        let input = "func foo():\n\tpass\n\nsaracatunga = 3 + 7\n$coso.3\n";

        assert_parse_roundtrip(input);
    }

    #[test]
    fn parse_function_with_body_even_if_it_has_unknowns() {
        let input = "func foo():\n\tpass\n\tsaracatunga = 3 + 7\n";

        assert_parse_eq(input,
        Program {
            is_tool: false,
            super_class: None,
            declarations: vec![
                dec_function("foo", None, vec![], vec![
                    Statement::Pass,
                    Statement::Unknown("saracatunga = 3 + 7".to_string())
                ])
            ]
        });
    }

    #[test]
    fn parse_functions_with_parameters() {
        let input = "func foo(arg1, arg2):\n\tpass\n";

        assert_parse_eq(input,
        Program {
            is_tool: false,
            super_class: None,
            declarations: vec![
                dec_function("foo",
                None,
                vec![
                    Parameter { pair: None, name: "arg1" },
                    Parameter { pair: None, name: "arg2" }],
                vec![
                    Statement::Pass
                ])
            ]
        });
    }

    #[test]
    fn function_with_parameters_to_string() {
        let program = Program {
            is_tool: false,
            super_class: None,
            declarations: vec![
                dec_function("foo",
                None,
                vec![
                    Parameter { pair: None, name: "arg1" },
                    Parameter { pair: None, name: "arg2" }],
                vec![
                    Statement::Pass
                ])
            ]
        };

        assert_eq!(program.as_str(),
            "func foo(arg1, arg2):\n\tpass\n"
        );
    }

    #[test]
    fn function_with_parameters_roundtrip() {
        assert_parse_roundtrip("func foo(arg1, arg2):\n\tpass\n");
    }

    #[test]
    fn parse_function_with_type() {
        let input = "func foo(arg1, arg2) -> String:\n\tpass\n";

        assert_parse_eq(input,
        Program {
            is_tool: false,
            super_class: None,
            declarations: vec![
                dec_function("foo",
                Some("String".to_string()),
                vec![
                    Parameter { pair: None, name: "arg1" },
                    Parameter { pair: None, name: "arg2" }],
                vec![
                    Statement::Pass
                ])
            ]
        });
    }

    #[test]
    fn function_with_type_to_string() {
        let program = Program {
            is_tool: false,
            super_class: None,
            declarations: vec![
                dec_function("foo",
                Some("String".to_string()),
                vec![
                    Parameter { pair: None, name: "arg1" },
                    Parameter { pair: None, name: "arg2" }],
                vec![
                    Statement::Pass
                ])
            ]
        };

        assert_eq!(program.as_str(), "func foo(arg1, arg2) -> String:\n\tpass\n");
    }

    #[test]
    fn a_program_is_not_valid_if_it_has_unknowns() {
        let program = GDScriptParser::parse_to_program("function foo():\n\tpass");

        assert!(!program.is_valid());
    }

    #[test]
    fn a_program_is_valid_if_it_does_not_have_any_unknown() {
        let program = GDScriptParser::parse_to_program("func foo():\n\tpass");

        assert!(program.is_valid());
    }

    #[test]
    fn a_valid_program_first_error_is_none() {
        let program = GDScriptParser::parse_to_program("func foo():\n\tpass");

        assert_eq!(None, program.first_error());
    }
    

    #[test]
    fn an_invalid_program_first_error_is_its_first_unknown() {
        let program = GDScriptParser::parse_to_program("function foo():\n\tpass");

        assert_eq!(Some(dec_unknown("function foo():")), program.without_pairs().first_error());
    }

    #[test]
    fn identifiers_of_functions_and_variables_can_have_underscores() {
        let input = "func __f_o_o__():\n\tpass\nvar _b_a_r_ = 2";

        assert_parse_eq(input, Program {
            is_tool: false,
            super_class: None,
            declarations: vec![
                dec_function("__f_o_o__", None, vec![], vec![Statement::Pass]),
                dec_var("_b_a_r_".to_string(), "2", None)
            ]
        })
    }

    // #[test]
    // fn extract_function_creates_a_function_from_some_statements() {
    //     let input = "func foo():\n\tpass\n";

    //     let program = GDScriptParser::try_parse_to_program(input);
    //     let refactored_program = program.extract_to_function(vec![2], "bar");

    //     assert_eq!(
    //         refactored_program.as_str(),
    //         "func foo():\n\tself.bar()\nfunc bar():\n\tpass\n";
    //     )
    // }
}
