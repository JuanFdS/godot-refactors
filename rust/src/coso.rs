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
        Self::parse_to_ast(input, Rule::function, Self::to_declaration)
    }

    pub fn parse_to_statement<'a>(input: &'a str) -> Statement {
        let to_statement = |x| Self::to_statement(x).unwrap();
        Self::parse_to_ast(input, Rule::statement,  to_statement)
    }

    fn to_program(parse_result: Pair<Rule>) -> Program {
        match parse_result.as_rule() {
            Rule::program => {
                let declarations = parse_result
                    .into_inner()
                    .map(|declaration| Self::to_declaration(declaration))
                    .collect();
                Program { declarations }
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
            }
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
    declarations: Vec<Declaration<'a>>,
}

impl Program<'_> {
    pub fn as_str(&self) -> String {
        self.declarations
            .iter()
            .cloned()
            .map(|declaration| declaration.as_str())
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn move_declaration_down(&self, declaration: Declaration) -> Program {
        let maybe_idx = self.declarations.iter().position(|x| *x == declaration).filter(|idx| (idx + 1) < self.declarations.len());
        match maybe_idx {
            Some(idx) => {
                let mut updated_declarations = self.declarations.clone();
                updated_declarations.swap(idx, idx + 1);
                Program { declarations: updated_declarations }
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
                Program { declarations: updated_declarations }
            }
            None => self.clone(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Declaration<'a> {
    // Function(nombre, )
    Function(&'a str, Vec<Statement>),
}

impl Declaration<'_> {
    fn as_str(&self) -> String {
        match self {
            Declaration::Function(name, statements) => {
                let identation = "    ";
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
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Statement {
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

    #[test]
    fn test_program_with_one_function() {
        assert_parse_eq(
            "func foo():\n    pass",
            Program { declarations: vec![Declaration::Function("foo", vec![Statement::Pass])] },
        );
    }

    #[test]
    fn test_program_with_two_functions() {
        assert_parse_eq(
            "func foo():\n    pass\nfunc bar():\n    pass",
            Program {
                declarations: vec![
                    Declaration::Function("foo", vec![Statement::Pass]),
                    Declaration::Function("bar", vec![Statement::Pass]),
                ],
            },
        );
    }

    #[test]
    fn test_function_to_string() {
        assert_eq!(
            Declaration::Function("foo", vec![Statement::Pass]).as_str(),
            "func foo():\n    pass"
        )
    }

    #[test]
    fn program_to_string() {
        assert_eq!(
            Program { declarations: 
                vec![
                    Declaration::Function("foo", vec![Statement::Pass]),
                    Declaration::Function("bar", vec![Statement::Pass])
                ]
            }.as_str(),
            "func foo():\n    pass\nfunc bar():\n    pass"
        )
    }

    #[test]
    fn move_function_down() {
        let foo = Declaration::Function("foo", vec![Statement::Pass]);
        let bar = Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program { declarations: vec![foo.clone(), bar.clone()] }.move_declaration_down(foo.clone()),
            Program { declarations: vec![bar.clone(), foo.clone()] }
        )
    }

    #[test]
    fn move_function_down_when_it_is_the_last_function_leaves_the_program_as_it_is() {
        let foo = Declaration::Function("foo", vec![Statement::Pass]);
        let bar = Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program { declarations: vec![foo.clone(), bar.clone()] }.move_declaration_down(bar.clone()),
            Program { declarations: vec![foo.clone(), bar.clone()] }
        )
    }

    #[test]
    fn move_function_up_when_it_is_the_first_function_leaves_the_program_as_it_is() {
        let foo = Declaration::Function("foo", vec![Statement::Pass]);
        let bar = Declaration::Function("bar", vec![Statement::Pass]);
        assert_eq!(
            Program { declarations: vec![foo.clone(), bar.clone()] }.move_declaration_up(foo.clone()),
            Program { declarations: vec![foo.clone(), bar.clone()] }
        )
    }

    #[test]
    fn move_function_up_changes_the_code_so_the_declaration_goes_up() {
        let program = GDScriptParser::parse_to_program("func foo():\n    pass\nfunc bar():\n    pass");

        let foo_function = GDScriptParser::parse_to_declaration("func foo():\n    pass");

        let refactored_program = program.move_declaration_down(foo_function);

        assert_eq!(
            refactored_program.as_str(),
            "func bar():\n    pass\nfunc foo():\n    pass"
        )

    }
}
