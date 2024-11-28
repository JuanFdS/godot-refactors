use crate::godot_ast_types::*;
use text_block_macros::text_block_fnl;

fn create_program(declarations: Vec<Declaration>) -> Program {
    Program {
        is_tool: false,
        super_class: None,
        declarations,
    }
}

fn expr_message_send<'a>(
    receiver: Expression,
    message_name: &'a str,
    arguments: Vec<Expression>,
) -> Expression {
    Expression::new(
        None,
        ExpressionKind::MessageSend(Box::new(receiver), message_name.into(), arguments),
    )
}

fn statement_empty_return<'a>() -> Statement {
    Statement::new(None, StatementKind::Return(None))
}

fn statement_return<'a>(returned_expression: Expression) -> Statement {
    Statement::new(None, StatementKind::Return(Some(returned_expression)))
}

fn expr_self<'a>() -> Expression {
    Expression::new(None, ExpressionKind::LiteralSelf)
}

fn expr_variable_usage<'a>(variable_name: String) -> Expression {
    expr_unknown(variable_name)
}

fn expr_unknown<'a>(text: String) -> Expression {
    Expression::new(None, ExpressionKind::Unknown(text))
}

fn expression_binary_op<'a>(a: Expression, op: &'a str, b: Expression) -> Expression {
    Expression::new(
        None,
        ExpressionKind::BinaryOperation(Box::new(a), op.into(), Box::new(b)),
    )
}

fn literal_int<'a>(number: usize) -> Expression {
    Expression::new(None, ExpressionKind::LiteralInt(number))
}

fn _statement_with_kind<'a>(kind: StatementKind) -> Statement {
    kind.to_statement(None)
}

fn statement_expression(expression: Expression) -> Statement {
    _statement_with_kind(StatementKind::Expression(expression))
}

fn statement_pass<'a>() -> Statement {
    _statement_with_kind(StatementKind::Pass)
}

fn statement_var_declaration<'a>(name: &'a str, expression: Expression) -> Statement {
    _statement_with_kind(StatementKind::VarDeclaration(name.into(), expression))
}

fn dec_function(
    function_name: &str,
    function_type: Option<String>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
) -> Declaration {
    DeclarationKind::Function {
        name: function_name.into(),
        return_type: function_type,
        parameters,
        statements,
    }
    .to_declaration(None)
}

fn dec_empty_line<'a>() -> Declaration {
    DeclarationKind::EmptyLine.to_declaration(None)
}

fn dec_var(identifier: String, value: &str, annotation: Option<Annotation>) -> Declaration {
    DeclarationKind::Var {
        identifier,
        value: value.into(),
        annotation,
    }
    .to_declaration(None)
}

fn dec_unknown(content: &str) -> Declaration {
    DeclarationKind::Unknown(content.into()).to_declaration(None)
}

fn parameter(name: &str) -> Parameter {
    Parameter::new(None, name)
}

fn annotation_export() -> Annotation {
    AnnotationKind::Export.to_annotation()
}

fn annotation_onready() -> Annotation {
    AnnotationKind::OnReady.to_annotation()
}

fn annotation_export_tool_button<'a>(text: &'a str) -> Annotation {
    AnnotationKind::ExportToolButton(text.into()).to_annotation()
}

#[cfg(test)]
mod tests {
    use pest::Parser;
    // use pretty_assertions::assert_eq;

    use crate::parsing::{GDScriptParser, Rule};

    use super::*;

    #[track_caller]
    fn assert_program_prints_to(program: Program, expected_text: &str) {
        assert_eq!(program.to_string().as_str(), expected_text.trim_start())
    }

    #[track_caller]
    fn assert_parse_eq(input: &str, expected_result: Program) {
        let parse_result = GDScriptParser::parse(Rule::program, input)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();

        assert_eq!(
            expected_result,
            GDScriptParser::to_program(parse_result).without_pairs()
        );
    }

    #[track_caller]
    fn assert_parse_roundtrip(input: &str) {
        let parsed_program: Program = GDScriptParser::parse_to_program(input);
        let parsed_program_as_string = parsed_program.to_string();

        assert_eq!(input, parsed_program_as_string.as_str());
    }

    #[test]
    fn test_program_with_one_function() {
        let input = text_block_fnl! {
            "func foo():"
            "    pass"
        };
        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![statement_pass()])],
            },
        );
    }

    #[test]
    fn test_program_extract_variable() {
        let input = text_block_fnl! {
          "func foo():"
          "\t2+2"
        };
        let program = GDScriptParser::parse_to_program(input);

        let (new_program, lines_to_select) = program.extract_variable((1, 1), (1, 3), "coso");

        let expected = text_block_fnl! {
          "func foo():"
          "\tvar coso = 2 + 2"
          "\tcoso"
        };
        assert_program_prints_to(new_program, expected);
        assert_eq!(lines_to_select, vec![(1, 5)..(1, 9), (2, 1)..(2, 5)]);
    }

    #[test]
    fn test_parse_function_with_empty_return() {
        let input = "func foo():\n\treturn\n";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    None,
                    vec![],
                    vec![statement_empty_return()],
                )],
            },
        );
        assert_parse_roundtrip(input);
    }

    #[test]
    fn test_parse_function_with_message_send_in_its_body() {
        let input = "func foo():\n\treturn 2 + 2\nfunc bar():\n\tself.foo()\n";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function(
                        "foo",
                        None,
                        vec![],
                        vec![statement_return(expression_binary_op(
                            literal_int(2),
                            "+",
                            literal_int(2),
                        ))],
                    ),
                    dec_function(
                        "bar",
                        None,
                        vec![],
                        vec![statement_expression(expr_message_send(
                            expr_self(),
                            "foo",
                            vec![],
                        ))],
                    ),
                ],
            },
        );

        assert_parse_roundtrip(input);
    }

    #[test]
    fn test_program_extract_variable_when_the_part_to_extract_is_repeated_in_more_than_one_function(
    ) {
        let program = GDScriptParser::parse_to_program(
            "func foo():\n\treturn 2 + 2\nfunc bar():\n\tself.foo()\n",
        );

        let (new_program, ranges) = program.extract_variable((3, 1), (3, 7), "x");

        assert_program_prints_to(
            new_program,
            "func foo():\n\treturn 2 + 2\nfunc bar():\n\tvar x = self.foo()\n\tx\n",
        );
        assert_eq!(ranges, vec![(3, 5)..(3, 6), (4, 1)..(4, 2)])
    }

    #[test]
    fn test_program_extract_variable_from_receiver_of_a_message_send() {
        let program = GDScriptParser::parse_to_program(
            "func foo():\n\treturn 2 + 2\nfunc bar():\n\tself.foo()\n",
        );

        let (new_program, ranges) = program.extract_variable((3, 1), (3, 3), "x");

        assert_program_prints_to(
            new_program,
            "func foo():\n\treturn 2 + 2\nfunc bar():\n\tvar x = self\n\tx.foo()\n",
        );

        assert_eq!(ranges, vec![(3, 5)..(3, 6), (4, 1)..(4, 2)])
    }

    #[test]
    fn test_program_extract_variable_that_is_being_returned() {
        let program = GDScriptParser::parse_to_program("func foo():\n\treturn 2 + 2\n");

        let (new_program, ranges) = program.extract_variable((1, 8), (1, 11), "x");

        assert_program_prints_to(new_program, "func foo():\n\tvar x = 2 + 2\n\treturn x\n");
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 8)..(2, 9)])
    }

    #[test]
    fn test_extract_variable_first_binary_operation_within_parentheses() {
        let input = text_block_fnl! {
          "func foo():"
          "\t(2 + 3) + 4"
        };
        let program = GDScriptParser::parse_to_program(input);

        let (new_program, ranges) = program.extract_variable((1, 2), (1, 5), "x");

        let expected = text_block_fnl! {
          "func foo():"
          "\tvar x = 2 + 3"
          "\tx + 4"
        };
        assert_program_prints_to(new_program, expected);
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 1)..(2, 2)])
    }

    #[test]
    fn test_extract_variable_when_first_member_of_binary_operation_is_message_send() {
        let program = GDScriptParser::parse_to_program(text_block_fnl! {
            "func foo():"
            "\tself.bar() + 4"
        });

        let (new_program, ranges) = program.extract_variable((1, 1), (1, 10), "x");

        assert_program_prints_to(
            new_program,
            text_block_fnl! {
                "func foo():"
                "\tvar x = self.bar()"
                "\tx + 4"
            },
        );
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 1)..(2, 2)])
    }

    #[test]
    fn test_inline_variable_into_first_member_of_binary_operation() {
        let input = text_block_fnl! {
            "func foo():"
            "\tvar x = 4"
            "\tx + 2"
        };

        let program = GDScriptParser::parse_to_program(input);

        let new_program = program.inline_variable((1, 5), (1, 5)).0;

        let expected = text_block_fnl! {
          "func foo():"
          "\t4 + 2"
        };
        assert_program_prints_to(new_program, expected);
    }

    #[test]
    fn test_program_extract_variable_that_is_a_subexpression_of_what_is_being_returned() {
        let program = GDScriptParser::parse_to_program("func foo():\n\treturn 2 + 2\n");

        let (new_program, ranges) = program.extract_variable((1, 8), (1, 8), "x");

        assert_program_prints_to(new_program, "func foo():\n\tvar x = 2\n\treturn x + 2\n");
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 8)..(2, 9)])
    }

    #[test]
    fn test_program_parses_return_with_integer_and_message_send() {
        let input = "func foo():\n\treturn 2 + self.bar()\n";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    None,
                    vec![],
                    vec![statement_return(expression_binary_op(
                        literal_int(2),
                        "+",
                        expr_message_send(expr_self(), "bar", vec![]),
                    ))],
                )],
            },
        );

        assert_parse_roundtrip(input);
    }

    #[test]
    fn test_program_extract_variable_when_returning_the_addition_of_an_int_and_a_message_send() {
        let program = GDScriptParser::parse_to_program("func foo():\n\treturn 2 + self.bar()\n");

        let (new_program, ranges) = program.extract_variable((1, 8), (1, 8), "dos");

        assert_program_prints_to(
            new_program,
            "func foo():\n\tvar dos = 2\n\treturn dos + self.bar()\n",
        );
        assert_eq!(ranges, vec![(1, 5)..(1, 8), (2, 8)..(2, 11)])
    }

    #[test]
    fn test_parse_var_declaration_inside_function() {
        let input = "func foo():\n\tvar x = 2\n";

        assert_parse_eq(
            input,
            create_program(vec![dec_function(
                "foo",
                None,
                vec![],
                vec![statement_var_declaration("x", literal_int(2))],
            )]),
        );
        assert_parse_roundtrip(input);
    }

    #[test]
    fn test_program_extract_variable_inside_a_var_declaration() {
        let program = GDScriptParser::parse_to_program("func foo():\n\tvar x = 2\n\treturn x\n");

        let (new_program, ranges) = program.extract_variable((1, 9), (1, 9), "y");

        assert_program_prints_to(
            new_program,
            "func foo():\n\tvar y = 2\n\tvar x = y\n\treturn x\n",
        );
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 9)..(2, 10)])
    }

    #[test]
    fn test_program_extract_variable_extract_subexpression_of_subexpression() {
        let program = GDScriptParser::parse_to_program("func foo():\n\treturn 2 + self.bar()\n");

        let (new_program, ranges) = program.extract_variable((1, 12), (1, 15), "x");

        assert_program_prints_to(
            new_program,
            "func foo():\n\tvar x = self\n\treturn 2 + x.bar()\n",
        );
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 12)..(2, 13)])
    }

    #[test]
    fn test_program_extract_variable_when_the_statement_is_at_the_end_of_the_function() {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+2\n\t3+5");

        let (new_program, ranges) = program.extract_variable((2, 1), (2, 3), "y");

        assert_program_prints_to(new_program, "func foo():\n\t2 + 2\n\tvar y = 3 + 5\n\ty\n");
        assert_eq!(ranges, vec![(2, 5)..(2, 6), (3, 1)..(3, 2)])
    }

    #[test]
    fn parse_expression_with_binary_operation_and_literal_numbers() {
        let input = "func foo():\n\t2 + 3\n";
        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    None,
                    vec![],
                    vec![statement_expression(expression_binary_op(
                        literal_int(2),
                        "+",
                        literal_int(3),
                    ))],
                )],
            },
        );

        assert_parse_roundtrip(input);
    }

    #[test]
    fn test_program_extract_variable_lets_me_extract_a_subexpression() {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+3\n");

        let (new_program, ranges) = program.extract_variable((1, 1), (1, 1), "y");

        assert_program_prints_to(
            new_program,
            "
func foo():
\tvar y = 2
\ty + 3
",
        );
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 1)..(2, 2)])
    }

    #[test]
    fn test_program_extract_variable_lets_me_extract_a_subexpression_even_if_its_the_rightmost_expression(
    ) {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+3\n");

        let (new_program, ranges) = program.extract_variable((1, 3), (1, 3), "y");

        assert_program_prints_to(
            new_program,
            "
func foo():
\tvar y = 3
\t2 + y
",
        );
        assert_eq!(ranges, vec![(1, 5)..(1, 6), (2, 3)..(2, 4)])
    }

    #[test]
    fn test_program_inline_variable() {
        let input = "func foo():\n\tvar suma = 2 + 3\n\tsuma\n";
        let program = GDScriptParser::parse_to_program(input);

        assert_parse_eq(
            input,
            create_program(vec![dec_function(
                "foo",
                None,
                vec![],
                vec![
                    statement_var_declaration(
                        "suma",
                        expression_binary_op(literal_int(2), "+", literal_int(3)),
                    ),
                    statement_expression(expr_variable_usage("suma".to_string())),
                ],
            )]),
        );

        let new_program = program.inline_variable((1, 5), (1, 8)).0;

        assert_program_prints_to(new_program, "func foo():\n\t2 + 3\n");
    }

    #[test]
    fn test_program_inline_variable_works_when_there_are_2_functions() {
        let input = "func bar():\n\tpass\nfunc foo():\n\tvar suma = 2 + 3\n\tsuma\n";
        let program = GDScriptParser::parse_to_program(input);

        let new_program = program.inline_variable((3, 5), (3, 8)).0;

        assert_program_prints_to(new_program, "func bar():\n\tpass\nfunc foo():\n\t2 + 3\n");
    }
    #[test]
    fn test_program_inline_variable_inside_return() {
        let input = "func foo():\n\tvar suma = 2 + 3\n\treturn suma\n";
        let program = GDScriptParser::parse_to_program(input);

        assert_parse_eq(
            input,
            create_program(vec![dec_function(
                "foo",
                None,
                vec![],
                vec![
                    statement_var_declaration(
                        "suma",
                        expression_binary_op(literal_int(2), "+", literal_int(3)),
                    ),
                    statement_return(expr_variable_usage("suma".to_string())),
                ],
            )]),
        );

        let new_program = program.inline_variable((1, 5), (1, 8)).0;

        assert_program_prints_to(new_program, "func foo():\n\treturn 2 + 3\n");
    }

    #[test]
    fn test_program_with_two_functions() {
        assert_parse_eq(
            "func foo():\n    pass\nfunc bar():\n    pass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("foo", None, vec![], vec![statement_pass()]),
                    dec_function("bar", None, vec![], vec![statement_pass()]),
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
                    dec_function("foo", None, vec![], vec![statement_pass()]),
                    dec_empty_line(),
                    dec_function("bar", None, vec![], vec![statement_pass()]),
                ],
            },
        )
    }

    #[test]
    fn tabs_are_valid_identation() {
        assert_parse_eq(
            "func foo():\n\tpass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![statement_pass()])],
            },
        );
    }

    #[test]
    fn test_function_to_string() {
        assert_eq!(
            dec_function("foo", None, vec![], vec![statement_pass()]).to_string(),
            "func foo():\n\tpass".to_string()
        )
    }

    #[test]
    fn program_to_string() {
        assert_program_prints_to(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("foo", None, vec![], vec![statement_pass()]),
                    dec_function("bar", None, vec![], vec![statement_pass()]),
                ],
            },
            "func foo():\n\tpass\nfunc bar():\n\tpass\n",
        )
    }

    #[test]
    fn program_with_newline_to_string() {
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_empty_line(),
                    dec_function("foo", None, vec![], vec![statement_pass()]),
                    dec_function("bar", None, vec![], vec![statement_pass()]),
                ],
            }
            .to_string(),
            "
func foo():
\tpass
func bar():
\tpass
",
        )
    }

    #[test]
    fn move_function_down() {
        let foo = || dec_function("foo", None, vec![], vec![statement_pass()]);
        let bar = || dec_function("bar", None, vec![], vec![statement_pass()]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), bar()]
            }
            .move_declaration_down(foo()),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![bar(), foo()]
            }
        )
    }

    #[test]
    fn move_function_down_when_it_is_the_last_function_adds_a_newline_before_it() {
        let foo = || dec_function("foo", None, vec![], vec![statement_pass()]);
        let bar = || dec_function("bar", None, vec![], vec![statement_pass()]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), bar()]
            }
            .move_declaration_down(bar()),
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), dec_empty_line(), bar()]
            }
        )
    }

    #[test]
    fn move_function_up_when_it_is_the_first_function_leaves_the_program_as_it_is() {
        let foo = || dec_function("foo", None, vec![], vec![statement_pass()]);
        let bar = || dec_function("bar", None, vec![], vec![statement_pass()]);
        assert_eq!(
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![foo(), bar()]
            }
            .move_declaration_up(foo()),
            Program {
                is_tool: false,
                declarations: vec![foo(), bar()],
                super_class: None,
            }
        )
    }

    #[test]
    fn move_function_down_when_there_is_an_empty_line_between_functions_moves_the_function_into_the_empty_line(
    ) {
        let foo = || dec_function("foo", None, vec![], vec![statement_pass()]);
        let bar = || dec_function("bar", None, vec![], vec![statement_pass()]);
        assert_eq!(
            Program {
                is_tool: false,
                declarations: vec![foo(), dec_empty_line(), bar()],
                super_class: None,
            }
            .move_declaration_down(foo()),
            Program {
                is_tool: false,
                declarations: vec![dec_empty_line(), foo(), bar()],
                super_class: None,
            }
        )
    }

    #[test]
    fn move_function_down_changes_the_code_so_the_declaration_goes_down() {
        let program =
            GDScriptParser::parse_to_program("func foo():\n    pass\nfunc bar():\n    pass\n");

        let foo_function = || GDScriptParser::parse_to_declaration("func foo():\n    pass");

        let refactored_program = program.move_declaration_down(foo_function());

        assert_program_prints_to(
            refactored_program,
            "
func bar():
\tpass
func foo():
\tpass
",
        )
    }

    #[test]
    fn moving_function_around_when_there_are_multiple_empty_lines_once() {
        let program = GDScriptParser::parse_to_program(
            "
func foo():
\tpass

func bar():
\tpass",
        );

        let foo_function = program.find_function_declaration_at_line(2).unwrap();

        let program_with_foo_one_time_down = program.move_declaration_down(foo_function.clone());

        assert_eq!(
            program_with_foo_one_time_down.to_string().as_str(),
            "

func foo():
\tpass
func bar():
\tpass
"
        );
    }

    #[test]
    fn moving_function_around_when_there_are_multiple_empty_lines_twice() {
        let program = GDScriptParser::parse_to_program(
            "

func foo():
\tpass
func bar():
\tpass",
        );

        let foo_function = program.find_function_declaration_at_line(3).unwrap();

        let program_with_foo_two_times_down = program.move_declaration_down(foo_function.clone());
        assert_eq!(
            program_with_foo_two_times_down.to_string().as_str(),
            "

func bar():
\tpass
func foo():
\tpass
"
        )
    }

    #[test]
    fn variable_declaration_parses() {
        assert_parse_eq(
            "var x = 2",
            Program {
                is_tool: false,
                declarations: vec![dec_var("x".to_string(), "2", None)],
                super_class: None,
            },
        )
    }

    #[test]
    fn variable_can_start_with_underscore() {
        assert_parse_eq(
            "var _x = 2",
            Program {
                is_tool: false,
                declarations: vec![dec_var("_x".to_string(), "2", None)],
                super_class: None,
            },
        )
    }

    #[test]
    fn variable_declaration_parses_even_with_annotations() {
        assert_parse_eq(
            "@export var x = 2",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_var("x".to_string(), "2", Some(annotation_export()))],
            },
        )
    }

    #[test]
    fn toggle_export_annotation_adds_export_if_var_declaration_doesnt_has_it() {
        assert_eq!(
            dec_var("x".to_string(), "2", None).toggle_annotation(annotation_export()),
            dec_var("x".to_string(), "2", Some(annotation_export()))
        )
    }

    #[test]
    fn toggle_export_annotation_changes_annotation_to_export_if_it_is_a_different_one() {
        assert_eq!(
            dec_var("x".to_string(), "2", Some(annotation_onready()))
                .toggle_annotation(annotation_export()),
            dec_var("x".to_string(), "2", Some(annotation_export()))
        )
    }

    #[test]
    fn toggle_export_annotation_removes_annotation_if_it_has_export() {
        assert_eq!(
            dec_var("x".to_string(), "2", Some(annotation_export()))
                .toggle_annotation(annotation_export()),
            dec_var("x".to_string(), "2", None)
        )
    }

    #[test]
    fn program_can_be_tool() {
        let input = text_block_fnl! {
            "@tool"
            "func foo():"
            "\tpass"
        };
        assert_parse_eq(
            input,
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![statement_pass()])],
            },
        )
    }

    #[test]
    fn tool_program_to_string() {
        let expected = text_block_fnl! {
         "@tool"
         "func foo():"
         "\tpass"
        };
        assert_eq!(
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![statement_pass()])]
            }
            .to_string()
            .as_str(),
            expected
        )
    }

    #[test]
    fn toggle_tool_on_a_tool_program_makes_it_nontool() {
        assert_eq!(
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![]
            }
            .toggle_tool(),
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
            }
            .toggle_tool(),
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![]
            }
        )
    }

    #[test]
    fn a_tool_program_with_inheritance_can_be_parsed() {
        let input = text_block_fnl! {
            "@tool"
            "extends Node2D"
            "func foo():"
            "\tpass"
        };
        assert_parse_eq(
            input,
            Program {
                is_tool: true,
                super_class: Some("Node2D".to_owned()),
                declarations: vec![dec_function("foo", None, vec![], vec![statement_pass()])],
            },
        );
    }

    #[test]
    fn tool_program_with_inheritance_as_string() {
        let program = Program {
            is_tool: true,
            super_class: Some("Node2D".to_owned()),
            declarations: vec![dec_function("foo", None, vec![], vec![statement_pass()])],
        };

        let expected = text_block_fnl! {
            "@tool"
            "extends Node2D"
            "func foo():"
            "\tpass"
        };
        assert_program_prints_to(program, expected)
    }

    #[test]
    fn tool_program_with_inheritance_roundtrip() {
        let input = text_block_fnl! {
         "@tool"
         "extends Node2D"
         "func foo():"
         "\tpass"
        };
        assert_parse_roundtrip(input);
    }

    #[test]
    fn parsing_program_with_export_tool_button_annotation() {
        let input = text_block_fnl! {
            "@tool"
            "extends Node"
            "@export_tool_button(\"Bleh\") var _foo = foo"
            "func foo():"
            "\tpass"
        }
        .to_string();

        assert_parse_eq(
            &input,
            Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![
                    dec_var(
                        "_foo".to_string(),
                        "foo",
                        Some(annotation_export_tool_button("Bleh")),
                    ),
                    dec_function("foo", None, vec![], vec![statement_pass()]),
                ],
            },
        );
    }

    #[test]
    fn toggle_tool_button_on_function() {
        let input = text_block_fnl! {
            "@tool"
            "extends Node"
            "func foo():"
            "\tpass"
        };
        let program = GDScriptParser::parse_to_program(input);

        let refactored_program =
            program.toggle_tool_button(program.find_function_declaration_at_line(3).unwrap());

        let expected = text_block_fnl! {
            "@tool"
            "extends Node"
            "@export_tool_button(\"foo\") var _foo = foo"
            "func foo():"
            "\tpass"
        };
        assert_eq!(refactored_program.to_string(), expected);
    }

    #[test]
    fn toggle_tool_button_on_function_from_string_to_string() {
        let input = text_block_fnl! {
            "@tool"
            "extends Node"
            ""
            "func foo():"
            "\tpass"
        };

        let program = GDScriptParser::parse_to_program(input);
        let function = program.find_function_declaration_at_line(4).unwrap();

        let new_program = program.toggle_tool_button(function);

        let expected = text_block_fnl! {
          "@tool"
          "extends Node"
          ""
          "@export_tool_button(\"foo\") var _foo = foo"
          "func foo():"
          "\tpass"
        };
        assert_program_prints_to(new_program, expected);
    }

    #[test]
    fn toggle_tool_button_on_function_that_already_had_a_button_removes_it() {
        let input = text_block_fnl! {
            "@tool"
            "extends Node"
            "@export_tool_button(\"foo\") var _foo = foo"
            "func foo():"
            "\tpass"
        };

        let program = GDScriptParser::parse_to_program(input);

        let refactored_program =
            program.toggle_tool_button(program.find_function_declaration_at_line(4).unwrap());

        let expected = text_block_fnl! {
          "@tool"
          "extends Node"
          "func foo():"
          "\tpass"
        };
        assert_eq!(refactored_program.to_string(), expected);
    }

    #[test]
    fn toggle_tool_button_on_function_that_already_had_a_button_removes_it_even_with_underscores() {
        let input = text_block_fnl! {
            "@export_tool_button(\"f_oo\") var _f_oo = f_oo"
            "func f_oo():"
            "\tpass"
        };
        let program = GDScriptParser::parse_to_program(input);
        let function = program.find_function_declaration_at_line(2).unwrap();

        let new_program = program.toggle_tool_button(function);

        assert_program_prints_to(new_program, "func f_oo():\n\tpass\n")
    }

    #[test]
    fn if_there_are_unknown_lines_they_are_parsed_as_unknwowns() {
        let input = text_block_fnl! {
            "func foo():"
            "\tpass"
            ""
            "saracatunga = 3 + 7"
            "$coso.3"
        };

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("foo", None, vec![], vec![statement_pass()]),
                    dec_empty_line(),
                    dec_unknown("saracatunga = 3 + 7"),
                    dec_unknown("$coso.3"),
                ],
            },
        );
    }

    #[test]
    fn if_there_are_unknown_lines_they_are_parsed_back_as_they_were() {
        let input = text_block_fnl! {
            "func foo():"
            "\tpass"
            ""
            "saracatunga = 3 + 7"
            "$coso.3"
        };

        assert_parse_roundtrip(input);
    }

    #[test]
    fn parse_function_with_body_even_if_it_has_unknowns() {
        let input = text_block_fnl! {
            "func foo():"
            "\tpass"
            "\tsaracatunga = 3 + 7"
        };

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    None,
                    vec![],
                    vec![
                        statement_pass(),
                        statement_expression(expr_unknown("saracatunga = 3 + 7".to_string())),
                    ],
                )],
            },
        );
    }

    #[test]
    fn parse_functions_with_parameters() {
        let input = text_block_fnl! {
            "func foo(arg1, arg2):"
            "\tpass"
        };

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    None,
                    vec![parameter("arg1"), parameter("arg2")],
                    vec![statement_pass()],
                )],
            },
        );
    }

    #[test]
    fn function_with_parameters_to_string() {
        let program = Program {
            is_tool: false,
            super_class: None,
            declarations: vec![dec_function(
                "foo",
                None,
                vec![parameter("arg1"), parameter("arg2")],
                vec![statement_pass()],
            )],
        };

        let expected = text_block_fnl! {
            "func foo(arg1, arg2):"
            "\tpass"
        };
        assert_program_prints_to(program, expected);
    }

    #[test]
    fn function_with_parameters_roundtrip() {
        let input = text_block_fnl! {
            "func foo(arg1, arg2):"
            "\tpass"
        };
        assert_parse_roundtrip(input);
    }

    #[test]
    fn parse_function_with_type() {
        let input = text_block_fnl! {
            "func foo(arg1, arg2) -> String:"
            "\tpass"
        };

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    Some("String".to_string()),
                    vec![parameter("arg1"), parameter("arg2")],
                    vec![statement_pass()],
                )],
            },
        );
    }

    #[test]
    fn function_with_type_to_string() {
        let program = Program {
            is_tool: false,
            super_class: None,
            declarations: vec![dec_function(
                "foo",
                Some("String".to_string()),
                vec![parameter("arg1"), parameter("arg2")],
                vec![statement_pass()],
            )],
        };

        let expected = text_block_fnl! {
            "func foo(arg1, arg2) -> String:"
            "\tpass"
        };

        assert_program_prints_to(program, expected);
    }

    #[test]
    fn a_program_is_not_valid_if_it_has_unknowns() {
        let input = text_block_fnl! {
            "function foo():"
            "\tpass"
        };
        let program = GDScriptParser::parse_to_program(input);

        assert!(!program.is_valid());
    }

    #[test]
    fn a_program_is_valid_if_it_does_not_have_any_unknown() {
        let input = text_block_fnl! {
            "func foo():"
            "\tpass"
        };
        let program = GDScriptParser::parse_to_program(input);

        assert!(program.is_valid());
    }

    #[test]
    fn a_valid_program_first_error_is_none() {
        let input = text_block_fnl! {
            "func foo():"
            "\tpass"
        };
        let program = GDScriptParser::parse_to_program(input);

        assert_eq!(None, program.first_error());
    }

    #[test]
    fn an_invalid_program_first_error_is_its_first_unknown() {
        let input = text_block_fnl! {
            "function foo():"
            "\tpass"
        };

        let program = GDScriptParser::parse_to_program(input);

        assert_eq!(
            Some(dec_unknown("function foo():")),
            program.without_pairs().first_error()
        );
    }

    #[test]
    fn identifiers_of_functions_and_variables_can_have_underscores() {
        let input = text_block_fnl! {
            "func __f_o_o__():"
            "\tpass"
            "var _b_a_r_ = 2"
        };

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("__f_o_o__", None, vec![], vec![statement_pass()]),
                    dec_var("_b_a_r_".to_string(), "2", None),
                ],
            },
        )
    }

    #[test]
    #[ignore]
    fn get_ocurrences_of_variable_returns_a_list_of_text_ranges_where_the_variable_is_declared_and_used(
    ) {
        let input = text_block_fnl! {
           "func foo():"
            "\tvar bar = 2"
            "\tvar x = 3 + bar"
            "\treturn bar - bar"
        };

        let program = GDScriptParser::parse_to_program(input);
        let occurrences = program.get_ocurrences_of_variable((2, 6), (2, 8));
        assert_eq!(
            occurrences,
            vec![(2, 6)..(2, 8), (3, 9)..(3, 11), (3, 15)..(3, 17)]
        )
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
