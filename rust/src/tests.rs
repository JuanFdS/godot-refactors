use crate::godot_ast_types::*;

fn dec_function<'a>(
    function_name: &'a str,
    function_type: Option<String>,
    parameters: Vec<Parameter<'a>>,
    statements: Vec<Statement<'a>>,
) -> Declaration<'a> {
    DeclarationKind::Function(function_name, function_type, parameters, statements)
        .to_declaration(None)
}

fn dec_empty_line<'a>() -> Declaration<'a> {
    DeclarationKind::EmptyLine.to_declaration(None)
}

fn dec_var<'a>(
    identifier: String,
    value: &'a str,
    annotation: Option<Annotation<'a>>,
) -> Declaration<'a> {
    DeclarationKind::Var(identifier, value, annotation).to_declaration(None)
}

fn dec_unknown<'a>(content: &'a str) -> Declaration<'a> {
    DeclarationKind::Unknown(content).to_declaration(None)
}

fn parameter<'a>(name: &'a str) -> Parameter<'a> {
    Parameter { pair: None, name }
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

#[cfg(test)]
mod tests {
    use pest::Parser;

    use crate::parsing::{GDScriptParser, Rule};

    use super::*;

    fn assert_program_prints_to(program: Program, expected_text: &str) {
        assert_eq!(program.to_string().as_str(), expected_text.trim_start())
    }

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

    fn assert_parse_roundtrip(input: &str) {
        let parsed_program: Program<'_> = GDScriptParser::parse_to_program(input);
        let parsed_program_as_string = parsed_program.to_string();

        assert_eq!(input, parsed_program_as_string.as_str());
    }

    #[test]
    fn test_program_with_one_function() {
        assert_parse_eq(
            "func foo():\n    pass",
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])],
            },
        );
    }

    #[test]
    fn test_program_extract_variable() {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+2\n");

        let new_program = program.extract_variable((2, 1), (2, 4), "coso");

        assert_program_prints_to(new_program, "func foo():\n\tvar coso = 2+2\n\tcoso\n");
    }

    #[test]
    fn test_program_extract_variable_when_the_part_to_extract_is_repeated_in_more_than_one_function(
    ) {
        let program = GDScriptParser::parse_to_program(
            "func foo():\n\treturn 2+2\nfunc bar():\n\tself.foo()\n",
        );

        let new_program = program.extract_variable((4, 0), (4, 4), "x");

        assert_program_prints_to(
            new_program,
            "func foo():\n\treturn 2+2\nfunc bar():\n\tvar x = self.foo()\n\tx\n",
        );
    }

    #[test]
    fn test_program_extract_variable_when_the_statement_is_at_the_end_of_the_function() {
        let program = GDScriptParser::parse_to_program("func foo():\n\t2+2\n\t3+5");

        let new_program = program.extract_variable((3, 0), (3, 4), "y");

        assert_program_prints_to(new_program, "func foo():\n\t2+2\n\tvar y = 3+5\n\ty\n");
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
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                    dec_empty_line(),
                    dec_function("bar", None, vec![], vec![Statement::Pass]),
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
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])],
            },
        );
    }

    #[test]
    fn test_function_to_string() {
        assert_eq!(
            dec_function("foo", None, vec![], vec![Statement::Pass]).to_string(),
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
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                    dec_function("bar", None, vec![], vec![Statement::Pass]),
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
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                    dec_function("bar", None, vec![], vec![Statement::Pass]),
                ],
            }.to_string(),
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
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
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
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
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
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
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
        let foo = || dec_function("foo", None, vec![], vec![Statement::Pass]);
        let bar = || dec_function("bar", None, vec![], vec![Statement::Pass]);
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
"
        )
    }

    #[test]
    fn moving_function_around_when_there_are_multiple_empty_lines() {
        let program =
            GDScriptParser::parse_to_program("func foo():\n\tpass\n\nfunc bar():\n\tpass");

        let foo_function = || GDScriptParser::parse_to_declaration("func foo():\n\tpass");

        let program_with_foo_one_time_down = program.move_declaration_down(foo_function());

        assert_eq!(
            program_with_foo_one_time_down.to_string().as_str(),
            "
func foo():
\tpass
func bar():
\tpass
"
        );

        let program_with_foo_two_times_down =
            program_with_foo_one_time_down.move_declaration_down(foo_function());
        assert_eq!(
            program_with_foo_two_times_down.to_string().as_str(),
            "\nfunc bar():\n\tpass\nfunc foo():\n\tpass\n"
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
        assert_parse_eq(
            "@tool\nfunc foo():\n\tpass\n",
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])],
            },
        )
    }

    #[test]
    fn tool_program_to_string() {
        assert_eq!(
            Program {
                is_tool: true,
                super_class: None,
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])]
            }
            .to_string()
            .as_str(),
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
        assert_parse_eq(
            "@tool\nextends Node2D\nfunc foo():\n\tpass",
            Program {
                is_tool: true,
                super_class: Some("Node2D".to_owned()),
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])],
            },
        );
    }

    #[test]
    fn tool_program_with_inheritance_as_string() {
        let program = Program {
            is_tool: true,
            super_class: Some("Node2D".to_owned()),
            declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])],
        };

        assert_program_prints_to(program, "@tool\nextends Node2D\nfunc foo():\n\tpass\n")
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
                        Some(annotation_export_tool_button("Bleh")),
                    ),
                    dec_function("foo", None, vec![], vec![Statement::Pass]),
                ],
            },
        );
    }

    #[test]
    fn toggle_tool_button_on_function() {
        let program = Program {
            is_tool: true,
            super_class: Some("Node".to_owned()),
            declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])],
        };

        let refactored_program =
            program.toggle_tool_button(dec_function("foo", None, vec![], vec![Statement::Pass]));

        assert_eq!(
            refactored_program,
            Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![
                    dec_var(
                        "_foo".to_string(),
                        "foo",
                        Some(AnnotationKind::ExportToolButton("foo").to_annotation())
                    ),
                    dec_function("foo", None, vec![], vec![Statement::Pass])
                ]
            }
        );
    }

    #[test]
    fn toggle_tool_button_on_function_from_string_to_string() {
        let input = "@tool\nextends Node\n\nfunc foo():\n\tpass\n";

        let program = GDScriptParser::parse_to_program(input);
        let foo = GDScriptParser::parse_to_declaration("func foo():\n\tpass");
        let new_program = program.toggle_tool_button(foo);

        assert_program_prints_to(
            new_program,
            "@tool\nextends Node\n\n@export_tool_button(\"foo\") var _foo = foo\nfunc foo():\n\tpass\n"
        );
    }

    #[test]
    fn toggle_tool_button_on_function_that_already_had_a_button_removes_it() {
        let program = Program {
            is_tool: true,
            super_class: Some("Node".to_owned()),
            declarations: vec![
                dec_var(
                    "_foo".to_string(),
                    "foo",
                    Some(AnnotationKind::ExportToolButton("foo").to_annotation()),
                ),
                dec_function("foo", None, vec![], vec![Statement::Pass]),
            ],
        };

        let refactored_program =
            program.toggle_tool_button(dec_function("foo", None, vec![], vec![Statement::Pass]));

        assert_eq!(
            refactored_program,
            Program {
                is_tool: true,
                super_class: Some("Node".to_owned()),
                declarations: vec![dec_function("foo", None, vec![], vec![Statement::Pass])]
            }
        )
    }

    #[test]
    fn toggle_tool_button_on_function_that_already_had_a_button_removes_it_even_with_underscores() {
        let input = "@export_tool_button(\"f_oo\") var _f_oo = f_oo\nfunc f_oo():\n\tpass\n";

        let program = GDScriptParser::parse_to_program(input);

        let foo_function = GDScriptParser::parse_to_declaration("func f_oo():\n\tpass\n");
        let new_program = program.toggle_tool_button(foo_function);

        assert_program_prints_to(new_program, "func f_oo():\n\tpass\n")
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
                    dec_unknown("$coso.3"),
                ],
            },
        );
    }

    #[test]
    fn if_there_are_unknown_lines_they_are_parsed_back_as_they_were() {
        let input = "func foo():\n\tpass\n\nsaracatunga = 3 + 7\n$coso.3\n";

        assert_parse_roundtrip(input);
    }

    #[test]
    fn parse_function_with_body_even_if_it_has_unknowns() {
        let input = "func foo():\n\tpass\n\tsaracatunga = 3 + 7\n";

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
                        Statement::Pass,
                        Statement::Unknown("saracatunga = 3 + 7".to_string()),
                    ],
                )],
            },
        );
    }

    #[test]
    fn parse_functions_with_parameters() {
        let input = "func foo(arg1, arg2):\n\tpass\n";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    None,
                    vec![
                        Parameter {
                            pair: None,
                            name: "arg1",
                        },
                        Parameter {
                            pair: None,
                            name: "arg2",
                        },
                    ],
                    vec![Statement::Pass],
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
                vec![
                    Parameter {
                        pair: None,
                        name: "arg1",
                    },
                    Parameter {
                        pair: None,
                        name: "arg2",
                    },
                ],
                vec![Statement::Pass],
            )],
        };

        assert_program_prints_to(program, "func foo(arg1, arg2):\n\tpass\n");
    }

    #[test]
    fn function_with_parameters_roundtrip() {
        assert_parse_roundtrip("func foo(arg1, arg2):\n\tpass\n");
    }

    #[test]
    fn parse_function_with_type() {
        let input = "func foo(arg1, arg2) -> String:\n\tpass\n";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![dec_function(
                    "foo",
                    Some("String".to_string()),
                    vec![
                        Parameter {
                            pair: None,
                            name: "arg1",
                        },
                        Parameter {
                            pair: None,
                            name: "arg2",
                        },
                    ],
                    vec![Statement::Pass],
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
                vec![
                    Parameter {
                        pair: None,
                        name: "arg1",
                    },
                    Parameter {
                        pair: None,
                        name: "arg2",
                    },
                ],
                vec![Statement::Pass],
            )],
        };

        assert_program_prints_to(program, "func foo(arg1, arg2) -> String:\n\tpass\n");
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

        assert_eq!(
            Some(dec_unknown("function foo():")),
            program.without_pairs().first_error()
        );
    }

    #[test]
    fn identifiers_of_functions_and_variables_can_have_underscores() {
        let input = "func __f_o_o__():\n\tpass\nvar _b_a_r_ = 2";

        assert_parse_eq(
            input,
            Program {
                is_tool: false,
                super_class: None,
                declarations: vec![
                    dec_function("__f_o_o__", None, vec![], vec![Statement::Pass]),
                    dec_var("_b_a_r_".to_string(), "2", None),
                ],
            },
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