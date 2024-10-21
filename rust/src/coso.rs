use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "gdscript.pest"]
pub struct GDScriptParser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_with_just_extends() {
        let node_2d_subclass_script = "extends Node2D\n";

        let parse_result = GDScriptParser::parse(Rule::program, node_2d_subclass_script)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();


        println!("{:?}", parse_result);
        // Pair { rule: program, span: Span { str: "extends Node2D", start: 0, end: 14 }, inner: [] }
        assert_eq!(parse_result.as_rule(), Rule::program);
        assert_eq!(parse_result.as_span().as_str(), "extends Node2D\n");
    }

    #[test]
    fn test_program_with_just_class_name() {
        let node_2d_subclass_script = "class_name MiClase\n";

        let parse_result = GDScriptParser::parse(Rule::program, node_2d_subclass_script)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();


        println!("{:?}", parse_result);
        // Pair { rule: program, span: Span { str: "extends Node2D", start: 0, end: 14 }, inner: [] }
        assert_eq!(parse_result.as_rule(), Rule::program);
        assert_eq!(parse_result.as_span().as_str(), "class_name MiClase");
    }

    #[test]
    fn test_program_with_one_function() {
        let function_definition = "
        func foo():
            pass
        ";

        let parse_result = GDScriptParser::parse(Rule::program, function_definition)
            .expect("Malio sal el parseo")
            .next()
            .unwrap();


        println!("{:?}", parse_result);
        // Pair { rule: program, span: Span { str: "extends Node2D", start: 0, end: 14 }, inner: [] }
        assert_eq!(parse_result.as_rule(), Rule::program);
        assert_eq!(parse_result.as_span().as_str(), function_definition);
    }

}
