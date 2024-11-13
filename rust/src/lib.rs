mod refactorings;
mod tests;
mod godot_ast_types;
mod parsing;
mod printing;

use godot_ast_types::*;
use godot::prelude::*;
use parsing::GDScriptParser;


struct MyExtension;

#[gdextension]
unsafe impl ExtensionLibrary for MyExtension {}

#[derive(GodotClass)]
#[class(tool, init, base=Node)]
struct MiParser {

}

#[godot_api]
impl MiParser {
    #[func]
    fn try_parse_program(&self, programa: String) -> Array<GString> {
        match GDScriptParser::try_parse_to_program(&programa) {
            Ok(program) => {
                program.all_errors().iter().map(|declaration| GString::from(declaration.to_string()))
                    .collect::<Array<GString>>()
            },
            Err(error) => array![GString::from(error.line())],
        }
    }

    #[func]
    fn extract_variable(
        &self,
        input: String,
        start_line: i32,
        start_column: i32,
        end_line: i32,
        end_column: i32,
        variable_name: String) -> GString {
        let program = GDScriptParser::parse_to_program(input.as_str());
        
        GString::from(
            program.extract_variable(
                (start_line as usize + 1, start_column as usize + 1),
                 (end_line as usize + 1, end_column as usize + 1), 
                 variable_name.as_str()
                ).to_string()
        )
    }

    #[func]
    fn toggle_export(&self, linea: String) -> GString {
        let var_declaration = GDScriptParser::parse_to_declaration(&linea);
        let refactored_declaration = var_declaration.toggle_annotation(AnnotationKind::Export.to_annotation());

        GString::from(refactored_declaration.to_string())
    }
    #[func]
    fn bajar(&self, texto_seleccionado: String, todo_el_archivo: String) -> GString {
        let program = GDScriptParser::parse_to_program(&todo_el_archivo);
        let function = GDScriptParser::parse_to_declaration(&texto_seleccionado);

        let refactored_program = program.move_declaration_down(function);


        GString::from(refactored_program.to_string())
    }
    #[func]
    fn subir(&self, texto_seleccionado: String, todo_el_archivo: String) -> GString {
        let program = GDScriptParser::parse_to_program(&todo_el_archivo);
        let function = GDScriptParser::parse_to_declaration(&texto_seleccionado);

        let refactored_program = program.move_declaration_up(function);

        GString::from(refactored_program.to_string())
    }
    #[func]
    fn toggle_tool_button(&self, texto_seleccionado: String, todo_el_archivo: String) -> GString {
        let program = GDScriptParser::parse_to_program(&todo_el_archivo);
        let function = GDScriptParser::parse_to_declaration(&texto_seleccionado);
        let refactored_program = program.toggle_tool_button(function);

        GString::from(refactored_program.to_string())
    }
    #[func]
    fn function_at_line(&self, line: i32, archivo: String) -> GString {
        let donde_empieza = archivo.lines().skip(line as usize).collect::<Vec<&str>>().join("\n");

        match GDScriptParser::parse_to_declaration(&donde_empieza).kind {
            f @ DeclarationKind::Function(_, _, _, _) => GString::from(f.as_str()),
            _ => GString::from("")
        }
    }
}

