use coso::{Annotation, GDScriptParser};
mod coso;
use godot::{classes::GDScript, prelude::*};
use pest_derive::Parser;


#[derive(Parser)]
#[grammar = "csv.pest"]
pub struct CSVParser;

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
    fn toggle_export(&self, linea: String) -> GString {
        let var_declaration = GDScriptParser::parse_to_declaration(&linea);
        let refactored_declaration = var_declaration.toggle_annotation(Annotation::Export);

        GString::from(refactored_declaration.as_str())
    }
    #[func]
    fn bajar(&self, texto_seleccionado: String, todo_el_archivo: String) -> GString {
        let program = GDScriptParser::parse_to_program(&todo_el_archivo);
        let function = GDScriptParser::parse_to_declaration(&texto_seleccionado);

        let refactored_program = program.move_declaration_down(function);


        GString::from(refactored_program.as_str())
    }
    #[func]
    fn subir(&self, texto_seleccionado: String, todo_el_archivo: String) -> GString {
        let program = GDScriptParser::parse_to_program(&todo_el_archivo);
        let function = GDScriptParser::parse_to_declaration(&texto_seleccionado);

        let refactored_program = program.move_declaration_up(function);

        GString::from(refactored_program.as_str())
    }
}

