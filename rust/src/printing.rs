use std::fmt::Display;
use crate::godot_ast_types::*;

impl Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut code_text = "".to_string();
        if self.is_tool {
            code_text += "@tool\n";
        }
        self.super_class.iter().for_each(|super_class_name| {
            code_text += format!("extends {super_class_name}\n").as_str();
        });
        code_text += &self.declarations
            .iter()
            .cloned()
            .map(|declaration| declaration.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        code_text += "\n";
        
        write!(f, "{}", code_text)
    }
}

impl Display for Declaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match &self.kind {
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
        };

        write!(f, "{}", text)
    }
}