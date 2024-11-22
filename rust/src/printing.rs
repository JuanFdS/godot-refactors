use crate::godot_ast_types::*;
use std::fmt::Display;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut code_text = "".to_string();
        if self.is_tool {
            code_text += "@tool\n";
        }
        self.super_class.iter().for_each(|super_class_name| {
            code_text += format!("extends {super_class_name}\n").as_str();
        });
        code_text += &self
            .declarations
            .iter()
            .cloned()
            .map(|declaration| declaration.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        code_text += "\n";

        write!(f, "{}", code_text)
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match &self.kind {
            DeclarationKind::Function {
                name,
                return_type,
                parameters,
                statements,
            } => {
                let identation = "\t";
                let param_list = parameters
                    .iter()
                    .cloned()
                    .map(|param| param.name)
                    .collect::<Vec<String>>()
                    .join(", ");
                let function_return_type: String = return_type
                    .clone()
                    .map_or_else(|| "".to_string(), |a_type| format!(" -> {}", a_type));
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
            DeclarationKind::Var {
                identifier,
                value,
                annotation,
            } => format!(
                "{annotation}var {identifier} = {value}",
                identifier = identifier,
                value = value,
                annotation = annotation
                    .clone()
                    .map(|x| x.as_str() + " ")
                    .unwrap_or("".to_string())
            ),
            DeclarationKind::Unknown(string) => string.to_string(),
        };

        write!(f, "{}", text)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = self.kind.to_string();

        write!(f, "{}", text)
    }
}

impl Display for ExpressionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match &self {
            ExpressionKind::LiteralInt(number) => number.to_string(),
            ExpressionKind::BinaryOperation(expression, op, expression1) => {
                format!("{expression} {op} {expression1}")
            }
            ExpressionKind::Unknown(content) => content.to_string(),
            ExpressionKind::MessageSend(expression, message_name, vec) => {
                if !vec.is_empty() {
                    panic!("TODO: manejar el caso en el que hay argumentos")
                } else {
                    format!("{expression}.{message_name}()")
                }
            }
            ExpressionKind::LiteralSelf => "self".to_owned(),
            ExpressionKind::VariableUsage(var_name) => var_name.to_string(),
        };

        write!(f, "{}", text)
    }
}
