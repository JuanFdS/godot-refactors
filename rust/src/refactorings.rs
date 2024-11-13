use pest::iterators::Pair;

use crate::{godot_ast_types::*, parsing::Rule};
use std::ops::Range;

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

    pub fn without_pairs(&self) -> Program {
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


impl<'a> DeclarationKind<'a> {
    fn to_function(&self) -> Option<Function> {
        match self {
            DeclarationKind::Function(name, tipe, parameters, statements) =>
                Some(Function { name, tipe: tipe.clone(), parameters: parameters.to_vec(), statements: statements.to_vec() }),
            _ => None
        }
    }

    pub fn to_declaration(&self, pair: Option<Pair<'a, Rule>> ) -> Declaration<'a> {
        Declaration { pair, kind: self.clone() }
    }

    pub fn as_str(&self) -> String {
        self.to_declaration(None).to_string()
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



impl Parameter<'_> {
    pub fn from(name: &'_ str) -> Parameter {
        Parameter { pair: None, name }
    }
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


impl<'a> Statement<'a> {
    pub fn as_str(&self) -> String {
        match self {
            Statement::Pass => "pass".to_string(),
            Statement::Unknown(string) => string.to_string(),
            Statement::VarDeclaration(name, expression) => format!("var {name} = {expression}"),
        }
    }
}

