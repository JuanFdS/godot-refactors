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
            }
            None => panic!("Index out of bounds"),
        }
    }
}

pub fn range_contains<T: PartialOrd>(outer: &Range<T>, inner: &Range<T>) -> bool {
    outer.start <= inner.start && outer.end >= inner.end
}

pub type LineCol = (usize, usize);

pub trait ExtendedPair {
    fn contains_range(&self, range: Range<LineCol>) -> bool;

    fn is_contained_in_range(&self, range: Range<LineCol>) -> bool;

    fn line_col_range(&self) -> Range<LineCol>;
}

impl<'a> ExtendedPair for Pair<'a, Rule> {
    fn contains_range(&self, range: Range<LineCol>) -> bool {
        range_contains(&self.line_col_range(), &range)
    }

    fn is_contained_in_range(&self, range: Range<LineCol>) -> bool {
        range_contains(&range, &self.line_col_range())
    }

    fn line_col_range(&self) -> Range<LineCol> {
        let start = self.line_col();
        let end = self.as_span().end_pos().line_col();
        Range { start, end }
    }
}

type SelectionRange = Range<LineCol>;

impl<'a> Program {
    fn find_function_declaration_by_selection_range(&self, range: &SelectionRange) -> Option<(usize, &Declaration)> {
        self.declarations.iter().enumerate().find(|(_idx, declaration)|
            {
                let is_function = match declaration.kind {
                    DeclarationKind::Function(_, _, _, _) => true,
                    _ => false
                };
                let contains_selected_range = declaration.contains_range(range);
                is_function && contains_selected_range
            }
        )
    }

    fn transform_declaration(&self, index: usize, f: impl Fn(&Declaration) -> Declaration) -> Program {
        let new_declarations = self.declarations.replace(index, f);
        self.with_declarations(new_declarations)
    }

    pub fn get_ocurrences_of_variable(&self, start_line_column: LineCol, end_line_column: LineCol) -> Vec<Range<LineCol>> {
        let selected_range = start_line_column..end_line_column;
        let (_declaration_idx, _declaration) = match self.find_function_declaration_by_selection_range(&selected_range) {
            Some(result) => result,
            None => return vec![]
        };
        vec![]
    }

    pub fn inline_variable(
        &self,
        start_line_column: LineCol,
        end_line_column: LineCol,
    ) -> (Program, Vec<Range<LineCol>>) {
        let selected_range = start_line_column..end_line_column;
        let (declaration_idx, _declaration) =
            match self.find_function_declaration_by_selection_range(&selected_range) {
                Some(result) => result,
                None => return (self.clone(), vec![])
            };
        let new_program = self.transform_declaration(declaration_idx, |function| {
            let (f_name, f_type, f_params, statements) = match &function.kind {
                DeclarationKind::Function(f_name, f_type, f_params, s) =>
                    (f_name, f_type, f_params, s),
                _ => return function.clone()
            };
            let maybe_statement_idx = statements.iter().position(|statement| {
                statement.contains_range(&selected_range)
            });
            if maybe_statement_idx.is_none() {
                return function.clone();
            }
            let statement_idx = maybe_statement_idx.unwrap();
            let (variable_name, expr_to_inline) = match &statements.get(statement_idx).unwrap().kind {
                StatementKind::Pass | StatementKind::Unknown(_) | StatementKind::Expression(_) | StatementKind::Return(_) => return function.clone(),
                StatementKind::VarDeclaration(var_name, expression) => (var_name, expression),
            };
            let mut new_statements: Vec<Statement> = vec![];

            for (idx, statement) in statements.clone().iter().enumerate() {
                if idx == statement_idx {} else {
                    fn replace_variable_usage<'a>(
                        old_expression: Expression,
                        variable_name: String,
                        expression_to_inline: Expression,
                    ) -> Option<(Expression, Vec<Range<LineCol>>)> {
                        match old_expression.clone().kind {
                            ExpressionKind::Unknown(var_name) if var_name == variable_name => Some(
                                (expression_to_inline, vec![old_expression.line_col_range()])
                            ),
                            ExpressionKind::LiteralInt(_) | ExpressionKind::Unknown(_) | ExpressionKind::LiteralSelf => None,
                            ExpressionKind::BinaryOperation(expression1, op, expression2) => {
                                let maybe_replacement1 = replace_variable_usage(*expression1.clone(), variable_name.clone(), expression_to_inline.clone());
                                let maybe_replacement2 = replace_variable_usage(*expression2.clone(), variable_name, expression_to_inline);
                                let mut lines_to_select = vec![];
                                let mut new_expression1: Expression = *expression1;
                                let mut new_expression2: Expression = *expression2;
                                if let Some((new_expr1, mut lines_to_select1)) = maybe_replacement1 {
                                    lines_to_select.append(&mut lines_to_select1);
                                    new_expression1 = new_expr1;
                                }
                                if let Some((new_expr2, mut lines_to_select2)) = maybe_replacement2 {
                                    lines_to_select.append(&mut lines_to_select2);
                                    new_expression2 = new_expr2;
                                }
                                let kind = ExpressionKind::BinaryOperation(Box::new(new_expression1), op, Box::new(new_expression2));
                                Some((Expression::new(None, kind), lines_to_select))
                            }
                            ExpressionKind::MessageSend(expression, message_name, vec) => {
                                let maybe_replacement = replace_variable_usage(*expression.clone(), variable_name.clone(), expression_to_inline.clone());
                                match maybe_replacement {
                                    Some((new_expr, lines_to_select)) => {
                                        let kind = ExpressionKind::MessageSend(Box::new(new_expr), message_name, vec);
                                        Some((Expression::new(None, kind), lines_to_select))
                                    }

                                    None => None
                                }
                            }
                            ExpressionKind::VariableUsage(var_name) if var_name == variable_name => Some((expression_to_inline, vec![])),
                            ExpressionKind::VariableUsage(_) => None,
                        }
                    }

                    match statement.clone().kind {
                        StatementKind::Pass | StatementKind::Unknown(_) | StatementKind::Return(None) => new_statements.push(statement.clone()),
                        StatementKind::VarDeclaration(var_name, expression) => {
                            match replace_variable_usage(expression, variable_name.to_string(), expr_to_inline.clone()) {
                                Some((new_expr, _lines_to_select)) =>
                                    new_statements.push(Statement::new(None, StatementKind::VarDeclaration(var_name, new_expr))),
                                None => new_statements.push(statement.clone()),
                            }
                        }
                        StatementKind::Expression(expression) => {
                            match replace_variable_usage(expression, variable_name.to_string(), expr_to_inline.clone()) {
                                Some((new_expr, _lines_to_select)) =>
                                    new_statements.push(Statement::new(None, StatementKind::Expression(new_expr))),
                                None => new_statements.push(statement.clone()),
                            }
                        }
                        StatementKind::Return(Some(expression)) => {
                            match replace_variable_usage(expression, variable_name.to_string(), expr_to_inline.clone()) {
                                Some((new_expr, _lines_to_select)) =>
                                    new_statements.push(Statement::new(None, StatementKind::Return(Some(new_expr)))),
                                None => new_statements.push(statement.clone()),
                            }
                        }
                    }
                }
            }
            Declaration::new(None, DeclarationKind::Function(f_name.into(), f_type.clone(), f_params.to_vec(), new_statements))
        });
        (new_program, vec![])
    }

    pub fn extract_variable(
        &self,
        start_line_column: (usize, usize),
        end_line_column: (usize, usize),
        variable_name: &'a str,
    ) -> (Program, Vec<Range<LineCol>>) {
        static mut pos_start_variable_usage: LineCol = (0, 0);
        let selected_range = start_line_column..end_line_column;
        let (declaration_idx, _declaration) =
            match self.find_function_declaration_by_selection_range(&selected_range) {
                Some(result) => result,
                None => return (self.clone(), vec![])
            };
        let this = &self;
        let f = move |declaration: &Declaration| -> Declaration {
            match declaration.clone().kind {
                DeclarationKind::Function(function_name, function_type, params, ref statements) => {
                    let maybe_statement_idx = statements.iter().position(|statement|
                        statement.contains_range(&selected_range)
                    );
                    if maybe_statement_idx.is_none() {
                        return declaration.clone();
                    }
                    let statement_idx = maybe_statement_idx.unwrap();

                    let variable_usage = Expression::new(None, ExpressionKind::Unknown(variable_name.to_owned()));
                    let old_statement = statements.get(statement_idx).unwrap().clone();
                    let (old_expr, new_statement) =
                        match old_statement.clone().kind {
                            StatementKind::VarDeclaration(var_name, expression) => {
                                // match &expression.replace_expression_by_selection(selected_range, variable_usage) {
                                //     Some(ExpressionReplacement { old, new, range_of_new_expression }) =>
                                //         (old.clone(), StatementKind::VarDeclaration(var_name, new.clone()).to_statement(None)),
                                //     None => return declaration.clone()
                                // }
                                let (old_expr, new_expr) = expression_replacement(expression.clone(), variable_usage, selected_range.clone());
                                (old_expr, StatementKind::VarDeclaration(var_name, new_expr).to_statement(None))
                            }
                            StatementKind::Expression(expression) => {
                                let (old_expr, new_expr) = expression_replacement(expression.clone(), variable_usage, selected_range.clone());
                                (old_expr, StatementKind::Expression(new_expr).to_statement(None))
                            }
                            StatementKind::Return(Some(expression)) => {
                                let (old_expr, new_expr) = expression_replacement(expression.clone(), variable_usage, selected_range.clone());
                                (old_expr, StatementKind::Return(Some(new_expr)).to_statement(None))
                            }
                            _ => return declaration.clone()
                        };

                    fn expression_replacement<'a>(
                        expression: Expression,
                        variable_usage: Expression,
                        text_range: Range<(usize, usize)>) -> (Expression, Expression) {
                        let (old_expr, new_expr) = match &expression.kind {
                            ExpressionKind::LiteralInt(_) | ExpressionKind::Unknown(_) |
                            ExpressionKind::LiteralSelf | ExpressionKind::VariableUsage(_) => {
                                (expression.clone(), variable_usage.clone())
                            }
                            ExpressionKind::BinaryOperation(
                                expression1,
                                op,
                                expression2
                            ) => {
                                let old_expr: Expression;
                                let kind: ExpressionKind;
                                if expression1.contains_range(&text_range) {
                                    let (inner_old_expr, inner_kind) =
                                        expression_replacement(*expression1.clone(), variable_usage.clone(), text_range);
                                    old_expr = inner_old_expr;
                                    kind = ExpressionKind::BinaryOperation(Box::new(inner_kind), op.into(), expression2.clone())
                                } else if expression2.contains_range(&text_range) {
                                    let (inner_old_expr, inner_kind) =
                                        expression_replacement(*expression2.clone(), variable_usage.clone(), text_range);
                                    old_expr = inner_old_expr;
                                    kind = ExpressionKind::BinaryOperation(expression1.clone(), op.into(), Box::new(inner_kind))
                                } else {
                                    old_expr = expression.clone();
                                    kind = variable_usage.clone().kind;
                                }
                                (old_expr, Expression::new(None, kind))
                            }
                            ExpressionKind::MessageSend(receiver, method_name, arguments) => {
                                let old_expr: Expression;
                                let kind: ExpressionKind;
                                if receiver.contains_range(&text_range) {
                                    old_expr = *receiver.clone();
                                    kind = ExpressionKind::MessageSend(Box::new(variable_usage.clone()), method_name.into(), arguments.to_vec())
                                } else {
                                    old_expr = expression.clone();
                                    kind = variable_usage.clone().kind;
                                }
                                (old_expr, Expression::new(None, kind))
                            }
                        };
                        unsafe { pos_start_variable_usage = old_expr.line_col_range().start };
                        (old_expr, new_expr)
                    }

                    let mut new_statements = statements.clone();
                    new_statements.replace_mut(statement_idx, move |_| new_statement.clone());
                    let new_statement = StatementKind::VarDeclaration(variable_name.into(), old_expr);
                    new_statements.insert(statement_idx, new_statement.to_statement(None));

                    DeclarationKind::Function(function_name, function_type, params, new_statements).to_declaration(None)
                }
                _ => panic!()
            }
        };
        let new_declarations = {
            let this = &this.declarations;
            let mut new_vector = this.clone();
            match this.get(declaration_idx) {
                Some(old_value) => {
                    let new_value = f(old_value);
                    new_vector.push(new_value);
                    new_vector.swap_remove(declaration_idx);
                    new_vector
                }
                None => panic!("Index out of bounds"),
            }
        };
        let new_program = this.with_declarations(new_declarations);

        if self.without_pairs() == new_program.without_pairs() {
            return (new_program, vec![]);
        };

        let (start_line_variable_usage, start_column_variable_usage) = unsafe { pos_start_variable_usage };

        let start_column = "\tvar ".len();
        let range_where_declaration_happens =
            (start_line_variable_usage - 1, start_column)..(start_line_variable_usage - 1, start_column + variable_name.len());

        let range_where_variable_usage_happens =
            (start_line_variable_usage, start_column_variable_usage - 1)..(start_line_variable_usage, start_column_variable_usage - 1 + variable_name.len());

        (new_program, vec![range_where_declaration_happens, range_where_variable_usage_happens])
    }

    pub fn without_pairs(&self) -> Program {
        Program {
            is_tool: self.is_tool,
            super_class: self.super_class.clone(),
            declarations: self.declarations.iter().map(|declaration| declaration.without_pairs()).collect::<Vec<Declaration>>(),
        }
    }

    pub fn all_errors(&self) -> Vec<&Declaration> {
        self.declarations.iter().filter(|declaration|
            match declaration.kind {
                DeclarationKind::Unknown(_) => true,
                _ => false
            }
        ).collect::<Vec<&Declaration>>()
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

    fn with_declarations(&self, new_declarations: Vec<Declaration>) -> Program {
        let mut new_program = self.clone();
        new_program.declarations = new_declarations;
        new_program
    }

    pub fn move_declaration_down(&self, declaration: Declaration) -> Program {
        let maybe_idx = self.declarations.iter().position(
            |x| x.kind == declaration.kind
        );
        match maybe_idx {
            Some(idx) if idx + 1 == self.declarations.len() => {
                let mut updated_declarations = self.declarations.clone();
                updated_declarations.insert(idx, DeclarationKind::EmptyLine.to_declaration(None));
                self.with_declarations(updated_declarations)
            }
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

    pub fn toggle_tool(&self) -> Program {
        let mut new_program = self.clone();
        new_program.is_tool = !self.is_tool;
        new_program
    }

    pub fn toggle_tool_button(&self, function: Declaration) -> Program {
        match &function.kind {
            DeclarationKind::Function(name, _, _, _) => {
                let mut declarations: Vec<Declaration> = self.declarations.clone();
                let maybe_idx: Option<usize> = declarations.iter().position(
                    |declaration| declaration.kind == function.kind
                );
                match maybe_idx {
                    None => self.clone(),
                    Some(idx) => {
                        let already_defined_toggle_button_var_idx = declarations.clone().into_iter().position(|declaration|
                            match declaration.kind {
                                DeclarationKind::Var(_, f_name, Some(Annotation { kind: AnnotationKind::ExportToolButton(_), .. })) => f_name == *name,
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
                                let button_variable: Declaration = Declaration::new(
                                    None,
                                    DeclarationKind::Var(
                                        var_name,
                                        name.into(),
                                        Some(Annotation::new(None, AnnotationKind::ExportToolButton(name.into()))),
                                    ),
                                );
                                declarations.insert(idx, button_variable);
                                self.with_declarations(declarations)
                            }
                        }
                    }
                }
            }
            _ => self.clone()
        }
    }
}

impl<'a> Function {
    fn transform_statement(&self, index: usize, f: impl Fn(&Statement) -> Statement) -> Function {
        let new_statements = self.statements.replace(index, f);
        self.with_statements(new_statements)
    }

    fn with_statements(&self, new_statements: Vec<Statement>) -> Function {
        Function { name: self.name.clone(), tipe: self.tipe.clone(), parameters: self.parameters.clone(), statements: new_statements }
    }

    fn to_declaration_kind(&self) -> DeclarationKind {
        DeclarationKind::Function(self.name.clone(), self.tipe.clone(), self.parameters.clone(), self.statements.clone())
    }
}


impl<'a> DeclarationKind {
    fn to_function(&self) -> Option<Function> {
        match self {
            DeclarationKind::Function(name, tipe, parameters, statements) =>
                Some(Function { name: name.into(), tipe: tipe.clone(), parameters: parameters.to_vec(), statements: statements.to_vec() }),
            _ => None
        }
    }

    pub fn to_declaration(&self, pair: Option<Pair<'a, Rule>>) -> Declaration {
        Declaration::new(pair, self.clone())
    }

    pub fn as_str(&self) -> String {
        self.to_declaration(None).to_string()
    }

    fn without_pairs(&self) -> DeclarationKind {
        match self {
            DeclarationKind::Function(_, _, _, _) => self.clone(),
            DeclarationKind::EmptyLine => self.clone(),
            DeclarationKind::Var(name, value, maybe_annotation) =>
                DeclarationKind::Var(name.to_string(), value.into(), maybe_annotation.clone().map(|annotation| {
                    let this = &annotation;
                    Annotation::new(None, this.kind.clone())
                })),
            DeclarationKind::Unknown(_) => self.clone(),
        }
    }
}


impl Parameter {
    pub fn from(name: &'_ str) -> Parameter {
        Parameter::new(None, name)
    }
}

impl<'a> AnnotationKind {
    pub fn to_annotation(&self) -> Annotation {
        Annotation::new(None, self.clone())
    }
}

impl Annotation {
    pub fn as_str(&self) -> String {
        match &self.kind {
            AnnotationKind::Export => "@export".to_string(),
            AnnotationKind::OnReady => "@onready".to_string(),
            AnnotationKind::ExportToolButton(some_string) =>
                format!("@export_tool_button(\"{some_string}\")"),
        }
    }

    fn without_pairs(&self) -> Annotation {
        Annotation::new(None, self.kind.clone())
    }
}

impl<'a> Declaration {
    pub fn toggle_annotation(&self, annotation: Annotation) -> Declaration {
        match self.kind.clone() {
            DeclarationKind::Var(identifier, value, Some(previous_annotation))
            if previous_annotation.kind == annotation.kind => DeclarationKind::Var(
                identifier.to_string(), value, None).to_declaration(None),
            DeclarationKind::Var(identifier, value, _) =>
                DeclarationKind::Var(identifier.to_string(), value, Some(annotation)).to_declaration(None),
            _ => self.clone()
        }
    }

    fn without_pairs(&self) -> Declaration {
        Declaration::new(
            None,
            {
                let this = &self.kind;
                match this {
                    DeclarationKind::Function(fname, ftype, parameters, statements) => {
                        let new_statements = statements.into_iter().map(|s| s.without_pairs()).collect();
                        DeclarationKind::Function(fname.into(), ftype.clone(), parameters.to_vec(), new_statements)
                    }
                    DeclarationKind::EmptyLine => this.clone(),
                    DeclarationKind::Var(name, value, maybe_annotation) =>
                        DeclarationKind::Var(name.to_string(), value.into(), maybe_annotation.clone().map(|annotation| {
                            let this = &annotation;
                            Annotation::new(None, this.kind.clone())
                        })),
                    DeclarationKind::Unknown(_) => this.clone(),
                }
            },
        )
    }
}


impl<'a> Statement {
    pub fn as_str(&self) -> String {
        match &self.kind {
            StatementKind::Pass => "pass".to_string(),
            StatementKind::Unknown(string) => string.to_string(),
            StatementKind::VarDeclaration(name, expression) => format!("var {name} = {expression}"),
            StatementKind::Expression(expression) => expression.to_string(),
            StatementKind::Return(Some(expression)) => format!("return {expression}"),
            StatementKind::Return(None) => "return".to_string(),
        }
    }

    pub fn without_pairs(&self) -> Statement {
        Statement::new(None, self.kind.without_pairs())
    }
}

impl StatementKind {
    pub fn to_statement(&self, pair: Option<Pair<Rule>>) -> Statement {
        Statement::new(pair, self.clone())
    }

    pub fn without_pairs(&self) -> StatementKind {
        match self {
            StatementKind::Expression(expression) =>
                StatementKind::Expression(expression.without_pairs()),
            StatementKind::Return(maybe_expression) =>
                StatementKind::Return(maybe_expression.clone().map(|expr| expr.without_pairs())),
            StatementKind::VarDeclaration(var_name, expression) =>
                StatementKind::VarDeclaration(var_name.into(), expression.without_pairs()),
            StatementKind::Pass | StatementKind::Unknown(_) => self.clone()
        }
    }
}

struct ExpressionReplacement {
    old: Expression,
    new: Expression,
    range_of_new_expression: SelectionRange,
}

impl<'a> Expression {
    pub fn without_pairs(&self) -> Expression {
        Expression::new(None, self.kind.without_pairs())
    }

    fn replace_expression_by_selection(
        &self,
        selection_to_remove: SelectionRange,
        expression_to_add: Expression) -> Option<ExpressionReplacement> {
        if !self.contains_range(&selection_to_remove) {
            return None;
        };
        match &self.kind {
            ExpressionKind::LiteralInt(_) | ExpressionKind::Unknown(_) |
            ExpressionKind::LiteralSelf | ExpressionKind::VariableUsage(_) => {
                Some(ExpressionReplacement {
                    old: self.clone(),
                    new: expression_to_add.clone(),
                    range_of_new_expression:
                    {
                        let (start_line, start_col) = self.line_col();
                        (start_line, start_col)..(start_line, start_col + expression_to_add.to_string().len())
                    },
                })
            }
            ExpressionKind::BinaryOperation(
                expression1,
                op,
                expression2
            ) => {
                let old: Expression;
                let kind: ExpressionKind;
                let range_of_new_expression: SelectionRange;
                if let Some(replacement) = expression1.replace_expression_by_selection(selection_to_remove.clone(), expression_to_add.clone()) {
                    old = replacement.old;
                    kind = ExpressionKind::BinaryOperation(Box::new(replacement.new), op.into(), expression2.clone());
                    range_of_new_expression = replacement.range_of_new_expression;
                } else if let Some(replacement) = expression2.replace_expression_by_selection(selection_to_remove.clone(), expression_to_add.clone()) {
                    old = replacement.old;
                    kind = ExpressionKind::BinaryOperation(expression1.clone(), op.into(), Box::new(replacement.new));
                    range_of_new_expression = replacement.range_of_new_expression;
                } else {
                    return None;
                }
                Some(ExpressionReplacement { old: old.clone(), new: Expression::new(None, kind), range_of_new_expression })
            }
            ExpressionKind::MessageSend(receiver, method_name, arguments) => {
                let old: Expression;
                let kind: ExpressionKind;
                if receiver.contains_range(&selection_to_remove) {
                    old = *receiver.clone();
                    kind = ExpressionKind::MessageSend(Box::new(expression_to_add.clone()), method_name.into(), arguments.to_vec())
                } else {
                    return None;
                }
                Some(ExpressionReplacement {
                    old: old.clone(),
                    new: Expression::new(None, kind),
                    range_of_new_expression:
                    {
                        let (start_line, start_col) = old.line_col();
                        (start_line, start_col)..(start_line, start_col + expression_to_add.to_string().len())
                    },
                })
            }
        }
    }
}

impl<'a> ExpressionKind {
    pub fn without_pairs(&self) -> ExpressionKind {
        match &self {
            ExpressionKind::BinaryOperation(lhs, op, rhs) =>
                ExpressionKind::BinaryOperation(Box::new(lhs.without_pairs()), op.into(), Box::new(rhs.without_pairs())),
            ExpressionKind::MessageSend(receiver, message_name, arguments) =>
                ExpressionKind::MessageSend(Box::new(receiver.without_pairs()), message_name.into(), arguments.to_vec()),
            _ => self.clone()
        }
    }
}