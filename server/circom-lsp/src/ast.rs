use ropey::Rope;
use tower_lsp::lsp_types;

use circom_structure::abstract_syntax_tree::ast;
use circom_structure::function_data::FunctionData;
use circom_structure::template_data::TemplateData;

use num_traits::cast::ToPrimitive;

use std::fmt;

use crate::parse;
use crate::wrappers::*;

pub struct TokenInfo {
    name: String,
    token_type: TokenType,
    range: lsp_types::Range,
    declaration_location: lsp_types::Location,
    docs: Option<String>,
}

impl TokenInfo {
    pub fn to_hover(&self) -> lsp_types::Hover {
        let range = Some(self.range);
        let contents =
            lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(format!("{}", &self)));

        lsp_types::Hover { range, contents }
    }

    pub fn to_goto_definition(&self) -> lsp_types::GotoDefinitionResponse {
        lsp_types::GotoDefinitionResponse::Scalar(self.declaration_location.clone())
    }

    fn new(
        name: String,
        scope: &Scope,
        range: std::ops::Range<usize>,
        archive: &ProgramArchive,
        file_id: usize,
    ) -> TokenInfo {
        let lsp_types::Location { range, .. } =
            get_location(range, file_id, archive).expect("unmatching document");
        let docs = get_docs(&name, archive);
        let (token_type, declaration_location) =
            find_declaration(&name, Some(scope), archive, file_id)
                .unwrap_or_else(|| panic!("token should exist in scope: {}", name));

        TokenInfo {
            name,
            token_type,
            declaration_location,
            range,
            docs,
        }
    }

    fn try_new_definition(
        name: String,
        start: usize,
        archive: &ProgramArchive,
        file_id: usize,
    ) -> Option<TokenInfo> {
        let lsp_types::Location { range, .. } =
            get_location(start..(start + name.len()), file_id, archive)
                .expect("unmatching document");
        let docs = get_docs(&name, archive);
        let (token_type, declaration_location) = find_declaration(&name, None, archive, file_id)?;

        Some(TokenInfo {
            name,
            token_type,
            declaration_location,
            range,
            docs,
        })
    }
}

struct Scope<'a> {
    body: StatementOrExpression<'a>,
    params: &'a Vec<String>,
    params_location: std::ops::Range<usize>,
}

impl<'a> Scope<'a> {
    fn new(definition_data: DefinitionData<'a>) -> Scope<'a> {
        let (body, params, params_location) = match definition_data {
            DefinitionData::Template(x) => (
                StatementOrExpression::Statement(x.get_body()),
                x.get_name_of_params(),
                x.get_param_location(),
            ),
            DefinitionData::Function(x) => (
                StatementOrExpression::Statement(x.get_body()),
                x.get_name_of_params(),
                x.get_param_location(),
            ),
        };

        Scope {
            body,
            params,
            params_location,
        }
    }
}

struct TagList(Vec<String>);

struct Access(Vec<Option<AccessType>>);

enum Contender<'a> {
    StatementOrExpression(StatementOrExpression<'a>),
    Contender(TokenInfo),
}

#[derive(Clone, Copy)]
pub enum StatementOrExpression<'a> {
    Statement(&'a ast::Statement),
    Expression(&'a ast::Expression),
}

enum TokenType {
    Variable(Access),
    Signal(Access, SignalType, TagList),
    Component(Access),
    Definition(DefinitionType, Vec<String>),
}

enum SignalType {
    Output,
    Input,
    Intermediate,
}

enum DefinitionType {
    Template,
    Function,
}

enum AccessType {
    Num(u32),
    Var(String),
}

enum DefinitionData<'a> {
    Template(&'a TemplateData),
    Function(&'a FunctionData),
}

/// `start` is the character offset at which the token starts.
/// `word` is the name of the token.
pub fn find_token(
    start: usize,
    word: &str,
    file_id: usize,
    archive: &ProgramArchive,
) -> Option<TokenInfo> {
    let definitions = archive
        .inner
        .functions
        .values()
        .filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(Scope::new(DefinitionData::Function(x)))
            } else {
                None
            }
        })
        .chain(archive.inner.templates.values().filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(Scope::new(DefinitionData::Template(x)))
            } else {
                None
            }
        }))
        .collect::<Vec<_>>();

    let mut result = None;
    'main: for definition in definitions {
        let mut contenders = vec![Contender::StatementOrExpression(definition.body)];
        'inner: loop {
            let Some(statement_or_expression) = contenders.pop() else {
                break 'inner;
            };

            match iterate_contender(
                start,
                word,
                statement_or_expression,
                &definition,
                archive,
                file_id,
            ) {
                Ok(token_info) => {
                    result = Some(token_info);
                    break 'main;
                }
                Err(mut add_to_stack) => {
                    contenders.append(&mut add_to_stack);
                }
            }
        }
    }

    result.or_else(|| TokenInfo::try_new_definition(word.to_owned(), start, archive, file_id))
}

fn iterate_contender<'a>(
    start: usize,
    word: &str,
    contender: Contender<'a>,
    scope: &Scope,
    archive: &ProgramArchive,
    file_id: usize,
) -> Result<TokenInfo, Vec<Contender<'a>>> {
    match contender {
        Contender::Contender(token_info) => Ok(token_info),
        Contender::StatementOrExpression(statement_or_expression) => {
            let mut contenders = Vec::new();
            let meta = get_meta(statement_or_expression);
            if !(meta.start <= start && start <= meta.end) {
                return Err(vec![]);
            }

            let is_match = match statement_or_expression {
                StatementOrExpression::Statement(statement) => match statement {
                    ast::Statement::Declaration { name, .. } => name == word,
                    ast::Statement::Substitution { var, .. } => {
                        // order matters here
                        if var == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                scope,
                                meta.start..meta.end,
                                archive,
                                file_id,
                            )));
                        }
                        false
                    }
                    _ => false,
                },
                StatementOrExpression::Expression(expression) => match expression {
                    ast::Expression::Variable { name, .. } => {
                        // TODO (maybe): exception for template and function params?
                        word == name
                    }
                    ast::Expression::Call { id, .. } => {
                        // order matters here
                        if id == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                scope,
                                meta.start..meta.end,
                                archive,
                                file_id,
                            )));
                        }
                        false
                    }
                    ast::Expression::AnonymousComp { id, .. } => {
                        // order matters here
                        if id == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                scope,
                                meta.start..meta.end,
                                archive,
                                file_id,
                            )));
                        }
                        false
                    }
                    _ => false,
                },
            };

            contenders.append(
                &mut get_next_statements_or_expression(statement_or_expression)
                    .into_iter()
                    .map(Contender::StatementOrExpression)
                    .collect(),
            );

            if is_match {
                Ok(TokenInfo::new(
                    word.to_owned(),
                    scope,
                    meta.start..meta.end,
                    archive,
                    file_id,
                ))
            } else {
                Err(contenders)
            }
        }
    }
}

fn find_declaration(
    symbol: &str,
    scope: Option<&Scope>,
    archive: &ProgramArchive,
    file_id: usize,
) -> Option<(TokenType, lsp_types::Location)> {
    let result = match scope {
        Some(scope) => {
            let mut statements_or_expressions = vec![scope.body];
            let result = loop {
                let Some(statement_or_expression) = statements_or_expressions.pop() else {
                    break None
                };

                let result = match statement_or_expression {
                    StatementOrExpression::Statement(ast::Statement::Declaration {
                        meta,
                        xtype,
                        name,
                        dimensions,
                        ..
                    }) => {
                        if symbol != name {
                            continue;
                        }
                        let access = Access(
                            dimensions
                                .iter()
                                .map(|e| match e {
                                    ast::Expression::Number(_, big_int) => Some(AccessType::Num(
                                        big_int
                                            .to_u32()
                                            .expect("signal array length shouldnt be big"),
                                    )),
                                    ast::Expression::Variable { name, .. } => {
                                        Some(AccessType::Var(name.to_owned()))
                                    }
                                    _ => None,
                                })
                                .collect(),
                        );

                        let location = get_location(meta.start..meta.end, file_id, archive)
                            .expect("find_declaration location should be valid");
                        let token_type = match xtype {
                            ast::VariableType::Var => TokenType::Variable(access),
                            ast::VariableType::Signal(signal_type, tag_list) => TokenType::Signal(
                                access,
                                match signal_type {
                                    ast::SignalType::Output => SignalType::Output,
                                    ast::SignalType::Input => SignalType::Input,
                                    ast::SignalType::Intermediate => SignalType::Intermediate,
                                },
                                TagList(tag_list.clone()),
                            ),
                            ast::VariableType::Component => TokenType::Component(access),
                            ast::VariableType::AnonymousComponent => TokenType::Component(access),
                        };

                        Some((token_type, location))
                    }
                    _ => None,
                };

                if result.is_some() {
                    break result;
                } else {
                    statements_or_expressions.append(&mut get_next_statements_or_expression(
                        statement_or_expression,
                    ))
                }
            };
            result.or_else(|| {
                if scope.params.contains(&symbol.to_string()) {
                    Some((
                        TokenType::Variable(Access(vec![])),
                        get_location(scope.params_location.clone(), file_id, archive)
                            .expect("find_declaration location should be valid"),
                    ))
                } else {
                    None
                }
            })
        }
        _ => None,
    };

    result.or_else(|| find_definition_declaration(symbol, archive))
}

fn get_docs(name: &str, archive: &ProgramArchive) -> Option<String> {
    let definition = find_definition(name, archive)?;
    let (file_id, start) = match definition {
        DefinitionData::Template(data) => (data.get_file_id(), data.get_param_location().start),
        DefinitionData::Function(data) => (data.get_file_id(), data.get_param_location().start),
    };
    let file = archive
        .inner
        .file_library
        .to_storage()
        .get(file_id)
        .expect("file_id of definition should be valid");
    let content = Rope::from_str(file.source());
    let start = content.try_byte_to_char(start).ok()?;

    parse::read_comment(&content, start)
}

fn find_definition<'a>(name: &str, archive: &'a ProgramArchive) -> Option<DefinitionData<'a>> {
    if let Some(template_data) = archive.inner.templates.get(name) {
        Some(DefinitionData::Template(template_data))
    } else {
        archive
            .inner
            .functions
            .get(name)
            .map(DefinitionData::Function)
    }
}

fn find_definition_declaration(
    name: &str,
    archive: &ProgramArchive,
) -> Option<(TokenType, lsp_types::Location)> {
    let definition_data = find_definition(name, archive)?;
    let (token_type, file_id, start) = match definition_data {
        DefinitionData::Template(x) => (
            TokenType::Definition(DefinitionType::Template, x.get_name_of_params().clone()),
            x.get_file_id(),
            x.get_param_location().start,
        ),
        DefinitionData::Function(x) => (
            TokenType::Definition(DefinitionType::Function, x.get_name_of_params().clone()),
            x.get_file_id(),
            x.get_param_location().start,
        ),
    };

    // range is param location there fore length is not easily estimatable
    Some((token_type, get_location(start..start, file_id, archive)?))
}

fn get_location(
    range: std::ops::Range<usize>,
    file_id: usize,
    archive: &ProgramArchive,
) -> Option<lsp_types::Location> {
    let simple_file = archive.inner.file_library.to_storage().get(file_id)?;

    let uri = parse::circom_filename_to_uri(simple_file.name());
    let source = simple_file.source();
    let rope = Rope::from_str(source);
    let range = rope.try_byte_to_char(range.start).ok()?..rope.try_byte_to_char(range.end).ok()?;

    let document = Rope::from_str(source);
    Some(lsp_types::Location {
        uri,
        range: parse::char_range_to_position_range(&document, range).ok()?,
    })
}

fn get_next_statements_or_expression(
    statement_or_expression: StatementOrExpression,
) -> Vec<StatementOrExpression> {
    let mut statements_or_expressions = Vec::new();

    match statement_or_expression {
        StatementOrExpression::Statement(statement) => match statement {
            ast::Statement::IfThenElse {
                cond,
                if_case,
                else_case,
                ..
            } => {
                statements_or_expressions.push(StatementOrExpression::Expression(cond));
                statements_or_expressions.push(StatementOrExpression::Statement(if_case));
                if let Some(else_case) = else_case {
                    statements_or_expressions.push(StatementOrExpression::Statement(else_case));
                }
            }
            ast::Statement::While { cond, stmt, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(cond));
                statements_or_expressions.push(StatementOrExpression::Statement(stmt));
            }
            ast::Statement::Return { value, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(value));
            }
            ast::Statement::InitializationBlock {
                initializations, ..
            } => {
                statements_or_expressions.append(
                    &mut initializations
                        .iter()
                        .map(StatementOrExpression::Statement)
                        .collect(),
                );
            }
            ast::Statement::Declaration { dimensions, .. } => {
                statements_or_expressions.append(
                    &mut dimensions
                        .iter()
                        .map(StatementOrExpression::Expression)
                        .collect(),
                );
            }
            ast::Statement::Substitution { rhe, access, .. } => {
                statements_or_expressions.append(
                    &mut access
                        .iter()
                        .filter_map(|x| match x {
                            ast::Access::ArrayAccess(e) => {
                                Some(StatementOrExpression::Expression(e))
                            }
                            _ => None,
                        })
                        .collect(),
                );
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::MultSubstitution { lhe, rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::UnderscoreSubstitution { rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::ConstraintEquality { lhe, rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::LogCall { args, .. } => {
                let mut args_processed = args
                    .iter()
                    .filter_map(|x| match x {
                        ast::LogArgument::LogExp(exp) => {
                            Some(StatementOrExpression::Expression(exp))
                        }
                        _ => None,
                    })
                    .collect();

                statements_or_expressions.append(&mut args_processed);
            }
            ast::Statement::Block { stmts, .. } => {
                statements_or_expressions
                    .append(&mut stmts.iter().map(StatementOrExpression::Statement).collect());
            }
            ast::Statement::Assert { arg, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(arg));
            }
        },
        StatementOrExpression::Expression(expression) => match expression {
            ast::Expression::InfixOp { lhe, rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Expression::PrefixOp { rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Expression::InlineSwitchOp {
                cond,
                if_true,
                if_false,
                ..
            } => {
                statements_or_expressions.push(StatementOrExpression::Expression(cond));
                statements_or_expressions.push(StatementOrExpression::Expression(if_true));
                statements_or_expressions.push(StatementOrExpression::Expression(if_false));
            }
            ast::Expression::ParallelOp { rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Expression::Variable { access, .. } => {
                statements_or_expressions.append(
                    &mut access
                        .iter()
                        .filter_map(|x| match x {
                            ast::Access::ArrayAccess(e) => {
                                Some(StatementOrExpression::Expression(e))
                            }
                            _ => None,
                        })
                        .collect(),
                );
            }
            ast::Expression::Call { args, .. } => {
                let mut args_processed =
                    args.iter().map(StatementOrExpression::Expression).collect();

                statements_or_expressions.append(&mut args_processed);
            }
            ast::Expression::AnonymousComp {
                params, signals, ..
            } => {
                let mut params_processed = params
                    .iter()
                    .map(StatementOrExpression::Expression)
                    .collect();
                let mut signals_processed = signals
                    .iter()
                    .map(StatementOrExpression::Expression)
                    .collect();

                statements_or_expressions.append(&mut params_processed);
                statements_or_expressions.append(&mut signals_processed);
            }
            ast::Expression::ArrayInLine { values, .. } => {
                let mut values_processed = values
                    .iter()
                    .map(StatementOrExpression::Expression)
                    .collect();

                statements_or_expressions.append(&mut values_processed);
            }
            ast::Expression::Tuple { values, .. } => {
                let mut values_processed = values
                    .iter()
                    .map(StatementOrExpression::Expression)
                    .collect();

                statements_or_expressions.append(&mut values_processed);
            }
            ast::Expression::UniformArray {
                value, dimension, ..
            } => {
                statements_or_expressions.push(StatementOrExpression::Expression(dimension));
                statements_or_expressions.push(StatementOrExpression::Expression(value));
            }
            _ => (),
        },
    };

    statements_or_expressions
}

pub fn get_meta(statement_or_expression: StatementOrExpression) -> &ast::Meta {
    match statement_or_expression {
        StatementOrExpression::Statement(x) => match x {
            ast::Statement::IfThenElse { meta, .. } => meta,
            ast::Statement::While { meta, .. } => meta,
            ast::Statement::Return { meta, .. } => meta,
            ast::Statement::InitializationBlock { meta, .. } => meta,
            ast::Statement::Declaration { meta, .. } => meta,
            ast::Statement::Substitution { meta, .. } => meta,
            ast::Statement::MultSubstitution { meta, .. } => meta,
            ast::Statement::UnderscoreSubstitution { meta, .. } => meta,
            ast::Statement::ConstraintEquality { meta, .. } => meta,
            ast::Statement::LogCall { meta, .. } => meta,
            ast::Statement::Block { meta, .. } => meta,
            ast::Statement::Assert { meta, .. } => meta,
        },
        StatementOrExpression::Expression(x) => match x {
            ast::Expression::InfixOp { meta, .. } => meta,
            ast::Expression::PrefixOp { meta, .. } => meta,
            ast::Expression::InlineSwitchOp { meta, .. } => meta,
            ast::Expression::ParallelOp { meta, .. } => meta,
            ast::Expression::Variable { meta, .. } => meta,
            ast::Expression::Number(meta, _) => meta,
            ast::Expression::Call { meta, .. } => meta,
            ast::Expression::AnonymousComp { meta, .. } => meta,
            ast::Expression::ArrayInLine { meta, .. } => meta,
            ast::Expression::Tuple { meta, .. } => meta,
            ast::Expression::UniformArray { meta, .. } => meta,
        },
    }
}

pub fn get_definition_body(definition: &ast::Definition) -> &ast::Statement {
    match definition {
        ast::Definition::Template { body, .. } => body,
        ast::Definition::Function { body, .. } => body,
    }
}

// includes root
pub struct ASTIteratorBFS<'a> {
    stack: Vec<StatementOrExpression<'a>>,
}

impl<'a> ASTIteratorBFS<'a> {
    pub fn new(root: StatementOrExpression<'a>) -> ASTIteratorBFS<'a> {
        ASTIteratorBFS { stack: vec![root] }
    }
}

impl<'a> Iterator for ASTIteratorBFS<'a> {
    type Item = StatementOrExpression<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.stack.pop()?;
        self.stack
            .append(&mut get_next_statements_or_expression(result));

        Some(result)
    }
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(docs) = &self.docs {
            write!(f, "{}: {}\n---\n{}", self.name, self.token_type, docs)
        } else {
            write!(f, "{}: {}", self.name, self.token_type)
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Variable(access) => write!(f, "Variable{}", access),
            TokenType::Signal(access, signal_type, tag_list) => {
                write!(f, "Signal{} {} {}", access, signal_type, tag_list)
            }
            TokenType::Component(access) => write!(f, "Component{}", access),
            TokenType::Definition(definition_type, params) => {
                write!(f, "{}({})", definition_type, params.join(", "))
            }
        }
    }
}

impl fmt::Display for DefinitionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DefinitionType::Template => write!(f, "Template"),
            DefinitionType::Function => write!(f, "Function"),
        }
    }
}

impl fmt::Display for Access {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "")
        } else {
            let access = self
                .0
                .iter()
                .map(|x| match x {
                    Some(x) => format!(r"\[{}\]", x),
                    None => r"\[\_\]".to_string(),
                })
                .collect::<String>();

            write!(f, "{}", access)
        }
    }
}

impl fmt::Display for AccessType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AccessType::Num(x) => write!(f, "{}", x),
            AccessType::Var(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for SignalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SignalType::Output => write!(f, "Output"),
            SignalType::Input => write!(f, "Input"),
            SignalType::Intermediate => write!(f, "Intermediate"),
        }
    }
}

impl fmt::Display for TagList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "")
        } else {
            write!(f, "{{{}}}", self.0.join(","))
        }
    }
}
