use circom_structure::error_definition::Report;
use circom_structure::file_definition::FileLibrary;
use codespan_reporting::diagnostic::{LabelStyle, Severity};
use ropey::Rope;
use tempfile::NamedTempFile;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Write;
use std::io::Seek;
use std::io::Write as OtherWrite;
use std::num::ParseIntError;
use std::sync::Mutex;

use crate::ast;
use crate::constants;
use crate::lsp_types_util;
use crate::parse;
use crate::wrappers::*;

enum FileLibrarySource {
    ProgramArchive(ProgramArchive),
    FileLibrary(FileLibrary),
}

#[derive(Debug)]
enum OnChangeError {
    TempfileError(std::io::Error),
    UrlError,
}

#[derive(Debug)]
struct DocumentData {
    content: Rope,
    optional: Option<OptionalDocumentData>,
}

#[derive(Debug)]
struct OptionalDocumentData {
    archive: ProgramArchive,
    main_file_id: usize,
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: Option<i32>,
}

#[derive(Debug)]
pub struct Backend {
    client: Client,
    document_map: Mutex<RefCell<HashMap<Url, DocumentData>>>,
}

impl Backend {
    pub fn new(client: Client) -> Backend {
        Backend {
            client,
            document_map: Mutex::new(RefCell::new(HashMap::new())),
        }
    }

    async fn on_change(
        &self,
        params: TextDocumentItem,
        publish_diagnostics: bool,
    ) -> Result<(), OnChangeError> {
        // do not compute archive if publish_diagnostics flag is not set, this
        // is done to prevent from tons of calls to the circom compiler
        //
        // also, as of now circom's parser function doesn't seperate the file library creation
        // logic from the parseing, so it's impossible to run the parser on an intermediate buffer
        let archive = if publish_diagnostics {
            let (_tmp, file_name, version_string) = {
                let main_file_name = params
                    .uri
                    .to_file_path()
                    .map_err(|_| OnChangeError::UrlError)?
                    .to_str()
                    .expect("pathbuf from url to be comprised of valid utf-8")
                    .to_owned();

                if let Ok(ast) = parse::preprocess(&params.text)
                    .and_then(|x| {
                        circom_parser::lang::ParseAstParser::new()
                            .parse(0, &mut vec![], &x)
                            .map_err(|_| ())
                    })
                    .and_then(|ast| {
                        if ast.main_component.is_none() {
                            Ok(ast)
                        } else {
                            Err(())
                        }
                    })
                {
                    let mut tmp = NamedTempFile::new().map_err(OnChangeError::TempfileError)?;
                    let tmp_file_name = tmp
                        .path()
                        .to_str()
                        .expect("tmpfile name to be comprised of valid utf-8")
                        .to_owned();
                    let text = produce_main(&main_file_name, &ast, HashMap::new());
                    tmp.write_all(text.as_bytes())
                        .map_err(OnChangeError::TempfileError)?;

                    let version_string = parse::version_string(
                        ast.get_version()
                            .unwrap_or_else(|| constants::LATEST_VERSION),
                    );

                    // if there are any unusual output dimensions, reproduce main
                    let output_dimensions = get_definitions_output_dimensions(
                        &tmp_file_name,
                        &text,
                        &version_string,
                        &params.uri,
                    )
                    .unwrap_or_else(|e| {
                        panic!("unexpected output dimensions error, reason: {:?}", e)
                    });

                    if !output_dimensions.is_empty() {
                        let text = produce_main(&main_file_name, &ast, output_dimensions);

                        let tmp_mut_ref = tmp.as_file_mut();
                        tmp_mut_ref
                            .set_len(0)
                            .map_err(OnChangeError::TempfileError)?;
                        tmp_mut_ref
                            .seek(std::io::SeekFrom::End(0))
                            .map_err(OnChangeError::TempfileError)?;

                        tmp.write_all(text.as_bytes())
                            .map_err(OnChangeError::TempfileError)?;
                    }

                    (Some(tmp), tmp_file_name, version_string)
                } else {
                    (
                        None,
                        main_file_name,
                        parse::version_string(constants::LATEST_VERSION),
                    )
                }
            };

            let (reports, file_library_source) = match circom_parser::run_parser(
                file_name,
                &version_string,
                vec![], // TODO: add linked library support
            ) {
                Ok((mut archive, mut reports)) => {
                    let mut type_reports =
                        match circom_type_checker::check_types::check_types(&mut archive) {
                            Ok(type_reports) => type_reports,
                            Err(type_reports) => type_reports,
                        };
                    reports.append(&mut type_reports);
                    (
                        reports,
                        FileLibrarySource::ProgramArchive(ProgramArchive::new(archive)),
                    )
                }
                Err((file_library, reports)) => {
                    (reports, FileLibrarySource::FileLibrary(file_library))
                }
            };

            let file_library = match &file_library_source {
                FileLibrarySource::ProgramArchive(archive) => &archive.inner.file_library,
                FileLibrarySource::FileLibrary(file_library) => file_library,
            };

            let diagnostics: Vec<_> = reports
                .into_iter()
                .map(|x| Self::report_to_diagnostic(x, file_library, &params.uri))
                .collect();

            let mut main_file_diags = Vec::new();
            let mut other_files_diags: HashMap<_, Vec<_>> = HashMap::new();

            for (diagnostic, uri) in diagnostics {
                if uri == params.uri {
                    main_file_diags.push(diagnostic);
                } else {
                    match other_files_diags.entry(uri) {
                        Entry::Occupied(mut x) => {
                            x.get_mut().push(diagnostic);
                        }
                        Entry::Vacant(x) => {
                            x.insert(vec![diagnostic]);
                        }
                    }
                }
            }

            if let Some(other_files_error) = Self::other_files_diagnostic(&other_files_diags) {
                main_file_diags.push(other_files_error);
            };

            // publish main errors
            self.client
                .publish_diagnostics(params.uri.clone(), main_file_diags, params.version)
                .await;

            // publish errors in other files
            for (uri, diags) in other_files_diags {
                self.client
                    .publish_diagnostics(uri, diags, params.version)
                    .await;
            }

            match file_library_source {
                FileLibrarySource::ProgramArchive(x) => Some(x),
                _ => None,
            }
        } else {
            None
        };

        let document_map = self
            .document_map
            .lock()
            .expect("document map mutex poisened");
        // if new archive computed succesfully, insert it.
        // otherwise, use old archive
        let archive = match archive {
            Some(new_archive) => Some(new_archive),
            None => match document_map
                .borrow_mut()
                .remove(&params.uri)
                .map(|x| x.optional.map(|y| y.archive))
            {
                Some(existing_archive) => existing_archive,
                None => None,
            },
        };

        // elaborate process is needed because the actual main is a tmp file
        let optional = if let Some(mut archive) = archive {
            let mut i = 0;
            let file_library = archive.inner.get_file_library().to_storage();

            let main_file_id = 'result: {
                while let Some(simple_file) = file_library.get(i) {
                    if parse::circom_filename_to_uri(simple_file.name()) == params.uri {
                        break 'result i;
                    }
                    i += 1;
                }
                panic!("archive should contain uri of document");
            };
            archive.inner.file_id_main = main_file_id;
            Some(OptionalDocumentData {
                archive,
                main_file_id,
            })
        } else {
            None
        };

        let document = DocumentData {
            content: Rope::from_str(&params.text),
            optional,
        };

        document_map.borrow_mut().insert(params.uri, document);
        Ok(())
    }

    // file_library is needed to decide in what file does the report occurs
    fn report_to_diagnostic(
        report: Report,
        file_library: &FileLibrary,
        main_uri: &Url,
    ) -> (Diagnostic, Url) {
        let diagnostic = report.to_diagnostic();

        let label = diagnostic
            .labels
            .into_iter()
            .reduce(|cur, a| match (cur.style, a.style) {
                (LabelStyle::Primary, LabelStyle::Secondary) => cur,
                (LabelStyle::Secondary, LabelStyle::Primary) => a,
                _ => {
                    if cur.range.start < a.range.start {
                        cur
                    } else {
                        a
                    }
                }
            });

        let (url, range) = match label {
            Some(label) => {
                let simple_file = file_library
                    .to_storage()
                    .get(label.file_id)
                    .expect("invalid file_id from label");

                let uri = parse::circom_filename_to_uri(simple_file.name());
                let rope = Rope::from_str(simple_file.source());

                (
                    uri,
                    Range {
                        start: parse::char_to_position(&rope, rope.byte_to_char(label.range.start))
                            .expect("valid label range start"),
                        end: parse::char_to_position(&rope, rope.byte_to_char(label.range.end))
                            .expect("valid label range end"),
                    },
                )
            }
            None => (
                main_uri.clone(),
                Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
            ),
        };

        let severity = match diagnostic.severity {
            Severity::Bug | Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Help => DiagnosticSeverity::HINT,
            Severity::Note => DiagnosticSeverity::INFORMATION,
        };

        let message = diagnostic.message;

        (
            Diagnostic {
                range,
                severity: Some(severity),
                code: None,
                code_description: None,
                source: Some(String::from("circom_lsp")),
                message,
                related_information: None,
                tags: None,
                data: None,
            },
            url,
        )
    }

    fn other_files_diagnostic(
        other_files_diags: &HashMap<Url, Vec<Diagnostic>>,
    ) -> Option<Diagnostic> {
        let locations: String = other_files_diags
            .keys()
            .map(|uri| {
                format!(
                    "\n{}",
                    uri.to_file_path()
                        .expect("uri to be valid path")
                        .to_str()
                        .expect("path to be comprised of valid utf-8")
                )
            })
            .collect();

        if locations.is_empty() {
            None
        } else {
            Some(Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some(String::from("circom_lsp")),
                message: format!("errors found in the following files:{}", locations,),
                related_information: None,
                tags: None,
                data: None,
            })
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "Opened!").await;

        self.on_change(
            TextDocumentItem {
                uri: params.text_document.uri,
                text: params.text_document.text,
                version: Some(params.text_document.version),
            },
            true,
        )
        .await
        .expect("on change failed");
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            TextDocumentItem {
                uri: params.text_document.uri,
                text: params.content_changes[0].text.clone(),
                version: Some(params.text_document.version),
            },
            false,
        )
        .await
        .expect("on change failed");
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let result = {
            let document_map = self
                .document_map
                .lock()
                .expect("document_map mutex poisened");
            let document_map = document_map.borrow();
            document_map
                .get(&params.text_document.uri)
                .map(|x| x.content.to_string())
        };

        if let Some(document) = result {
            self.on_change(
                TextDocumentItem {
                    uri: params.text_document.uri,
                    text: document,
                    version: None,
                },
                true,
            )
            .await
            .expect("on change failed");
        };
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let document_map = self
            .document_map
            .lock()
            .expect("document_map mutex poisened");
        let document_map = document_map.borrow();
        let document_data = document_map
            .get(&uri)
            .expect("document map should have uri on hover");

        // find what word is selected
        let Ok(Some((pos, word))) = parse::find_word(&document_data.content, params.text_document_position_params.position) else {
            return Ok(None);
        };

        let Some(OptionalDocumentData { archive, main_file_id }) = &document_data.optional else {
            return Ok(Some(lsp_types_util::simple_hover(String::from("Could not find information (are there any compilation errors?)"))))
        };

        Ok(ast::find_token(pos, &word, *main_file_id, archive).map(|x| x.to_hover()))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let document_map = self
            .document_map
            .lock()
            .expect("document_map mutex poisened");
        let document_map = document_map.borrow();
        let document_data = document_map
            .get(&uri)
            .expect("document map should have uri on hover");

        // find what word is selected
        let Ok(Some((pos, word))) = parse::find_word(&document_data.content, params.text_document_position_params.position) else {
            return Ok(None);
        };

        let Some(OptionalDocumentData { archive, main_file_id }) = &document_data.optional else {
            return Ok(None)
        };

        Ok(ast::find_token(pos, &word, *main_file_id, archive).map(|x| x.to_goto_definition()))
    }
}

/// Generates Circom code with a main component that uses every definition from the ast.
///
/// Used because Circom compilation of a file without a main component fails, and also because
/// ProgramArchives includes information only about the definitions used in the main component.
fn produce_main(
    file_name: &str,
    ast: &circom_structure::ast::AST,
    output_dimensions: HashMap<usize, usize>,
) -> String {
    let version = ast
        .get_version()
        .unwrap_or_else(|| constants::LATEST_VERSION);
    let version = parse::version_string(version);

    let mut result = format!(
        "pragma circom {};include \"{}\";template {}() {{",
        version,
        file_name,
        constants::DUMMY_MAIN_NAME
    );
    for (i, definition) in ast.definitions.iter().enumerate() {
        let (name, args, var_type) = match definition {
            circom_structure::ast::Definition::Template { name, args, .. } => {
                (name, args, "component")
            }
            circom_structure::ast::Definition::Function { name, args, .. } => (name, args, "var"),
        };

        let dimensions = *output_dimensions.get(&i).unwrap_or(&0);
        let dimensions = "[1]".repeat(dimensions);

        // needs to know how if a variable is an array, and if so what depth
        // not using hashmap because order is important
        let mut args_max_access = args.iter().map(|name| (name, 0)).collect::<Vec<_>>();
        for ast_node in ast::ASTIteratorBFS::new(ast::StatementOrExpression::Statement(
            ast::get_definition_body(definition),
        )) {
            if let ast::StatementOrExpression::Expression(
                circom_structure::ast::Expression::Variable { name, access, .. },
            ) = ast_node
            {
                for (arg_name, curr_max) in args_max_access.iter_mut() {
                    if **arg_name == *name {
                        // accesses can be both index access and component access, assuming
                        // component access is impossible for variables
                        let access_depth = access.len();
                        if access_depth > *curr_max {
                            *curr_max = access_depth;
                        }
                    }
                }
            }
        }

        let mut dummy_args = args_max_access
            .into_iter()
            .map(|(_, max_access)| {
                format!("{}0{},", "[".repeat(max_access), "]".repeat(max_access))
            })
            .collect::<String>();
        // rmeove last ",";
        dummy_args.pop();

        write!(
            result,
            "{} a{}{} = {}({});",
            var_type, i, dimensions, name, dummy_args
        )
        .unwrap();
    }
    write!(
        result,
        "}} component main = {}();",
        constants::DUMMY_MAIN_NAME
    )
    .unwrap();

    result
}

#[derive(Debug)]
enum OutputDimensionsError {
    Ropey(ropey::Error),
    Parse(ParseIntError),
    InvalidReport(InvalidReportError),
}

#[derive(Debug)]
enum InvalidReportError {
    NoWordFound,
    UnexpectedFirstWord,
    NotFoundNextWord,
}
// Inferring through the AST the return types of functions is a pain in the ass
// It's better to simply compile twice, and use data from reports to fix the issue
//
// assumes a single run covers all
fn get_definitions_output_dimensions(
    tmp_file_name: &str,
    tmp_file_text: &str,
    version_string: &str,
    main_uri: &Url,
) -> Result<HashMap<usize, usize>, OutputDimensionsError> {
    let mut problematic_definitions = HashMap::new();
    if let Ok((mut archive, _)) =
        circom_parser::run_parser(tmp_file_name.to_owned(), version_string, vec![])
    {
        let rope = Rope::from_str(tmp_file_text);
        let reports = match circom_type_checker::check_types::check_types(&mut archive) {
            Ok(type_reports) => type_reports,
            Err(type_reports) => type_reports,
        };
        let file_library = archive.file_library;
        for report in reports {
            let report_code = *report.get_code();
            let (diag, uri) = Backend::report_to_diagnostic(report, &file_library, main_uri);
            let report_file_name = uri
                .to_file_path()
                .expect("uri to be valid path")
                .to_str()
                .expect("path to be comprised of valid utf-8")
                .to_owned();

            if report_file_name == tmp_file_name {
                if let circom_structure::error_code::ReportCode::WrongTypesInAssignOperationDims(
                    _,
                    expected,
                ) = report_code
                {
                    // according to manual tests, this type error's diagnostic
                    // first character points at the keyword "var" in the case of
                    // the way tmp main files are generated
                    let (start, word) = parse::find_word(&rope, diag.range.start)
                        .map_err(OutputDimensionsError::Ropey)?
                        .ok_or(OutputDimensionsError::InvalidReport(
                            InvalidReportError::NoWordFound,
                        ))?;
                    if word != "var" {
                        Err(OutputDimensionsError::InvalidReport(
                            InvalidReportError::UnexpectedFirstWord,
                        ))?
                    }

                    let start_of_next_word = 'result: {
                        let chars = rope.chars_at(start).enumerate();
                        let mut found_whitespace = false;
                        for (i, c) in chars {
                            match (found_whitespace, c.is_whitespace()) {
                                (false, true) => found_whitespace = true,
                                (true, false) => break 'result start + i,
                                _ => (),
                            }
                        }
                        Err(OutputDimensionsError::InvalidReport(
                            InvalidReportError::NotFoundNextWord,
                        ))?
                    };

                    let next_word_position = parse::char_to_position(&rope, start_of_next_word)
                        .map_err(OutputDimensionsError::Ropey)?;
                    let (_, next_word) = parse::find_word(&rope, next_word_position)
                        .map_err(OutputDimensionsError::Ropey)?
                        .ok_or(OutputDimensionsError::InvalidReport(
                            InvalidReportError::NotFoundNextWord,
                        ))?;

                    let mut next_word_chars = next_word.chars();
                    next_word_chars.next();
                    let next_word_slice = next_word_chars.as_str();
                    let num = next_word_slice
                        .parse::<usize>()
                        .map_err(OutputDimensionsError::Parse)?;
                    problematic_definitions.insert(num, expected);
                }
            }
        }
    }
    Ok(problematic_definitions)
}
