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
use std::io::Write as OtherWrite;
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
            let (_tmp, file_name, version) = {
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
                    let text = produce_main(&main_file_name, &ast);
                    tmp.write_all(text.as_bytes())
                        .map_err(OnChangeError::TempfileError)?;

                    (
                        Some(tmp),
                        tmp_file_name,
                        ast.get_version()
                            .unwrap_or_else(|| constants::LATEST_VERSION),
                    )
                } else {
                    (None, main_file_name, constants::LATEST_VERSION)
                }
            };

            let version_string = parse::version_string(version);
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
                message: format!("errors found in the following files:{}", locations),
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
fn produce_main(file_name: &str, ast: &circom_structure::ast::AST) -> String {
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

        write!(result, "{} a{} = {}({});", var_type, i, name, dummy_args).unwrap();
    }
    write!(
        result,
        "}} component main = {}();",
        constants::DUMMY_MAIN_NAME
    )
    .unwrap();

    result
}
