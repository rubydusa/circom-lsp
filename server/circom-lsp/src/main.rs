mod ast;
mod backend;
mod cli;
mod constants;
mod lsp_types_util;
mod parse;
mod wrappers;

use backend::Backend;
use clap::Parser;
use cli::ServerOptions;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let _server_options = ServerOptions::parse();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
