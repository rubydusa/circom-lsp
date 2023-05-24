use tower_lsp::lsp_types::*;

pub fn simple_hover(message: String) -> Hover {
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(message)),
        range: None,
    }
}
