/**
 * This code was adopted from example code from the tower-lsp crate.
 * https://github.com/ebkalderon/tower-lsp
 */
use dashmap::DashMap;
use ntlcc::parser::parse;
use ntlcc::type_checker::TypedTerm;
use serde::{Deserialize, Serialize};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<String, TypedTerm>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "NTLC-LSP".to_string(),
                version: Some("0.0.1".to_string()),
            }),
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        self.client
            .log_message(MessageType::INFO, "inlay hint")
            .await;

        let uri = &params.text_document.uri;

        if let Some(ast) = self.ast_map.get(uri.as_str()) {
            let term = [ast.value()];

            let inlay_hint_list = term
                .iter()
                .map(|v| {
                    (
                        0,
                        u32::MAX,
                        match v {
                            ntlcc::type_checker::TypedTerm::Boolean(_) => "BOOL".to_string(),
                            ntlcc::type_checker::TypedTerm::Integer(_) => "INT".to_string(),
                            ntlcc::type_checker::TypedTerm::Void => "VOID".to_string(),
                        },
                    )
                })
                .map(|item| InlayHint {
                    text_edits: None,
                    tooltip: None,
                    kind: Some(InlayHintKind::TYPE),
                    padding_left: None,
                    padding_right: None,
                    data: None,
                    position: Position {
                        line: 0,
                        character: item.1,
                    },
                    label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
                        value: item.2,
                        tooltip: None,
                        location: Some(Location {
                            uri: params.text_document.uri.clone(),
                            range: Range {
                                start: Position::new(0, 0),
                                end: Position::new(0, u32::MAX),
                            },
                        }),
                        command: None,
                    }]),
                })
                .collect::<Vec<_>>();

            return Ok(Some(inlay_hint_list));
        }

        Ok(None)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }
}
#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}

impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let program_source = params.text.trim();

        let lex_result = ntlcc::lexer::scan(program_source).map_err(|e| e.to_string());

        let parse_result = lex_result.and_then(|tokens| parse(tokens).map_err(|e| e.to_string()));

        let type_checker_result = parse_result
            .and_then(|ast| ntlcc::type_checker::infer(&ast).map_err(|e| e.to_string()));

        let (ast, errors) = match type_checker_result {
            Ok(ast) => (Some(ast), vec![]),
            Err(e) => (None, vec![e]),
        };

        let diagnostics = errors
            .into_iter()
            .map(|item| {
                let start_position = Position {
                    line: 0,
                    character: 0,
                };
                let end_position = Position {
                    line: 0,
                    character: u32::MAX,
                };
                Diagnostic::new_simple(Range::new(start_position, end_position), item)
            })
            .collect();

        self.client
            .log_message(MessageType::INFO, "before publishing diagnostics")
            .await;

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        if let Some(ast) = ast {
            self.ast_map.insert(params.uri.to_string(), ast);
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
