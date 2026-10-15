//! terminusdb-vectorlink — embedded vectorlink engine for TerminusDB.
//!
//! This shim crate links the vectorlink library into the TerminusDB
//! `librust.dylib` and runs vectorlink's own axum router in-process.
//! Requests are dispatched via `tower::ServiceExt::oneshot`, so embedded
//! mode preserves the exact wire semantics of the remote HTTP backend —
//! same routes, same auth, same streaming NDJSON progress — without a
//! socket.
//!
//! Two integration surfaces:
//!
//! - Indexer transport: `vectorlink_init` installs an `EngineTransport` into the
//!   webserver's IndexerRegistry, so the commit-push pipeline
//!   (/last-indexed, /push, DELETE /domain) flows in-process.
//! - FFI predicates in module `$vectorlink` for the Prolog plugin:
//!   `vectorlink_init/2` and `vectorlink_request/6`, used by the
//!   builtin vectorlink plugin to forward search traffic.

use std::os::fd::IntoRawFd;
use std::sync::{Arc, RwLock};

use futures::StreamExt;
use swipl::prelude::*;
use terminusdb_webserver::indexer::{
    boxed_sync_stream, set_engine_transport, EngineRequest, EngineResponse, EngineTransport,
};
#[cfg(test)]
use terminusdb_webserver::indexer::ByteStream;

/// The embedded vectorlink engine: its axum router, built without the
/// auth layer — in-process requests are already authorized by the host.
struct EmbeddedEngine {
    router: axum::Router,
}

impl EmbeddedEngine {
    /// Build an `http::Request` from a transport request.
    fn build_request(
        &self,
        req: EngineRequest,
    ) -> Result<axum::http::Request<axum::body::Body>, String> {
        let mut builder = axum::http::Request::builder()
            .method(req.method)
            .uri(&req.path);
        if let Some(ct) = req.content_type {
            builder = builder.header("Content-Type", ct);
        }
        let body = match req.body {
            Some(stream) => axum::body::Body::from_stream(stream),
            None => axum::body::Body::empty(),
        };
        builder
            .body(body)
            .map_err(|e| format!("failed to build embedded request: {}", e))
    }

    /// Dispatch a prepared request through the in-process router.
    async fn dispatch(
        &self,
        http_req: axum::http::Request<axum::body::Body>,
    ) -> Result<EngineResponse, String> {
        use tower::ServiceExt;

        let resp = match self.router.clone().oneshot(http_req).await {
            Ok(r) => r,
            // Router's service error type is Infallible.
            Err(e) => match e {},
        };
        let (parts, body) = resp.into_parts();
        Ok(EngineResponse {
            status: parts.status.as_u16(),
            headers: parts.headers,
            body: boxed_sync_stream(
                http_body_util::BodyExt::into_data_stream(body)
                    .map(|r| r.map_err(|e| e.to_string())),
            ),
        })
    }

    /// Dispatch one transport request through the in-process router.
    ///
    /// Errors carry "{METHOD} {path} failed: {cause}" to match the HTTP
    /// transport's format — indexer call sites log the string verbatim.
    async fn call(&self, req: EngineRequest) -> Result<EngineResponse, String> {
        let context = format!("{} {}", req.method, req.path);
        let http_req = self
            .build_request(req)
            .map_err(|e| format!("{} failed: {}", context, e))?;
        self.dispatch(http_req)
            .await
            .map_err(|e| format!("{} failed: {}", context, e))
    }
}

impl EngineTransport for EmbeddedEngine {
    fn execute<'a>(
        &'a self,
        req: EngineRequest,
    ) -> futures::future::BoxFuture<'a, Result<EngineResponse, String>> {
        Box::pin(self.call(req))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// Process-wide engine singleton. `None` until `vectorlink_init` runs.
static ENGINE: RwLock<Option<Arc<EmbeddedEngine>>> = RwLock::new(None);

fn engine() -> Option<Arc<EmbeddedEngine>> {
    ENGINE.read().unwrap().clone()
}

/// Build the embedded engine: open the Lance store under `data_dir`,
/// load the tokenizer from `config.tokenizer_path`, construct the
/// `SearchService` and axum router, then install the engine as the
/// indexer transport.
///
/// Embedding provider settings come from the standard `VECTORLINK_*`
/// environment variables via `Config::from_env` (e.g.
/// `VECTORLINK_EMBED_PROVIDER`, `VECTORLINK_EMBED_URL`,
/// `VECTORLINK_MODEL`, `VECTORLINK_DIM`). `data_dir` is supplied by
/// TerminusDB instead.
fn init_engine(data_dir: String, tokenizer_path: String) -> Result<(), String> {
    if engine().is_some() {
        // Idempotent: a second init (e.g. after a plugin reload) is a
        // no-op. The engine is process-wide by design.
        return Ok(());
    }

    let handle = terminusdb_webserver::dispatch::tokio_handle().ok_or_else(|| {
        "tokio runtime not available — embedded vectorlink requires \
         TERMINUSDB_SERVER_BACKEND=rust"
            .to_string()
    })?;

    let mut config = vectorlink::config::Config::from_env();
    config.data_dir = data_dir;
    // TerminusDB supplies the tokenizer path (resolved against the builtin
    // plugin dir or VECTORLINK_TOKENIZER_PATH) so the asset travels with
    // the embedding model choice, not the binary.
    if !tokenizer_path.is_empty() {
        config.tokenizer_path = tokenizer_path;
    }

    let tokenizer = vectorlink::chunk::io_load_tokenizer(std::path::Path::new(
        &config.tokenizer_path,
    ))
    .map_err(|e| format!("failed to load tokenizer {}: {}", config.tokenizer_path, e))?;

    let data_path = std::path::Path::new(&config.data_dir);
    std::fs::create_dir_all(data_path)
        .map_err(|e| format!("failed to create data dir {}: {}", config.data_dir, e))?;

    let dim = config.embed_provider.expected_dim();
    let store = Arc::new(vectorlink::store::lance::LanceStore::new(
        data_path,
        dim,
        config.lance_index_cache_bytes,
        config.lance_metadata_cache_bytes,
    ));

    // Startup cleanup: remove stale compaction branches left by a
    // crashed compaction (mirrors vectorlink's own server startup).
    let cleanup_store = store.clone();
    let cleaned = handle
        .block_on(async move { cleanup_store.io_cleanup_compaction_branches().await })
        .map_err(|e| format!("compaction branch cleanup failed: {}", e))?;
    if !cleaned.is_empty() {
        terminusdb_webserver::log::log_info(format!(
            "[vectorlink] cleaned {} stale compaction branch(es) from crashed compaction(s)",
            cleaned.len()
        ));
    }

    let service = vectorlink::service::SearchService::new(store, config.clone(), tokenizer);
    // No auth layer: requests only reach this router through the FFI from
    // already-authorized Prolog handlers.
    let router = vectorlink::http_api::trusted_router(service, config.clone());

    let engine = Arc::new(EmbeddedEngine { router });
    *ENGINE.write().unwrap() = Some(engine.clone());
    set_engine_transport(engine);

    terminusdb_webserver::log::log_info(format!(
        "[vectorlink] embedded engine initialized | data_dir={} | model={}",
        config.data_dir,
        config.embed_provider.model_name()
    ));
    Ok(())
}

/// Parse an HTTP method string case-insensitively.
///
/// `http::Method` is case-sensitive: `from_bytes(b"get")` yields an
/// extension method that axum's router will not match as GET (405).
fn parse_method(s: &str) -> Result<axum::http::Method, String> {
    axum::http::Method::from_bytes(s.to_uppercase().as_bytes())
        .map_err(|_| format!("invalid HTTP method: {}", s))
}

/// Collect a body stream into a byte buffer (test helper).
#[cfg(test)]
async fn collect_stream(mut stream: ByteStream) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    while let Some(chunk) = stream.next().await {
        out.extend_from_slice(&chunk?);
    }
    Ok(out)
}

predicates! {
    /// Initialize the embedded vectorlink engine.
    ///
    /// Signature:
    /// `$vectorlink:vectorlink_init(+DataDir, +TokenizerPathOrEmpty)`.
    /// Called once at startup from the vectorlink plugin when
    /// `TERMINUSDB_INDEXER_BACKEND=vectorlink`. Idempotent. An empty
    /// tokenizer path falls back to `Config::from_env` (i.e.
    /// `VECTORLINK_TOKENIZER_PATH` or its default).
    #[module("$vectorlink")]
    pub semidet fn vectorlink_init(_context, data_dir_term, tokenizer_term) {
        let data_dir: PrologText = data_dir_term.get_ex()?;
        let tokenizer: PrologText = tokenizer_term.get_ex()?;
        init_engine(data_dir.into_inner(), tokenizer.into_inner())
        .map_err(|e| {
            terminusdb_webserver::log::log_error(format!(
                "[vectorlink] init failed: {}",
                e
            ));
            PrologError::Failure
        })
    }

    /// Forward one request to the embedded vectorlink engine.
    ///
    /// Signature:
    /// `$vectorlink:vectorlink_request(+Method, +Path, +HeadersOrNone, +BodyOrNone, -MetaJson, -ReadFd)`.
    ///
    /// Method is an atom/string (`get`, `post`, `delete`, ...), Path is the
    /// path + query string (e.g. `"/search?domain=admin/db&branch=main"`),
    /// HeadersOrNone is a JSON object string of extra request headers or
    /// the atom `none`, BodyOrNone is a string body or the atom `none`.
    /// No credentials are needed — the embedded router is built without
    /// the engine's auth layer.
    ///
    /// MetaJson unifies with a JSON string `{"status":N,"headers":{...}}`
    /// — parse it with `atom_json_dict/3`. ReadFd unifies with a POSIX
    /// read-end fd carrying the streaming response body; wrap it with
    /// `$appserver:appserver_open_fd_stream(Fd, read, utf8, In)` then
    /// `appserver_close_fd/1`. A tokio task pumps engine chunks into the
    /// pipe, so consumers get real backpressure: a full kernel buffer
    /// stalls the engine stream until Prolog reads. Fails when the
    /// engine is not initialized or the request cannot be dispatched.
    #[module("$vectorlink")]
    pub semidet fn vectorlink_request(_context, method_term, path_term, headers_term, body_term, response_term, fd_term) {
        let method_str = term_to_string(&method_term)?;
        let path: PrologText = path_term.get_ex()?;
        let headers_json = term_to_string(&headers_term)?;
        let body_is_none = body_term.is_atom()
            && body_term.get_ex::<Atom>().map(|a| a.name() == "none").unwrap_or(false);

        let engine = match engine() {
            Some(e) => e,
            None => {
                terminusdb_webserver::log::log_error(
                    "[vectorlink] request failed: engine not initialized".to_string()
                );
                return Err(PrologError::Failure);
            }
        };
        let handle = match terminusdb_webserver::dispatch::tokio_handle() {
            Some(h) => h,
            None => {
                terminusdb_webserver::log::log_error(
                    "[vectorlink] request failed: tokio runtime not available".to_string()
                );
                return Err(PrologError::Failure);
            }
        };

        let method = match parse_method(&method_str) {
            Ok(m) => m,
            Err(e) => {
                terminusdb_webserver::log::log_error(format!(
                    "[vectorlink] request failed: {}",
                    e
                ));
                return Err(PrologError::Failure);
            }
        };

        let mut builder = axum::http::Request::builder()
            .method(method)
            .uri(path.into_inner());
        if headers_json != "none" {
            let map: serde_json::Map<String, serde_json::Value> =
                match serde_json::from_str(&headers_json) {
                    Ok(m) => m,
                    Err(e) => {
                        terminusdb_webserver::log::log_error(format!(
                            "[vectorlink] request failed: bad headers json: {}",
                            e
                        ));
                        return Err(PrologError::Failure);
                    }
                };
            for (name, value) in map {
                if let Some(v) = value.as_str() {
                    builder = builder.header(name, v);
                }
            }
        }
        let body = if body_is_none {
            axum::body::Body::empty()
        } else {
            let req_body: PrologText = body_term.get_ex()?;
            axum::body::Body::from(req_body.into_inner())
        };
        let http_req = match builder.body(body) {
            Ok(r) => r,
            Err(e) => {
                terminusdb_webserver::log::log_error(format!(
                    "[vectorlink] request failed: {}",
                    e
                ));
                return Err(PrologError::Failure);
            }
        };

        let resp = handle
            .block_on(engine.dispatch(http_req))
            .map_err(|e| {
                terminusdb_webserver::log::log_error(format!(
                    "[vectorlink] request failed: {}",
                    e
                ));
                PrologError::Failure
            })?;

        // Enter the runtime so pipe::Sender::from_owned_fd (AsyncFd)
        // sees a reactor — FFI threads are not runtime threads.
        let _runtime_guard = handle.enter();

        // Pipe the response body to Prolog through a kernel pipe —
        // the same mechanism the request worker pool uses for handler
        // output. The write end is pumped by a tokio task; EPIPE when
        // the reader goes away ends the pump and drops the stream.
        let (read_fd, write_fd) = match nix::unistd::pipe() {
            Ok(p) => p,
            Err(e) => {
                terminusdb_webserver::log::log_error(format!(
                    "[vectorlink] request failed: pipe() {}",
                    e
                ));
                return Err(PrologError::Failure);
            }
        };
        let mut sender = match tokio::net::unix::pipe::Sender::from_owned_fd(write_fd) {
            Ok(s) => s,
            Err(e) => {
                terminusdb_webserver::log::log_error(format!(
                    "[vectorlink] request failed: pipe sender {}",
                    e
                ));
                return Err(PrologError::Failure);
            }
        };
        let mut body = resp.body;
        handle.spawn(async move {
            use tokio::io::AsyncWriteExt;
            while let Some(chunk) = body.next().await {
                match chunk {
                    Ok(bytes) => {
                        if sender.write_all(&bytes).await.is_err() {
                            break;
                        }
                    }
                    Err(e) => {
                        terminusdb_webserver::log::log_error(format!(
                            "[vectorlink] response body stream error, truncating: {}",
                            e
                        ));
                        break;
                    }
                }
            }
            // sender dropped here → reader sees EOF.
        });

        let headers: serde_json::Map<String, serde_json::Value> = resp
            .headers
            .iter()
            .map(|(name, value)| {
                (
                    name.as_str().to_string(),
                    serde_json::Value::String(
                        value.to_str().unwrap_or_default().to_string(),
                    ),
                )
            })
            .collect();
        let meta_json = serde_json::json!({
            "status": resp.status,
            "headers": headers,
        });
        let json_str = serde_json::to_string(&meta_json)
            .map_err(|_| PrologError::Failure)?;
        response_term.unify(&json_str).map_err(|_| PrologError::Failure)?;
        // Borrow the fd for unify so a failure still drops (closes) the
        // OwnedFd; only release ownership once Prolog has the number.
        fd_term
            .unify(&(std::os::fd::AsRawFd::as_raw_fd(&read_fd) as i64))
            .map_err(|_| PrologError::Failure)?;
        let _ = read_fd.into_raw_fd();
        Ok(())
    }
}

/// Extract a string from a term that may be an atom or a string.
fn term_to_string(term: &Term) -> PrologResult<String> {
    if term.is_atom() {
        let atom: Atom = term.get_ex()?;
        Ok(atom.name())
    } else {
        term.get_ex::<String>()
    }
}

/// Called from `terminusdb-dylib`'s `install()` during dylib load.
pub fn install() {
    register_vectorlink_init();
    register_vectorlink_request();
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::http::HeaderMap;
    use axum::routing::{get, post};
    use axum::Router;

    /// A stub engine: /echo reflects a header + the body, /status/N
    /// returns the given status, /chunks streams a multi-chunk body.
    fn test_engine() -> EmbeddedEngine {
        let router = Router::new()
            .route(
                "/echo",
                post(|headers: HeaderMap, body: String| async move {
                    format!(
                        "{}|{}|{}",
                        headers
                            .get("authorization")
                            .and_then(|v| v.to_str().ok())
                            .unwrap_or(""),
                        headers
                            .get("x-test")
                            .and_then(|v| v.to_str().ok())
                            .unwrap_or(""),
                        body
                    )
                }),
            )
            .route(
                "/status",
                get(|| async {
                    (axum::http::StatusCode::SERVICE_UNAVAILABLE, "retry later")
                }),
            )
            .route(
                "/chunks",
                get(|| async {
                    axum::body::Body::from_stream(futures::stream::iter([
                        Ok::<_, std::io::Error>(axum::body::Bytes::from_static(b"one\n")),
                        Ok(axum::body::Bytes::from_static(b"two\n")),
                    ]))
                }),
            );
        EmbeddedEngine { router }
    }

    fn get_req(path: &str) -> EngineRequest {
        EngineRequest {
            method: axum::http::Method::GET,
            path: path.to_string(),
            body: None,
            content_type: None,
            timeout: None,
        }
    }

    #[tokio::test]
    async fn dispatch_preserves_status_and_body() {
        let engine = test_engine();
        let resp = engine.call(get_req("/status")).await.unwrap();
        assert_eq!(resp.status, 503);
        let body = collect_stream(resp.body).await.unwrap();
        assert_eq!(body, b"retry later");
    }

    #[tokio::test]
    async fn dispatch_streams_multi_chunk_response() {
        let engine = test_engine();
        let resp = engine.call(get_req("/chunks")).await.unwrap();
        assert_eq!(resp.status, 200);
        let body = collect_stream(resp.body).await.unwrap();
        assert_eq!(body, b"one\ntwo\n");
    }

    #[tokio::test]
    async fn dispatch_streams_request_body_and_injects_auth() {
        let engine = test_engine();
        let body = boxed_sync_stream(futures::stream::iter([
            Ok(axum::body::Bytes::from_static(b"{\"a\":1}\n")),
            Ok(axum::body::Bytes::from_static(b"{\"a\":2}\n")),
        ]));
        let resp = engine
            .call(EngineRequest {
                method: axum::http::Method::POST,
                path: "/echo".to_string(),
                body: Some(body),
                content_type: Some("application/x-ndjson".to_string()),
                timeout: None,
            })
            .await
            .unwrap();
        assert_eq!(resp.status, 200);
        let body = collect_stream(resp.body).await.unwrap();
        // No auth is injected — in-process callers are trusted.
        assert_eq!(body, b"||{\"a\":1}\n{\"a\":2}\n".as_slice());
    }

    #[tokio::test]
    async fn unknown_route_returns_404() {
        let engine = test_engine();
        let resp = engine.call(get_req("/missing")).await.unwrap();
        assert_eq!(resp.status, 404);
    }

    /// Boot the real vectorlink router in-process and exercise the GET
    /// routes the Prolog plugin forwards. The engine has no embed
    /// provider reachable, so /search may fail inside the handler —
    /// but it must route (not 405).
    #[tokio::test]
    async fn real_vectorlink_router_routes_get_search() {
        let dir = std::env::temp_dir().join(format!("vl-shim-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let mut config =
            vectorlink::config::Config::new("admin".to_string(), "root".to_string(), 0);
        config.data_dir = dir.to_string_lossy().into_owned();
        let dim = config.embed_provider.expected_dim();
        let tokenizer = vectorlink::chunk::io_load_tokenizer(std::path::Path::new(
            concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../../../builtin_plugins/assets/tokenizer.json.bz2"
            ),
        ))
        .unwrap();
        let store = Arc::new(vectorlink::store::lance::LanceStore::new(
            &dir,
            dim,
            4 * 1024 * 1024,
            4 * 1024 * 1024,
        ));
        let service = vectorlink::service::SearchService::new(store, config.clone(), tokenizer);
        let engine = EmbeddedEngine {
            router: vectorlink::http_api::trusted_router(service, config),
        };

        let resp = engine
            .call(get_req("/last-indexed?domain=admin%2Fdb&branch=main"))
            .await
            .unwrap();
        assert_eq!(resp.status, 200, "last-indexed should route");

        let resp = engine
            .call(get_req("/search?domain=admin%2Fdb&branch=main&q=test"))
            .await
            .unwrap();
        let body = collect_stream(resp.body).await.unwrap();
        assert_ne!(
            resp.status,
            405,
            "GET /search must route to handle_search_get, body: {}",
            String::from_utf8_lossy(&body)
        );
    }

    /// Transport errors carry "{METHOD} {path} failed: {cause}" — the
    /// indexer logs the string verbatim, so pin the format.
    #[tokio::test]
    async fn call_error_includes_method_and_path() {
        let engine = test_engine();
        let err = engine
            .call(EngineRequest {
                method: axum::http::Method::GET,
                path: "bad uri with spaces".to_string(),
                body: None,
                content_type: None,
                timeout: None,
            })
            .await
            .err()
            .expect("invalid URI must fail");
        assert!(
            err.starts_with("GET bad uri with spaces failed:"),
            "unexpected error format: {}",
            err
        );
    }

    /// Prolog sends lowercase method atoms ("get", "post"). `http::Method`
    /// is case-sensitive, so these must normalize to the standard methods —
    /// otherwise the router sees an extension method and returns 405.
    #[test]
    fn parse_method_normalizes_lowercase() {
        assert_eq!(parse_method("get").unwrap(), axum::http::Method::GET);
        assert_eq!(parse_method("post").unwrap(), axum::http::Method::POST);
        assert_eq!(parse_method("delete").unwrap(), axum::http::Method::DELETE);
        assert_eq!(parse_method("GET").unwrap(), axum::http::Method::GET);
        assert!(parse_method("not a method").is_err());
    }
}
