use axum::{
    body::Body,
    extract::Path,
    http::{header, HeaderMap, Request, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use futures::Stream;
use percent_encoding::percent_decode_str;
use serde_json::json;
use std::{
    collections::HashMap,
    path::{Component, Path as StdPath, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
};
use swipl::prelude::*;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;

/// A route registered by a Prolog plugin.
#[derive(Clone, Debug)]
pub struct PluginRoute {
    pub method: String,
    pub path: String,
    pub module: String,
    pub handler: String,
}

/// A static file serving endpoint registered by a Prolog plugin.
#[derive(Clone, Debug)]
pub struct PluginStaticPath {
    pub prefix: String,
    pub directory: String,
    pub fallback: Option<String>,
    pub auth: String,
}

/// A streaming endpoint registered by a Prolog plugin.
#[derive(Clone, Debug)]
pub struct PluginStream {
    pub method: String,
    pub path: String,
    pub module: String,
    pub handler: String,
}

/// Unique identifier for an active streaming response.
pub type StreamId = u64;

/// Registry of active stream senders.
#[derive(Default)]
pub struct StreamRegistry {
    next_id: StreamId,
    senders: HashMap<StreamId, mpsc::Sender<axum::body::Bytes>>,
}

impl StreamRegistry {
    fn new() -> Self {
        Self::default()
    }

    fn add(&mut self, sender: mpsc::Sender<axum::body::Bytes>) -> StreamId {
        let id = self.next_id;
        self.next_id += 1;
        self.senders.insert(id, sender);
        id
    }

    pub fn remove(&mut self, id: StreamId) {
        self.senders.remove(&id);
    }

    #[cfg(test)]
    pub fn senders(&self) -> Vec<mpsc::Sender<axum::body::Bytes>> {
        self.senders.values().cloned().collect()
    }

    pub fn send(&mut self, id: StreamId, bytes: axum::body::Bytes) -> Result<(), String> {
        match self.senders.get(&id) {
            Some(sender) => match sender.try_send(bytes) {
                Ok(()) => Ok(()),
                Err(mpsc::error::TrySendError::Closed(_)) => {
                    self.senders.remove(&id);
                    Err("stream closed".to_string())
                }
                Err(mpsc::error::TrySendError::Full(_)) => {
                    Err("stream buffer full".to_string())
                }
            },
            None => Err("stream not found".to_string()),
        }
    }
}

lazy_static::lazy_static! {
    static ref STREAM_REGISTRY: Arc<Mutex<StreamRegistry>> = Arc::new(Mutex::new(StreamRegistry::new()));
}

/// Return a handle to the global stream registry.
pub fn stream_registry() -> Arc<Mutex<StreamRegistry>> {
    STREAM_REGISTRY.clone()
}

/// Registry of named broadcast channels.
///
/// Prolog publishes to a channel by name; Rust forwards the message to every
/// connected stream that has subscribed to that channel. Senders are stored in
/// the existing `StreamRegistry`, so channel cleanup is driven by stream
/// lifecycle (via `StreamGuard::drop`).
#[derive(Default)]
pub struct BroadcastRegistry {
    channels: HashMap<String, std::collections::HashSet<StreamId>>,
}

impl BroadcastRegistry {
    fn new() -> Self {
        Self::default()
    }

    pub fn subscribe(&mut self, channel: String, stream_id: StreamId) {
        self.channels.entry(channel).or_default().insert(stream_id);
    }

    pub fn unsubscribe(&mut self, channel: &str, stream_id: StreamId) {
        if let Some(set) = self.channels.get_mut(channel) {
            set.remove(&stream_id);
        }
    }

    pub fn unsubscribe_all(&mut self, stream_id: StreamId) {
        for set in self.channels.values_mut() {
            set.remove(&stream_id);
        }
    }

    pub fn broadcast(
        &mut self,
        channel: &str,
        bytes: axum::body::Bytes,
        stream_registry: &mut StreamRegistry,
    ) -> Result<(), String> {
        let stream_ids: Vec<StreamId> = self
            .channels
            .get(channel)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect();
        if stream_ids.is_empty() {
            return Ok(());
        }

        let mut failed: Vec<StreamId> = Vec::new();
        for stream_id in stream_ids {
            if stream_registry.send(stream_id, bytes.clone()).is_err() {
                failed.push(stream_id);
            }
        }

        if let Some(set) = self.channels.get_mut(channel) {
            for stream_id in failed {
                set.remove(&stream_id);
            }
        }

        Ok(())
    }
}

lazy_static::lazy_static! {
    static ref BROADCAST_REGISTRY: Arc<Mutex<BroadcastRegistry>> = Arc::new(Mutex::new(BroadcastRegistry::new()));
}

/// Return a handle to the global broadcast registry.
pub fn broadcast_registry() -> Arc<Mutex<BroadcastRegistry>> {
    BROADCAST_REGISTRY.clone()
}

/// Resolve a static directory path.
///
/// Absolute paths are returned unchanged. Relative paths are resolved against
/// the `TERMINUSDB_ADDON_PATH` environment variable, or the current working
/// directory if the variable is not set.
fn resolve_static_directory(directory: &str) -> PathBuf {
    let path = PathBuf::from(directory);
    if path.is_absolute() {
        return path;
    }

    std::env::var("TERMINUSDB_ADDON_PATH")
        .map(PathBuf::from)
        .or_else(|_| std::env::current_dir())
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(path)
}

/// Convert a Prolog term to a Rust String, accepting either an atom or a string term.
fn term_to_string(term: &Term) -> PrologResult<String> {
    if term.is_atom() {
        let atom: Atom = term.get_ex()?;
        Ok(atom.name())
    } else {
        term.get_ex::<String>()
    }
}

/// Collect routes registered by Prolog plugins through the
/// `appserver_hooks:appserver_route/3` hook.
///
/// Each handler must be specified as `Module:Handler`, where `Handler` is a
/// predicate with arity two (`+Request, -Response`).
///
/// This is called from the `appserver_start` Prolog predicate,
/// so a Prolog engine is active.
pub fn collect_routes(context: &Context<impl QueryableContextType>) -> PrologResult<Vec<PluginRoute>> {
    let frame = context.open_frame();
    let [method_term, path_term, handler_term] = frame.new_term_refs();

    let open_call = frame.open(
        pred!("appserver_hooks:appserver_route/3"),
        [&method_term, &path_term, &handler_term],
    );

    let mut routes = Vec::new();
    while attempt_opt(open_call.next_solution())?.is_some() {
        let method: Atom = method_term.get_ex()?;
        let path = term_to_string(&path_term)?;
        let module: Atom = handler_term.get_arg(1)?;
        let handler: Atom = handler_term.get_arg(2)?;
        let module = module.name();
        let handler = handler.name();
        routes.push(PluginRoute {
            method: method.name(),
            path,
            module,
            handler,
        });
    }

    Ok(routes)
}

/// Collect static file serving endpoints registered by Prolog plugins.
///
/// Plugins register via `appserver_hooks:appserver_static_path/3` with
/// an option list. Prolog normalizes these into
/// `appserver_hooks:appserver_static_path_normalized/4`, which this
/// function reads.
///
/// Arguments are: prefix, directory, fallback file, auth style.
pub fn collect_static_paths(
    context: &Context<impl QueryableContextType>,
) -> PrologResult<Vec<PluginStaticPath>> {
    let frame = context.open_frame();
    let [prefix_term, directory_term, fallback_term, auth_term] = frame.new_term_refs();

    let open_call = frame.open(
        pred!("appserver_hooks:appserver_static_path_normalized/4"),
        [&prefix_term, &directory_term, &fallback_term, &auth_term],
    );

    let mut paths = Vec::new();
    while attempt_opt(open_call.next_solution())?.is_some() {
        let prefix = term_to_string(&prefix_term)?;
        let directory = term_to_string(&directory_term)?;
        let directory = resolve_static_directory(&directory)
            .to_string_lossy()
            .into_owned();
        let fallback: Atom = fallback_term.get_ex()?;
        let auth: Atom = auth_term.get_ex()?;

        let fallback = if fallback.name().is_empty() {
            None
        } else {
            Some(fallback.name())
        };

        paths.push(PluginStaticPath {
            prefix,
            directory,
            fallback,
            auth: auth.name(),
        });
    }

    Ok(paths)
}

/// Collect streaming endpoints registered by Prolog plugins through the
/// `appserver_hooks:appserver_stream/3` hook.
///
/// Each registration is: method, path, handler (as `Module:Handler`).
/// The handler must have arity 3 and accept `+Request`, `+StreamId`, `-Response`.
pub fn collect_streams(context: &Context<impl QueryableContextType>) -> PrologResult<Vec<PluginStream>> {
    let frame = context.open_frame();
    let [method_term, path_term, handler_term] = frame.new_term_refs();

    let open_call = frame.open(
        pred!("appserver_hooks:appserver_stream/3"),
        [&method_term, &path_term, &handler_term],
    );

    let mut streams = Vec::new();
    while attempt_opt(open_call.next_solution())?.is_some() {
        let method: Atom = method_term.get_ex()?;
        let path = term_to_string(&path_term)?;
        let module: Atom = handler_term.get_arg(1)?;
        let handler: Atom = handler_term.get_arg(2)?;
        streams.push(PluginStream {
            method: method.name(),
            path,
            module: module.name(),
            handler: handler.name(),
        });
    }

    Ok(streams)
}

/// Build a router with the plugin routes.
///
/// Registrations that share the same method and path are pooled. Incoming
/// requests are distributed round-robin among the registered handlers, so a
/// plugin can register several workers for the same endpoint without needing
/// its own load balancing.
pub fn build_plugin_router(routes: Vec<PluginRoute>) -> Router {
    let mut groups: HashMap<(String, String), Vec<(String, String)>> = HashMap::new();
    for route in routes {
        groups
            .entry((route.method, route.path))
            .or_default()
            .push((route.module, route.handler));
    }

    let mut router = Router::new();
    for ((method, path), handlers) in groups {
        let pool = Arc::new(HandlerPool::new(handlers));
        let pattern = path.clone();
        let route_handler = move |req: Request<Body>| async move {
            let (module, handler) = pool.pick();
            dispatch_plugin_request(module, handler, pattern.clone(), req).await
        };

        router = match method.as_str() {
            "get" => router.route(&path, axum::routing::get(route_handler)),
            "head" => router.route(&path, axum::routing::head(route_handler)),
            "options" => router.route(&path, axum::routing::options(route_handler)),
            "post" => router.route(&path, axum::routing::post(route_handler)),
            "put" => router.route(&path, axum::routing::put(route_handler)),
            "delete" => router.route(&path, axum::routing::delete(route_handler)),
            _ => router,
        };
    }

    router
}

/// A pool of Prolog handlers for a single endpoint.
///
/// Handlers are selected round-robin. This lets plugins register multiple
/// workers for the same path and have Rust distribute the load.
struct HandlerPool {
    handlers: Vec<(String, String)>,
    next: AtomicUsize,
}

impl HandlerPool {
    fn new(handlers: Vec<(String, String)>) -> Self {
        Self {
            handlers,
            next: AtomicUsize::new(0),
        }
    }

    fn pick(&self) -> (String, String) {
        let index = self.next.fetch_add(1, Ordering::Relaxed) % self.handlers.len();
        self.handlers[index].clone()
    }
}

/// Build a router for plugin-registered static file serving endpoints.
///
/// Each registered prefix is served by a symlink-safe handler. Only files whose
/// canonical path is inside the addon directory are served. If a fallback file
/// is configured, it is served when a concrete file is not found, enabling
/// SPA-style routing.
pub fn build_static_router(static_paths: Vec<PluginStaticPath>) -> Router {
    let mut router = Router::new();
    for static_path in static_paths {
        let dir = PathBuf::from(&static_path.directory);
        let fallback = static_path.fallback.as_ref().map(|f| dir.join(f));

        let root_handler = {
            let dir = dir.clone();
            let fallback = fallback.clone();
            move || async move { serve_static_file(&dir, fallback.as_deref(), "/").await }
        };

        let sub_handler = {
            let dir = dir.clone();
            let fallback = fallback.clone();
            move |Path(uri_path): Path<String>| async move {
                serve_static_file(&dir, fallback.as_deref(), &uri_path).await
            }
        };

        router = router
            .route(&static_path.prefix, get(root_handler.clone()))
            .route(&format!("{}/", static_path.prefix), get(root_handler))
            .route(&format!("{}/*path", static_path.prefix), get(sub_handler));
    }
    router
}

/// Build a router for plugin-registered streaming endpoints.
///
/// Registrations that share the same method and path are pooled. Each incoming
/// connection is assigned to one handler round-robin, so a plugin can register
/// a pool of stream workers for the same endpoint.
pub fn build_stream_router(streams: Vec<PluginStream>) -> Router {
    let mut groups: HashMap<(String, String), Vec<(String, String)>> = HashMap::new();
    for stream in streams {
        groups
            .entry((stream.method, stream.path))
            .or_default()
            .push((stream.module, stream.handler));
    }

    let mut router = Router::new();
    for ((method, path), handlers) in groups {
        let pool = Arc::new(HandlerPool::new(handlers));
        let pattern = path.clone();
        let route_handler = move |req: Request<Body>| async move {
            let (module, handler) = pool.pick();
            dispatch_stream_request(module, handler, pattern.clone(), req).await
        };

        router = match method.as_str() {
            "get" => router.route(&path, get(route_handler)),
            "post" => router.route(&path, post(route_handler)),
            "put" => router.route(&path, axum::routing::put(route_handler)),
            "delete" => router.route(&path, axum::routing::delete(route_handler)),
            _ => router,
        };
    }
    router
}

/// Serve a static file for a single plugin-registered prefix.
///
/// The request path is resolved relative to `dir`, percent-decoded, and checked
/// for directory traversal. The canonical path is then verified to be within
/// `dir` (after resolving symlinks). Missing files, unsafe paths, and symlink
/// escapes all result in a 404 response.
async fn serve_static_file(dir: &StdPath, fallback: Option<&StdPath>, uri_path: &str) -> Response<Body> {
    let canonical_dir = match tokio::fs::canonicalize(dir).await {
        Ok(d) => d,
        Err(_) => return not_found_response(),
    };

    let decoded_path = percent_decode_str(uri_path).decode_utf8_lossy();
    let mut candidate = canonical_dir.clone();
    for component in StdPath::new(decoded_path.as_ref()).components() {
        match component {
            Component::Normal(name) => candidate.push(name),
            Component::RootDir => {}
            // Reject `..`, Windows prefixes, and other special components.
            _ => return not_found_response(),
        }
    }

    // Try the requested file (or its index.html for directories).
    if let Some(path) = canonical_file_within_dir(&canonical_dir, &candidate, true).await {
        return serve_file_bytes(&path).await;
    }

    // Try the configured fallback file.
    if let Some(fallback) = fallback {
        if let Some(path) = canonical_file_within_dir(&canonical_dir, fallback, false).await {
            return serve_file_bytes(&path).await;
        }
    }

    not_found_response()
}

/// Canonicalize `candidate` and return it only if it is a file inside `dir`.
///
/// When `allow_index` is true, a directory candidate is also accepted if it
/// contains an `index.html` that stays within `dir`. Symlinks are resolved by
/// `canonicalize`, so a symlink pointing outside `dir` is rejected.
async fn canonical_file_within_dir(dir: &StdPath, candidate: &StdPath, allow_index: bool) -> Option<PathBuf> {
    if let Ok(path) = tokio::fs::canonicalize(candidate).await {
        if path.starts_with(dir) {
            if let Ok(metadata) = tokio::fs::metadata(&path).await {
                if metadata.is_file() {
                    return Some(path);
                }
                if allow_index && metadata.is_dir() {
                    let index = path.join("index.html");
                    if let Ok(index) = tokio::fs::canonicalize(&index).await {
                        if index.starts_with(dir) {
                            if let Ok(index_metadata) = tokio::fs::metadata(&index).await {
                                if index_metadata.is_file() {
                                    return Some(index);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Read a file and return it as a response with a MIME type derived from the
/// extension.
async fn serve_file_bytes(path: &StdPath) -> Response<Body> {
    match tokio::fs::read(path).await {
        Ok(bytes) => Response::builder()
            .status(StatusCode::OK)
            .header(header::CONTENT_TYPE, static_mime_type(path))
            .body(Body::from(bytes))
            .unwrap(),
        Err(_) => not_found_response(),
    }
}

/// Return a JSON 404 response.
fn not_found_response() -> Response<Body> {
    let body = json!({
        "@type": "api:ErrorResponse",
        "api:status": "api:not_found",
        "api:error": {"@type": "api:NotFound"},
        "api:message": "Resource not found"
    });
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(body.to_string()))
        .unwrap()
}

/// Simple MIME type mapping based on file extension.
fn static_mime_type(path: &StdPath) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some("html") => "text/html",
        Some("css") => "text/css",
        Some("js") => "application/javascript",
        Some("json") => "application/json",
        Some("png") => "image/png",
        Some("jpg") | Some("jpeg") => "image/jpeg",
        Some("svg") => "image/svg+xml",
        Some("ico") => "image/x-icon",
        Some("woff") => "font/woff",
        Some("woff2") => "font/woff2",
        Some("ttf") => "font/ttf",
        Some("otf") => "font/otf",
        Some("txt") => "text/plain",
        _ => "application/octet-stream",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        body::Body,
        http::{Request, StatusCode},
    };
    use tower::ServiceExt;

    #[tokio::test]
    async fn static_router_serves_existing_file() {
        let dir = tempfile::tempdir().unwrap();
        let app_dir = dir.path().join("app");
        tokio::fs::create_dir(&app_dir).await.unwrap();
        tokio::fs::write(app_dir.join("hello.txt"), "hello").await.unwrap();

        let router = build_static_router(vec![PluginStaticPath {
            prefix: "/app".to_string(),
            directory: app_dir.to_string_lossy().into_owned(),
            fallback: None,
            auth: "none".to_string(),
        }]);

        let response = router
            .oneshot(Request::builder().uri("/app/hello.txt").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn static_router_rejects_symlink_outside_directory() {
        let dir = tempfile::tempdir().unwrap();
        let app_dir = dir.path().join("app");
        tokio::fs::create_dir(&app_dir).await.unwrap();

        let outside = dir.path().join("secret.txt");
        tokio::fs::write(&outside, "secret").await.unwrap();
        let link = app_dir.join("link.txt");
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&outside, &link).unwrap();
        }
        #[cfg(windows)]
        {
            std::os::windows::fs::symlink_file(&outside, &link).unwrap();
        }

        let router = build_static_router(vec![PluginStaticPath {
            prefix: "/app".to_string(),
            directory: app_dir.to_string_lossy().into_owned(),
            fallback: None,
            auth: "none".to_string(),
        }]);

        let response = router
            .oneshot(Request::builder().uri("/app/link.txt").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[test]
    fn extract_route_params_parses_wildcards() {
        let mut params = extract_route_params("/files/:id", "/files/123");
        assert_eq!(params.get("id"), Some(&"123".to_string()));

        params = extract_route_params("/files/:id/*path", "/files/123/a/b/c");
        assert_eq!(params.get("id"), Some(&"123".to_string()));
        assert_eq!(params.get("path"), Some(&"a/b/c".to_string()));

        params = extract_route_params("/static/*path", "/static/app/alpha/index.html");
        assert_eq!(params.get("path"), Some(&"app/alpha/index.html".to_string()));

        params = extract_route_params("/api/info", "/api/info");
        assert!(params.is_empty());
    }

    #[test]
    fn handler_pool_distributes_round_robin() {
        let pool = HandlerPool::new(vec![
            ("a".to_string(), "h1".to_string()),
            ("b".to_string(), "h2".to_string()),
            ("c".to_string(), "h3".to_string()),
        ]);
        let picks: Vec<_> = (0..6).map(|_| pool.pick()).collect();
        assert_eq!(picks[0].0, "a");
        assert_eq!(picks[1].0, "b");
        assert_eq!(picks[2].0, "c");
        assert_eq!(picks[3].0, "a");
        assert_eq!(picks[4].0, "b");
        assert_eq!(picks[5].0, "c");
    }

    #[tokio::test]
    async fn stream_registry_sends_and_removes_on_close() {
        let mut registry = StreamRegistry::new();
        let (tx, mut rx) = mpsc::channel::<axum::body::Bytes>(10);
        let id = registry.add(tx);

        let event = axum::body::Bytes::from(r#"{"event":"test"}"#);
        assert!(registry.send(id, event.clone()).is_ok());
        let received = rx.recv().await.unwrap();
        assert_eq!(received, event);

        drop(rx);
        assert!(registry.send(id, event).is_err());
        assert!(registry.senders().is_empty());
    }

    #[tokio::test]
    async fn static_router_rejects_directory_traversal() {
        let dir = tempfile::tempdir().unwrap();
        let app_dir = dir.path().join("app");
        tokio::fs::create_dir(&app_dir).await.unwrap();
        tokio::fs::write(dir.path().join("secret.txt"), "secret").await.unwrap();

        let router = build_static_router(vec![PluginStaticPath {
            prefix: "/app".to_string(),
            directory: app_dir.to_string_lossy().into_owned(),
            fallback: None,
            auth: "none".to_string(),
        }]);

        let response = router
            .oneshot(
                Request::builder()
                    .uri("/app/../secret.txt")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn broadcast_registry_multiplexes_to_multiple_streams() {
        let mut stream_registry = StreamRegistry::new();
        let mut broadcast_registry = BroadcastRegistry::new();

        let (tx1, mut rx1) = mpsc::channel::<axum::body::Bytes>(10);
        let id1 = stream_registry.add(tx1);
        let (tx2, mut rx2) = mpsc::channel::<axum::body::Bytes>(10);
        let id2 = stream_registry.add(tx2);

        broadcast_registry.subscribe("actions".to_string(), id1);
        broadcast_registry.subscribe("actions".to_string(), id2);

        let event = axum::body::Bytes::from(r#"{"action":"test"}"#);
        assert!(
            broadcast_registry
                .broadcast("actions", event.clone(), &mut stream_registry)
                .is_ok()
        );

        let received1 = rx1.recv().await.unwrap();
        let received2 = rx2.recv().await.unwrap();
        assert_eq!(received1, event);
        assert_eq!(received2, event);
    }
}

/// Dispatch a plugin request to a Prolog handler.
///
/// The handler receives a request dict containing the HTTP method, path, query
/// string, headers, body, and any captured route parameters (`:name` and
/// `*name` wildcards). It returns a response dict.
async fn dispatch_plugin_request(
    module: String,
    handler: String,
    pattern: String,
    req: Request<Body>,
) -> impl IntoResponse {
    let (parts, body) = req.into_parts();
    let body_bytes = match hyper::body::to_bytes(body).await {
        Ok(bytes) => bytes,
        Err(_) => return plugin_error_response("failed to read request body").into_response(),
    };
    let body_string = String::from_utf8_lossy(&body_bytes).to_string();

    let method = parts.method.to_string();
    let path = parts.uri.path().to_string();
    let query = parts.uri.query().unwrap_or("").to_string();
    let headers: serde_json::Map<String, serde_json::Value> = parts
        .headers
        .iter()
        .map(|(k, v)| {
            let name = k.as_str().to_string();
            let value = v.to_str().unwrap_or("").to_string();
            (name, json!(value))
        })
        .collect();
    let params: serde_json::Map<String, serde_json::Value> = extract_route_params(&pattern, &path)
        .into_iter()
        .map(|(k, v)| (k, json!(v)))
        .collect();

    let request_json = json!({
        "method": method,
        "path": path,
        "query": query,
        "headers": headers,
        "body": body_string,
        "params": params,
    });
    crate::log::log_info(format!("{} {} (plugin)", method, path));

    let result = tokio::task::spawn_blocking(move || {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let request_term = context.new_term_ref();
        context
            .serialize_to_term(&request_term, &request_json)
            .map_err(|_| PrologError::Failure)?;

        let response_term = context.new_term_ref();
        let handler_atom = Atom::new(&handler);
        let module_atom = Atom::new(&module);
        let callable = CallablePredicate::new(Predicate::new(
            Functor::new(handler_atom, 2),
            Module::new(module_atom),
        ))
        .map_err(|_| PrologError::Failure)?;
        context.call_once(callable, [&request_term, &response_term])?;

        let response: serde_json::Value = context
            .deserialize_from_term(&response_term)
            .map_err(|_| PrologError::Failure)?;
        Ok(response)
    })
    .await
    .unwrap_or(Err(PrologError::Failure));

    match result {
        Ok(response) => {
            let status = response
                .get("status")
                .and_then(|s| s.as_u64())
                .unwrap_or(200) as u16;
            let status = StatusCode::from_u16(status).unwrap_or(StatusCode::OK);
            let body = response
                .get("body")
                .cloned()
                .unwrap_or(json!({"error": "empty response"}));
            let mut headers = HeaderMap::new();
            if let Some(headers_map) = response.get("headers").and_then(|h| h.as_object()) {
                for (key, value) in headers_map {
                    if let Some(value_str) = value.as_str() {
                        if let Ok(header_name) = key.parse::<header::HeaderName>() {
                            if let Ok(header_value) = value_str.parse() {
                                headers.insert(header_name, header_value);
                            }
                        }
                    }
                }
            }

            if let Some(text) = body.as_str() {
                let mut builder = Response::builder().status(status);
                for (key, value) in headers.iter() {
                    builder = builder.header(key, value);
                }
                builder.body(Body::from(text.to_string())).unwrap().into_response()
            } else {
                (status, headers, Json(body)).into_response()
            }
        }
        Err(_) => plugin_error_response("Plugin handler failed").into_response(),
    }
}

/// Extract parameters from a URI given a route pattern.
///
/// Supports the Axum/React Router style syntax:
/// - `:name` captures a single path segment.
/// - `*name` captures all remaining path segments (catch-all).
///
/// Returns an empty map if the pattern does not contain any captures.
fn extract_route_params(pattern: &str, uri: &str) -> HashMap<String, String> {
    let mut params = HashMap::new();
    let pattern_parts: Vec<&str> = pattern.split('/').collect();
    let uri_parts: Vec<&str> = uri.split('/').collect();

    let mut pattern_idx = 0;
    let mut uri_idx = 0;
    while pattern_idx < pattern_parts.len() && uri_idx < uri_parts.len() {
        let part = pattern_parts[pattern_idx];
        if let Some(name) = part.strip_prefix('*') {
            let remaining = uri_parts[uri_idx..].join("/");
            params.insert(name.to_string(), remaining);
            return params;
        } else if let Some(name) = part.strip_prefix(':') {
            params.insert(name.to_string(), uri_parts[uri_idx].to_string());
            pattern_idx += 1;
            uri_idx += 1;
        } else {
            pattern_idx += 1;
            uri_idx += 1;
        }
    }

    params
}

/// Guard that removes a stream sender from the registry when the stream ends.
struct StreamGuard {
    id: StreamId,
}

impl Drop for StreamGuard {
    fn drop(&mut self) {
        {
            let broadcast_arc = broadcast_registry();
            let mut broadcast = broadcast_arc.lock().unwrap();
            broadcast.unsubscribe_all(self.id);
        }
        let stream_arc = stream_registry();
        stream_arc.lock().unwrap().remove(self.id);
    }
}

/// Receiver stream that cleans up its registry entry on drop.
struct GuardedReceiverStream {
    inner: ReceiverStream<axum::body::Bytes>,
    _guard: StreamGuard,
}

impl Stream for GuardedReceiverStream {
    type Item = Result<axum::body::Bytes, std::convert::Infallible>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        std::pin::Pin::new(&mut self.inner)
            .poll_next(cx)
            .map(|opt| opt.map(Ok))
    }
}

/// Dispatch a streaming request to a Prolog handler.
///
/// The handler receives a request dict (with a UTF-8 body string) and a
/// `StreamId`. It returns a response dict. When the response body is the atom
/// `stream`, the response is an NDJSON stream fed by `appserver_stream_send/2`.
async fn dispatch_stream_request(
    module: String,
    handler: String,
    pattern: String,
    req: Request<Body>,
) -> impl IntoResponse {
    let (parts, body) = req.into_parts();
    let body_bytes = match hyper::body::to_bytes(body).await {
        Ok(bytes) => bytes,
        Err(_) => return plugin_error_response("failed to read request body"),
    };
    let body_string = String::from_utf8_lossy(&body_bytes).to_string();

    let (tx, rx) = mpsc::channel::<axum::body::Bytes>(128);
    let stream_id = stream_registry().lock().unwrap().add(tx);

    let method = parts.method.to_string();
    let path = parts.uri.path().to_string();
    let query = parts.uri.query().unwrap_or("").to_string();
    let headers: serde_json::Map<String, serde_json::Value> = parts
        .headers
        .iter()
        .map(|(k, v)| {
            let name = k.as_str().to_string();
            let value = v.to_str().unwrap_or("").to_string();
            (name, json!(value))
        })
        .collect();
    let params: serde_json::Map<String, serde_json::Value> = extract_route_params(&pattern, &path)
        .into_iter()
        .map(|(k, v)| (k, json!(v)))
        .collect();

    let request_json = json!({
        "method": method,
        "path": path,
        "query": query,
        "headers": headers,
        "body": body_string,
        "params": params,
    });
    crate::log::log_info(format!("{} {} (stream)", method, path));

    let result = tokio::task::spawn_blocking(move || {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let request_term = context.new_term_ref();
        context
            .serialize_to_term(&request_term, &request_json)
            .map_err(|_| PrologError::Failure)?;

        let stream_id_term = context.new_term_ref();
        stream_id_term
            .put(&stream_id)
            .map_err(|_| PrologError::Failure)?;

        let response_term = context.new_term_ref();
        let handler_atom = Atom::new(&handler);
        let module_atom = Atom::new(&module);
        let callable = CallablePredicate::new(Predicate::new(
            Functor::new(handler_atom, 3),
            Module::new(module_atom),
        ))
        .map_err(|_| PrologError::Failure)?;
        context.call_once(callable, [&request_term, &stream_id_term, &response_term])?;

        let response: serde_json::Value = context
            .deserialize_from_term(&response_term)
            .map_err(|_| PrologError::Failure)?;
        Ok(response)
    })
    .await
    .unwrap_or(Err(PrologError::Failure));

    match result {
        Ok(response) => {
            let status = response
                .get("status")
                .and_then(|s| s.as_u64())
                .unwrap_or(200) as u16;
            let status = StatusCode::from_u16(status).unwrap_or(StatusCode::OK);
            let mut response_headers = HeaderMap::new();
            if let Some(headers_map) = response.get("headers").and_then(|h| h.as_object()) {
                for (key, value) in headers_map {
                    if let Some(value_str) = value.as_str() {
                        if let Ok(header_name) = key.parse::<header::HeaderName>() {
                            if let Ok(header_value) = value_str.parse() {
                                response_headers.insert(header_name, header_value);
                            }
                        }
                    }
                }
            }

            let body_value = response.get("body").cloned().unwrap_or(json!("stream"));
            if body_value == json!("stream") {
                if !response_headers.contains_key(header::CONTENT_TYPE) {
                    response_headers.insert(
                        header::CONTENT_TYPE,
                        "application/x-ndjson".parse().unwrap(),
                    );
                }
                let stream = GuardedReceiverStream {
                    inner: ReceiverStream::new(rx),
                    _guard: StreamGuard { id: stream_id },
                };
                let mut builder = Response::builder().status(status);
                for (key, value) in response_headers.iter() {
                    builder = builder.header(key, value);
                }
                builder.body(Body::wrap_stream(stream)).unwrap()
            } else {
                stream_registry().lock().unwrap().remove(stream_id);
                let mut builder = Response::builder().status(status);
                for (key, value) in response_headers.iter() {
                    builder = builder.header(key, value);
                }
                if let Some(body_str) = body_value.as_str() {
                    builder.body(Body::from(body_str.to_string()))
                } else {
                    builder.body(Body::from(body_value.to_string()))
                }.unwrap()
            }
        }
        Err(_) => {
            stream_registry().lock().unwrap().remove(stream_id);
            plugin_error_response("Plugin stream handler failed")
        }
    }
}

/// Return a JSON error response.
fn plugin_error_response(message: &str) -> Response<Body> {
    let body = json!({
        "@type": "api:ErrorResponse",
        "api:status": "api:failure",
        "api:error": {"@type": "api:InternalServerError"},
        "api:message": message
    });
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(body.to_string()))
        .unwrap()
}
