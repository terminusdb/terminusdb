use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use axum::middleware::Next;
use axum::extract::Request;
use serde_json::json;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use tower_http::cors::{Any, CorsLayer};

use crate::routes::root_redirect;

/// Global counter for active HTTP connections.
static ACTIVE_CONNECTIONS: AtomicU64 = AtomicU64::new(0);

/// Get the current number of active connections.
pub fn active_connections() -> u64 {
    ACTIVE_CONNECTIONS.load(Ordering::SeqCst)
}

/// Middleware that tracks active connections by incrementing on entry
/// and decrementing when the response completes.
async fn connection_counter_middleware(req: Request, next: Next) -> impl IntoResponse {
    ACTIVE_CONNECTIONS.fetch_add(1, Ordering::SeqCst);
    let resp = next.run(req).await;
    ACTIVE_CONNECTIONS.fetch_sub(1, Ordering::SeqCst);
    resp
}

/// Middleware that intercepts Axum's default 405 Method Not Allowed responses
/// (which have an empty body) and replaces them with a standardized JSON-LD
/// error response matching the Prolog backend's format.
async fn method_not_allowed_middleware(req: Request, next: Next) -> Response {
    let method = req.method().clone();
    let path = req.uri().path().to_string();
    let response = next.run(req).await;
    if response.status() == StatusCode::METHOD_NOT_ALLOWED {
        let allow = response
            .headers()
            .get(axum::http::header::ALLOW)
            .cloned();
        let body = Json(json!({
            "@type": "api:MethodNotAllowedErrorResponse",
            "api:status": "api:method_not_allowed",
            "api:message": format!("HTTP method {} is not allowed for {}", method, path),
            "api:error": {
                "@type": "api:MethodNotAllowed",
                "api:method": method.as_str().to_uppercase(),
                "api:path": path
            }
        }));
        let mut new_response = (StatusCode::METHOD_NOT_ALLOWED, body).into_response();
        if let Some(allow) = allow {
            new_response.headers_mut().insert(axum::http::header::ALLOW, allow);
        }
        new_response
    } else {
        response
    }
}

/// Build the Axum application router.
///
/// Exposes a root redirect and health-check endpoint. Additional routes are
/// registered by Prolog plugins through the webserver hooks.
pub fn app() -> Router {
    Router::new()
        .route("/", get(root_redirect))
        .route("/api/v1/health", get(health_handler))
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any),
        )
}

/// Health-check handler.
async fn health_handler() -> Json<serde_json::Value> {
    Json(json!({"status": "ok"}))
}

/// Fallback handler for unmatched routes.
///
/// Prolog's `/api` prefix 404 handler handles paths that match its wildcard,
/// but Axum 0.6 refuses to match less-specific wildcards when a more specific
/// prefix route exists (e.g., `/api/branch` does not fall back to `/api/*path`
/// because `/api/branch/` is registered). This fallback produces the same JSON
/// 404 response so that the behavior is consistent with the SWI-Prolog backend.
async fn fallback_not_found(uri: axum::http::Uri) -> impl IntoResponse {
    let path = uri.path().to_string();
    let msg = format!("Path not found: {}", path);
    let body = Json(json!({
        "api:status": "api:not_found",
        "api:path": path,
        "api:message": msg,
    }));
    (StatusCode::NOT_FOUND, body)
}

/// Start the webserver on the given port.
///
/// This spawns a tokio runtime on a background thread and returns
/// immediately so the calling Prolog thread is not blocked.
pub fn start(port: u16) -> Result<(), String> {
    start_with_routes(port, Vec::new(), Vec::new(), Vec::new())
}

/// Start the webserver with plugin routes, static file serving paths, and
/// streaming endpoints.
///
/// Returns `Ok(())` once the background thread has bound the port and created
/// its Tokio runtime. Returns `Err(...)` if the port cannot be bound or the
/// runtime cannot be created. Runtime errors after the server starts serving
/// are logged by the background thread.
pub fn start_with_routes(
    port: u16,
    plugin_routes: Vec<crate::dispatch::PluginRoute>,
    plugin_static_paths: Vec<crate::dispatch::PluginStaticPath>,
    plugin_streams: Vec<crate::dispatch::PluginStream>,
) -> Result<(), String> {
    crate::dispatch::init_dispatcher();

    let (tx, rx) = std::sync::mpsc::channel();

    std::thread::spawn(move || {
        let addr = SocketAddr::from(([0, 0, 0, 0], port));

        // Use socket2 to create a listener with a large backlog. The
        // default backlog (128 on macOS) is too small for high connection
        // counts — 30000 simultaneous connections overflow the accept
        // queue and drop ~50% of connections.
        let socket = match socket2::Socket::new(
            socket2::Domain::IPV4,
            socket2::Type::STREAM,
            None,
        ) {
            Ok(s) => s,
            Err(e) => {
                tx.send(Err(format!(
                    "unable to create socket for port {}: {}. Webserver not started.",
                    port, e
                )))
                .ok();
                return;
            }
        };

        // Allow rebinding while previous socket is in TIME_WAIT
        if let Err(e) = socket.set_reuse_address(true) {
            tx.send(Err(format!(
                "failed to set SO_REUSEADDR for port {}: {}",
                port, e
            )))
            .ok();
            return;
        }

        if let Err(e) = socket.bind(&addr.into()) {
            tx.send(Err(format!(
                "unable to bind port {}: {}. Webserver not started.",
                port, e
            )))
            .ok();
            return;
        }

        // Listen with a large backlog. The OS caps this at somaxconn,
        // but we request a high value — the OS will use min(our_value, somaxconn).
        // On macOS with somaxconn=128, this still helps because the kernel
        // may use a higher internal queue.
        if let Err(e) = socket.listen(65535) {
            tx.send(Err(format!(
                "failed to listen on port {}: {}",
                port, e
            )))
            .ok();
            return;
        }

        let listener: std::net::TcpListener = socket.into();

        // Tokio requires the socket to be in non-blocking mode before
        // TcpListener::from_std. Without this, accept() can block a runtime
        // worker thread, causing intermittent stalls where incoming
        // connections are not serviced until the next connection arrives.
        if let Err(e) = listener.set_nonblocking(true) {
            tx.send(Err(format!(
                "failed to set non-blocking on listener for port {}: {}",
                port, e
            )))
            .ok();
            return;
        }

        let runtime = match tokio::runtime::Runtime::new() {
            Ok(runtime) => {
                // Store the handle so FFI functions can spawn tasks
                crate::dispatch::set_tokio_handle(runtime.handle().clone());
                runtime
            }
            Err(e) => {
                tx.send(Err(format!("failed to create tokio runtime: {}", e))).ok();
                return;
            }
        };

        runtime.block_on(async move {
            let plugin_router = crate::dispatch::build_plugin_router(plugin_routes);
            let static_router = crate::dispatch::build_static_router(plugin_static_paths);
            let stream_router = crate::dispatch::build_stream_router(plugin_streams);
            let app = app()
                .merge(plugin_router)
                .merge(static_router)
                .merge(stream_router)
                .fallback(fallback_not_found)
                .layer(axum::middleware::from_fn(method_not_allowed_middleware))
                .layer(axum::middleware::from_fn(connection_counter_middleware));
            let tokio_listener = match tokio::net::TcpListener::from_std(listener) {
                Ok(listener) => listener,
                Err(e) => {
                    tx.send(Err(format!(
                        "failed to configure server on port {}: {}",
                        port, e
                    )))
                    .ok();
                    return;
                }
            };
            tx.send(Ok(())).ok();
            if let Err(e) = axum::serve(tokio_listener, app).await {
                crate::log::log_error(format!(
                    "[terminusdb-webserver] server error on port {}: {}",
                    port, e
                ));
            }
        });
    });

    rx.recv().map_err(|e| format!("webserver startup channel closed: {}", e))?
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
    async fn health_returns_ok() {
        let app = app();
        let response = app
            .clone()
            .oneshot(Request::builder().uri("/api/v1/health").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn root_redirects_to_default_app() {
        let app = app();
        let response = app
            .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
        assert_eq!(
            response.headers().get("location").unwrap().to_str().unwrap(),
            "/app/data"
        );
    }

    #[tokio::test]
    async fn fallback_returns_api_not_found() {
        let app = app().fallback(fallback_not_found);
        let response = app
            .oneshot(Request::builder().uri("/api/unknown").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
        let body = axum::body::to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(json["api:status"], "api:not_found");
        assert_eq!(json["api:path"], "/api/unknown");
    }
}
