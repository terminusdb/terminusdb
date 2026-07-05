use axum::{
    routing::get,
    Json, Router,
};
use serde_json::json;
use std::net::SocketAddr;
use tower_http::cors::{Any, CorsLayer};

use crate::routes::root_redirect;

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
    let (tx, rx) = std::sync::mpsc::channel();

    std::thread::spawn(move || {
        let addr = SocketAddr::from(([0, 0, 0, 0], port));
        let listener = match std::net::TcpListener::bind(addr) {
            Ok(listener) => listener,
            Err(e) => {
                tx.send(Err(format!(
                    "unable to bind port {}: {}. Webserver not started.",
                    port, e
                )))
                .ok();
                return;
            }
        };

        let runtime = match tokio::runtime::Runtime::new() {
            Ok(runtime) => runtime,
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
                .merge(stream_router);
            match axum::Server::from_tcp(listener) {
                Ok(server) => {
                    tx.send(Ok(())).ok();
                    if let Err(e) = server.serve(app.into_make_service()).await {
                        crate::log::log_error(format!(
                            "[terminusdb-webserver] server error on port {}: {}",
                            port, e
                        ));
                    }
                }
                Err(e) => {
                    tx.send(Err(format!(
                        "failed to configure server on port {}: {}",
                        port, e
                    )))
                    .ok();
                }
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
            "/app/alpha"
        );
    }
}
