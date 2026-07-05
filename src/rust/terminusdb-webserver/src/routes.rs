use axum::{
    http::{header, HeaderMap, StatusCode},
    response::IntoResponse,
};

/// Return the configured root redirect target.
///
/// TODO: move this to a configuration module once the design is finalized.
pub fn root_redirect_target() -> String {
    std::env::var("TERMINUSDB_ROOT_REDIRECT").unwrap_or_else(|_| "/app/admin".to_string())
}

/// Redirect `/` to the configured root redirect target.
pub async fn root_redirect() -> impl IntoResponse {
    let target = root_redirect_target();
    let mut headers = HeaderMap::new();
    headers.insert(
        header::LOCATION,
        target.parse().unwrap(),
    );
    (StatusCode::TEMPORARY_REDIRECT, headers, "")
}

