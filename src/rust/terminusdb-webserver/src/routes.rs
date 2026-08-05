use axum::{
    http::{header, HeaderMap, StatusCode},
    response::IntoResponse,
};

use crate::config::root_redirect_target;

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

