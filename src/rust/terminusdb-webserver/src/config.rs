//! Runtime configuration for the TerminusDB webserver.
//!
//! Centralizes configuration so that handlers and middleware don't each
//! reach into `std::env` directly. The root redirect target is set once
//! at startup from Prolog's `terminus_config.pl` via the `appserver_start/2`
//! FFI predicate.

use std::sync::OnceLock;

static ROOT_REDIRECT_TARGET: OnceLock<String> = OnceLock::new();

/// Set the root redirect target, called once from `appserver_start/2`.
///
/// Panics if called more than once (would indicate a logic bug).
pub fn set_root_redirect_target(target: String) {
    ROOT_REDIRECT_TARGET
        .set(target)
        .expect("root_redirect_target already set");
}

/// Return the configured root redirect target.
///
/// Set at startup from Prolog's `config(root_redirect_target/1)`.
/// Falls back to `/app/admin` if not yet configured (e.g. in unit tests).
pub fn root_redirect_target() -> String {
    ROOT_REDIRECT_TARGET
        .get()
        .cloned()
        .unwrap_or_else(|| "/app/admin".to_string())
}
