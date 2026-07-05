//! TerminusDB webserver crate.
//!
//! This crate provides an Axum/Tokio webserver that runs inside the
//! SWI-Prolog process and is loaded through the existing `librust.dylib`.

pub mod dispatch;
pub mod log;
pub mod plugin;
pub mod routes;
pub mod server;

/// Install the webserver predicates into the Prolog runtime.
///
/// Called from `terminusdb_community::install()` during dylib load.
pub fn install() {
    plugin::register();
}
