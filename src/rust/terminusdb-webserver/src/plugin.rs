use swipl::prelude::*;

use crate::dispatch::{collect_routes, collect_static_paths, collect_streams};
use crate::server;

/// Convert a Prolog term to a Rust String, accepting either an atom or a string term.
fn term_to_string(term: &Term) -> PrologResult<String> {
    if term.is_atom() {
        let atom: Atom = term.get_ex()?;
        Ok(atom.name())
    } else {
        term.get_ex::<String>()
    }
}

predicates! {
    /// Start the Rust webserver on the given port.
    ///
    /// Signature: `rust_webserver_start(+Port)` where Port is an integer.
    ///
    /// Routes are collected from `webserver_hooks:rust_webserver_route/3` and
    /// static file serving paths from `webserver_hooks:rust_webserver_static_path/3`.
    ///
    /// Fails if the port is outside the valid TCP range (1..65535) or if the
    /// server cannot bind to it.
    #[module("$webserver")]
    pub semidet fn rust_webserver_start(context, port_term) {
        let port: u64 = port_term.get_ex()?;
        if port == 0 || port > u16::MAX as u64 {
            crate::log::log_error(format!(
                "[terminusdb-webserver] invalid port: {}, must be 1..65535",
                port
            ));
            return Err(PrologError::Failure);
        }
        let routes = collect_routes(context)?;
        let static_paths = collect_static_paths(context)?;
        let streams = collect_streams(context)?;
        server::start_with_routes(port as u16, routes, static_paths, streams)
            .map_err(|e| {
                crate::log::log_error(format!("[terminusdb-webserver] {}", e));
                PrologError::Failure
            })
    }

    /// Send a chunk to an active streaming response.
    ///
    /// Signature: `rust_webserver_stream_send(+StreamId, +Data)` where StreamId
    /// is the identifier given to the stream handler and Data is either a string
    /// (sent as raw bytes) or any term serializable to JSON (sent as NDJSON).
    /// A trailing newline is added automatically.
    #[module("$webserver")]
    pub semidet fn rust_webserver_stream_send(context, stream_id_term, data_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        let mut bytes = if let Ok(data) = data_term.get_ex::<String>() {
            data.into_bytes()
        } else {
            let data: serde_json::Value = context
                .deserialize_from_term(data_term)
                .map_err(|_| PrologError::Failure)?;
            data.to_string().into_bytes()
        };
        bytes.push(b'\n');
        crate::dispatch::stream_registry()
            .lock()
            .unwrap()
            .send(stream_id, axum::body::Bytes::from(bytes))
            .map_err(|_| PrologError::Failure)
    }

    /// Close an active streaming response.
    ///
    /// Signature: `rust_webserver_stream_close(+StreamId)`. Removes the stream
    /// sender from the registry, which lets the HTTP response body end and the
    /// client see EOF.
    #[module("$webserver")]
    pub semidet fn rust_webserver_stream_close(context, stream_id_term) {
        let _ = context;
        let stream_id: u64 = stream_id_term.get_ex()?;
        crate::dispatch::stream_registry()
            .lock()
            .unwrap()
            .remove(stream_id);
        Ok(())
    }

    /// Subscribe an existing stream to a named broadcast channel.
    ///
    /// Signature: `rust_webserver_broadcast_subscribe(+Channel, +StreamId)`.
    /// The channel name is an atom or string. After subscribing, any data sent
    /// to the channel with `rust_webserver_broadcast_send/2` is forwarded to
    /// this stream by Rust.
    #[module("$webserver")]
    pub semidet fn rust_webserver_broadcast_subscribe(_context, channel_term, stream_id_term) {
        let channel = term_to_string(channel_term)?;
        let stream_id: u64 = stream_id_term.get_ex()?;
        crate::dispatch::broadcast_registry()
            .lock()
            .unwrap()
            .subscribe(channel, stream_id);
        Ok(())
    }

    /// Broadcast data to every stream subscribed to a named channel.
    ///
    /// Signature: `rust_webserver_broadcast_send(+Channel, +Data)`. Data is
    /// serialized to JSON and forwarded with a trailing newline to every
    /// stream in the channel. Rust performs the multiplexing, so Prolog only
    /// needs to send once.
    #[module("$webserver")]
    pub semidet fn rust_webserver_broadcast_send(context, channel_term, data_term) {
        let channel = term_to_string(channel_term)?;
        let data: serde_json::Value = context
            .deserialize_from_term(data_term)
            .map_err(|_| PrologError::Failure)?;
        let mut bytes = data.to_string().into_bytes();
        bytes.push(b'\n');
        let broadcast_arc = crate::dispatch::broadcast_registry();
        let stream_arc = crate::dispatch::stream_registry();
        let mut broadcast = broadcast_arc.lock().unwrap();
        let mut stream_registry = stream_arc.lock().unwrap();
        broadcast
            .broadcast(&channel, axum::body::Bytes::from(bytes), &mut stream_registry)
            .map_err(|_| PrologError::Failure)
    }
}

pub fn register() {
    register_rust_webserver_start();
    register_rust_webserver_stream_send();
    register_rust_webserver_stream_close();
    register_rust_webserver_broadcast_subscribe();
    register_rust_webserver_broadcast_send();
}
