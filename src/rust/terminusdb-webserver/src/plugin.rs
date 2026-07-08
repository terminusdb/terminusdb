use std::os::raw::c_char;
use swipl::fli::{
    IOENC_ENC_OCTET, IOENC_ENC_UTF8, PL_unify_stream, Sclose, Sfdopen, Ssetenc,
};
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
    /// Signature: `appserver_start(+Port)` where Port is an integer.
    ///
    /// Routes are collected from `appserver_hooks:appserver_route/3` and
    /// static file serving paths from `appserver_hooks:appserver_static_path/3`.
    ///
    /// Fails if the port is outside the valid TCP range (1..65535) or if the
    /// server cannot bind to it.
    #[module("$appserver")]
    pub semidet fn appserver_start(context, port_term) {
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
    /// Signature: `appserver_stream_send(+StreamId, +Data)` where StreamId
    /// is the identifier given to the stream handler and Data is either a string
    /// (sent as raw bytes) or any term serializable to JSON (sent as NDJSON).
    /// A trailing newline is added automatically.
    ///
    /// Strings are extracted via PL_get_nchars with REP_UTF8 to ensure the
    /// bytes are always valid UTF-8, regardless of the internal string
    /// representation (SWI-Prolog stores Latin-1-range strings as single
    /// bytes internally, which would be invalid UTF-8 if passed through
    /// directly). Binary data that is not valid UTF-8 falls back to
    /// PL_get_string which preserves all 8-bit values.
    #[module("$appserver")]
    pub semidet fn appserver_stream_send(context, stream_id_term, data_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        let mut bytes = if let Ok(data) = data_term.get_ex::<String>() {
            data.into_bytes()
        } else if let Ok(data) = data_term.get_ex::<Vec<u8>>() {
            data
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

    /// Send raw bytes to an active streaming response without a trailing newline.
    ///
    /// Signature: `appserver_stream_send_raw(+StreamId, +Data)` where StreamId
    /// is the identifier given to the stream handler and Data is a Prolog string
    /// containing raw octets. The bytes are extracted via PL_get_string which
    /// preserves all 8-bit values. No trailing newline is added.
    #[module("$appserver")]
    pub semidet fn appserver_stream_send_raw(_context, stream_id_term, data_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        let bytes: Vec<u8> = data_term.get_ex()?;
        crate::dispatch::stream_registry()
            .lock()
            .unwrap()
            .send(stream_id, axum::body::Bytes::from(bytes))
            .map_err(|_| PrologError::Failure)
    }

    /// Close an active streaming response.
    ///
    /// Signature: `appserver_stream_close(+StreamId)`. Removes the stream
    /// sender from the registry, which lets the HTTP response body end and the
    /// client see EOF.
    #[module("$appserver")]
    pub semidet fn appserver_stream_close(context, stream_id_term) {
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
    /// Signature: `appserver_broadcast_subscribe(+Channel, +StreamId)`.
    /// The channel name is an atom or string. After subscribing, any data sent
    /// to the channel with `appserver_broadcast_send/2` is forwarded to
    /// this stream by Rust.
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_subscribe(_context, channel_term, stream_id_term) {
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
    /// Signature: `appserver_broadcast_send(+Channel, +Data)`. Data is
    /// serialized to JSON and forwarded with a trailing newline to every
    /// stream in the channel. Rust performs the multiplexing, so Prolog only
    /// needs to send once.
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_send(context, channel_term, data_term) {
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

    /// Receive the next chunk from an input stream.
    ///
    /// Signature: `appserver_stream_recv(+StreamId, -Data)`. Data is a Prolog
    /// string containing the raw bytes of the next chunk of the request body,
    /// or the atom `end_of_stream` if the input stream has been closed.
    /// Fails if the stream id is unknown.
    #[module("$appserver")]
    pub semidet fn appserver_stream_recv(context, stream_id_term, data_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        let registry_arc = crate::dispatch::input_stream_registry();
        let mut registry = registry_arc.lock().unwrap();
        let mut receiver = registry
            .take(stream_id)
            .ok_or(PrologError::Failure)?;
        // Release the registry lock while waiting for the next chunk.
        drop(registry);
        let result = receiver.blocking_recv();
        let mut registry = registry_arc.lock().unwrap();
        match result {
            Some(bytes) => {
                // Put the receiver back so the worker can read the next chunk.
                registry.replace(stream_id, receiver);
                // Use unify instead of put — PL_unify_* works with term refs
                // from the Prolog call frame, while PL_put_* requires a
                // foreign frame which the semidet trampoline doesn't open.
                let f = context.open_frame();
                let tmp = f.new_term_ref();
                tmp.put(&bytes[..]).map_err(|_| PrologError::Failure)?;
                let result = data_term.unify(&tmp);
                f.close();
                result
            }
            None => {
                // Stream is closed and fully consumed. Leave the receiver out
                // of the registry.
                let f = context.open_frame();
                let tmp = f.new_term_ref();
                tmp.put(&Atom::new("end_of_stream")).map_err(|_| PrologError::Failure)?;
                let result = data_term.unify(&tmp);
                f.close();
                result
            }
        }
        .map_err(|_| PrologError::Failure)
    }

    /// Open a POSIX file descriptor as a SWI-Prolog stream.
    ///
    /// Signature: `appserver_open_fd_stream(+Fd, +Mode, +Encoding, -Stream)`
    /// where Fd is an integer, Mode is atom `read` or `write`,
    /// Encoding is atom `octet` or `utf8`, and Stream is unified with
    /// the resulting Prolog stream.
    ///
    /// The FD is duplicated with `dup()` before being passed to `Sfdopen`.
    /// This means Prolog's `close/1` closes the duplicate, not the original
    /// FD. The caller retains ownership of the original FD and can safely
    /// close it with `appserver_close_fd/1` as a safety net without any
    /// risk of double-close or FD recycling races.
    #[module("$appserver")]
    pub semidet fn appserver_open_fd_stream(
        _context,
        fd_term,
        mode_term,
        encoding_term,
        stream_term,
    ) {
        let fd: i64 = fd_term.get_ex()?;
        let fd = fd as i32;
        let mode_atom: Atom = mode_term.get_ex()?;
        let encoding_atom: Atom = encoding_term.get_ex()?;
        let mode_str = mode_atom.name();
        let enc_str = encoding_atom.name();

        let c_mode = match mode_str.as_str() {
            "read" => "r\0",
            "write" => "w\0",
            _ => return Err(PrologError::Failure),
        };

        let enc = match enc_str.as_str() {
            "octet" => IOENC_ENC_OCTET,
            "utf8" => IOENC_ENC_UTF8,
            _ => return Err(PrologError::Failure),
        };

        // Duplicate the FD so that Prolog's close/1 closes the duplicate,
        // not the original. This prevents FD recycling races where the
        // safety-net appserver_close_fd/1 could close an unrelated socket
        // that the kernel assigned the same FD number after Prolog's close.
        let dup_fd = unsafe { libc::dup(fd) };
        if dup_fd < 0 {
            return Err(PrologError::Failure);
        }

        let stream = unsafe { Sfdopen(dup_fd, c_mode.as_ptr() as *const c_char) };
        if stream.is_null() {
            return Err(PrologError::Failure);
        }

        let enc_result = unsafe { Ssetenc(stream, enc, std::ptr::null_mut()) };
        if (enc_result as i32) < 0 {
            unsafe { Sclose(stream) };
            return Err(PrologError::Failure);
        }

        let unify_result = unsafe { PL_unify_stream(stream_term.term_ptr(), stream) };
        if !unify_result {
            unsafe { Sclose(stream) };
            return Err(PrologError::Failure);
        }

        Ok(())
    }

    /// Close a raw file descriptor directly, bypassing the Prolog stream layer.
    ///
    /// This is used as a safety net in pipe cleanup to guarantee that the
    /// original FD is closed even when `close/1` on the corresponding Prolog
    /// stream fails (e.g. with a broken pipe error). Because
    /// `appserver_open_fd_stream` duplicates the FD before handing it to
    /// Prolog, this call closes the original FD — it is never a double-close
    /// of the same FD that Prolog's `close/1` already closed.
    ///
    /// Signature: `appserver_close_fd(+Fd)` where Fd is an integer.
    #[module("$appserver")]
    pub semidet fn appserver_close_fd(_context, fd_term) {
        let fd: i64 = fd_term.get_ex()?;
        let fd = fd as i32;
        // unsafe justification: close() is safe to call on any valid FD.
        // The FD is the original (not the dup), so this is the first and
        // only close of this FD. We ignore the return value because the
        // purpose is purely to ensure the FD is not leaked.
        unsafe {
            libc::close(fd);
        }
        Ok(())
    }
}

pub fn register() {
    register_appserver_start();
    register_appserver_stream_send();
    register_appserver_stream_send_raw();
    register_appserver_stream_close();
    register_appserver_broadcast_subscribe();
    register_appserver_broadcast_send();
    register_appserver_stream_recv();
    register_appserver_open_fd_stream();
    register_appserver_close_fd();
}
