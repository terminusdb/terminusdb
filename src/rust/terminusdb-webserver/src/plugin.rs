use std::os::raw::c_char;
use swipl::fli::{
    IOENC_ENC_OCTET, IOENC_ENC_UTF8, PL_unify_stream, Sclose, Sfdopen, Ssetenc,
};
use swipl::prelude::*;
use swipl::term::Nil;

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

    /// Send a batch of Prolog dicts as NDJSON in a single FFI call.
    ///
    /// Signature: `appserver_stream_send_batch(+StreamId, +ListOfDicts)` where
    /// StreamId is the identifier given to the stream handler and ListOfDicts
    /// is a Prolog list of dicts. Each dict is deserialized to a serde_json::Value
    /// and serialized to a JSON line with a trailing newline. The entire batch
    /// is sent as a single chunk via one `send` call, amortizing FFI overhead
    /// and mpsc lock contention.
    #[module("$appserver")]
    pub semidet fn appserver_stream_send_batch(context, stream_id_term, list_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        let mut bytes = Vec::with_capacity(4096);
        for term in context.term_list_iter(&list_term) {
            let value: serde_json::Value = context
                .deserialize_from_term(&term)
                .map_err(|_| PrologError::Failure)?;
            serde_json::to_writer(&mut bytes, &value)
                .map_err(|_| PrologError::Failure)?;
            bytes.push(b'\n');
        }
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

    /// Check if a stream with the given id is still active.
    ///
    /// Signature: `appserver_stream_exists(+StreamId)`. Succeeds if the
    /// stream is in the registry, fails otherwise. Used by Prolog to
    /// detect stale `commit_stream/3` entries when `timeout=false` (no
    /// timeout thread to clean them up).
    #[module("$appserver")]
    pub semidet fn appserver_stream_exists(_context, stream_id_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        if crate::dispatch::stream_registry()
            .lock()
            .unwrap()
            .contains(stream_id)
        {
            Ok(())
        } else {
            Err(PrologError::Failure)
        }
    }

    /// Subscribe an existing stream to a named broadcast channel with idle timeout.
    ///
    /// Signature: `appserver_broadcast_subscribe(+Channel, +StreamId, +IdleTimeout)`.
    /// The channel name is an atom or string. After subscribing, any data sent
    /// to the channel with `appserver_broadcast_send/2` is forwarded to
    /// this stream by a background tokio task.
    ///
    /// IdleTimeout is the idle timeout in seconds: if no messages arrive
    /// for this duration, the stream is closed. 0 or negative means no
    /// timeout (stream stays open until client disconnects).
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_subscribe(_context, channel_term, stream_id_term, timeout_term) {
        let channel = term_to_string(channel_term)?;
        let stream_id: u64 = stream_id_term.get_ex()?;
        let timeout_secs: i64 = timeout_term.get_ex()?;
        let idle_timeout = if timeout_secs > 0 {
            Some(std::time::Duration::from_secs(timeout_secs as u64))
        } else {
            None
        };

        // Clone the stream's mpsc::Sender so the forwarder task can
        // write to it without holding the stream_registry mutex.
        let stream_sender = {
            let registry = crate::dispatch::stream_registry();
            let registry = registry.lock().unwrap();
            match registry.get_sender(stream_id) {
                Some(sender) => sender,
                None => return Err(PrologError::Failure),
            }
        };

        crate::dispatch::broadcast_registry()
            .lock()
            .unwrap()
            .subscribe(channel, stream_id, stream_sender, idle_timeout);
        Ok(())
    }

    /// Unsubscribe a stream from a specific broadcast channel.
    ///
    /// Signature: `appserver_broadcast_unsubscribe(+Channel, +StreamId)`.
    /// Aborts the forwarder task, which drops the broadcast::Receiver.
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_unsubscribe(_context, channel_term, stream_id_term) {
        let channel = term_to_string(channel_term)?;
        let stream_id: u64 = stream_id_term.get_ex()?;
        crate::dispatch::broadcast_registry()
            .lock()
            .unwrap()
            .unsubscribe(&channel, stream_id);
        Ok(())
    }

    /// Unsubscribe a stream from all broadcast channels.
    ///
    /// Signature: `appserver_broadcast_unsubscribe_all(+StreamId)`.
    /// Aborts all forwarder tasks for this stream.
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_unsubscribe_all(_context, stream_id_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        crate::dispatch::broadcast_registry()
            .lock()
            .unwrap()
            .unsubscribe_all(stream_id);
        Ok(())
    }

    /// Broadcast data to every stream subscribed to a named channel.
    ///
    /// Signature: `appserver_broadcast_send(+Channel, +Data)`. Data is
    /// serialized to JSON and forwarded with a trailing newline to every
    /// stream subscribed to the channel. This is a single `broadcast::send()`
    /// call — no per-stream loop, no stream_registry lock. Forwarder tasks
    /// in the tokio runtime handle delivery to each stream asynchronously.
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_send(context, channel_term, data_term) {
        let channel = term_to_string(channel_term)?;
        let data: serde_json::Value = context
            .deserialize_from_term(data_term)
            .map_err(|_| PrologError::Failure)?;
        let mut bytes = data.to_string().into_bytes();
        bytes.push(b'\n');
        crate::dispatch::broadcast_registry()
            .lock()
            .unwrap()
            .send(&channel, axum::body::Bytes::from(bytes))
            .map_err(|_| PrologError::Failure)
    }

    /// Broadcast pre-serialized JSON to every stream subscribed to a channel.
    ///
    /// Signature: `appserver_broadcast_send_raw(+Channel, +JsonString)`.
    /// The string is forwarded verbatim with a trailing newline. Use this
    /// when the Prolog side has already serialized the data with
    /// `json_write_dict/3` to avoid term-to-JSON conversion issues (e.g.
    /// Prolog `[]` being mapped to JSON `null` by `deserialize_from_term`).
    ///
    /// This is a single `broadcast::send()` call — no stream_registry lock.
    #[module("$appserver")]
    pub semidet fn appserver_broadcast_send_raw(_context, channel_term, json_term) {
        let channel = term_to_string(channel_term)?;
        let json_string: String = json_term.get_ex()?;
        let mut bytes = json_string.into_bytes();
        bytes.push(b'\n');
        crate::dispatch::broadcast_registry()
            .lock()
            .unwrap()
            .send(&channel, axum::body::Bytes::from(bytes))
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
                let f = context.open_frame();
                let tmp = f.new_term_ref();
                tmp.put(&bytes[..]).map_err(|_| PrologError::Failure)?;
                let result = data_term.unify(&tmp);
                f.close();
                result
            }
            None => {
                // Stream is closed and fully consumed. Leave the receiver out
                // of the registry so subsequent calls fail fast.
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

    /// Receive the next chunk from an input stream, split into complete lines.
    ///
    /// Signature: `appserver_stream_recv_lines(+StreamId, -Lines, -Remaining)`.
    /// Lines is a Prolog list of strings (one per complete line, without the
    /// trailing newline). Remaining is the partial line at the end of the chunk
    /// (no trailing newline), or "" if the chunk ended on a newline. If the
    /// stream is closed, Lines = [] and Remaining = end_of_stream.
    #[module("$appserver")]
    pub semidet fn appserver_stream_recv_lines(context, stream_id_term, lines_term, remaining_term) {
        let stream_id: u64 = stream_id_term.get_ex()?;
        let registry_arc = crate::dispatch::input_stream_registry();
        let mut registry = registry_arc.lock().unwrap();
        let mut receiver = registry
            .take(stream_id)
            .ok_or(PrologError::Failure)?;
        drop(registry);
        let result = receiver.blocking_recv();
        let mut registry = registry_arc.lock().unwrap();
        match result {
            Some(bytes) => {
                registry.replace(stream_id, receiver);

                let text = String::from_utf8_lossy(&bytes);
                let mut lines: Vec<&str> = Vec::new();
                let mut remaining = "";
                let mut last_end = 0;
                for (i, b) in text.bytes().enumerate() {
                    if b == b'\n' {
                        lines.push(&text[last_end..i]);
                        last_end = i + 1;
                    }
                }
                if last_end < text.len() {
                    remaining = &text[last_end..];
                }

                // Build Prolog list directly into lines_term using context.
                // Use unify (not put) for head values — put overwrites the term ref
                // instead of binding the list cell's head variable.
                let mut tails: Vec<Term<'_>> = Vec::with_capacity(lines.len());
                let mut cur = lines_term;
                for line in &lines {
                    let (head, tail) = context.unify_list_functor(cur)?;
                    let str_tmp = context.new_term_ref();
                    str_tmp.put(*line).map_err(|_| PrologError::Failure)?;
                    head.unify(&str_tmp).map_err(|_| PrologError::Failure)?;
                    tails.push(tail);
                    cur = tails.last().unwrap();
                }
                cur.unify(&Nil).map_err(|_| PrologError::Failure)?;

                // Unify remaining_term.
                let rem_tmp = context.new_term_ref();
                rem_tmp.put(remaining).map_err(|_| PrologError::Failure)?;
                remaining_term.unify(&rem_tmp).map_err(|_| PrologError::Failure)
            }
            None => {
                lines_term.unify(&Nil).map_err(|_| PrologError::Failure)?;
                let rem_tmp = context.new_term_ref();
                rem_tmp.put(&Atom::new("end_of_stream")).map_err(|_| PrologError::Failure)?;
                remaining_term.unify(&rem_tmp).map_err(|_| PrologError::Failure)
            }
        }
        .map_err(|_| PrologError::Failure)
    }

    /// Get the current number of active HTTP connections in the Rust webserver.
    ///
    /// Signature: `appserver_active_connections(-Count)` where Count is a
    /// non-negative integer.
    #[module("$appserver")]
    pub semidet fn appserver_active_connections(_context, count_term) {
        let count = crate::server::active_connections();
        count_term.unify(count).map_err(|_| PrologError::Failure)
    }
}

pub fn register() {
    register_appserver_start();
    register_appserver_stream_send();
    register_appserver_stream_send_raw();
    register_appserver_stream_send_batch();
    register_appserver_stream_close();
    register_appserver_stream_exists();
    register_appserver_broadcast_subscribe();
    register_appserver_broadcast_unsubscribe();
    register_appserver_broadcast_unsubscribe_all();
    register_appserver_broadcast_send();
    register_appserver_broadcast_send_raw();
    register_appserver_stream_recv();
    register_appserver_stream_recv_lines();
    register_appserver_open_fd_stream();
    register_appserver_close_fd();
    register_appserver_active_connections();
}
