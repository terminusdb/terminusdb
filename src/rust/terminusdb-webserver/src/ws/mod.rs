//! WebSocket handler for GraphQL subscriptions.
//!
//! Implements the `graphql-transport-ws` protocol for real-time GraphQL
//! subscription event delivery over WebSocket.

use axum::{
    extract::{
        ws::{Message, WebSocketUpgrade},
        Path,
    },
    http::{HeaderMap, StatusCode},
    response::IntoResponse,
};
use serde::Deserialize;
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};
use futures::{SinkExt, StreamExt};
use tokio::io::AsyncReadExt;
use tokio::sync::mpsc;

use crate::dispatch::{broadcast_registry, StreamId};

/// Maximum number of active HTTP connections in the Rust webserver.
const MAX_SUBSCRIPTIONS_PER_CONNECTION: usize = 100;

/// Rate limiting: max subscriptions created within the rate limit window.
const RATE_LIMIT_MAX: usize = 10;

/// Rate limiting window duration.
const RATE_LIMIT_WINDOW: Duration = Duration::from_secs(1);

/// Maximum `connection_init` payload size in bytes.
const CONNECTION_INIT_MAX_SIZE: usize = 4096;

/// Maximum length of a subscription `id` field.
const MAX_SUBSCRIPTION_ID_LEN: usize = 2048;

/// Ping interval for keepalive.
const PING_INTERVAL: Duration = Duration::from_secs(30);

/// Per-subscriber buffer size (same as BroadcastRegistry's SUBSCRIBER_BUFFER).
const SUBSCRIBER_BUFFER: usize = 64;

/// Global counter for generating unique WebSocket subscription StreamIds.
/// These IDs are used to track subscriptions in the BroadcastRegistry.
/// They don't need to correspond to stream_registry IDs — the BroadcastRegistry
/// uses them as opaque keys for unsubscribe.
static WS_STREAM_ID_COUNTER: AtomicU64 = AtomicU64::new(1_000_000);

/// Result of a subscribe dispatch to Prolog.
#[derive(Debug, Deserialize)]
struct SubscribeResult {
    cohort_key: String,
    raw_channel: String,
    field_name: String,
}

/// Dispatch a subscribe request to Prolog via a synchronous pipe dispatch.
///
/// Creates pipes, sends a `PipeDispatchRequest` to the worker pool with
/// the subscribe payload, reads the CGI response, and parses the JSON body
/// to extract `cohort_key`, `raw_channel`, and `field_name`.
async fn dispatch_subscribe_to_prolog(
    branch_path: &str,
    query: &str,
    variables: &Value,
    operation_name: &str,
    auth_header: &str,
) -> Result<SubscribeResult, String> {
    use crate::dispatch::{DispatchMessage, PipeDispatchRequest};
    use nix::unistd::pipe;
    use std::os::fd::IntoRawFd;
    use tokio::net::unix::pipe::Receiver;

    let payload = json!({
        "query": query,
        "variables": variables,
        "operationName": operation_name,
    });

    let mut headers = serde_json::Map::new();
    if !auth_header.is_empty() {
        headers.insert("Authorization".to_string(), json!(auth_header));
    }

    let request_json = json!({
        "method": "POST",
        "path": format!("/api/graphql-ws/{}", branch_path),
        "query": "",
        "headers": headers,
        "body": "",
        "params": {},
        "payload": payload,
    });

    // Create output pipe.
    let (output_read, output_write) = pipe().map_err(|e| format!("pipe creation failed: {}", e))?;
    let output_write_fd = output_write.into_raw_fd();

    let dispatch_req = PipeDispatchRequest {
        request_json,
        handler_module: "webserver_graphql_subs".to_string(),
        handler_name: "graphql_subscribe_handler".to_string(),
        input_read_fd: None,
        output_write_fd,
        binary: false,
    };

    // Send to dispatch queue.
    let queue = dispatch_queue_safe()?;
    if let Err(e) = queue.send(DispatchMessage::Pipe(dispatch_req)).await {
        return Err(format!("dispatch queue send failed: {}", e));
    }

    // Read the CGI response from the output pipe.
    let mut output_receiver = Receiver::from_owned_fd(output_read)
        .map_err(|e| format!("failed to create pipe receiver: {}", e))?;
    let (status, _headers, body_bytes) = read_cgi_response(&mut output_receiver).await?;

    if status != 200 {
        let body_str = String::from_utf8_lossy(&body_bytes);
        return Err(format!("subscribe dispatch returned status {}: {}", status, body_str));
    }

    // Parse the JSON body.
    let result: SubscribeResult = serde_json::from_slice(&body_bytes)
        .map_err(|e| format!("failed to parse subscribe response: {}", e))?;

    Ok(result)
}

/// Dispatch an authentication request to Prolog via pipe dispatch.
///
/// Sends the auth header to `graphql_authenticate_handler` in Prolog,
/// which validates it and returns the Auth URI. Returns `Ok(auth_uri)` on
/// success or `Err(message)` on failure.
async fn dispatch_authenticate_to_prolog(auth_header: &str) -> Result<String, String> {
    use crate::dispatch::{DispatchMessage, PipeDispatchRequest};
    use nix::unistd::pipe;
    use std::os::fd::IntoRawFd;
    use tokio::net::unix::pipe::Receiver;

    let mut headers = serde_json::Map::new();
    headers.insert("Authorization".to_string(), json!(auth_header));

    let request_json = json!({
        "method": "POST",
        "path": "/api/graphql-ws/auth",
        "query": "",
        "headers": headers,
        "body": "",
        "params": {},
    });

    let (output_read, output_write) = pipe().map_err(|e| format!("pipe creation failed: {}", e))?;
    let output_write_fd = output_write.into_raw_fd();

    let dispatch_req = PipeDispatchRequest {
        request_json,
        handler_module: "webserver_graphql_subs".to_string(),
        handler_name: "graphql_authenticate_handler".to_string(),
        input_read_fd: None,
        output_write_fd,
        binary: false,
    };

    let queue = dispatch_queue_safe()?;
    if let Err(e) = queue.send(DispatchMessage::Pipe(dispatch_req)).await {
        return Err(format!("dispatch queue send failed: {}", e));
    }

    let mut output_receiver = Receiver::from_owned_fd(output_read)
        .map_err(|e| format!("failed to create pipe receiver: {}", e))?;
    let (status, _headers, body_bytes) = read_cgi_response(&mut output_receiver).await?;

    if status != 200 {
        return Err("Authentication failed".to_string());
    }

    let result: serde_json::Value = serde_json::from_slice(&body_bytes)
        .map_err(|e| format!("failed to parse auth response: {}", e))?;

    result
        .get("auth")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .ok_or_else(|| "missing auth field in response".to_string())
}

/// Dispatch an unregister request to Prolog via pipe dispatch.
///
/// Notifies Prolog to decrement the cohort member count for the given
/// cohort key. Called when a subscription is completed or a WebSocket
/// connection is closed.
async fn dispatch_unregister_to_prolog(cohort_key: &str) -> Result<(), String> {
    use crate::dispatch::{DispatchMessage, PipeDispatchRequest};
    use nix::unistd::pipe;
    use std::os::fd::IntoRawFd;
    use tokio::net::unix::pipe::Receiver;

    let payload = json!({
        "cohort_key": cohort_key,
    });

    let request_json = json!({
        "method": "POST",
        "path": "/api/graphql-ws/unregister",
        "query": "",
        "headers": {},
        "body": "",
        "params": {},
        "payload": payload,
    });

    let (output_read, output_write) = pipe().map_err(|e| format!("pipe creation failed: {}", e))?;
    let output_write_fd = output_write.into_raw_fd();

    let dispatch_req = PipeDispatchRequest {
        request_json,
        handler_module: "webserver_graphql_subs".to_string(),
        handler_name: "graphql_unregister_handler".to_string(),
        input_read_fd: None,
        output_write_fd,
        binary: false,
    };

    let queue = dispatch_queue_safe()?;
    if let Err(e) = queue.send(DispatchMessage::Pipe(dispatch_req)).await {
        return Err(format!("dispatch queue send failed: {}", e));
    }

    let mut output_receiver = Receiver::from_owned_fd(output_read)
        .map_err(|e| format!("failed to create pipe receiver: {}", e))?;
    let (status, _headers, _body_bytes) = read_cgi_response(&mut output_receiver).await?;

    if status != 200 {
        return Err(format!("unregister dispatch returned status {}", status));
    }

    Ok(())
}

/// Safe wrapper around dispatch_queue that returns an error instead of
/// panicking when the dispatcher is not initialized (e.g., in unit tests).
fn dispatch_queue_safe() -> Result<mpsc::Sender<crate::dispatch::DispatchMessage>, String> {
    let result = std::panic::catch_unwind(|| {
        crate::dispatch::dispatch_queue()
    });
    result.map_err(|_| "dispatch queue not initialized".to_string())
}

/// Read a complete CGI response (headers + body) from a pipe receiver.
/// Returns (status, headers, body_bytes).
async fn read_cgi_response(
    reader: &mut tokio::net::unix::pipe::Receiver,
) -> Result<(u16, Vec<(String, String)>, Vec<u8>), String> {
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];
    let mut header_end: Option<usize> = None;

    // Phase 1: Read until we find the header separator.
    while header_end.is_none() {
        let n = reader.read(&mut chunk).await.map_err(|e| format!("read error: {}", e))?;
        if n == 0 {
            return Err("EOF before CGI header separator".to_string());
        }
        buf.extend_from_slice(&chunk[..n]);
        // Check for \n\n separator.
        if let Some(pos) = buf.windows(2).position(|w| w == b"\n\n") {
            header_end = Some(pos + 2);
        }
        // Check for \r\n\r\n separator.
        else if let Some(pos) = buf.windows(4).position(|w| w == b"\r\n\r\n") {
            header_end = Some(pos + 4);
        }
    }

    let header_end = header_end.unwrap();
    let header_bytes = &buf[..header_end];
    let header_str = std::str::from_utf8(header_bytes)
        .map_err(|e| format!("invalid UTF-8 in headers: {}", e))?;

    let mut status: u16 = 200;
    let mut headers = Vec::new();
    for line in header_str.lines() {
        if let Some(colon_pos) = line.find(':') {
            let key = line[..colon_pos].trim().to_string();
            let value = line[colon_pos + 1..].trim().to_string();
            if key.eq_ignore_ascii_case("status") {
                let code_str = value.split_whitespace().next().unwrap_or("200");
                status = code_str.parse().unwrap_or(200);
            } else if !key.eq_ignore_ascii_case("transfer-encoding")
                && !key.eq_ignore_ascii_case("connection")
            {
                headers.push((key, value));
            }
        }
    }

    let mut body = buf[header_end..].to_vec();

    // Phase 2: Read the body.
    let content_length = headers.iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("content-length"))
        .and_then(|(_, v)| v.parse::<usize>().ok());

    if let Some(cl) = content_length {
        // Read until we have Content-Length bytes.
        while body.len() < cl {
            let n = reader.read(&mut chunk).await.map_err(|e| format!("read error: {}", e))?;
            if n == 0 { break; }
            body.extend_from_slice(&chunk[..n]);
        }
        body.truncate(cl);
    } else {
        // No Content-Length — read until EOF (worker closes pipe after response).
        loop {
            let n = reader.read(&mut chunk).await.map_err(|e| format!("read error: {}", e))?;
            if n == 0 { break; }
            body.extend_from_slice(&chunk[..n]);
        }
    }

    Ok((status, headers, body))
}

/// Generate a unique StreamId for a WebSocket subscription.
fn next_ws_stream_id() -> StreamId {
    WS_STREAM_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Errors that can occur during WebSocket protocol parsing.
#[derive(Debug)]
pub enum WsProtocolError {
    InvalidJson(serde_json::Error),
    UnknownMessageType(String),
    MissingField(&'static str),
    PayloadTooLarge,
    SubscriptionIdTooLong,
}

impl std::fmt::Display for WsProtocolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WsProtocolError::InvalidJson(e) => write!(f, "invalid JSON: {}", e),
            WsProtocolError::UnknownMessageType(t) => write!(f, "unknown message type: {}", t),
            WsProtocolError::MissingField(field) => write!(f, "missing required field: {}", field),
            WsProtocolError::PayloadTooLarge => write!(f, "payload too large"),
            WsProtocolError::SubscriptionIdTooLong => write!(f, "subscription id too long"),
        }
    }
}

/// Client-to-server messages in the `graphql-transport-ws` protocol.
#[derive(Debug)]
pub enum WsClientMessage {
    ConnectionInit {
        payload: Option<Value>,
    },
    Subscribe {
        id: String,
        payload: SubscribePayload,
    },
    Complete {
        id: String,
    },
    Ping {
        payload: Option<Value>,
    },
    Pong {
        payload: Option<Value>,
    },
}

/// Payload for a `subscribe` message.
#[derive(Debug)]
pub struct SubscribePayload {
    pub query: String,
    pub variables: Option<HashMap<String, Value>>,
    pub operation_name: Option<String>,
}

/// Result of handling a client message.
#[derive(Debug)]
pub enum WsMessageResult {
    Continue,
    Close(u16, String),
    NotImplemented,
}

impl WsClientMessage {
    /// Parse a WebSocket message payload into a `WsClientMessage`.
    ///
    /// Enforces the `connection_init` payload size limit.
    pub fn from_bytes(data: &[u8], is_connection_init: bool) -> Result<Self, WsProtocolError> {
        if is_connection_init && data.len() > CONNECTION_INIT_MAX_SIZE {
            return Err(WsProtocolError::PayloadTooLarge);
        }

        let value: Value = serde_json::from_slice(data).map_err(WsProtocolError::InvalidJson)?;

        let msg_type = value
            .get("type")
            .and_then(|t| t.as_str())
            .ok_or(WsProtocolError::MissingField("type"))?;

        match msg_type {
            "connection_init" => {
                let payload = value.get("payload").cloned();
                Ok(WsClientMessage::ConnectionInit { payload })
            }
            "subscribe" => {
                let id = value
                    .get("id")
                    .and_then(|v| v.as_str())
                    .ok_or(WsProtocolError::MissingField("id"))?
                    .to_string();
                if id.len() > MAX_SUBSCRIPTION_ID_LEN {
                    return Err(WsProtocolError::SubscriptionIdTooLong);
                }

                let payload = value
                    .get("payload")
                    .ok_or(WsProtocolError::MissingField("payload"))?;

                let query = payload
                    .get("query")
                    .and_then(|q| q.as_str())
                    .ok_or(WsProtocolError::MissingField("query"))?
                    .to_string();

                let variables = payload
                    .get("variables")
                    .and_then(|v| v.as_object())
                    .map(|obj| {
                        obj.iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect::<HashMap<_, _>>()
                    });

                let operation_name = payload
                    .get("operationName")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());

                Ok(WsClientMessage::Subscribe {
                    id,
                    payload: SubscribePayload {
                        query,
                        variables,
                        operation_name,
                    },
                })
            }
            "complete" => {
                let id = value
                    .get("id")
                    .and_then(|v| v.as_str())
                    .ok_or(WsProtocolError::MissingField("id"))?
                    .to_string();
                if id.len() > MAX_SUBSCRIPTION_ID_LEN {
                    return Err(WsProtocolError::SubscriptionIdTooLong);
                }
                Ok(WsClientMessage::Complete { id })
            }
            "ping" => {
                let payload = value.get("payload").cloned();
                Ok(WsClientMessage::Ping { payload })
            }
            "pong" => {
                let payload = value.get("payload").cloned();
                Ok(WsClientMessage::Pong { payload })
            }
            other => Err(WsProtocolError::UnknownMessageType(other.to_string())),
        }
    }
}

// --- Server-to-client message helpers ---

pub fn msg_connection_ack() -> String {
    json!({"type": "connection_ack"}).to_string()
}

pub fn msg_next(id: &str, data: Value) -> String {
    json!({"type": "next", "id": id, "payload": {"data": data}}).to_string()
}

pub fn msg_error(id: &str, message: &str) -> String {
    json!({"type": "error", "id": id, "payload": {"message": message}}).to_string()
}

pub fn msg_complete(id: &str) -> String {
    json!({"type": "complete", "id": id}).to_string()
}

pub fn msg_ping() -> String {
    json!({"type": "ping"}).to_string()
}

pub fn msg_pong() -> String {
    json!({"type": "pong"}).to_string()
}

// --- WsSubscriptionRegistry ---

/// State for a single cohort.
struct CohortState {
    /// Raw channel name (Prolog broadcasts events here).
    raw_channel: String,
    /// Resolved channel name (cohort resolver broadcasts here).
    resolved_channel: String,
    /// Set of subscriber StreamIds.
    subscribers: HashSet<StreamId>,
    /// Handle to the cohort resolver task.
    resolver_task: Option<tokio::task::JoinHandle<()>>,
    /// StreamId of the cohort resolver's subscription to the raw channel.
    resolver_stream_id: StreamId,
}

/// Global registry of WebSocket subscription cohorts.
struct WsSubscriptionRegistry {
    /// Map: cohort_key → CohortState
    cohorts: HashMap<String, CohortState>,
}

impl WsSubscriptionRegistry {
    fn new() -> Self {
        Self {
            cohorts: HashMap::new(),
        }
    }
}

static WS_REGISTRY: OnceLock<Arc<Mutex<WsSubscriptionRegistry>>> = OnceLock::new();

fn ws_registry() -> &'static Arc<Mutex<WsSubscriptionRegistry>> {
    WS_REGISTRY.get_or_init(|| Arc::new(Mutex::new(WsSubscriptionRegistry::new())))
}

/// Compact event broadcast by Prolog to the raw cohort channel.
/// The cohort resolver task parses this and resolves the full document.
#[derive(Debug, Deserialize)]
struct GqlEvent {
    doc_iri: String,
    change_type: String, // "added", "changed", "deleted"
    commit_id: String,
}

// --- Per-connection state ---

/// State for a single WebSocket connection.
pub struct WsConnectionState {
    /// Whether `connection_init` has been received.
    pub initialized: bool,
    /// Map of subscription_id → (cohort_key, stream_id, field_name).
    pub subscriptions: HashMap<String, (String, StreamId, String)>,
    /// Rate limiter: timestamps of recent subscription creations.
    pub subscription_timestamps: VecDeque<Instant>,
    /// WebSocket message sender (for writing to the client).
    pub ws_tx: mpsc::Sender<Message>,
    /// Branch path extracted from the URL.
    pub branch_path: String,
    /// Authorization header value extracted from the HTTP upgrade request
    /// or from `connection_init` payload. Empty string means anonymous.
    pub auth_header: String,
}

impl WsConnectionState {
    pub fn new(ws_tx: mpsc::Sender<Message>, branch_path: String, auth_header: String) -> Self {
        Self {
            initialized: false,
            subscriptions: HashMap::new(),
            subscription_timestamps: VecDeque::new(),
            ws_tx,
            branch_path,
            auth_header,
        }
    }

    /// Check and enforce the rate limit for subscription creation.
    /// Returns true if within the rate limit, false if throttled.
    pub fn check_rate_limit(&mut self) -> bool {
        let now = Instant::now();
        // Evict timestamps outside the window.
        while let Some(&front) = self.subscription_timestamps.front() {
            if now.duration_since(front) > RATE_LIMIT_WINDOW {
                self.subscription_timestamps.pop_front();
            } else {
                break;
            }
        }
        if self.subscription_timestamps.len() >= RATE_LIMIT_MAX {
            return false;
        }
        self.subscription_timestamps.push_back(now);
        true
    }

    /// Check if the connection has room for another subscription.
    pub fn has_subscription_capacity(&self) -> bool {
        self.subscriptions.len() < MAX_SUBSCRIPTIONS_PER_CONNECTION
    }
}

/// Cohort resolver task — receives raw events from Prolog, resolves the
/// full document via Juniper, and broadcasts resolved JSON to the
/// resolved channel.
///
/// This is a placeholder implementation for Phase 4. Full Juniper
/// resolution requires the subscription RootNode and
/// SubscriptionResolveContext, which will be wired in subsequent steps.
async fn cohort_resolver_task(
    mut raw_rx: mpsc::Receiver<axum::body::Bytes>,
    resolved_channel: String,
    branch_path: String,
) {
    crate::log::log_info(format!(
        "[ws] cohort resolver started for branch={} channel={}",
        branch_path, resolved_channel
    ));

    while let Some(bytes) = raw_rx.recv().await {
        // Parse the compact event from Prolog.
        let event: GqlEvent = match serde_json::from_slice(&bytes) {
            Ok(e) => e,
            Err(e) => {
                crate::log::log_error(format!(
                    "[ws] cohort resolver: failed to parse event: {}",
                    e
                ));
                continue;
            }
        };

        // Document resolution is done via pipe dispatch to Prolog, which
        // calls get_document/3 with the cohort's stored selection set and
        // filter. This broadcasts the raw event so the pipeline is testable
        // before the Prolog resolve handler is wired in.
        // Alternative: build SubscriptionResolveContext in Rust and resolve
        // via Juniper's execute_validated_query — avoids a Prolog round-trip
        // but requires passing SyncStoreLayer handles through the pipe.
        let resolved = json!({
            "doc_iri": event.doc_iri,
            "change_type": event.change_type,
            "commit_id": event.commit_id,
        });
        let resolved_bytes = axum::body::Bytes::from(resolved.to_string());

        if let Err(e) = broadcast_registry()
            .lock()
            .unwrap()
            .send(&resolved_channel, resolved_bytes)
        {
            crate::log::log_error(format!(
                "[ws] cohort resolver: failed to send to resolved channel: {}",
                e
            ));
        }
    }

    crate::log::log_info(format!(
        "[ws] cohort resolver stopped for channel={}",
        resolved_channel
    ));
}

/// Per-subscriber task — receives resolved JSON from the resolved channel
/// and forwards it as `next` messages over the WebSocket.
async fn ws_subscriber_task(
    mut resolved_rx: mpsc::Receiver<axum::body::Bytes>,
    ws_tx: mpsc::Sender<Message>,
    subscription_id: String,
    field_name: String,
) {
    while let Some(bytes) = resolved_rx.recv().await {
        // Parse the resolved JSON.
        let data: Value = match serde_json::from_slice(&bytes) {
            Ok(v) => v,
            Err(e) => {
                crate::log::log_error(format!(
                    "[ws] subscriber task: failed to parse resolved data: {}",
                    e
                ));
                continue;
            }
        };

        // Wrap in a graphql-transport-ws `next` message.
        let next_msg = msg_next(&subscription_id, json!({ field_name.clone(): data }));
        if ws_tx.send(Message::Text(next_msg.into())).await.is_err() {
            // WebSocket closed — stop forwarding.
            break;
        }
    }
}

/// Result of handling a client message.
pub enum HandleResult {
    Continue,
    Close(u16, String),
}

/// Handle a parsed client message and update connection state.
pub async fn handle_client_message(
    state: &mut WsConnectionState,
    msg: WsClientMessage,
) -> HandleResult {
    match msg {
        WsClientMessage::ConnectionInit { payload } => {
            if state.initialized {
                let _ = state
                    .ws_tx
                    .send(Message::Text(msg_error(
                        "null",
                        "Connection already initialized",
                    ).into()))
                    .await;
                return HandleResult::Close(4429, "Too many initialisation requests".to_string());
            }
            // If payload contains an `authorization` field, use that for auth.
            // This supports browser WebSocket APIs that can't set HTTP headers.
            if let Some(ref p) = payload {
                if let Some(auth) = p.get("authorization").and_then(|v| v.as_str()) {
                    if !auth.is_empty() {
                        // Validate the auth token from the payload.
                        if let Err(_) = dispatch_authenticate_to_prolog(auth).await {
                            let _ = state
                                .ws_tx
                                .send(Message::Text(msg_error(
                                    "null",
                                    "Unauthorized",
                                ).into()))
                                .await;
                            return HandleResult::Close(4401, "Unauthorized".to_string());
                        }
                        state.auth_header = auth.to_string();
                    }
                }
            }
            state.initialized = true;
            let _ = state
                .ws_tx
                .send(Message::Text(msg_connection_ack().into()))
                .await;
            HandleResult::Continue
        }
        WsClientMessage::Subscribe { id, payload } => {
            if !state.initialized {
                let _ = state
                    .ws_tx
                    .send(Message::Text(msg_error(
                        &id,
                        "Connection not initialized",
                    ).into()))
                    .await;
                return HandleResult::Close(4401, "Unauthorized".to_string());
            }

            if !state.has_subscription_capacity() {
                let _ = state
                    .ws_tx
                    .send(Message::Text(msg_error(
                        &id,
                        "Too many subscriptions",
                    ).into()))
                    .await;
                return HandleResult::Continue;
            }

            if !state.check_rate_limit() {
                let _ = state
                    .ws_tx
                    .send(Message::Text(msg_error(
                        &id,
                        "Rate limit exceeded",
                    ).into()))
                    .await;
                return HandleResult::Continue;
            }

            // Dispatch one-shot to Prolog via pipe dispatch.
            // Prolog will parse the subscription query, register the cohort,
            // and return the cohort key, raw channel, and field name.
            let variables_json = payload.variables
                .map(|m| Value::Object(m.into_iter().map(|(k, v)| (k, v)).collect()))
                .unwrap_or(json!({}));
            let operation_name_str = payload.operation_name.as_deref().unwrap_or("");
            let subscribe_result = dispatch_subscribe_to_prolog(
                &state.branch_path,
                &payload.query,
                &variables_json,
                operation_name_str,
                &state.auth_header,
            ).await;

            let (field_name, cohort_key, raw_channel) = match subscribe_result {
                Ok(result) => (result.field_name, result.cohort_key, result.raw_channel),
                Err(e) => {
                    crate::log::log_error(format!(
                        "[ws] subscribe dispatch failed: {}", e
                    ));
                    let _ = state
                        .ws_tx
                        .send(Message::Text(msg_error(
                            &id,
                            &format!("Subscription registration failed: {}", e),
                        ).into()))
                        .await;
                    return HandleResult::Continue;
                }
            };
            let resolved_channel = format!("graphql_resolved_{}", cohort_key);

            // Create a direct mpsc channel for this subscriber.
            // The BroadcastRegistry will drain into the sender via its
            // per-subscriber task, and our ws_subscriber_task reads from
            // the receiver and forwards `next` messages over WebSocket.
            let (sub_tx, sub_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);

            // Generate a unique StreamId for this subscriber.
            let stream_id = next_ws_stream_id();

            // Register in the WsSubscriptionRegistry.
            {
                let mut registry = ws_registry().lock().unwrap();
                let cohort = registry.cohorts.entry(cohort_key.clone()).or_insert_with(|| {
                    // Create the cohort resolver task for a new cohort.
                    // The resolver gets its own independent StreamId so that
                    // subscriber unsubscriptions never affect the resolver.
                    let resolver_stream_id = next_ws_stream_id();
                    let (resolver_tx, resolver_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
                    let task = tokio::spawn(cohort_resolver_task(
                        resolver_rx,
                        resolved_channel.clone(),
                        state.branch_path.clone(),
                    ));
                    // Subscribe the resolver to the raw channel.
                    broadcast_registry().lock().unwrap().subscribe(
                        raw_channel.clone(),
                        resolver_stream_id,
                        resolver_tx,
                        None,
                    );
                    CohortState {
                        raw_channel: raw_channel.clone(),
                        resolved_channel: resolved_channel.clone(),
                        subscribers: HashSet::new(),
                        resolver_task: Some(task),
                        resolver_stream_id,
                    }
                });
                cohort.subscribers.insert(stream_id);
            }

            // Subscribe this subscriber to the resolved channel.
            broadcast_registry().lock().unwrap().subscribe(
                resolved_channel.clone(),
                stream_id,
                sub_tx,
                None,
            );

            // Spawn the per-subscriber task that forwards resolved events
            // as `next` messages over the WebSocket.
            tokio::spawn(ws_subscriber_task(
                sub_rx,
                state.ws_tx.clone(),
                id.clone(),
                field_name.clone(),
            ));

            // Record the subscription in connection state.
            state.subscriptions.insert(
                id.clone(),
                (cohort_key.clone(), stream_id, field_name.clone()),
            );

            HandleResult::Continue
        }
        WsClientMessage::Complete { id } => {
            if let Some((cohort_key, stream_id, _field_name)) = state.subscriptions.remove(&id) {
                // Unsubscribe from the resolved channel.
                let resolved_channel = {
                    let registry = ws_registry().lock().unwrap();
                    registry
                        .cohorts
                        .get(&cohort_key)
                        .map(|c| c.resolved_channel.clone())
                };
                if let Some(ref ch) = resolved_channel {
                    broadcast_registry().lock().unwrap().unsubscribe(ch, stream_id);
                }
                // Safety net: unsubscribe from all channels.
                broadcast_registry().lock().unwrap().unsubscribe_all(stream_id);

                // Remove from cohort and tear down if empty.
                let should_teardown = {
                    let mut registry = ws_registry().lock().unwrap();
                    if let Some(cohort) = registry.cohorts.get_mut(&cohort_key) {
                        cohort.subscribers.remove(&stream_id);
                        cohort.subscribers.is_empty()
                    } else {
                        false
                    }
                };
                if should_teardown {
                    let mut registry = ws_registry().lock().unwrap();
                    if let Some(cohort) = registry.cohorts.remove(&cohort_key) {
                        // Unsubscribe resolver from raw channel.
                        broadcast_registry()
                            .lock()
                            .unwrap()
                            .unsubscribe(&cohort.raw_channel, cohort.resolver_stream_id);
                        // Abort resolver task.
                        if let Some(task) = cohort.resolver_task {
                            task.abort();
                        }
                    }
                }

                // Notify Prolog to decrement cohort member count.
                if let Err(e) = dispatch_unregister_to_prolog(&cohort_key).await {
                    crate::log::log_error(format!(
                        "[ws] unregister dispatch to Prolog failed: {}", e
                    ));
                }

                // Send complete confirmation.
                let _ = state
                    .ws_tx
                    .send(Message::Text(msg_complete(&id).into()))
                    .await;
            }
            HandleResult::Continue
        }
        WsClientMessage::Ping { payload: _ } => {
            let _ = state.ws_tx.send(Message::Text(msg_pong().into())).await;
            HandleResult::Continue
        }
        WsClientMessage::Pong { payload: _ } => {
            // Keepalive acknowledgment — no-op.
            HandleResult::Continue
        }
    }
}

/// Validate that a branch path matches the expected descriptor format:
/// `org/db/repo/branch/branch_name` (5 non-empty segments, no path traversal).
/// This mirrors the validation that Prolog's `resolve_absolute_string_descriptor/2`
/// performs. Authoritative validation still happens in Prolog when
/// `register_subscription` is called.
fn validate_branch_path(path: &str) -> bool {
    if path.is_empty() || path.len() > 1024 {
        return false;
    }
    let segments: Vec<&str> = path.split('/').collect();
    if segments.len() != 5 {
        return false;
    }
    segments.iter().all(|s| {
        !s.is_empty()
            && *s != ".."
            && *s != "."
            && !s.contains('\0')
    })
}

/// WebSocket connection handler.
///
/// Called by axum when a client connects to a WebSocket route.
/// The `path` parameter captures the wildcard segment from the route pattern
/// (e.g., `org/db/local/branch/main` from `/api/graphql-ws/*path`).
pub async fn handle_ws_connection(
    ws_upgrade: WebSocketUpgrade,
    Path(path): Path<String>,
    headers: HeaderMap,
) -> impl IntoResponse {
    if !validate_branch_path(&path) {
        return (StatusCode::BAD_REQUEST, "Invalid branch path").into_response();
    }
    // Extract Authorization header from the HTTP upgrade request.
    // Browser WebSocket APIs can't set headers, so this may be empty —
    // in that case, auth can be provided via connection_init payload.
    let auth_header = headers
        .get("authorization")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("")
        .to_string();

    // If an Authorization header is present, validate it before upgrading.
    // If no header is present, allow the upgrade — auth can come via
    // connection_init payload, and per-subscription auth is always checked.
    if !auth_header.is_empty() {
        if let Err(e) = dispatch_authenticate_to_prolog(&auth_header).await {
            crate::log::log_info(format!(
                "[ws] rejecting WebSocket upgrade: auth failed (not logging token value)"
            ));
            return (StatusCode::UNAUTHORIZED, e).into_response();
        }
    }

    // Only accept the `graphql-transport-ws` subprotocol.
    ws_upgrade
        .protocols(["graphql-transport-ws"])
        .on_upgrade(move |socket| async move {
            run_ws_connection(socket, path, auth_header).await;
        })
        .into_response()
}

/// Main WebSocket connection loop.
///
/// Splits the WebSocket into sender and receiver, creates an outbound message
/// channel, spawns a writer task, and runs the message loop on the receiver.
async fn run_ws_connection(socket: axum::extract::ws::WebSocket, branch_path: String, auth_header: String) {
    let (ws_tx, mut ws_rx) = socket.split();
    let _ = &ws_tx; // Suppress unused warning until Phase 4.

    // Channel for outbound messages (writer task reads from this).
    let (outbound_tx, mut outbound_rx) = mpsc::channel::<Message>(128);

    // Spawn the writer task: reads from outbound channel and sends over WebSocket.
    let writer_task = tokio::spawn(async move {
        let mut ws_tx = ws_tx;
        while let Some(msg) = outbound_rx.recv().await {
            if ws_tx.send(msg).await.is_err() {
                break;
            }
            // Yield to allow the receiver side to be polled.
            tokio::task::yield_now().await;
        }
    });

    // Spawn the ping task: sends ping every 30 seconds.
    let ping_tx = outbound_tx.clone();
    let ping_task = tokio::spawn(async move {
        let mut interval = tokio::time::interval(PING_INTERVAL);
        interval.tick().await; // Skip first immediate tick.
        loop {
            interval.tick().await;
            if ping_tx.send(Message::Text(msg_ping().into())).await.is_err() {
                break;
            }
        }
    });

    let mut state = WsConnectionState::new(outbound_tx, branch_path, auth_header);

    // Main message loop.
    while let Some(msg_result) = ws_rx.next().await {
        let msg = match msg_result {
            Ok(msg) => msg,
            Err(e) => {
                crate::log::log_error(format!("[ws] receive error: {}", e));
                break;
            }
        };

        let (_data, _is_binary) = match msg {
            Message::Text(text) => (text.as_bytes().to_vec(), false),
            Message::Binary(data) => (data.to_vec(), true),
            Message::Close(_) => break,
            // Ping/Pong are auto-handled by axum.
            _ => continue,
        };

        // Determine if this could be a connection_init (for size limit enforcement).
        let is_connection_init = String::from_utf8_lossy(&_data)
            .contains("\"connection_init\"");

        let parsed = match WsClientMessage::from_bytes(&_data, is_connection_init) {
            Ok(m) => m,
            Err(e) => {
                crate::log::log_error(format!("[ws] protocol error: {}", e));
                let _ = state
                    .ws_tx
                    .send(Message::Text(msg_error(
                        "null",
                        &format!("Protocol error: {}", e),
                    ).into()))
                    .await;
                continue;
            }
        };

        match handle_client_message(&mut state, parsed).await {
            HandleResult::Continue => {}
            HandleResult::Close(code, reason) => {
                crate::log::log_info(format!(
                    "[ws] closing connection: code={} reason={}",
                    code, reason
                ));
                break;
            }
        }
    }

    // Cleanup: unsubscribe all active subscriptions.
    for (_, (cohort_key, stream_id, _)) in &state.subscriptions {
        // Unsubscribe from resolved channel.
        let resolved_channel = {
            let registry = ws_registry().lock().unwrap();
            registry
                .cohorts
                .get(cohort_key)
                .map(|c| c.resolved_channel.clone())
        };
        if let Some(ref ch) = resolved_channel {
            broadcast_registry().lock().unwrap().unsubscribe(ch, *stream_id);
        }
        // Safety net: unsubscribe from all channels in case of any
        // missed registrations.
        broadcast_registry().lock().unwrap().unsubscribe_all(*stream_id);

        // Notify Prolog to decrement cohort member count.
        if let Err(e) = dispatch_unregister_to_prolog(cohort_key).await {
            crate::log::log_error(format!(
                "[ws] cleanup: unregister dispatch to Prolog failed: {}", e
            ));
        }
    }

    // Tear down empty cohorts.
    {
        let mut registry = ws_registry().lock().unwrap();
        let empty_cohorts: Vec<String> = registry
            .cohorts
            .iter()
            .filter_map(|(key, cohort)| {
                // Check if any subscriber from this connection is still in the cohort.
                // All our subscribers have been unsubscribed above, but the cohort
                // may still have subscribers from other connections.
                if cohort.subscribers.is_empty() {
                    Some(key.clone())
                } else {
                    None
                }
            })
            .collect();
        for key in empty_cohorts {
            if let Some(cohort) = registry.cohorts.remove(&key) {
                broadcast_registry()
                    .lock()
                    .unwrap()
                    .unsubscribe(&cohort.raw_channel, cohort.resolver_stream_id);
                if let Some(task) = cohort.resolver_task {
                    task.abort();
                }
            }
        }
    }
    state.subscriptions.clear();

    // Abort background tasks.
    ping_task.abort();
    writer_task.abort();

    crate::log::log_info("[ws] connection closed".to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_connection_init_without_payload() {
        let data = br#"{"type":"connection_init"}"#;
        let msg = WsClientMessage::from_bytes(data, true).unwrap();
        match msg {
            WsClientMessage::ConnectionInit { payload: None } => {}
            _ => panic!("expected ConnectionInit with no payload"),
        }
    }

    #[test]
    fn parse_connection_init_with_payload() {
        let data = br#"{"type":"connection_init","payload":{"token":"abc"}}"#;
        let msg = WsClientMessage::from_bytes(data, true).unwrap();
        match msg {
            WsClientMessage::ConnectionInit { payload: Some(v) } => {
                assert_eq!(v["token"], "abc");
            }
            _ => panic!("expected ConnectionInit with payload"),
        }
    }

    #[test]
    fn parse_connection_init_payload_too_large() {
        let big_payload = format!(
            r#"{{"type":"connection_init","payload":{{"data":"{}"}}}}"#,
            "x".repeat(5000)
        );
        let result = WsClientMessage::from_bytes(big_payload.as_bytes(), true);
        assert!(matches!(result, Err(WsProtocolError::PayloadTooLarge)));
    }

    #[test]
    fn parse_subscribe() {
        let data = br#"{"type":"subscribe","id":"1","payload":{"query":"subscription { Person_added { _id } }"}}"#;
        let msg = WsClientMessage::from_bytes(data, false).unwrap();
        match msg {
            WsClientMessage::Subscribe { id, payload } => {
                assert_eq!(id, "1");
                assert_eq!(payload.query, "subscription { Person_added { _id } }");
                assert!(payload.variables.is_none());
                assert!(payload.operation_name.is_none());
            }
            _ => panic!("expected Subscribe"),
        }
    }

    #[test]
    fn parse_subscribe_with_variables() {
        let data = br#"{"type":"subscribe","id":"2","payload":{"query":"subscription { Person_added { _id } }","variables":{"name":"Alice"},"operationName":"MySub"}}"#;
        let msg = WsClientMessage::from_bytes(data, false).unwrap();
        match msg {
            WsClientMessage::Subscribe { id, payload } => {
                assert_eq!(id, "2");
                assert_eq!(payload.operation_name, Some("MySub".to_string()));
                let vars = payload.variables.unwrap();
                assert_eq!(vars["name"], "Alice");
            }
            _ => panic!("expected Subscribe"),
        }
    }

    #[test]
    fn parse_complete() {
        let data = br#"{"type":"complete","id":"1"}"#;
        let msg = WsClientMessage::from_bytes(data, false).unwrap();
        match msg {
            WsClientMessage::Complete { id } => assert_eq!(id, "1"),
            _ => panic!("expected Complete"),
        }
    }

    #[test]
    fn parse_ping() {
        let data = br#"{"type":"ping"}"#;
        let msg = WsClientMessage::from_bytes(data, false).unwrap();
        match msg {
            WsClientMessage::Ping { payload: None } => {}
            _ => panic!("expected Ping"),
        }
    }

    #[test]
    fn parse_pong() {
        let data = br#"{"type":"pong"}"#;
        let msg = WsClientMessage::from_bytes(data, false).unwrap();
        match msg {
            WsClientMessage::Pong { payload: None } => {}
            _ => panic!("expected Pong"),
        }
    }

    #[test]
    fn parse_invalid_json() {
        let data = b"not json at all";
        let result = WsClientMessage::from_bytes(data, false);
        assert!(matches!(result, Err(WsProtocolError::InvalidJson(_))));
    }

    #[test]
    fn parse_unknown_message_type() {
        let data = br#"{"type":"unknown_type"}"#;
        let result = WsClientMessage::from_bytes(data, false);
        assert!(matches!(result, Err(WsProtocolError::UnknownMessageType(_))));
    }

    #[test]
    fn parse_missing_type_field() {
        let data = br#"{"id":"1"}"#;
        let result = WsClientMessage::from_bytes(data, false);
        assert!(matches!(result, Err(WsProtocolError::MissingField("type"))));
    }

    #[test]
    fn parse_subscribe_missing_id() {
        let data = br#"{"type":"subscribe","payload":{"query":"{}"}}"#;
        let result = WsClientMessage::from_bytes(data, false);
        assert!(matches!(result, Err(WsProtocolError::MissingField("id"))));
    }

    #[test]
    fn parse_subscribe_missing_payload() {
        let data = br#"{"type":"subscribe","id":"1"}"#;
        let result = WsClientMessage::from_bytes(data, false);
        assert!(matches!(result, Err(WsProtocolError::MissingField("payload"))));
    }

    #[test]
    fn parse_subscribe_missing_query() {
        let data = br#"{"type":"subscribe","id":"1","payload":{}}"#;
        let result = WsClientMessage::from_bytes(data, false);
        assert!(matches!(result, Err(WsProtocolError::MissingField("query"))));
    }

    #[test]
    fn msg_connection_ack_correct() {
        let msg = msg_connection_ack();
        let v: Value = serde_json::from_str(&msg).unwrap();
        assert_eq!(v["type"], "connection_ack");
    }

    #[test]
    fn msg_next_correct() {
        let msg = msg_next("1", json!({"Person_added": {"_id": "doc1"}}));
        let v: Value = serde_json::from_str(&msg).unwrap();
        assert_eq!(v["type"], "next");
        assert_eq!(v["id"], "1");
        assert_eq!(v["payload"]["data"]["Person_added"]["_id"], "doc1");
    }

    #[test]
    fn msg_error_correct() {
        let msg = msg_error("1", "something went wrong");
        let v: Value = serde_json::from_str(&msg).unwrap();
        assert_eq!(v["type"], "error");
        assert_eq!(v["id"], "1");
        assert_eq!(v["payload"]["message"], "something went wrong");
    }

    #[test]
    fn msg_complete_correct() {
        let msg = msg_complete("1");
        let v: Value = serde_json::from_str(&msg).unwrap();
        assert_eq!(v["type"], "complete");
        assert_eq!(v["id"], "1");
    }

    #[test]
    fn msg_ping_correct() {
        let msg = msg_ping();
        let v: Value = serde_json::from_str(&msg).unwrap();
        assert_eq!(v["type"], "ping");
    }

    #[test]
    fn msg_pong_correct() {
        let msg = msg_pong();
        let v: Value = serde_json::from_str(&msg).unwrap();
        assert_eq!(v["type"], "pong");
    }

    #[tokio::test]
    async fn rate_limit_allows_burst_then_throttles() {
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());

        // First RATE_LIMIT_MAX subscriptions should pass.
        for _ in 0..RATE_LIMIT_MAX {
            assert!(state.check_rate_limit());
        }

        // The next one should be throttled.
        assert!(!state.check_rate_limit());
    }

    #[test]
    fn subscription_capacity_check() {
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());

        // Fill up to MAX_SUBSCRIPTIONS_PER_CONNECTION.
        for i in 0..MAX_SUBSCRIPTIONS_PER_CONNECTION {
            state.subscriptions.insert(format!("sub_{}", i), ("cohort".to_string(), i as u64, "field".to_string()));
        }
        assert!(!state.has_subscription_capacity());

        // Remove one, should have capacity again.
        state.subscriptions.remove("sub_0");
        assert!(state.has_subscription_capacity());
    }

    #[test]
    fn gql_event_parsing() {
        let json = r#"{"doc_iri":"http://example.com/Person/1","change_type":"added","commit_id":"abc123"}"#;
        let event: GqlEvent = serde_json::from_str(json).unwrap();
        assert_eq!(event.doc_iri, "http://example.com/Person/1");
        assert_eq!(event.change_type, "added");
        assert_eq!(event.commit_id, "abc123");
    }

    #[test]
    fn next_ws_stream_id_is_unique() {
        let id1 = next_ws_stream_id();
        let id2 = next_ws_stream_id();
        assert_ne!(id1, id2);
    }

    #[test]
    fn subscription_id_too_long_rejected() {
        let long_id = "x".repeat(MAX_SUBSCRIPTION_ID_LEN + 1);
        let data = format!(
            r#"{{"type":"subscribe","id":"{}","payload":{{"query":"subscription {{ Person_added {{ _id }} }}"}}}}"#,
            long_id
        );
        let result = WsClientMessage::from_bytes(data.as_bytes(), false);
        assert!(matches!(result, Err(WsProtocolError::SubscriptionIdTooLong)));
    }

    #[test]
    fn subscription_id_at_limit_accepted() {
        let id = "x".repeat(MAX_SUBSCRIPTION_ID_LEN);
        let data = format!(
            r#"{{"type":"subscribe","id":"{}","payload":{{"query":"subscription {{ Person_added {{ _id }} }}"}}}}"#,
            id
        );
        let result = WsClientMessage::from_bytes(data.as_bytes(), false);
        assert!(result.is_ok());
    }

    #[test]
    fn complete_id_too_long_rejected() {
        let long_id = "x".repeat(MAX_SUBSCRIPTION_ID_LEN + 1);
        let data = format!(r#"{{"type":"complete","id":"{}"}}"#, long_id);
        let result = WsClientMessage::from_bytes(data.as_bytes(), false);
        assert!(matches!(result, Err(WsProtocolError::SubscriptionIdTooLong)));
    }

    #[test]
    fn validate_branch_path_valid() {
        assert!(validate_branch_path("org/db/local/branch/main"));
        assert!(validate_branch_path("admin/system/local/branch/dev"));
    }

    #[test]
    fn validate_branch_path_invalid_empty() {
        assert!(!validate_branch_path(""));
    }

    #[test]
    fn validate_branch_path_invalid_traversal() {
        assert!(!validate_branch_path("org/db/../branch/main"));
        assert!(!validate_branch_path("org/db/local/branch/.."));
    }

    #[test]
    fn validate_branch_path_invalid_wrong_segment_count() {
        assert!(!validate_branch_path("org/db/local/branch"));
        assert!(!validate_branch_path("org/db/local/branch/main/extra"));
    }

    #[test]
    fn validate_branch_path_invalid_empty_segment() {
        assert!(!validate_branch_path("org/db//branch/main"));
        assert!(!validate_branch_path("org/db/local/branch/"));
    }

    #[test]
    fn validate_branch_path_invalid_null_byte() {
        assert!(!validate_branch_path("org/db/local/branch/ma\0in"));
    }

    #[tokio::test]
    async fn duplicate_connection_init_closes() {
        let (tx, mut rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());
        state.initialized = true;

        let msg = WsClientMessage::ConnectionInit { payload: None };
        let result = handle_client_message(&mut state, msg).await;

        match result {
            HandleResult::Close(code, _) => assert_eq!(code, 4429),
            _ => panic!("expected Close(4429) for duplicate connection_init"),
        }

        // Verify error message was sent before closing.
        let sent = rx.recv().await.unwrap();
        match sent {
            Message::Text(t) => assert!(t.contains("Connection already initialized")),
            _ => panic!("expected Text message"),
        }
    }

    #[tokio::test]
    async fn connection_init_payload_auth_rejected_without_prolog() {
        // In unit tests, no Prolog dispatch queue is running, so auth
        // validation via dispatch_authenticate_to_prolog will fail.
        // This verifies that invalid/unverifiable auth tokens are rejected.
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());
        assert_eq!(state.auth_header, "");

        let payload = json!({"authorization": "Bearer token123"});
        let msg = WsClientMessage::ConnectionInit { payload: Some(payload) };
        let result = handle_client_message(&mut state, msg).await;
        match result {
            HandleResult::Close(code, _) => assert_eq!(code, 4401),
            _ => panic!("expected Close(4401) for unverifiable auth token"),
        }
    }

    #[tokio::test]
    async fn connection_init_without_auth_keeps_empty() {
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());

        let msg = WsClientMessage::ConnectionInit { payload: None };
        let result = handle_client_message(&mut state, msg).await;
        assert!(matches!(result, HandleResult::Continue));
        assert_eq!(state.auth_header, "");
    }

    #[tokio::test]
    async fn connection_init_payload_without_auth_field_succeeds() {
        // Payload without authorization field should not trigger validation
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());

        let payload = json!({"some_other_field": "value"});
        let msg = WsClientMessage::ConnectionInit { payload: Some(payload) };
        let result = handle_client_message(&mut state, msg).await;
        assert!(matches!(result, HandleResult::Continue));
        assert_eq!(state.auth_header, "");
    }

    // --- cohort_resolver_task tests ---

    #[tokio::test]
    async fn cohort_resolver_forwards_gql_event_to_resolved_channel() {
        crate::dispatch::set_tokio_handle(tokio::runtime::Handle::current());
        let resolved_channel = "graphql_resolved_test_cohort_1".to_string();
        let (raw_tx, raw_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);

        // Subscribe to the resolved channel so we can receive the forwarded event.
        let (sub_tx, mut sub_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
        let sub_stream_id = next_ws_stream_id();
        broadcast_registry().lock().unwrap().subscribe(
            resolved_channel.clone(),
            sub_stream_id,
            sub_tx,
            None,
        );

        // Spawn the cohort resolver task.
        let task = tokio::spawn(cohort_resolver_task(
            raw_rx,
            resolved_channel.clone(),
            "test/db/local/branch/main".to_string(),
        ));

        // Send a GqlEvent JSON to the raw channel.
        let event_json = r#"{"doc_iri":"http://example.com/Person/1","change_type":"added","commit_id":"abc123"}"#;
        raw_tx.send(axum::body::Bytes::from(event_json)).await.unwrap();

        // Wait for the resolved event to arrive on the subscriber.
        let received = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            sub_rx.recv(),
        )
        .await
        .expect("timeout waiting for resolved event")
        .expect("resolved channel closed");

        let parsed: Value = serde_json::from_slice(&received).expect("resolved event is valid JSON");
        assert_eq!(parsed["doc_iri"], "http://example.com/Person/1");
        assert_eq!(parsed["change_type"], "added");
        assert_eq!(parsed["commit_id"], "abc123");

        // Clean up.
        drop(raw_tx);
        task.abort();
        broadcast_registry().lock().unwrap().unsubscribe(&resolved_channel, sub_stream_id);
    }

    #[tokio::test]
    async fn cohort_resolver_skips_invalid_json() {
        crate::dispatch::set_tokio_handle(tokio::runtime::Handle::current());
        let resolved_channel = "graphql_resolved_test_cohort_2".to_string();
        let (raw_tx, raw_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);

        let (sub_tx, mut sub_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
        let sub_stream_id = next_ws_stream_id();
        broadcast_registry().lock().unwrap().subscribe(
            resolved_channel.clone(),
            sub_stream_id,
            sub_tx,
            None,
        );

        let task = tokio::spawn(cohort_resolver_task(
            raw_rx,
            resolved_channel.clone(),
            "test/db/local/branch/main".to_string(),
        ));

        // Send invalid JSON — should be skipped, not crash the resolver.
        raw_tx
            .send(axum::body::Bytes::from("not valid json at all"))
            .await
            .unwrap();

        // Send a valid event after the invalid one — should still work.
        let event_json = r#"{"doc_iri":"http://example.com/Person/2","change_type":"changed","commit_id":"def456"}"#;
        raw_tx.send(axum::body::Bytes::from(event_json)).await.unwrap();

        let received = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            sub_rx.recv(),
        )
        .await
        .expect("timeout waiting for resolved event after invalid JSON")
        .expect("resolved channel closed");

        let parsed: Value = serde_json::from_slice(&received).unwrap();
        assert_eq!(parsed["doc_iri"], "http://example.com/Person/2");

        drop(raw_tx);
        task.abort();
        broadcast_registry().lock().unwrap().unsubscribe(&resolved_channel, sub_stream_id);
    }

    #[tokio::test]
    async fn cohort_resolver_stops_when_raw_channel_closed() {
        let resolved_channel = "graphql_resolved_test_cohort_3".to_string();
        let (raw_tx, raw_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);

        let task = tokio::spawn(cohort_resolver_task(
            raw_rx,
            resolved_channel.clone(),
            "test/db/local/branch/main".to_string(),
        ));

        // Close the raw channel by dropping the sender.
        drop(raw_tx);

        // The resolver task should exit cleanly.
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            task,
        )
        .await
        .expect("resolver task did not exit after raw channel closed");

        assert!(result.is_ok(), "resolver task should complete without panic");
    }

    // --- ws_subscriber_task tests ---

    #[tokio::test]
    async fn ws_subscriber_wraps_resolved_data_in_next_message() {
        let (resolved_tx, resolved_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
        let (ws_tx, mut ws_rx) = mpsc::channel::<Message>(10);

        let task = tokio::spawn(ws_subscriber_task(
            resolved_rx,
            ws_tx,
            "sub1".to_string(),
            "Person_added".to_string(),
        ));

        // Send resolved JSON data.
        let data = json!({"_id": "http://example.com/Person/1", "name": "Alice"});
        resolved_tx
            .send(axum::body::Bytes::from(data.to_string()))
            .await
            .unwrap();

        let received = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            ws_rx.recv(),
        )
        .await
        .expect("timeout waiting for next message")
        .expect("ws channel closed");

        match received {
            Message::Text(t) => {
                let v: Value = serde_json::from_str(&t).unwrap();
                assert_eq!(v["type"], "next");
                assert_eq!(v["id"], "sub1");
                assert_eq!(v["payload"]["data"]["Person_added"]["_id"], "http://example.com/Person/1");
                assert_eq!(v["payload"]["data"]["Person_added"]["name"], "Alice");
            }
            _ => panic!("expected Text message"),
        }

        drop(resolved_tx);
        task.abort();
    }

    #[tokio::test]
    async fn ws_subscriber_stops_on_ws_close() {
        let (resolved_tx, resolved_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
        let (ws_tx, ws_rx) = mpsc::channel::<Message>(10);

        let task = tokio::spawn(ws_subscriber_task(
            resolved_rx,
            ws_tx,
            "sub2".to_string(),
            "Person_added".to_string(),
        ));

        // Drop the receiver — simulates WebSocket closed.
        drop(ws_rx);

        // Send data — ws_tx.send will fail, task should exit.
        resolved_tx
            .send(axum::body::Bytes::from(r#"{"_id":"doc1"}"#))
            .await
            .unwrap();

        let result = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            task,
        )
        .await
        .expect("subscriber task did not exit after ws closed");

        assert!(result.is_ok(), "subscriber task should complete without panic");
    }

    // --- cohort_resolver → ws_subscriber pipeline test ---

    #[tokio::test]
    async fn pipeline_cohort_resolver_to_ws_subscriber() {
        crate::dispatch::set_tokio_handle(tokio::runtime::Handle::current());
        let resolved_channel = "graphql_resolved_pipeline_test".to_string();
        let (raw_tx, raw_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
        let (ws_tx, mut ws_rx) = mpsc::channel::<Message>(10);

        // Spawn cohort resolver.
        let resolver_task = tokio::spawn(cohort_resolver_task(
            raw_rx,
            resolved_channel.clone(),
            "test/db/local/branch/main".to_string(),
        ));

        // Spawn ws subscriber that listens on the resolved channel.
        // We need an intermediate subscriber to bridge BroadcastRegistry → ws_subscriber.
        let (bridge_tx, bridge_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
        let bridge_stream_id = next_ws_stream_id();
        broadcast_registry().lock().unwrap().subscribe(
            resolved_channel.clone(),
            bridge_stream_id,
            bridge_tx,
            None,
        );

        let subscriber_task = tokio::spawn(ws_subscriber_task(
            bridge_rx,
            ws_tx,
            "pipe-sub-1".to_string(),
            "MyClass_added".to_string(),
        ));

        // Send a GqlEvent through the raw channel.
        let event_json = r#"{"doc_iri":"http://example.com/MyClass/42","change_type":"added","commit_id":"commit-xyz"}"#;
        raw_tx.send(axum::body::Bytes::from(event_json)).await.unwrap();

        // The event should flow: raw → cohort_resolver → BroadcastRegistry → bridge → ws_subscriber → ws_rx
        let received = tokio::time::timeout(
            std::time::Duration::from_secs(3),
            ws_rx.recv(),
        )
        .await
        .expect("timeout waiting for pipeline output")
        .expect("ws channel closed");

        match received {
            Message::Text(t) => {
                let v: Value = serde_json::from_str(&t).unwrap();
                assert_eq!(v["type"], "next");
                assert_eq!(v["id"], "pipe-sub-1");
                assert_eq!(v["payload"]["data"]["MyClass_added"]["doc_iri"], "http://example.com/MyClass/42");
                assert_eq!(v["payload"]["data"]["MyClass_added"]["change_type"], "added");
                assert_eq!(v["payload"]["data"]["MyClass_added"]["commit_id"], "commit-xyz");
            }
            _ => panic!("expected Text message"),
        }

        // Clean up.
        drop(raw_tx);
        resolver_task.abort();
        subscriber_task.abort();
        broadcast_registry().lock().unwrap().unsubscribe(&resolved_channel, bridge_stream_id);
    }

    // --- validate_branch_path oversize test ---

    #[test]
    fn validate_branch_path_oversize_rejected() {
        let long_segment = "a".repeat(1030);
        let path = format!("{}/{}/{}/{}/{}", long_segment, "db", "local", "branch", "main");
        assert!(!validate_branch_path(&path), "path >1024 chars should be rejected");
    }

    // --- read_cgi_response tests ---

    #[tokio::test]
    async fn read_cgi_response_parses_lf_separator() {
        use nix::unistd::pipe;
        use tokio::io::AsyncWriteExt;

        let (read_fd, write_fd) = pipe().unwrap();

        // Write CGI response with \n\n separator.
        let response = "Status: 200 OK\nContent-Type: application/json\n\n{\"cohort_key\":\"test\"}";
        let mut writer = tokio::net::unix::pipe::Sender::from_owned_fd(write_fd).unwrap();
        writer.write_all(response.as_bytes()).await.unwrap();
        drop(writer);

        let mut reader = tokio::net::unix::pipe::Receiver::from_owned_fd(read_fd).unwrap();
        let (status, headers, body) = read_cgi_response(&mut reader).await.unwrap();

        assert_eq!(status, 200);
        assert!(headers.iter().any(|(k, v)| k == "Content-Type" && v == "application/json"));
        let body_str = String::from_utf8_lossy(&body);
        assert!(body_str.contains("cohort_key"));
    }

    #[tokio::test]
    async fn read_cgi_response_parses_crlf_separator() {
        use nix::unistd::pipe;
        use tokio::io::AsyncWriteExt;

        let (read_fd, write_fd) = pipe().unwrap();

        let response = "Status: 401 Unauthorized\r\nContent-Type: text/plain\r\n\r\nAuth failed";
        let mut writer = tokio::net::unix::pipe::Sender::from_owned_fd(write_fd).unwrap();
        writer.write_all(response.as_bytes()).await.unwrap();
        drop(writer);

        let mut reader = tokio::net::unix::pipe::Receiver::from_owned_fd(read_fd).unwrap();
        let (status, _headers, body) = read_cgi_response(&mut reader).await.unwrap();

        assert_eq!(status, 401);
        let body_str = String::from_utf8_lossy(&body);
        assert_eq!(body_str, "Auth failed");
    }

    #[tokio::test]
    async fn read_cgi_response_content_length_truncation() {
        use nix::unistd::pipe;
        use tokio::io::AsyncWriteExt;

        let (read_fd, write_fd) = pipe().unwrap();

        // Body has extra bytes beyond Content-Length — should be truncated.
        let response = "Status: 200\nContent-Length: 5\n\nhelloEXTRA";
        let mut writer = tokio::net::unix::pipe::Sender::from_owned_fd(write_fd).unwrap();
        writer.write_all(response.as_bytes()).await.unwrap();
        drop(writer);

        let mut reader = tokio::net::unix::pipe::Receiver::from_owned_fd(read_fd).unwrap();
        let (status, _headers, body) = read_cgi_response(&mut reader).await.unwrap();

        assert_eq!(status, 200);
        assert_eq!(body.len(), 5);
        assert_eq!(&body, b"hello");
    }

    #[tokio::test]
    async fn read_cgi_response_eof_before_separator_returns_error() {
        use nix::unistd::pipe;
        use tokio::io::AsyncWriteExt;

        let (read_fd, write_fd) = pipe().unwrap();

        // Write data without a header separator, then close.
        let mut writer = tokio::net::unix::pipe::Sender::from_owned_fd(write_fd).unwrap();
        writer.write_all(b"no separator here").await.unwrap();
        drop(writer);

        let mut reader = tokio::net::unix::pipe::Receiver::from_owned_fd(read_fd).unwrap();
        let result = read_cgi_response(&mut reader).await;

        assert!(result.is_err(), "should return error on EOF before separator");
        let err = result.unwrap_err();
        assert!(err.contains("EOF before CGI header separator"), "error should mention EOF, got: {}", err);
    }

    // --- handle_client_message Subscribe flow test (without Prolog) ---

    #[tokio::test]
    async fn subscribe_without_prolog_dispatch_returns_error() {
        // In unit tests, no Prolog dispatch queue is running, so
        // dispatch_subscribe_to_prolog will fail. This verifies that
        // the subscribe flow properly reports the error to the client.
        let (tx, mut rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test/db/local/branch/main".to_string(), "".to_string());
        state.initialized = true;

        let msg = WsClientMessage::Subscribe {
            id: "sub-test".to_string(),
            payload: SubscribePayload {
                query: "subscription { Person_added { _id } }".to_string(),
                variables: None,
                operation_name: None,
            },
        };
        let result = handle_client_message(&mut state, msg).await;

        // Should continue (not close) — the error is sent as a ws error message.
        assert!(matches!(result, HandleResult::Continue));

        // Verify error message was sent.
        let received = rx.recv().await.expect("should receive error message");
        match received {
            Message::Text(t) => {
                let v: Value = serde_json::from_str(&t).unwrap();
                assert_eq!(v["type"], "error");
                assert_eq!(v["id"], "sub-test");
                assert!(v["payload"]["message"].as_str().unwrap().contains("Subscription registration failed"));
            }
            _ => panic!("expected Text message"),
        }

        // No subscription should be recorded in state.
        assert!(state.subscriptions.is_empty());
    }

    // --- handle_client_message Complete flow test ---

    #[tokio::test]
    async fn complete_removes_subscription_and_sends_confirmation() {
        let (tx, mut rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());
        state.initialized = true;

        // Manually insert a fake subscription into state.
        let cohort_key = "cohort(test/db/local/branch/main,Person,added,hash1)".to_string();
        let stream_id = next_ws_stream_id();
        state.subscriptions.insert(
            "sub-1".to_string(),
            (cohort_key.clone(), stream_id, "Person_added".to_string()),
        );

        let msg = WsClientMessage::Complete { id: "sub-1".to_string() };
        let result = handle_client_message(&mut state, msg).await;

        assert!(matches!(result, HandleResult::Continue));
        assert!(state.subscriptions.is_empty(), "subscription should be removed from state");

        // Verify complete confirmation was sent.
        let received = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            rx.recv(),
        )
        .await
        .expect("timeout waiting for complete message")
        .expect("ws channel closed");

        match received {
            Message::Text(t) => {
                let v: Value = serde_json::from_str(&t).unwrap();
                assert_eq!(v["type"], "complete");
                assert_eq!(v["id"], "sub-1");
            }
            _ => panic!("expected Text message"),
        }
    }

    #[tokio::test]
    async fn complete_unknown_id_is_noop() {
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());
        state.initialized = true;

        let msg = WsClientMessage::Complete { id: "nonexistent".to_string() };
        let result = handle_client_message(&mut state, msg).await;

        assert!(matches!(result, HandleResult::Continue));
        assert!(state.subscriptions.is_empty());
    }

    // --- handle_client_message Ping/Pong test ---

    #[tokio::test]
    async fn ping_responds_with_pong() {
        let (tx, mut rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());

        let msg = WsClientMessage::Ping { payload: None };
        let result = handle_client_message(&mut state, msg).await;

        assert!(matches!(result, HandleResult::Continue));

        let received = rx.recv().await.expect("should receive pong");
        match received {
            Message::Text(t) => {
                let v: Value = serde_json::from_str(&t).unwrap();
                assert_eq!(v["type"], "pong");
            }
            _ => panic!("expected Text message"),
        }
    }

    // --- handle_client_message Subscribe without init test ---

    #[tokio::test]
    async fn subscribe_without_init_closes_with_4401() {
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());
        // state.initialized is false by default

        let msg = WsClientMessage::Subscribe {
            id: "sub-no-init".to_string(),
            payload: SubscribePayload {
                query: "subscription { Person_added { _id } }".to_string(),
                variables: None,
                operation_name: None,
            },
        };
        let result = handle_client_message(&mut state, msg).await;

        match result {
            HandleResult::Close(code, _) => assert_eq!(code, 4401),
            _ => panic!("expected Close(4401) for subscribe without init"),
        }
    }

    // --- rate limit enforcement test ---

    #[tokio::test]
    async fn rate_limit_blocks_excess_subscriptions() {
        let (tx, _rx) = mpsc::channel::<Message>(10);
        let mut state = WsConnectionState::new(tx, "test".to_string(), "".to_string());
        state.initialized = true;

        // Exhaust the rate limit.
        for _ in 0..RATE_LIMIT_MAX {
            assert!(state.check_rate_limit());
        }

        // Next call should be rejected.
        assert!(!state.check_rate_limit());
    }
}
