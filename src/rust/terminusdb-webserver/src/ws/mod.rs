//! WebSocket handler for GraphQL subscriptions.
//!
//! Implements the `graphql-transport-ws` protocol for real-time GraphQL
//! subscription event delivery over WebSocket.

use axum::{
    extract::{
        ws::{Message, WebSocketUpgrade},
        Path,
    },
    http::StatusCode,
    response::IntoResponse,
};
use serde::Deserialize;
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};
use futures::{SinkExt, StreamExt};
use tokio::sync::mpsc;

use crate::dispatch::{broadcast_registry, StreamId};

/// Maximum number of active subscriptions per WebSocket connection.
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
}

impl WsConnectionState {
    pub fn new(ws_tx: mpsc::Sender<Message>, branch_path: String) -> Self {
        Self {
            initialized: false,
            subscriptions: HashMap::new(),
            subscription_timestamps: VecDeque::new(),
            ws_tx,
            branch_path,
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

        // TODO: Look up SubscriptionResolveContext from SUBSCRIPTION_LAYER_CACHE
        // keyed by (branch_path, event.commit_id).
        // TODO: Resolve document IRI to subject ID.
        // TODO: Check filter match using run_filter_query.
        // TODO: Resolve selection set via execute_validated_query.
        // For now, broadcast a placeholder event so the pipeline is testable.
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
        WsClientMessage::ConnectionInit { payload: _ } => {
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
            state.initialized = true;
            let _ = state
                .ws_tx
                .send(Message::Text(msg_connection_ack().into()))
                .await;
            HandleResult::Continue
        }
        WsClientMessage::Subscribe { id, payload: _payload } => {
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

            // TODO: Dispatch one-shot to Prolog via pipe dispatch.
            // Prolog will call '$graphql:parse_subscription_query/3' (Rust FFI)
            // to parse the query, then register_subscription_parsed/7 for
            // cohort bookkeeping, and return the cohort key, raw channel,
            // and field name.
            // For now, use placeholder values.
            let field_name = String::new();
            let cohort_key = format!("placeholder_{}", state.branch_path);
            let raw_channel = format!("graphql_raw_{}", cohort_key);
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
) -> impl IntoResponse {
    if !validate_branch_path(&path) {
        return (StatusCode::BAD_REQUEST, "Invalid branch path").into_response();
    }
    // Only accept the `graphql-transport-ws` subprotocol.
    ws_upgrade
        .protocols(["graphql-transport-ws"])
        .on_upgrade(move |socket| async move {
            run_ws_connection(socket, path).await;
        })
        .into_response()
}

/// Main WebSocket connection loop.
///
/// Splits the WebSocket into sender and receiver, creates an outbound message
/// channel, spawns a writer task, and runs the message loop on the receiver.
async fn run_ws_connection(socket: axum::extract::ws::WebSocket, branch_path: String) {
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

    let mut state = WsConnectionState::new(outbound_tx, branch_path);

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
        let mut state = WsConnectionState::new(tx, "test".to_string());

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
        let mut state = WsConnectionState::new(tx, "test".to_string());

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
        let mut state = WsConnectionState::new(tx, "test".to_string());
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
}
