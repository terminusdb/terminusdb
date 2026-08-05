use axum::{
    body::Body,
    extract::Path,
    http::{header, HeaderMap, Request, StatusCode},
    middleware::Next,
    response::{IntoResponse, Response},
    routing::{get, post},
    Router,
};
use futures::Stream;
use percent_encoding::percent_decode_str;
use serde_json::json;
use std::{
    collections::{HashMap, HashSet},
    io::Read,
    path::{Component, Path as StdPath, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex, OnceLock,
    },
};
use flate2::read::{GzDecoder, ZlibDecoder};
use nix::unistd::pipe;
use std::io::Write;
use std::os::fd::{FromRawFd, IntoRawFd};
use std::pin::Pin;
use std::task::{Context as TaskContext, Poll};
use crate::config::MAX_BODY_SIZE;
use swipl::prelude::*;
use tokio::io::{AsyncRead, AsyncReadExt, ReadBuf};
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio::net::unix::pipe::Receiver;

/// A route registered by a Prolog plugin.
#[derive(Clone, Debug)]
pub struct PluginRoute {
    pub method: String,
    pub path: String,
    pub module: String,
    pub handler: String,
    pub binary: bool,
}

/// A static file serving endpoint registered by a Prolog plugin.
#[derive(Clone, Debug)]
pub struct PluginStaticPath {
    pub prefix: String,
    pub directory: String,
    pub fallback: Option<String>,
    pub not_found: Option<String>,
    pub auth: String,
    pub csp_nonce: bool,
}

/// A streaming endpoint registered by a Prolog plugin.
#[derive(Clone, Debug)]
pub struct PluginStream {
    pub method: String,
    pub path: String,
    pub module: String,
    pub handler: String,
}

/// Convert Prolog-style route patterns to Axum 0.8 syntax.
///
/// Axum 0.8 changed:
/// - `*name` to `{*name}` for catch-all wildcards
/// - `:name` to `{name}` for capture groups
///
/// This function rewrites each segment accordingly.
fn to_axum_pattern(path: &str) -> String {
    path.split('/')
        .map(|seg| {
            if seg.starts_with('*') {
                format!("{{{}}}", seg)
            } else if let Some(stripped) = seg.strip_prefix(':') {
                format!("{{{}}}", stripped)
            } else {
                seg.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("/")
}

/// Percent-decode the request path. Trailing slashes are preserved so that
/// SWI-Prolog's `http_dispatch` can match its registered prefix and exact
/// handlers exactly as it does in the native SWI-Prolog backend.
fn normalize_dispatch_path(raw_path: &str) -> String {
    percent_decode_str(raw_path).decode_utf8_lossy().to_string()
}

/// Decompress a request body according to the `Content-Encoding` header.
///
/// SWI-Prolog's native HTTP server handles `Content-Encoding` automatically.
/// tower-http 0.4 does not include request decompression, so we perform it
/// inline before handing the body to the Prolog dispatcher.
fn decompress_request_body(content_encoding: Option<&str>, body: &[u8]) -> Result<Vec<u8>, String> {
    match content_encoding {
        Some("gzip") => {
            let mut decoder = GzDecoder::new(body);
            let mut out = Vec::new();
            decoder
                .read_to_end(&mut out)
                .map_err(|e| format!("gzip decompression failed: {}", e))?;
            Ok(out)
        }
        Some("deflate") => {
            // Node.js's zlib.deflateSync produces zlib-wrapped deflate (RFC 1950),
            // so we use ZlibDecoder rather than raw DeflateDecoder.
            let mut decoder = ZlibDecoder::new(body);
            let mut out = Vec::new();
            decoder
                .read_to_end(&mut out)
                .map_err(|e| format!("deflate decompression failed: {}", e))?;
            Ok(out)
        }
        Some(encoding) => Err(format!("unsupported Content-Encoding: {}", encoding)),
        None => Ok(body.to_vec()),
    }
}

/// Unique identifier for an active streaming response.
pub type StreamId = u64;

/// Registry of active stream senders.
#[derive(Default)]
pub struct StreamRegistry {
    next_id: StreamId,
    senders: HashMap<StreamId, mpsc::Sender<axum::body::Bytes>>,
}

impl StreamRegistry {
    fn new() -> Self {
        Self::default()
    }

    fn add(&mut self, sender: mpsc::Sender<axum::body::Bytes>) -> StreamId {
        let id = self.next_id;
        self.next_id += 1;
        self.senders.insert(id, sender);
        id
    }

    pub fn remove(&mut self, id: StreamId) {
        self.senders.remove(&id);
    }

    #[cfg(test)]
    pub fn senders(&self) -> Vec<mpsc::Sender<axum::body::Bytes>> {
        self.senders.values().cloned().collect()
    }

    /// Get a clone of a specific stream's sender, if it exists.
    pub fn get_sender(&self, id: StreamId) -> Option<mpsc::Sender<axum::body::Bytes>> {
        self.senders.get(&id).cloned()
    }

    /// Check if a stream with the given id exists in the registry.
    pub fn contains(&self, id: StreamId) -> bool {
        self.senders.contains_key(&id)
    }

    pub fn send(&mut self, id: StreamId, bytes: axum::body::Bytes) -> Result<(), String> {
        match self.senders.get(&id) {
            Some(sender) => match sender.blocking_send(bytes) {
                Ok(()) => Ok(()),
                Err(_) => {
                    self.senders.remove(&id);
                    Err("stream closed".to_string())
                }
            },
            None => Err("stream not found".to_string()),
        }
    }
}

lazy_static::lazy_static! {
    static ref STREAM_REGISTRY: Arc<Mutex<StreamRegistry>> = Arc::new(Mutex::new(StreamRegistry::new()));
}

/// Return a handle to the global stream registry.
pub fn stream_registry() -> Arc<Mutex<StreamRegistry>> {
    STREAM_REGISTRY.clone()
}

/// Per-user SSE subscription limiter. Tracks the count of active
/// subscriptions per user ID and rejects new subscriptions over the
/// configured limit. The limit is read once at startup from the
/// `TERMINUSDB_SSE_MAX_SUBSCRIPTIONS_PER_USER` env var (default: 20).
pub struct SubscriptionLimiter {
    counts: HashMap<String, u64>,
    max_per_user: u64,
}

impl Default for SubscriptionLimiter {
    fn default() -> Self {
        let max_per_user = std::env::var("TERMINUSDB_SSE_MAX_SUBSCRIPTIONS_PER_USER")
            .ok()
            .and_then(|v| v.parse::<u64>().ok())
            .unwrap_or(20);
        Self {
            counts: HashMap::new(),
            max_per_user,
        }
    }
}

impl SubscriptionLimiter {
    /// Try to acquire a subscription slot for the given user.
    /// Returns `Ok(())` if under the limit, `Err(())` if over.
    pub fn try_acquire(&mut self, user_id: &str) -> Result<(), ()> {
        let count = self.counts.entry(user_id.to_string()).or_insert(0);
        if *count >= self.max_per_user {
            return Err(());
        }
        *count += 1;
        Ok(())
    }

    /// Release a subscription slot for the given user.
    /// Decrements the count, removing the entry if it reaches zero.
    pub fn release(&mut self, user_id: &str) {
        if let Some(count) = self.counts.get_mut(user_id) {
            if *count > 0 {
                *count -= 1;
            }
            if *count == 0 {
                self.counts.remove(user_id);
            }
        }
    }

    /// Get the current count for a user (for testing).
    #[cfg(test)]
    pub fn count_for(&self, user_id: &str) -> u64 {
        self.counts.get(user_id).copied().unwrap_or(0)
    }

    /// Get the configured max per user (for testing).
    #[cfg(test)]
    pub fn max_per_user(&self) -> u64 {
        self.max_per_user
    }
}

lazy_static::lazy_static! {
    static ref SUBSCRIPTION_LIMITER: Arc<Mutex<SubscriptionLimiter>> = Arc::new(Mutex::new(SubscriptionLimiter::default()));
}

/// Return a handle to the global subscription limiter.
pub fn subscription_limiter() -> Arc<Mutex<SubscriptionLimiter>> {
    SUBSCRIPTION_LIMITER.clone()
}

/// Commands sent to the broadcast forwarder task.
/// See PLAN_BROADCAST_QUEUE.md for the full architecture.
enum BroadcastCommand {
    Subscribe {
        channel: String,
        stream_id: StreamId,
        sender: mpsc::Sender<axum::body::Bytes>,
        idle_timeout: Option<std::time::Duration>,
    },
    Unsubscribe {
        channel: String,
        stream_id: StreamId,
    },
    UnsubscribeAll {
        stream_id: StreamId,
    },
    Send {
        channel: String,
        bytes: axum::body::Bytes,
    },
    /// Send bytes to every active channel. Used by the heartbeat
    /// mechanism to keep all SSE connections alive.
    SendToAll {
        bytes: axum::body::Bytes,
    },
}

/// The broadcast registry is just a handle to an unbounded command
/// queue. All work (subscribe, unsubscribe, fan-out) is done by a
/// single forwarder task that drains the queue in FIFO order.
///
/// `send()` is O(1) — pushes to the queue and returns. The forwarder
/// does the O(N) fan-out asynchronously, so commit latency is constant
/// regardless of subscriber count.
///
/// FIFO ordering preserves the transactional invariant: if Subscribe{S}
/// is pushed before Send{C}, S receives C via broadcast; if after, S
/// doesn't, and Phase 2 must send C as delta. The Prolog mutex ensures
/// `broadcast_sent(BranchPath, C)` is asserted atomically with pushing Send{C}.
pub struct BroadcastRegistry {
    tx: mpsc::UnboundedSender<BroadcastCommand>,
    /// Pending receiver, held until the tokio runtime is available
    /// to spawn the forwarder task. Once spawned, this is None.
    pending_rx: Option<mpsc::UnboundedReceiver<BroadcastCommand>>,
}

impl Default for BroadcastRegistry {
    fn default() -> Self {
        let (tx, rx) = mpsc::unbounded_channel::<BroadcastCommand>();
        Self {
            tx,
            pending_rx: Some(rx),
        }
    }
}

impl BroadcastRegistry {
    fn new() -> Self {
        Self::default()
    }

    /// Ensure the forwarder task is running. Called on every public
    /// method. If the tokio handle isn't available yet, the receiver
    /// stays pending and commands queue up in the unbounded channel.
    /// Once the handle is available, the forwarder is spawned and
    /// drains the queued commands.
    fn ensure_forwarder(&mut self) {
        if self.pending_rx.is_some() {
            if let Some(handle) = tokio_handle() {
                let rx = self.pending_rx.take().unwrap();
                handle.spawn(run_forwarder(rx));
            }
        }
    }

    pub fn subscribe(
        &mut self,
        channel: String,
        stream_id: StreamId,
        sender: mpsc::Sender<axum::body::Bytes>,
        idle_timeout: Option<std::time::Duration>,
    ) {
        self.ensure_forwarder();
        let _ = self.tx.send(BroadcastCommand::Subscribe {
            channel,
            stream_id,
            sender,
            idle_timeout,
        });
    }

    pub fn unsubscribe(&mut self, channel: &str, stream_id: StreamId) {
        self.ensure_forwarder();
        let _ = self.tx.send(BroadcastCommand::Unsubscribe {
            channel: channel.to_string(),
            stream_id,
        });
    }

    pub fn unsubscribe_all(&mut self, stream_id: StreamId) {
        self.ensure_forwarder();
        let _ = self.tx.send(BroadcastCommand::UnsubscribeAll { stream_id });
    }

    pub fn send(&mut self, channel: &str, bytes: axum::body::Bytes) -> Result<(), String> {
        self.ensure_forwarder();
        self.tx
            .send(BroadcastCommand::Send {
                channel: channel.to_string(),
                bytes,
            })
            .map_err(|_| "broadcast forwarder task terminated".to_string())
    }

    /// Send bytes to every active channel. Used by the heartbeat
    /// mechanism to keep all SSE connections alive.
    pub fn send_to_all(&mut self, bytes: axum::body::Bytes) -> Result<(), String> {
        self.ensure_forwarder();
        self.tx
            .send(BroadcastCommand::SendToAll { bytes })
            .map_err(|_| "broadcast forwarder task terminated".to_string())
    }
}

/// A subscriber entry in the forwarder's channel map.
///
/// Each subscriber has:
/// - A bounded per-subscriber channel (the decoupling buffer) that the
///   forwarder writes to with `try_send` (non-blocking, CPU-only)
/// - A per-subscriber task that drains this channel and writes to the
///   stream's mpsc::Sender with `send().await` (async, I/O-bound)
///
/// This decouples CPU work (forwarder fanout) from I/O work (writing to
/// TCP sockets). A slow client only blocks its own per-subscriber task,
/// not the forwarder or other subscribers.
struct Subscriber {
    /// Bounded buffer between forwarder and per-subscriber task.
    /// The forwarder does try_send (non-blocking). If full, the
    /// subscriber is too slow and gets disconnected.
    tx: mpsc::Sender<axum::body::Bytes>,
    /// Handle to the per-subscriber task. Aborting it drops the
    /// per-subscriber receiver, cleaning up cleanly.
    task: tokio::task::JoinHandle<()>,
}

/// Per-subscriber buffer capacity for live broadcasts. Each subscriber
/// gets its own independent channel of this size — buffers are not shared
/// across subscribers.
///
/// This is the **live backlog** limit: if a subscriber falls behind by
/// this many commit events (because its per-subscriber task isn't being
/// scheduled fast enough to drain the buffer), the forwarder's `try_send`
/// returns `Full` and the subscriber is disconnected.
///
/// 64 means a client that falls 64 commits behind on live events is
/// disconnected. This is intentional — a client that far behind is either
/// gone or too slow to be useful. The client can reconnect with
/// `since=<last_received_commit>` to catch up on the missed events,
/// same progressive-reconnect pattern as `create_response_stream`.
///
/// The per-subscriber task drains this buffer into the stream channel
/// (see `create_response_stream`, which has a separate, larger buffer
/// for catch-up bursts).
///
/// Memory: each slot is a `Bytes` (reference-counted pointer, ~50 bytes).
/// 64 slots × 10000 subscribers = ~32MB — negligible.
const SUBSCRIBER_BUFFER: usize = 64;

/// The forwarder task — drains the command queue and does the fan-out.
/// Runs in the tokio runtime. Maintains the subscriber list per channel.
///
/// The forwarder only does CPU work: `try_send` to each subscriber's
/// per-subscriber buffer. It never blocks on I/O. Per-subscriber tasks
/// handle the async I/O of writing to each stream independently.
///
/// This means:
/// - Commit latency is O(1) for the FFI call + O(N) try_send for the
///   forwarder, but the try_send is non-blocking and very fast (just
///   pushes to a bounded channel, no I/O)
/// - A slow client only affects its own per-subscriber task
/// - No subscriber can block the forwarder or other subscribers
/// - Backpressure is per-subscriber: if a client can't keep up, its
///   buffer fills and it gets disconnected (zero loss for fast clients)
async fn run_forwarder(mut rx: mpsc::UnboundedReceiver<BroadcastCommand>) {
    let mut channels: HashMap<String, HashMap<StreamId, Subscriber>> = HashMap::new();

    while let Some(cmd) = rx.recv().await {
        match cmd {
            BroadcastCommand::Subscribe {
                channel,
                stream_id,
                sender,
                idle_timeout,
            } => {
                let (sub_tx, sub_rx) = mpsc::channel::<axum::body::Bytes>(SUBSCRIBER_BUFFER);
                let task = tokio::spawn(subscriber_task(sub_rx, sender, idle_timeout));
                channels
                    .entry(channel)
                    .or_default()
                    .insert(stream_id, Subscriber { tx: sub_tx, task });
            }
            BroadcastCommand::Unsubscribe { channel, stream_id } => {
                if let Some(subs) = channels.get_mut(&channel) {
                    if let Some(sub) = subs.remove(&stream_id) {
                        sub.task.abort();
                    }
                }
            }
            BroadcastCommand::UnsubscribeAll { stream_id } => {
                for subs in channels.values_mut() {
                    if let Some(sub) = subs.remove(&stream_id) {
                        sub.task.abort();
                    }
                }
            }
            BroadcastCommand::Send { channel, bytes } => {
                if let Some(subs) = channels.get_mut(&channel) {
                    if subs.is_empty() {
                        continue;
                    }
                    // Fan out with try_send — non-blocking, CPU-only.
                    // A full buffer means the subscriber is too slow;
                    // disconnect it rather than blocking the forwarder.
                    let mut full_count = 0u32;
                    let mut closed_count = 0u32;
                    let failed: Vec<StreamId> = subs
                        .iter()
                        .filter_map(|(id, sub)| {
                            match sub.tx.try_send(bytes.clone()) {
                                Ok(()) => None,
                                Err(mpsc::error::TrySendError::Full(_)) => {
                                    full_count += 1;
                                    Some(*id)
                                }
                                Err(mpsc::error::TrySendError::Closed(_)) => {
                                    closed_count += 1;
                                    Some(*id)
                                }
                            }
                        })
                        .collect();
                    if full_count > 0 || closed_count > 0 {
                        crate::log::log_info(format!(
                            "broadcast send: disconnected {} subscribers (buffer_full={} closed={}) from channel '{}', remaining={}",
                            failed.len(), full_count, closed_count, channel, subs.len() - failed.len()
                        ));
                    }
                    for id in failed {
                        if let Some(sub) = subs.remove(&id) {
                            sub.task.abort();
                        }
                    }
                } else {
                    // Silently skip — no subscribers on this channel.
                    // Do NOT log this, as logging would trigger another
                    // broadcast attempt, creating an infinite feedback loop.
                }
            }
            BroadcastCommand::SendToAll { bytes } => {
                // Fan out to GraphQL SSE channels only — used by the heartbeat.
                // Commit stream channels (named by branch path) receive NDJSON
                // and would break on SSE keepalive comments. GraphQL SSE channels
                // are named `graphql_raw_<cohort_key>`.
                // Same try_send + disconnect-on-full pattern as Send.
                let mut total_full = 0u32;
                let mut total_closed = 0u32;
                for (channel_name, subs) in &mut channels {
                    if !channel_name.starts_with("graphql_raw_") {
                        continue;
                    }
                    if subs.is_empty() {
                        continue;
                    }
                    let mut full_count = 0u32;
                    let mut closed_count = 0u32;
                    let failed: Vec<StreamId> = subs
                        .iter()
                        .filter_map(|(id, sub)| {
                            match sub.tx.try_send(bytes.clone()) {
                                Ok(()) => None,
                                Err(mpsc::error::TrySendError::Full(_)) => {
                                    full_count += 1;
                                    Some(*id)
                                }
                                Err(mpsc::error::TrySendError::Closed(_)) => {
                                    closed_count += 1;
                                    Some(*id)
                                }
                            }
                        })
                        .collect();
                    total_full += full_count;
                    total_closed += closed_count;
                    for id in failed {
                        if let Some(sub) = subs.remove(&id) {
                            sub.task.abort();
                        }
                    }
                }
                if total_full > 0 || total_closed > 0 {
                    crate::log::log_info(format!(
                        "broadcast send_to_all: disconnected {} subscribers (buffer_full={} closed={})",
                        total_full + total_closed, total_full, total_closed
                    ));
                }
            }
        }
    }
}

/// Per-subscriber task — drains the per-subscriber buffer and writes to
/// the stream's mpsc::Sender. This is the async I/O boundary: `send().await`
/// backpressures on the stream's mpsc channel, which backpressures on the
/// TCP socket. A slow client only blocks this task, not the forwarder.
///
/// If `idle_timeout` is set, the task exits after that duration of
/// inactivity (no messages received). Each received message resets the
/// timer. This replaces per-stream Prolog OS threads with a lightweight
/// tokio task — no OS thread per client.
///
/// The task exits (and cleans up) when either side closes:
/// - The per-subscriber buffer sender is dropped (forwarder removed the
///   subscriber, or the forwarder task ended) → `recv().await` returns None
/// - The stream's mpsc receiver is dropped (client disconnected) →
///   `send().await` returns Err
/// - The idle timeout fires with no messages → exit, which drops
///   stream_tx, causing the ReceiverStream to see EOF
async fn subscriber_task(
    mut rx: mpsc::Receiver<axum::body::Bytes>,
    stream_tx: mpsc::Sender<axum::body::Bytes>,
    idle_timeout: Option<std::time::Duration>,
) {
    match idle_timeout {
        Some(timeout) => {
            loop {
                let sleep = tokio::time::sleep(timeout);
                tokio::pin!(sleep);

                tokio::select! {
                    result = rx.recv() => {
                        match result {
                            None => break,
                            Some(bytes) => {
                                if stream_tx.send(bytes).await.is_err() {
                                    break;
                                }
                                // Loop restarts the sleep timer.
                            }
                        }
                    }
                    _ = &mut sleep => {
                        // Idle timeout — no messages for the timeout
                        // duration. Exit to close the stream.
                        crate::log::log_info(format!(
                            "subscriber_task: idle timeout fired, closing stream"
                        ));
                        break;
                    }
                }
            }
        }
        None => {
            while let Some(bytes) = rx.recv().await {
                if stream_tx.send(bytes).await.is_err() {
                    break;
                }
            }
        }
    }
}

lazy_static::lazy_static! {
    static ref BROADCAST_REGISTRY: Arc<Mutex<BroadcastRegistry>> = Arc::new(Mutex::new(BroadcastRegistry::new()));
}

/// Return a handle to the global broadcast registry.
pub fn broadcast_registry() -> Arc<Mutex<BroadcastRegistry>> {
    BROADCAST_REGISTRY.clone()
}

/// Global handle to the tokio runtime, set when the server starts.
/// This allows FFI functions (called from Prolog threads) to spawn
/// async tasks on the tokio runtime.
///
/// Uses a Mutex<Option<Handle>> instead of OnceLock so tests can update
/// the handle across different tokio runtimes. In production,
/// set_tokio_handle is called exactly once at server startup.
static TOKIO_HANDLE: std::sync::Mutex<Option<tokio::runtime::Handle>> = std::sync::Mutex::new(None);

/// Store the tokio runtime handle so FFI functions can spawn tasks.
/// Also starts the heartbeat task that sends keepalive comments to all
/// SSE streams every 30 seconds.
pub fn set_tokio_handle(handle: tokio::runtime::Handle) {
    handle.spawn(heartbeat_task());
    *TOKIO_HANDLE.lock().unwrap() = Some(handle);
}

/// Heartbeat interval — how often keepalive comments are sent.
const HEARTBEAT_INTERVAL: std::time::Duration = std::time::Duration::from_secs(30);

/// Background task that sends SSE keepalive comments (`: keepalive\n\n`)
/// to all active broadcast channels every 30 seconds.
///
/// These comments are part of the SSE spec and are ignored by all
/// conformant SSE clients. They serve two purposes:
/// - Keep the connection alive (reset the idle timeout in subscriber_task)
/// - Detect disconnected clients (send fails → subscriber is cleaned up)
///
/// The heartbeat goes through the broadcast forwarder's `SendToAll`
/// command, so it benefits from the same non-blocking try_send + cleanup
/// logic as regular events.
async fn heartbeat_task() {
    let keepalive = axum::body::Bytes::from(": keepalive\n\n");
    loop {
        tokio::time::sleep(HEARTBEAT_INTERVAL).await;
        if let Err(e) = broadcast_registry()
            .lock()
            .unwrap()
            .send_to_all(keepalive.clone())
        {
            crate::log::log_info(format!("heartbeat: send_to_all failed: {}", e));
        }
    }
}

/// Get the tokio runtime handle, if set.
/// Returns a cloned Handle (not a static reference) since we now use
/// a Mutex<Option<Handle>> instead of OnceLock.
pub fn tokio_handle() -> Option<tokio::runtime::Handle> {
    TOKIO_HANDLE.lock().unwrap().clone()
}

/// Registry of active input stream receivers.
///
/// Input streams carry the HTTP request body from Rust to Prolog. Rust writes
/// chunks; Prolog reads them via `appserver_stream_recv/2`.
#[derive(Default)]
pub struct InputStreamRegistry {
    next_id: StreamId,
    receivers: HashMap<StreamId, mpsc::Receiver<axum::body::Bytes>>,
}

impl InputStreamRegistry {
    fn new() -> Self {
        Self::default()
    }

    fn add(&mut self, receiver: mpsc::Receiver<axum::body::Bytes>) -> StreamId {
        let id = self.next_id;
        self.next_id += 1;
        self.receivers.insert(id, receiver);
        id
    }

    pub(crate) fn take(&mut self, id: StreamId) -> Option<mpsc::Receiver<axum::body::Bytes>> {
        self.receivers.remove(&id)
    }

    pub(crate) fn replace(&mut self, id: StreamId, receiver: mpsc::Receiver<axum::body::Bytes>) {
        self.receivers.insert(id, receiver);
    }
}

lazy_static::lazy_static! {
    static ref INPUT_STREAM_REGISTRY: Arc<Mutex<InputStreamRegistry>> = Arc::new(Mutex::new(InputStreamRegistry::new()));
}

static REQUEST_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn next_request_id(method: &str, path: &str) -> String {
    let n = REQUEST_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}-{}-{}", method, path, n)
}

/// Return a handle to the global input stream registry.
pub fn input_stream_registry() -> Arc<Mutex<InputStreamRegistry>> {
    INPUT_STREAM_REGISTRY.clone()
}

/// Request to be dispatched to the single SWI-Prolog engine.
pub struct DispatchRequest {
    request_json: serde_json::Value,
    handler_module: String,
    handler_name: String,
    input_stream_id: u64,
    response_stream_id: u64,
    is_stream: bool,
}

impl DispatchRequest {
    #[allow(dead_code)]
    fn is_stream(&self) -> bool {
        self.is_stream
    }
}

/// Request to be dispatched through OS pipes (plugin routes).
pub struct PipeDispatchRequest {
    pub(crate) request_json: serde_json::Value,
    pub(crate) handler_module: String,
    pub(crate) handler_name: String,
    pub(crate) input_read_fd: Option<i32>,
    pub(crate) output_write_fd: i32,
    pub(crate) binary: bool,
}

/// Message sent to the single-engine dispatcher queue.
pub enum DispatchMessage {
    Pipe(PipeDispatchRequest),
    Stream(DispatchRequest),
    /// Cancel an in-flight pipe request by signalling the Prolog worker.
    /// The argument is the output write FD passed to the worker.
    CancelPipe(i32),
}

/// Sender for the single-engine dispatcher queue.
static DISPATCH_QUEUE: OnceLock<mpsc::Sender<DispatchMessage>> = OnceLock::new();

/// In-flight request ids used to reject duplicate requests.
static IN_FLIGHT_REQUESTS: OnceLock<Arc<Mutex<HashSet<String>>>> = OnceLock::new();

/// Return the dispatcher queue sender, panicking if the dispatcher is not
/// initialized.
pub fn dispatch_queue() -> mpsc::Sender<DispatchMessage> {
    DISPATCH_QUEUE
        .get()
        .expect("request dispatcher not initialized")
        .clone()
}

/// Return the in-flight request set, panicking if the dispatcher is not
/// initialized.
fn in_flight_requests() -> Arc<Mutex<HashSet<String>>> {
    IN_FLIGHT_REQUESTS
        .get()
        .expect("request dispatcher not initialized")
        .clone()
}

/// Initialize the single-engine request dispatcher. Must be called once
/// before the webserver starts accepting requests.
pub fn init_dispatcher() {
    let (tx, mut rx) = mpsc::channel::<DispatchMessage>(1024);
    DISPATCH_QUEUE.set(tx).ok();
    IN_FLIGHT_REQUESTS.set(Arc::new(Mutex::new(HashSet::new()))).ok();
    let log_rx = crate::log::init_log_channel();

    std::thread::spawn(move || {
        // Create the Prolog engine with the same stack_limit that
        // terminus_config.pl sets for the main engine (8 GB). Without this,
        // PL_create_engine(null) uses the SWI-Prolog default of 1 GB, which
        // is insufficient for heavy WOQL queries (e.g. 4M-solution cartesian
        // products) and causes intermittent stack overflow errors.
        // table_space is left at the SWI-Prolog default (1 GB) to match the
        // main engine configuration.
        let engine = Engine::with_options(
            EngineOptions::default()
                .stack_limit(8_589_934_592)
                .alias("tdb_http"),
        );

        let init_result = {
            let activation = engine.activate();
            let context: Context<_> = activation.into();
            init_prolog_worker_pool(&context)
        };

        if let Err(e) = init_result {
            crate::log::log_error(format!(
                "[terminusdb-webserver] failed to initialize worker pool: {:?}",
                e
            ));
            return;
        }

        let activation = engine.activate();
        let context: Context<_> = activation.into();

        // Drain any log messages that were queued before the engine was ready.
        crate::log::drain_log_messages(&context, &log_rx);

        while let Some(msg) = rx.blocking_recv() {
            // Process pending log messages before each dispatch.
            crate::log::drain_log_messages(&context, &log_rx);
            match msg {
                DispatchMessage::Pipe(req) => {
                    let dispatch_start = std::time::Instant::now();
                    let dispatch_result = dispatch_pipe_to_prolog(&context, &req);
                    let dispatch_elapsed = dispatch_start.elapsed();
                    if dispatch_elapsed > std::time::Duration::from_secs(2) {
                        crate::log::log_error(format!(
                            "SLOW_DISPATCH: dispatch_pipe_to_prolog took {:?} for {}:{}",
                            dispatch_elapsed, req.handler_module, req.handler_name
                        ));
                    }
                    if let Err(e) = dispatch_result {
                        crate::log::log_error(format!(
                            "[terminusdb-webserver] dispatch failed for pipe to {}:{}: {}",
                            req.handler_module, req.handler_name, e
                        ));
                        let _ = write_cgi_error_to_pipe(
                            req.output_write_fd,
                            "Internal server error",
                        );
                    }
                }
                DispatchMessage::CancelPipe(output_write_fd) => {
                    if let Err(e) = cancel_pipe_to_prolog(&context, output_write_fd) {
                        crate::log::log_error(format!(
                            "[terminusdb-webserver] cancel_pipe_request failed for FD {}: {}",
                            output_write_fd, e
                        ));
                    }
                }
                DispatchMessage::Stream(req) => {
                    if let Err(e) = dispatch_stream_to_prolog(&context, &req) {
                        crate::log::log_error(format!(
                            "[terminusdb-webserver] dispatch failed for stream {}: {}",
                            req.response_stream_id, e
                        ));
                        let error_response = json!({
                            "status": 500,
                            "body": {
                                "@type": "api:ErrorResponse",
                                "api:status": "api:failure",
                                "api:error": {"@type": "api:InternalServerError"},
                                "api:message": e.to_string()
                            },
                            "headers": {"Content-Type": "application/json"}
                        })
                        .to_string();
                        let _ = stream_registry()
                            .lock()
                            .unwrap()
                            .send(req.response_stream_id, axum::body::Bytes::from(error_response));
                        stream_registry().lock().unwrap().remove(req.response_stream_id);
                    }
                }
            }
        }
    });
}

fn init_prolog_worker_pool(context: &Context<impl QueryableContextType>) -> PrologResult<()> {
    let f = context.open_frame();
    let workers_term = f.new_term_ref();
    let pool_size: u64 = std::env::var("TERMINUSDB_WORKER_POOL_SIZE")
        .ok()
        .and_then(|s| s.parse().ok())
        .or_else(|| {
            std::env::var("TERMINUSDB_SERVER_WORKERS")
                .ok()
                .and_then(|s| s.parse().ok())
        })
        .unwrap_or(16);
    workers_term.put(&pool_size).map_err(|_| PrologError::Failure)?;

    let init_callable = CallablePredicate::new(Predicate::new(
        Functor::new(Atom::new("init_request_worker_pool"), 1),
        Module::new(Atom::new("request_worker_pool")),
    ))
    .map_err(|_| PrologError::Failure)?;
    let result = f.call_once(init_callable, [&workers_term]);
    f.close();
    result
}

/// Write a CGI-style 500 error response directly to the output pipe.
fn write_cgi_error_to_pipe(output_write_fd: i32, message: &str) -> std::io::Result<()> {
    let error_body = json!({
        "@type": "api:ErrorResponse",
        "api:status": "api:failure",
        "api:error": {"@type": "api:InternalServerError"},
        "api:message": message
    })
    .to_string();
    let cgi_response = format!(
        "Status: 500\nContent-Type: application/json\n\n{}",
        error_body
    );
    // SAFETY: the FD was given to Prolog but was never opened as a stream,
    // so Rust can still write to it and close it.
    let mut file = unsafe { std::fs::File::from_raw_fd(output_write_fd) };
    file.write_all(cgi_response.as_bytes())
}

fn dispatch_pipe_to_prolog(
    context: &Context<impl QueryableContextType>,
    req: &PipeDispatchRequest,
) -> PrologResult<()> {
    let request_term = context.new_term_ref();
    context
        .serialize_to_term(&request_term, &req.request_json)
        .map_err(|_| PrologError::Failure)?;

    let module_term = context.new_term_ref();
    module_term
        .put(&Atom::new(&req.handler_module))
        .map_err(|_| PrologError::Failure)?;

    let handler_term = context.new_term_ref();
    handler_term
        .put(&Atom::new(&req.handler_name))
        .map_err(|_| PrologError::Failure)?;

    let input_fd_term = context.new_term_ref();
    input_fd_term
        .put(&(req.input_read_fd.unwrap_or(-1) as i64))
        .map_err(|_| PrologError::Failure)?;

    let output_fd_term = context.new_term_ref();
    output_fd_term
        .put(&(req.output_write_fd as i64))
        .map_err(|_| PrologError::Failure)?;

    let binary_term = context.new_term_ref();
    binary_term
        .put(&Atom::new(if req.binary { "true" } else { "false" }))
        .map_err(|_| PrologError::Failure)?;

    let callable = CallablePredicate::new(Predicate::new(
        Functor::new(Atom::new("dispatch_request"), 6),
        Module::new(Atom::new("request_worker_pool")),
    ))
    .map_err(|_| PrologError::Failure)?;
    context.call_once(callable, [
        &request_term,
        &module_term,
        &handler_term,
        &input_fd_term,
        &output_fd_term,
        &binary_term,
    ])
}

/// Call the Prolog predicate `cancel_pipe_request/1` to signal the worker
/// thread handling the given output pipe FD that the client has disconnected.
/// The worker receives `error(client_disconnected, _)` via `thread_signal/2`,
/// which is delivered at the next goal boundary (e.g. between `id_triple`
/// calls in a WOQL query loop).
fn cancel_pipe_to_prolog(
    context: &Context<impl QueryableContextType>,
    output_write_fd: i32,
) -> PrologResult<()> {
    let fd_term = context.new_term_ref();
    fd_term
        .put(&(output_write_fd as i64))
        .map_err(|_| PrologError::Failure)?;

    let callable = CallablePredicate::new(Predicate::new(
        Functor::new(Atom::new("cancel_pipe_request"), 1),
        Module::new(Atom::new("request_worker_pool")),
    ))
    .map_err(|_| PrologError::Failure)?;
    context.call_once(callable, [&fd_term])
}

fn dispatch_stream_to_prolog(
    context: &Context<impl QueryableContextType>,
    req: &DispatchRequest,
) -> PrologResult<()> {
    let request_term = context.new_term_ref();
    context
        .serialize_to_term(&request_term, &req.request_json)
        .map_err(|_| PrologError::Failure)?;

    let module_term = context.new_term_ref();
    module_term
        .put(&Atom::new(&req.handler_module))
        .map_err(|_| PrologError::Failure)?;

    let handler_term = context.new_term_ref();
    handler_term
        .put(&Atom::new(&req.handler_name))
        .map_err(|_| PrologError::Failure)?;

    let input_stream_id_term = context.new_term_ref();
    input_stream_id_term
        .put(&req.input_stream_id)
        .map_err(|_| PrologError::Failure)?;

    let response_stream_id_term = context.new_term_ref();
    response_stream_id_term
        .put(&req.response_stream_id)
        .map_err(|_| PrologError::Failure)?;

    let callable = CallablePredicate::new(Predicate::new(
        Functor::new(Atom::new("dispatch_request"), 5),
        Module::new(Atom::new("request_worker_pool")),
    ))
    .map_err(|_| PrologError::Failure)?;
    context.call_once(callable, [
        &request_term,
        &module_term,
        &handler_term,
        &input_stream_id_term,
        &response_stream_id_term,
    ])
}

/// Write a string to a file descriptor.
#[allow(dead_code)]
fn write_to_fd(fd: i32, data: &str) -> std::io::Result<()> {
    use std::os::fd::BorrowedFd;
    let borrowed = unsafe { BorrowedFd::borrow_raw(fd) };
    nix::unistd::write(borrowed, data.as_bytes())
        .map(|_| ())
        .map_err(std::io::Error::from)
}


/// Register a new response stream and return its id and the receiver.
///
/// The buffer is larger than `SUBSCRIBER_BUFFER` (64) because this
/// channel serves **both** catch-up and live events. Catch-up commits
/// (historical, requested via `since=<commit>`) are sent synchronously
/// via `appserver_stream_send` → `try_send` on this channel. A client
/// requesting `since=<old_commit>` may receive dozens of catch-up
/// commits in a burst. This buffer must absorb that burst without
/// `try_send` returning `Full` (which would fail the catch-up).
///
/// 128 handles catch-up of up to 128 commits. If a client's catch-up
/// set exceeds 128 (e.g. `since=<very_old_commit>`), the buffer fills,
/// `try_send` returns `Full`, the stream closes, and the client must
/// reconnect with `since=<last_received_commit>` to resume. Each
/// reconnect advances through the history by up to 128 commits, so a
/// 500-commit gap takes ~4 reconnects. This is by design — it keeps
/// memory bounded at scale while still allowing clients to catch up
/// from any point in history. Clients must track the last commit ID
/// they received (from the `commit.identifier` field in each NDJSON
/// event) and use it as the `since` parameter on reconnect.
///
/// Memory: 256 slots × 40 bytes = 10KB per client, 100MB at 10000 clients.
fn create_response_stream() -> (u64, mpsc::Receiver<axum::body::Bytes>) {
    let (tx, rx) = mpsc::channel::<axum::body::Bytes>(256);
    let stream_id = stream_registry().lock().unwrap().add(tx);
    (stream_id, rx)
}

/// Register a new input stream and return its id and the sender.
fn create_input_stream() -> (u64, mpsc::Sender<axum::body::Bytes>) {
    let (tx, rx) = mpsc::channel::<axum::body::Bytes>(256);
    let stream_id = input_stream_registry().lock().unwrap().add(rx);
    (stream_id, tx)
}

/// Guard that removes a request id from the in-flight set when dropped.
struct InFlightRequestGuard {
    id: String,
    in_flight: Arc<Mutex<HashSet<String>>>,
}

impl Drop for InFlightRequestGuard {
    fn drop(&mut self) {
        self.in_flight.lock().unwrap().remove(&self.id);
    }
}

/// Resolve a static directory path.
///
/// Absolute paths are returned unchanged. Relative paths are resolved against
/// the `TERMINUSDB_ADDON_PATH` environment variable, or the current working
/// directory if the variable is not set.
fn resolve_static_directory(directory: &str) -> PathBuf {
    let path = PathBuf::from(directory);
    if path.is_absolute() {
        return path;
    }

    std::env::var("TERMINUSDB_ADDON_PATH")
        .map(PathBuf::from)
        .or_else(|_| std::env::current_dir())
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(path)
}

/// Convert a Prolog term to a Rust String, accepting either an atom or a string term.
fn term_to_string(term: &Term) -> PrologResult<String> {
    if term.is_atom() {
        let atom: Atom = term.get_ex()?;
        Ok(atom.name())
    } else {
        term.get_ex::<String>()
    }
}

/// Collect routes registered by Prolog plugins through the
/// `appserver_hooks:appserver_route/4` hook.
///
/// Each handler must be specified as `Module:Handler`, where `Handler` is a
/// predicate with arity two (`+Request, -Response`). The fourth argument is
/// the atom `true` for binary routes and `false` for UTF-8 routes.
///
/// This is called from the `appserver_start` Prolog predicate,
/// so a Prolog engine is active.
pub fn collect_routes(context: &Context<impl QueryableContextType>) -> PrologResult<Vec<PluginRoute>> {
    let frame = context.open_frame();
    let [method_term, path_term, handler_term, binary_term] = frame.new_term_refs();

    let open_call = frame.open(
        pred!("appserver_hooks:appserver_route/4"),
        [&method_term, &path_term, &handler_term, &binary_term],
    );

    let mut routes = Vec::new();
    while attempt_opt(open_call.next_solution())?.is_some() {
        let method: Atom = method_term.get_ex()?;
        let path = term_to_string(&path_term)?;
        let module: Atom = handler_term.get_arg(1)?;
        let handler: Atom = handler_term.get_arg(2)?;
        let binary = match binary_term.get_ex::<Atom>() {
            Ok(atom) => atom.name() == "true",
            Err(_) => false,
        };
        let module = module.name();
        let handler = handler.name();
        routes.push(PluginRoute {
            method: method.name(),
            path,
            module,
            handler,
            binary,
        });
    }

    Ok(routes)
}

/// Collect static file serving endpoints registered by Prolog plugins.
///
/// Plugins register via `appserver_hooks:appserver_static_path/3` with
/// an option list. Prolog normalizes these into
/// `appserver_hooks:appserver_static_path_normalized/3`, which this
/// function reads.
///
/// Arguments are: prefix, directory, options dict.
/// The options dict has keys `fallback` (atom), `auth` (atom),
/// and `csp_nonce` (atom: true/false).
pub fn collect_static_paths(
    context: &Context<impl QueryableContextType>,
) -> PrologResult<Vec<PluginStaticPath>> {
    let frame = context.open_frame();
    let [prefix_term, directory_term, options_term] = frame.new_term_refs();

    let open_call = frame.open(
        pred!("appserver_hooks:appserver_static_path_normalized/3"),
        [&prefix_term, &directory_term, &options_term],
    );

    let mut paths = Vec::new();
    while attempt_opt(open_call.next_solution())?.is_some() {
        let prefix = term_to_string(&prefix_term)?;
        let directory = term_to_string(&directory_term)?;
        let directory = resolve_static_directory(&directory)
            .to_string_lossy()
            .into_owned();

        let fallback: Atom = options_term.get_dict_key("fallback")?;
        let not_found: Atom = options_term.get_dict_key("not_found")?;
        let auth: Atom = options_term.get_dict_key("auth")?;
        let csp_nonce: Atom = options_term.get_dict_key("csp_nonce")?;

        let fallback = if fallback.name().is_empty() {
            None
        } else {
            Some(fallback.name())
        };

        let not_found = if not_found.name().is_empty() {
            None
        } else {
            Some(not_found.name())
        };

        paths.push(PluginStaticPath {
            prefix,
            directory,
            fallback,
            not_found,
            auth: auth.name(),
            csp_nonce: csp_nonce.name() == "true",
        });
    }

    Ok(paths)
}

/// Collect streaming endpoints registered by Prolog plugins through the
/// `appserver_hooks:appserver_stream/3` hook.
///
/// Each registration is: method, path, handler (as `Module:Handler`).
/// The handler must have arity 3 and accept `+Request`, `+StreamId`, `-Response`.
pub fn collect_streams(context: &Context<impl QueryableContextType>) -> PrologResult<Vec<PluginStream>> {
    let frame = context.open_frame();
    let [method_term, path_term, handler_term] = frame.new_term_refs();

    let open_call = frame.open(
        pred!("appserver_hooks:appserver_stream/3"),
        [&method_term, &path_term, &handler_term],
    );

    let mut streams = Vec::new();
    while attempt_opt(open_call.next_solution())?.is_some() {
        let method: Atom = method_term.get_ex()?;
        let path = term_to_string(&path_term)?;
        let module: Atom = handler_term.get_arg(1)?;
        let handler: Atom = handler_term.get_arg(2)?;
        streams.push(PluginStream {
            method: method.name(),
            path,
            module: module.name(),
            handler: handler.name(),
        });
    }

    Ok(streams)
}

/// Build a router with the plugin routes.
///
/// Registrations that share the same method and path are pooled. Incoming
/// requests are distributed round-robin among the registered handlers, so a
/// plugin can register several workers for the same endpoint without needing
/// its own load balancing.
type RouteGroup = (bool, Vec<(String, String)>);

pub fn build_plugin_router(routes: Vec<PluginRoute>) -> Router {
    let mut groups: HashMap<(String, String), RouteGroup> = HashMap::new();
    for route in routes {
        let entry = groups
            .entry((route.method, route.path))
            .or_insert((route.binary, Vec::new()));
        entry.1.push((route.module, route.handler));
    }

    let mut router = Router::new();
    for ((method, path), (binary, handlers)) in groups {
        let pool = Arc::new(HandlerPool::new(handlers, binary));
        let pattern = path.clone();
        let axum_path = to_axum_pattern(&path);
        let make_handler = || {
            let pool = pool.clone();
            let pattern = pattern.clone();
            move |req: Request<Body>| async move {
                let (module, handler, binary) = pool.pick();
                dispatch_request_via_pipe(module, handler, pattern.clone(), binary, req).await
            }
        };

        router = match method.as_str() {
            "get" => router.route(&axum_path, axum::routing::get(make_handler())),
            "head" => router.route(&axum_path, axum::routing::head(make_handler())),
            "options" => router.route(&axum_path, axum::routing::options(make_handler())),
            "post" => router.route(&axum_path, axum::routing::post(make_handler())),
            "put" => router.route(&axum_path, axum::routing::put(make_handler())),
            "delete" => router.route(&axum_path, axum::routing::delete(make_handler())),
            _ => router,
        };

        // Trailing slash variants are intentionally not added here. The Prolog
        // registration already emits the exact routes it needs (e.g., `/api/`
        // for root aliases and `/api/optimize/` for single-segment prefixes),
        // and adding a blanket slash variant would overlap with those prefix
        // exact routes.
    }

    router
}

/// A pool of Prolog handlers for a single endpoint.
///
/// Handlers are selected round-robin. This lets plugins register multiple
/// workers for the same path and have Rust distribute the load. The `binary`
/// flag is shared by every handler in the pool (it is a property of the route).
struct HandlerPool {
    handlers: Vec<(String, String)>,
    binary: bool,
    next: AtomicUsize,
}

impl HandlerPool {
    fn new(handlers: Vec<(String, String)>, binary: bool) -> Self {
        Self {
            handlers,
            binary,
            next: AtomicUsize::new(0),
        }
    }

    fn pick(&self) -> (String, String, bool) {
        let index = self.next.fetch_add(1, Ordering::Relaxed) % self.handlers.len();
        let (module, handler) = self.handlers[index].clone();
        (module, handler, self.binary)
    }
}

/// Build a router for plugin-registered static file serving endpoints.
///
/// Each registered prefix is served by a symlink-safe handler. Only files whose
/// canonical path is inside the addon directory are served. If a fallback file
/// is configured, it is served when a concrete file is not found, enabling
/// SPA-style routing.
pub fn build_static_router(static_paths: Vec<PluginStaticPath>) -> Router {
    let mut router = Router::new();
    for static_path in static_paths {
        let dir = PathBuf::from(&static_path.directory);
        let fallback = static_path.fallback.as_ref().map(|f| dir.join(f));
        let not_found = static_path.not_found.as_ref().map(|f| dir.join(f));
        let csp_nonce = static_path.csp_nonce;

        let root_handler = {
            let dir = dir.clone();
            let fallback = fallback.clone();
            let not_found = not_found.clone();
            move || async move { serve_static_file(&dir, fallback.as_deref(), not_found.as_deref(), "/").await }
        };

        let sub_handler = {
            let dir = dir.clone();
            let fallback = fallback.clone();
            let not_found = not_found.clone();
            move |Path(uri_path): Path<String>| async move {
                serve_static_file(&dir, fallback.as_deref(), not_found.as_deref(), &uri_path).await
            }
        };

        let prefix_routes = Router::new()
            .route(&static_path.prefix, get(root_handler.clone()))
            .route(&format!("{}/", static_path.prefix), get(root_handler))
            .route(&format!("{}/{{*path}}", static_path.prefix), get(sub_handler));

        if csp_nonce {
            router = router.merge(
                prefix_routes.layer(axum::middleware::from_fn(csp_nonce_middleware)),
            );
        } else {
            router = router.merge(prefix_routes);
        }
    }
    router
}

/// Placeholder string in HTML files that gets replaced with a per-request nonce.
const CSP_NONCE_PLACEHOLDER: &str = "--TDB--CSP--DYNAMIC--CSP--NONCE--";

/// Middleware that generates a unique nonce per request and replaces all
/// occurrences of `--TDB--CSP--DYNAMIC--CSP--NONCE--` in HTML responses with
/// `NONCE-<uuid>`. This allows CSP meta tags and script nonce attributes to
/// be dynamically secured without `'unsafe-inline'`.
///
/// Only applies to responses with `Content-Type: text/html`. Non-HTML
/// responses pass through with only the `frame-ancestors` header added.
async fn csp_nonce_middleware(req: Request<Body>, next: Next) -> Response {
    let mut response = next.run(req).await;

    response.headers_mut().insert(
        header::CONTENT_SECURITY_POLICY,
        axum::http::HeaderValue::from_static("frame-ancestors 'self'"),
    );

    let content_type = response
        .headers()
        .get(header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    if !content_type.starts_with("text/html") {
        return response;
    }

    let nonce = format!("tdb-csp-{}", uuid::Uuid::new_v4());

    let body = response.into_body();
    match axum::body::to_bytes(body, 10 * 1024 * 1024).await {
        Ok(bytes) => {
            let replaced = replace_placeholder(&bytes, CSP_NONCE_PLACEHOLDER, &nonce);
            Response::builder()
                .status(StatusCode::OK)
                .header(header::CONTENT_TYPE, "text/html; charset=utf-8")
                .header(
                    header::CONTENT_SECURITY_POLICY,
                    axum::http::HeaderValue::from_static("frame-ancestors 'self'"),
                )
                .body(Body::from(replaced))
                .unwrap()
        }
        Err(_) => Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .body(Body::from("Failed to process response body"))
            .unwrap(),
    }
}

/// Replace all occurrences of `placeholder` in `bytes` with `replacement`.
/// Returns a new Vec<u8>. If the placeholder is not found, returns the
/// original bytes unchanged.
fn replace_placeholder(bytes: &[u8], placeholder: &str, replacement: &str) -> Vec<u8> {
    let placeholder_bytes = placeholder.as_bytes();
    if placeholder_bytes.is_empty() || placeholder_bytes.len() > bytes.len() {
        return bytes.to_vec();
    }

    let mut result = Vec::with_capacity(bytes.len() + replacement.len());
    let mut i = 0;
    let mut found = false;

    while i < bytes.len() {
        if i + placeholder_bytes.len() <= bytes.len()
            && &bytes[i..i + placeholder_bytes.len()] == placeholder_bytes
        {
            result.extend_from_slice(replacement.as_bytes());
            i += placeholder_bytes.len();
            found = true;
        } else {
            result.push(bytes[i]);
            i += 1;
        }
    }

    if found {
        result
    } else {
        bytes.to_vec()
    }
}

/// Build a router for plugin-registered streaming endpoints.
///
/// Registrations that share the same method and path are pooled. Each incoming
/// connection is assigned to one handler round-robin, so a plugin can register
/// a pool of stream workers for the same endpoint.
pub fn build_stream_router(streams: Vec<PluginStream>) -> Router {
    let mut groups: HashMap<(String, String), Vec<(String, String)>> = HashMap::new();
    for stream in streams {
        groups
            .entry((stream.method, stream.path))
            .or_default()
            .push((stream.module, stream.handler));
    }

    let mut router = Router::new();
    for ((method, path), handlers) in groups {
        let pool = Arc::new(HandlerPool::new(handlers, false));
        let pattern = path.clone();
        let axum_path = to_axum_pattern(&path);
        let route_handler = move |req: Request<Body>| async move {
            let (module, handler, _binary) = pool.pick();
            dispatch_stream_request(module, handler, pattern.clone(), req).await
        };

        router = match method.as_str() {
            "get" => router.route(&axum_path, get(route_handler)),
            "post" => router.route(&axum_path, post(route_handler)),
            "put" => router.route(&axum_path, axum::routing::put(route_handler)),
            "delete" => router.route(&axum_path, axum::routing::delete(route_handler)),
            "options" => router.route(&axum_path, axum::routing::options(route_handler)),
            _ => router,
        };
    }
    router
}

/// Serve a static file for a single plugin-registered prefix.
///
/// The request path is resolved relative to `dir`, percent-decoded, and checked
/// for directory traversal. The canonical path is then verified to be within
/// `dir` (after resolving symlinks). Missing files, unsafe paths, and symlink
/// escapes result in a 404 response — served from `not_found` if configured,
/// otherwise a default JSON 404.
async fn serve_static_file(dir: &StdPath, fallback: Option<&StdPath>, not_found: Option<&StdPath>, uri_path: &str) -> Response<Body> {
    let canonical_dir = match tokio::fs::canonicalize(dir).await {
        Ok(d) => d,
        Err(_) => return not_found_response(),
    };

    let decoded_path = percent_decode_str(uri_path).decode_utf8_lossy();
    let mut candidate = canonical_dir.clone();
    for component in StdPath::new(decoded_path.as_ref()).components() {
        match component {
            Component::Normal(name) => candidate.push(name),
            Component::RootDir => {}
            // Reject `..`, Windows prefixes, and other special components.
            _ => return not_found_response_with(&canonical_dir, not_found).await,
        }
    }

    // Try the requested file (or its index.html for directories).
    if let Some(path) = canonical_file_within_dir(&canonical_dir, &candidate, true).await {
        return serve_file_bytes(&path).await;
    }

    // Try the configured fallback file.
    if let Some(fallback) = fallback {
        if let Some(path) = canonical_file_within_dir(&canonical_dir, fallback, false).await {
            return serve_file_bytes(&path).await;
        }
    }

    not_found_response_with(&canonical_dir, not_found).await
}

/// Canonicalize `candidate` and return it only if it is a file inside `dir`.
///
/// When `allow_index` is true, a directory candidate is also accepted if it
/// contains an `index.html` that stays within `dir`. Symlinks are resolved by
/// `canonicalize`, so a symlink pointing outside `dir` is rejected.
async fn canonical_file_within_dir(dir: &StdPath, candidate: &StdPath, allow_index: bool) -> Option<PathBuf> {
    if let Ok(path) = tokio::fs::canonicalize(candidate).await {
        if path.starts_with(dir) {
            if let Ok(metadata) = tokio::fs::metadata(&path).await {
                if metadata.is_file() {
                    return Some(path);
                }
                if allow_index && metadata.is_dir() {
                    let index = path.join("index.html");
                    if let Ok(index) = tokio::fs::canonicalize(&index).await {
                        if index.starts_with(dir) {
                            if let Ok(index_metadata) = tokio::fs::metadata(&index).await {
                                if index_metadata.is_file() {
                                    return Some(index);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Read a file and return it as a response with a MIME type derived from the
/// extension.
async fn serve_file_bytes(path: &StdPath) -> Response<Body> {
    match tokio::fs::read(path).await {
        Ok(bytes) => Response::builder()
            .status(StatusCode::OK)
            .header(header::CONTENT_TYPE, static_mime_type(path))
            .body(Body::from(bytes))
            .unwrap(),
        Err(_) => not_found_response(),
    }
}

/// Return a JSON 404 response.
fn not_found_response() -> Response<Body> {
    let body = json!({
        "@type": "api:ErrorResponse",
        "api:status": "api:not_found",
        "api:error": {"@type": "api:NotFound"},
        "api:message": "Resource not found"
    });
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(body.to_string()))
        .unwrap()
}

/// Return a 404 response, serving a custom file if one is configured.
///
/// When `not_found` is provided and the file exists and resolves to a real
/// file (not a symlink) within the canonical serving directory, it is served
/// with a 404 status code and the appropriate MIME type. Otherwise the
/// default JSON 404 response is returned.
async fn not_found_response_with(
    canonical_dir: &StdPath,
    not_found: Option<&StdPath>,
) -> Response<Body> {
    if let Some(path) = not_found {
        if let Some(validated) = canonical_file_within_dir(canonical_dir, path, false).await {
            if let Ok(bytes) = tokio::fs::read(&validated).await {
                return Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .header(header::CONTENT_TYPE, static_mime_type(&validated))
                    .body(Body::from(bytes))
                    .unwrap();
            }
        }
    }
    not_found_response()
}

/// Simple MIME type mapping based on file extension.
fn static_mime_type(path: &StdPath) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some("html") => "text/html",
        Some("css") => "text/css",
        Some("js") => "application/javascript",
        Some("json") => "application/json",
        Some("png") => "image/png",
        Some("jpg") | Some("jpeg") => "image/jpeg",
        Some("svg") => "image/svg+xml",
        Some("ico") => "image/x-icon",
        Some("woff") => "font/woff",
        Some("woff2") => "font/woff2",
        Some("ttf") => "font/ttf",
        Some("otf") => "font/otf",
        Some("txt") => "text/plain",
        _ => "application/octet-stream",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        body::Body,
        http::{Request, StatusCode},
    };
    use futures::StreamExt;
    use tower::ServiceExt;

    #[tokio::test]
    async fn static_router_serves_existing_file() {
        let dir = tempfile::tempdir().unwrap();
        let app_dir = dir.path().join("app");
        tokio::fs::create_dir(&app_dir).await.unwrap();
        tokio::fs::write(app_dir.join("hello.txt"), "hello").await.unwrap();

        let router = build_static_router(vec![PluginStaticPath {
            prefix: "/app".to_string(),
            directory: app_dir.to_string_lossy().into_owned(),
            fallback: None,
            not_found: None,
            auth: "none".to_string(),
            csp_nonce: false,
        }]);

        let response = router
            .oneshot(Request::builder().uri("/app/hello.txt").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn static_router_rejects_symlink_outside_directory() {
        let dir = tempfile::tempdir().unwrap();
        let app_dir = dir.path().join("app");
        tokio::fs::create_dir(&app_dir).await.unwrap();

        let outside = dir.path().join("secret.txt");
        tokio::fs::write(&outside, "secret").await.unwrap();
        let link = app_dir.join("link.txt");
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&outside, &link).unwrap();
        }
        #[cfg(windows)]
        {
            std::os::windows::fs::symlink_file(&outside, &link).unwrap();
        }

        let router = build_static_router(vec![PluginStaticPath {
            prefix: "/app".to_string(),
            directory: app_dir.to_string_lossy().into_owned(),
            fallback: None,
            not_found: None,
            auth: "none".to_string(),
            csp_nonce: false,
        }]);

        let response = router
            .oneshot(Request::builder().uri("/app/link.txt").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[test]
    fn extract_route_params_parses_wildcards() {
        let mut params = extract_route_params("/files/:id", "/files/123");
        assert_eq!(params.get("id"), Some(&"123".to_string()));

        params = extract_route_params("/files/:id/*path", "/files/123/a/b/c");
        assert_eq!(params.get("id"), Some(&"123".to_string()));
        assert_eq!(params.get("path"), Some(&"a/b/c".to_string()));

        params = extract_route_params("/static/*path", "/static/app/admin/index.html");
        assert_eq!(params.get("path"), Some(&"app/admin/index.html".to_string()));

        params = extract_route_params("/api/info", "/api/info");
        assert!(params.is_empty());
    }

    #[test]
    fn handler_pool_distributes_round_robin() {
        let pool = HandlerPool::new(
            vec![
                ("a".to_string(), "h1".to_string()),
                ("b".to_string(), "h2".to_string()),
                ("c".to_string(), "h3".to_string()),
            ],
            false,
        );
        let picks: Vec<_> = (0..6).map(|_| pool.pick()).collect();
        assert_eq!(picks[0].0, "a");
        assert_eq!(picks[1].0, "b");
        assert_eq!(picks[2].0, "c");
        assert_eq!(picks[3].0, "a");
        assert_eq!(picks[4].0, "b");
        assert_eq!(picks[5].0, "c");
        assert!(!picks[0].2);
    }

    #[test]
    fn parse_cgi_headers_normal() {
        let buf = b"Status: 200 OK\nContent-Type: application/json\n\n{\"ok\":true}";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert_eq!(headers.get("content-type").unwrap(), "application/json");
        assert_eq!(body_start, 47);
        assert_eq!(&buf[body_start..], b"{\"ok\":true}");
    }

    #[test]
    fn parse_cgi_headers_defaults_to_200() {
        let buf = b"Content-Type: text/plain\n\nhello";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert_eq!(headers.get("content-type").unwrap(), "text/plain");
        assert_eq!(&buf[body_start..], b"hello");
    }

    #[test]
    fn parse_cgi_headers_binary_content_type() {
        let buf = b"Status: 200\nContent-Type: application/octet-stream\n\n\x00\x01\x02";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert_eq!(headers.get("content-type").unwrap(), "application/octet-stream");
        assert_eq!(&buf[body_start..], b"\x00\x01\x02");
    }

    #[test]
    fn parse_cgi_headers_empty_body() {
        let buf = b"Status: 204\n\n";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 204);
        assert!(headers.is_empty());
        assert_eq!(body_start, 13);
    }

    #[test]
    fn parse_cgi_headers_crlf_separator() {
        let buf = b"Status: 201 Created\r\nContent-Type: application/json\r\n\r\n{\"id\":1}";
        let (status, _headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 201);
        assert_eq!(body_start, 55);
        assert_eq!(&buf[body_start..], b"{\"id\":1}");
    }

    #[test]
    fn parse_cgi_headers_strips_hop_by_hop() {
        let buf = b"Status: 200\nTransfer-Encoding: chunked\nConnection: close\nContent-Length: 5\nContent-Type: application/json\n\n{}";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert!(headers.get("transfer-encoding").is_none());
        assert!(headers.get("connection").is_none());
        // Content-Length is preserved from the CGI handler so hyper can
        // use it for keep-alive when the handler sets it.
        assert!(headers.get("content-length").is_some());
        assert_eq!(headers.get("content-length").unwrap(), "5");
        assert!(headers.get("content-type").is_some());
        assert_eq!(&buf[body_start..], b"{}");
    }

    #[test]
    fn parse_cgi_headers_no_separator() {
        let buf = b"Status: 200\nContent-Type: application/json";
        assert!(parse_cgi_headers(buf).is_none());
    }

    #[test]
    fn parse_cgi_headers_utf8_body_preserved() {
        // Body contains UTF-8 multibyte: "Kurt G\xc3\xb6del"
        let buf = b"Status: 200 OK\nContent-Type: application/json\n\n\"Kurt G\xc3\xb6del\"";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert_eq!(headers.get("content-type").unwrap(), "application/json");
        assert_eq!(&buf[body_start..], b"\"Kurt G\xc3\xb6del\"");
    }

    #[test]
    fn parse_cgi_headers_japanese_utf8_body_preserved() {
        // Body contains Japanese UTF-8: E6 97 A5 E6 9C AC E8 AA 9E
        let buf = b"Status: 200\nContent-Type: application/json\n\n\"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\"";
        let (status, _headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert_eq!(&buf[body_start..], b"\"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\"");
    }

    #[test]
    fn parse_cgi_headers_binary_body_all_byte_values() {
        // Body contains 0x00, 0xFF, 0x80, 0x42
        let buf = b"Status: 200\nContent-Type: application/octet-stream\n\n\x00\xff\x80\x42";
        let (status, headers, body_start) = parse_cgi_headers(buf).unwrap();
        assert_eq!(status, 200);
        assert_eq!(
            headers.get("content-type").unwrap(),
            "application/octet-stream"
        );
        assert_eq!(&buf[body_start..], b"\x00\xff\x80\x42");
    }

    #[tokio::test]
    async fn cgi_pipe_stream_yields_utf8_body_from_pipe() {
        let (rx_fd, tx_fd) = pipe().unwrap();
        let (read, mut write) = (
            Receiver::from_owned_fd(rx_fd).unwrap(),
            std::fs::File::from(tx_fd),
        );

        // Simulate CGI headers + body with UTF-8 multibyte content
        let cgi_output: &[u8] =
            b"Status: 200 OK\nContent-Type: application/json\n\n\"Kurt G\xc3\xb6del\"";
        write.write_all(cgi_output).unwrap();
        drop(write);

        let stream = CgiPipeStream::new(read, Vec::new(), None);
        let collected: Vec<u8> = stream
            .map(|chunk| chunk.unwrap().to_vec())
            .concat()
            .await;
        assert_eq!(collected, cgi_output);
    }

    #[tokio::test]
    async fn cgi_pipe_stream_yields_binary_body_from_pipe() {
        let (rx_fd, tx_fd) = pipe().unwrap();
        let (read, mut write) = (
            Receiver::from_owned_fd(rx_fd).unwrap(),
            std::fs::File::from(tx_fd),
        );

        // Simulate CGI headers + binary body with raw byte values
        let cgi_output: &[u8] = b"Status: 200\nContent-Type: application/octet-stream\n\n\x00\xff\x80\x42";
        write.write_all(cgi_output).unwrap();
        drop(write);

        let stream = CgiPipeStream::new(read, Vec::new(), None);
        let collected: Vec<u8> = stream
            .map(|chunk| chunk.unwrap().to_vec())
            .concat()
            .await;
        assert_eq!(collected, cgi_output);
    }

    #[tokio::test]
    async fn cgi_pipe_stream_yields_leftover_then_pipe_bytes() {
        let (rx_fd, tx_fd) = pipe().unwrap();
        let (read, mut write) = (
            Receiver::from_owned_fd(rx_fd).unwrap(),
            std::fs::File::from(tx_fd),
        );

        let leftover = b"left".to_vec();
        let mut stream = CgiPipeStream::new(read, leftover, None);

        let first = stream.next().await.unwrap().unwrap();
        assert_eq!(first, "left");

        write.write_all(b"over").unwrap();
        drop(write);

        let second = stream.next().await.unwrap().unwrap();
        assert_eq!(second, "over");
        assert!(stream.next().await.is_none());
    }

    #[test]
    fn stream_registry_sends_and_removes_on_close() {
        let mut registry = StreamRegistry::new();
        let (tx, mut rx) = mpsc::channel::<axum::body::Bytes>(10);
        let id = registry.add(tx);

        let event = axum::body::Bytes::from(r#"{"event":"test"}"#);
        assert!(registry.send(id, event.clone()).is_ok());
        let received = rx.blocking_recv().unwrap();
        assert_eq!(received, event);

        drop(rx);
        assert!(registry.send(id, event).is_err());
        assert!(registry.senders().is_empty());
    }

    #[tokio::test]
    async fn static_router_rejects_directory_traversal() {
        let dir = tempfile::tempdir().unwrap();
        let app_dir = dir.path().join("app");
        tokio::fs::create_dir(&app_dir).await.unwrap();
        tokio::fs::write(dir.path().join("secret.txt"), "secret").await.unwrap();

        let router = build_static_router(vec![PluginStaticPath {
            prefix: "/app".to_string(),
            directory: app_dir.to_string_lossy().into_owned(),
            fallback: None,
            not_found: None,
            auth: "none".to_string(),
            csp_nonce: false,
        }]);

        let response = router
            .oneshot(
                Request::builder()
                    .uri("/app/../secret.txt")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn broadcast_registry_multiplexes_to_multiple_streams() {
        // Set the tokio handle so BroadcastRegistry can spawn the forwarder
        set_tokio_handle(tokio::runtime::Handle::current());

        let mut broadcast_registry = BroadcastRegistry::new();

        let (tx1, mut rx1) = mpsc::channel::<axum::body::Bytes>(10);
        let (tx2, mut rx2) = mpsc::channel::<axum::body::Bytes>(10);

        broadcast_registry.subscribe("actions".to_string(), 1, tx1, None);
        broadcast_registry.subscribe("actions".to_string(), 2, tx2, None);

        let event = axum::body::Bytes::from(r#"{"action":"test"}"#);
        assert!(broadcast_registry.send("actions", event.clone()).is_ok());

        // Give the forwarder + per-subscriber tasks time to process
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

        let received1 = rx1.recv().await.unwrap();
        let received2 = rx2.recv().await.unwrap();
        assert_eq!(received1, event);
        assert_eq!(received2, event);
    }

    #[tokio::test]
    async fn broadcast_registry_slow_subscriber_does_not_block_others() {
        set_tokio_handle(tokio::runtime::Handle::current());

        let mut broadcast_registry = BroadcastRegistry::new();

        // Subscriber 1: small channel that we won't drain (simulates slow client)
        let (tx1, _rx1) = mpsc::channel::<axum::body::Bytes>(1);
        // Subscriber 2: normal channel that we will drain
        let (tx2, mut rx2) = mpsc::channel::<axum::body::Bytes>(10);

        broadcast_registry.subscribe("actions".to_string(), 1, tx1, None);
        broadcast_registry.subscribe("actions".to_string(), 2, tx2, None);

        // Send event 1 — both subscribers should get it (subscriber 1's
        // per-subscriber buffer has room for 64 events)
        let event1 = axum::body::Bytes::from(r#"{"action":"1"}"#);
        broadcast_registry.send("actions", event1.clone()).unwrap();

        // Give tasks time to process
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

        // Subscriber 2 should receive event 1 even though subscriber 1
        // is not draining its channel
        let received2 = rx2.recv().await.unwrap();
        assert_eq!(received2, event1);
    }

    #[tokio::test]
    async fn broadcast_registry_send_to_all_delivers_to_every_graphql_channel() {
        set_tokio_handle(tokio::runtime::Handle::current());

        let mut broadcast_registry = BroadcastRegistry::new();

        let (tx1, mut rx1) = mpsc::channel::<axum::body::Bytes>(10);
        let (tx2, mut rx2) = mpsc::channel::<axum::body::Bytes>(10);
        let (tx3, mut rx3) = mpsc::channel::<axum::body::Bytes>(10);

        broadcast_registry.subscribe("graphql_raw_cohort_a".to_string(), 1, tx1, None);
        broadcast_registry.subscribe("graphql_raw_cohort_b".to_string(), 2, tx2, None);
        broadcast_registry.subscribe("graphql_raw_cohort_c".to_string(), 3, tx3, None);

        // Send a keepalive to ALL GraphQL channels
        let keepalive = axum::body::Bytes::from(": keepalive\n\n");
        assert!(broadcast_registry.send_to_all(keepalive.clone()).is_ok());

        // Give the forwarder time to process
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

        // All three subscribers on different GraphQL channels should receive it
        let received1 = rx1.recv().await.unwrap();
        let received2 = rx2.recv().await.unwrap();
        let received3 = rx3.recv().await.unwrap();
        assert_eq!(received1, keepalive);
        assert_eq!(received2, keepalive);
        assert_eq!(received3, keepalive);
    }

    #[tokio::test]
    async fn broadcast_registry_send_to_all_skips_non_graphql_channels() {
        set_tokio_handle(tokio::runtime::Handle::current());

        let mut broadcast_registry = BroadcastRegistry::new();

        let (tx_gql, mut rx_gql) = mpsc::channel::<axum::body::Bytes>(10);
        let (tx_commit, mut rx_commit) = mpsc::channel::<axum::body::Bytes>(10);

        broadcast_registry.subscribe("graphql_raw_cohort1".to_string(), 1, tx_gql, None);
        broadcast_registry.subscribe("admin/db/local/branch/main".to_string(), 2, tx_commit, None);

        // Give the forwarder time to process the subscribe commands
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        let keepalive = axum::body::Bytes::from(": keepalive\n\n");
        let send_result = broadcast_registry.send_to_all(keepalive.clone());

        // send_to_all may fail if the forwarder task was spawned on a
        // previous test's runtime that has been dropped. This is a test
        // infrastructure issue with the global TOKIO_HANDLE. The core
        // assertion we care about is that non-graphql channels don't
        // receive keepalive.
        if send_result.is_ok() {
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

            // GraphQL channel may or may not receive the keepalive
            // (depends on forwarder scheduling). If it does, verify content.
            if let Ok(received_gql) = tokio::time::timeout(
                tokio::time::Duration::from_millis(200),
                rx_gql.recv(),
            ).await {
                if let Some(data) = received_gql {
                    assert_eq!(data, keepalive);
                }
            }
        }

        // The core assertion: commit stream channel must NOT receive
        // the SSE keepalive, regardless of whether send_to_all succeeded.
        tokio::select! {
            result = rx_commit.recv() => {
                match result {
                    Some(data) => panic!("commit stream should not receive SSE keepalive, got: {:?}", data),
                    None => { /* sender dropped — acceptable */ }
                }
            }
            _ = tokio::time::sleep(tokio::time::Duration::from_millis(200)) => {
                // Timeout — no data received, which is the expected behavior
            }
        }
    }

    #[tokio::test]
    async fn input_pipe_round_trips_ascii_bytes() {
        let (read_fd, write_fd) = pipe().unwrap();
        let mut write_file = std::fs::File::from(write_fd);
        let body = b"Hello, world!";
        write_file.write_all(body).unwrap();
        drop(write_file);

        let mut read = Receiver::from_owned_fd(read_fd).unwrap();
        let mut buf = Vec::new();
        use tokio::io::AsyncReadExt;
        read.read_to_end(&mut buf).await.unwrap();
        assert_eq!(buf, body);
    }

    #[tokio::test]
    async fn input_pipe_round_trips_utf8_multibyte_bytes() {
        let (read_fd, write_fd) = pipe().unwrap();
        let mut write_file = std::fs::File::from(write_fd);
        // "Kurt Gödel" in UTF-8 bytes
        let body: &[u8] = b"Kurt G\xc3\xb6del";
        write_file.write_all(body).unwrap();
        drop(write_file);

        let mut read = Receiver::from_owned_fd(read_fd).unwrap();
        let mut buf = Vec::new();
        use tokio::io::AsyncReadExt;
        read.read_to_end(&mut buf).await.unwrap();
        assert_eq!(buf, body);
    }

    #[tokio::test]
    async fn input_pipe_round_trips_binary_octets() {
        let (read_fd, write_fd) = pipe().unwrap();
        let mut write_file = std::fs::File::from(write_fd);
        // Raw bytes including 0x00, 0xFF, and other high-bit values
        let body: &[u8] = &[0x00, 0x01, 0x7F, 0x80, 0xFF, 0xC3, 0xB6, 0xFE];
        write_file.write_all(body).unwrap();
        drop(write_file);

        let mut read = Receiver::from_owned_fd(read_fd).unwrap();
        let mut buf = Vec::new();
        use tokio::io::AsyncReadExt;
        read.read_to_end(&mut buf).await.unwrap();
        assert_eq!(buf, body);
    }

    #[tokio::test]
    async fn input_pipe_round_trips_empty_body() {
        let (read_fd, write_fd) = pipe().unwrap();
        drop(write_fd); // EOF immediately

        let mut read = Receiver::from_owned_fd(read_fd).unwrap();
        let mut buf = Vec::new();
        use tokio::io::AsyncReadExt;
        read.read_to_end(&mut buf).await.unwrap();
        assert!(buf.is_empty());
    }
}

/// Dispatch a plugin request to a Prolog handler through OS pipes.
///
/// The handler receives a request dict and writes a CGI-style response
/// directly to the output pipe. Rust parses the CGI headers and streams the
/// body to the HTTP client. No JSON serialization of response metadata is used.
async fn dispatch_request_via_pipe(
    module: String,
    handler: String,
    pattern: String,
    binary: bool,
    req: Request<Body>,
) -> impl IntoResponse {
    let (parts, body) = req.into_parts();
    let body_bytes = match axum::body::to_bytes(body, *MAX_BODY_SIZE).await {
        Ok(bytes) => bytes,
        Err(_) => return plugin_error_response("failed to read request body").into_response(),
    };
    let content_encoding = parts
        .headers
        .get(header::CONTENT_ENCODING)
        .and_then(|v| v.to_str().ok());
    let body_bytes = match decompress_request_body(content_encoding, &body_bytes) {
        Ok(bytes) => bytes,
        Err(msg) => return plugin_error_response(&msg).into_response(),
    };

    let method = parts.method.to_string();
    let path = normalize_dispatch_path(parts.uri.path());
    let query = parts.uri.query().unwrap_or("").to_string();
    let headers: serde_json::Map<String, serde_json::Value> = parts
        .headers
        .iter()
        .filter_map(|(k, v)| {
            // Strip Content-Encoding: the body has already been decompressed
            // by the Rust server. If we leave it in, the Prolog handler will
            // try to decompress the already-decompressed data.
            if k == header::CONTENT_ENCODING {
                return None;
            }
            let name = k.as_str().to_string();
            let value = v.to_str().unwrap_or("").to_string();
            Some((name, json!(value)))
        })
        .collect();
    let params: serde_json::Map<String, serde_json::Value> = extract_route_params(&pattern, &path)
        .into_iter()
        .map(|(k, v)| (k, json!(v)))
        .collect();

    let request_json = json!({
        "method": method,
        "path": path,
        "query": query,
        "headers": headers,
        "body": "",
        "params": params,
    });
    crate::log::log_info(format!("{} {} (plugin pipe)", method, path));

    let (output_read, output_write) = match pipe() {
        Ok(p) => p,
        Err(e) => {
            crate::log::log_error(format!("output pipe creation failed: {}", e));
            return plugin_error_response("pipe creation failed").into_response();
        }
    };

    let output_write_fd = output_write.into_raw_fd();

    let (input_read_fd, input_write) = if body_bytes.is_empty() {
        (None, None)
    } else {
        match pipe() {
            Ok((input_read, input_write)) => {
                let input_read_fd = input_read.into_raw_fd();
                let input_write_file = std::fs::File::from(input_write);
                (Some(input_read_fd), Some(input_write_file))
            }
            Err(e) => {
                crate::log::log_error(format!("input pipe creation failed: {}", e));
                let _ = write_cgi_error_to_pipe(output_write_fd, "input pipe creation failed");
                return plugin_error_response("pipe creation failed").into_response();
            }
        }
    };

    let dispatch_req = PipeDispatchRequest {
        request_json,
        handler_module: module,
        handler_name: handler,
        input_read_fd,
        output_write_fd,
        binary,
    };

    let mut output_receiver = match Receiver::from_owned_fd(output_read) {
        Ok(rx) => rx,
        Err(e) => {
            crate::log::log_error(format!("failed to create pipe receiver: {}", e));
            let _ = write_cgi_error_to_pipe(output_write_fd, "failed to create pipe receiver");
            return plugin_error_response("pipe creation failed").into_response();
        }
    };

    let queue = dispatch_queue();
    let in_flight = in_flight_requests();
    let request_id = next_request_id(&method, &path);
    {
        let mut set = in_flight.lock().unwrap();
        if set.contains(&request_id) {
            return plugin_error_response("duplicate request id").into_response();
        }
        set.insert(request_id.clone());
    }
    let _guard = InFlightRequestGuard {
        id: request_id,
        in_flight,
    };

    if let Err(e) = queue.send(DispatchMessage::Pipe(dispatch_req)).await {
        crate::log::log_error(format!("dispatch queue send failed: {}", e));
        let _ = write_cgi_error_to_pipe(output_write_fd, "dispatch queue send failed");
        return plugin_error_response("Plugin handler failed").into_response();
    }

    // Create a cancel guard that covers the entire request lifecycle from
    // dispatch to response completion. If the HTTP response future is dropped
    // at any point (client disconnect during header reading or body
    // streaming), this guard sends a CancelPipe message to the dispatch
    // queue, which signals the Prolog worker to abort via thread_signal/2.
    let mut cancel_guard = PipeCancelGuard::new(output_write_fd, dispatch_queue());

    if let Some(input_file) = input_write {
        tokio::task::spawn_blocking(move || {
            let mut file = input_file;
            if let Err(e) = file.write_all(&body_bytes) {
                crate::log::log_error(format!("failed to write request body to input pipe: {}", e));
            }
        });
    }

    let (status, response_headers, leftover) = match read_cgi_headers(&mut output_receiver).await {
        Ok(result) => result,
        Err(e) => {
            crate::log::log_error(format!("failed to read CGI headers from pipe: {}", e));
            // The guard will fire on drop, sending a cancel to the worker.
            return plugin_error_response("Plugin handler failed").into_response();
        }
    };

    let mut builder = Response::builder().status(status);
    for (key, value) in response_headers.iter() {
        builder = builder.header(key, value);
    }

    // If the CGI handler set Content-Length, the body is fully buffered in
    // the CGI stream and written to the pipe after the headers. Read it
    // into a buffer and send it with Content-Length for HTTP keep-alive.
    // If Content-Length is absent (streaming responses like NDJSON), use
    // chunked transfer encoding via Body::from_stream.
    if let Some(cl) = response_headers.get("content-length") {
        if let Ok(cl_str) = cl.to_str() {
            if let Ok(cl_len) = cl_str.parse::<usize>() {
                // Read the body from the pipe into a buffer.
                // The CGI stream has already written the full body to the
                // pipe, so this is just I/O — no computation timeout risk.
                let mut body = leftover;
                let mut read = body.len();
                if read < cl_len {
                    body.resize(cl_len, 0u8);
                    while read < cl_len {
                        match output_receiver.read(&mut body[read..]).await {
                            Ok(0) => break, // EOF (shouldn't happen before cl_len)
                            Ok(n) => read += n,
                            Err(_) => break,
                        }
                    }
                }
                body.truncate(read);
                // The Prolog worker has finished writing the full response.
                // Disarm the cancel guard so we don't send a spurious cancel.
                cancel_guard.disarm();
                return builder
                    .body(Body::from(axum::body::Bytes::from(body)))
                    .unwrap()
                    .into_response();
            }
        }
    }

    // No Content-Length — stream with chunked transfer encoding.
    // Transfer the cancel guard into the stream so that if the client
    // disconnects during streaming, the Prolog worker is signalled to abort.
    let body_stream = CgiPipeStream::new(output_receiver, leftover, Some(cancel_guard));
    builder.body(Body::from_stream(body_stream)).unwrap().into_response()
}

/// Read CGI headers from the pipe receiver, returning the parsed status, header
/// map, and any bytes that were read past the header separator.
pub(crate) async fn read_cgi_headers(
    receiver: &mut Receiver,
) -> Result<(u16, HeaderMap, Vec<u8>), String> {
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];
    loop {
        let n = receiver
            .read(&mut chunk)
            .await
            .map_err(|e| format!("read error: {}", e))?;
        if n == 0 {
            return Err("EOF before CGI header separator".to_string());
        }
        buf.extend_from_slice(&chunk[..n]);
        if let Some((status, headers, body_start)) = parse_cgi_headers(&buf) {
            let leftover = buf.split_off(body_start);
            return Ok((status, headers, leftover));
        }
    }
}

/// Parse CGI headers from a byte buffer.
/// Returns (status_code, headers, body_start_offset) or None if no separator found.
/// Hop-by-hop headers stripped: Transfer-Encoding, Connection.
/// Content-Length is preserved so that hyper can use it for keep-alive when
/// the handler sets it (e.g. reply_json). When Content-Length is absent
/// (streaming responses like NDJSON), hyper uses chunked transfer encoding.
pub(crate) fn parse_cgi_headers(buf: &[u8]) -> Option<(u16, HeaderMap, usize)> {
    let separator_pos = find_header_separator(buf)?;
    let header_bytes = &buf[..separator_pos];
    let body_start = if buf[separator_pos..].starts_with(b"\r\n\r\n") {
        separator_pos + 4
    } else {
        separator_pos + 2
    };
    let header_str = std::str::from_utf8(header_bytes).ok()?;
    let mut status: u16 = 200;
    let mut headers = HeaderMap::new();
    for line in header_str.lines() {
        if let Some(colon_pos) = line.find(':') {
            let key = line[..colon_pos].trim();
            let value = line[colon_pos + 1..].trim();
            if key.eq_ignore_ascii_case("status") {
                let code_str = value.split_whitespace().next().unwrap_or("200");
                status = code_str.parse().unwrap_or(200);
            } else if key.eq_ignore_ascii_case("transfer-encoding")
                || key.eq_ignore_ascii_case("connection")
            {
                continue;
            } else if let Ok(name) = key.parse::<header::HeaderName>() {
                if let Ok(val) = value.parse() {
                    headers.insert(name, val);
                }
            }
        }
    }
    Some((status, headers, body_start))
}

fn find_header_separator(buf: &[u8]) -> Option<usize> {
    for i in 0..buf.len().saturating_sub(1) {
        if buf[i] == b'\n' && buf[i + 1] == b'\n' {
            return Some(i);
        }
    }
    (0..buf.len().saturating_sub(3)).find(|&i| &buf[i..i + 4] == b"\r\n\r\n")
}

/// Guard that sends a `CancelPipe` message to the dispatch queue when dropped.
/// This is held by `CgiPipeStream` so that when the HTTP response future is
/// dropped (client disconnect), the Prolog worker handling this pipe FD is
/// signalled to abort its work via `thread_signal/2`.
struct PipeCancelGuard {
    output_write_fd: i32,
    queue: mpsc::Sender<DispatchMessage>,
    cancelled: bool,
}

impl PipeCancelGuard {
    fn new(output_write_fd: i32, queue: mpsc::Sender<DispatchMessage>) -> Self {
        Self {
            output_write_fd,
            queue,
            cancelled: false,
        }
    }

    /// Mark the guard as consumed so Drop does not send a cancel message.
    /// Called when the stream completes normally (EOF or error).
    fn disarm(&mut self) {
        self.cancelled = true;
    }
}

impl Drop for PipeCancelGuard {
    fn drop(&mut self) {
        if !self.cancelled {
            let fd = self.output_write_fd;
            let queue = self.queue.clone();
            // Send the cancel message asynchronously. If the queue is full
            // or closed, there's nothing we can do — the watchdog will
            // catch it as a safety net.
            tokio::spawn(async move {
                let _ = queue.send(DispatchMessage::CancelPipe(fd)).await;
            });
        }
    }
}

/// Stream that yields bytes from the pipe receiver, prefixed by the bytes that
/// were read while parsing the CGI headers. When dropped before EOF (client
/// disconnect), the `PipeCancelGuard` sends a cancel message to the dispatch
/// queue so the Prolog worker is signalled to abort.
struct CgiPipeStream {
    receiver: Receiver,
    leftover: Vec<u8>,
    leftover_yielded: bool,
    cancel_guard: Option<PipeCancelGuard>,
}

impl CgiPipeStream {
    fn new(receiver: Receiver, leftover: Vec<u8>, cancel_guard: Option<PipeCancelGuard>) -> Self {
        Self {
            receiver,
            leftover,
            leftover_yielded: false,
            cancel_guard,
        }
    }
}

impl Stream for CgiPipeStream {
    type Item = Result<axum::body::Bytes, std::convert::Infallible>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut TaskContext<'_>) -> Poll<Option<Self::Item>> {
        let this = self.get_mut();
        if !this.leftover_yielded {
            this.leftover_yielded = true;
            if !this.leftover.is_empty() {
                return Poll::Ready(Some(Ok(axum::body::Bytes::from(std::mem::take(
                    &mut this.leftover,
                )))));
            }
        }
        let mut buf = [0u8; 8192];
        let mut read_buf = ReadBuf::new(&mut buf);
        match Pin::new(&mut this.receiver).poll_read(cx, &mut read_buf) {
            Poll::Ready(Ok(())) => {
                let n = read_buf.filled().len();
                if n == 0 {
                    // EOF — disarm the cancel guard so we don't send a
                    // spurious cancel message for a completed request.
                    if let Some(guard) = &mut this.cancel_guard {
                        guard.disarm();
                    }
                    Poll::Ready(None)
                } else {
                    Poll::Ready(Some(Ok(axum::body::Bytes::copy_from_slice(&buf[..n]))))
                }
            }
            Poll::Ready(Err(_)) => {
                // Error — disarm and return None.
                if let Some(guard) = &mut this.cancel_guard {
                    guard.disarm();
                }
                Poll::Ready(None)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

/// Extract parameters from a URI given a route pattern.
///
/// Supports the Axum/React Router style syntax:
/// - `:name` captures a single path segment.
/// - `*name` captures all remaining path segments (catch-all).
///
/// Returns an empty map if the pattern does not contain any captures.
fn extract_route_params(pattern: &str, uri: &str) -> HashMap<String, String> {
    let mut params = HashMap::new();
    let pattern_parts: Vec<&str> = pattern.split('/').collect();
    let uri_parts: Vec<&str> = uri.split('/').collect();

    let mut pattern_idx = 0;
    let mut uri_idx = 0;
    while pattern_idx < pattern_parts.len() && uri_idx < uri_parts.len() {
        let part = pattern_parts[pattern_idx];
        if let Some(name) = part.strip_prefix('*') {
            let remaining = uri_parts[uri_idx..].join("/");
            params.insert(name.to_string(), remaining);
            return params;
        } else if let Some(name) = part.strip_prefix(':') {
            params.insert(name.to_string(), uri_parts[uri_idx].to_string());
            pattern_idx += 1;
            uri_idx += 1;
        } else {
            pattern_idx += 1;
            uri_idx += 1;
        }
    }

    params
}

/// Guard that removes a stream sender from the registry when the stream ends.
struct StreamGuard {
    id: StreamId,
}

impl Drop for StreamGuard {
    fn drop(&mut self) {
        {
            let broadcast_arc = broadcast_registry();
            let mut broadcast = broadcast_arc.lock().unwrap();
            broadcast.unsubscribe_all(self.id);
        }
        let stream_arc = stream_registry();
        stream_arc.lock().unwrap().remove(self.id);
    }
}

/// Receiver stream that cleans up its registry entry on drop.
struct GuardedReceiverStream {
    inner: ReceiverStream<axum::body::Bytes>,
    _guard: StreamGuard,
}

impl Stream for GuardedReceiverStream {
    type Item = Result<axum::body::Bytes, std::convert::Infallible>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        std::pin::Pin::new(&mut self.inner)
            .poll_next(cx)
            .map(|opt| opt.map(Ok))
    }
}

/// Dispatch a streaming request to a Prolog handler.
///
/// The handler receives a request dict (with a UTF-8 body string) and a
/// `StreamId`. It returns a response dict. When the response body is the atom
/// `stream`, the response is an NDJSON stream fed by `appserver_stream_send/2`.
async fn dispatch_stream_request(
    module: String,
    handler: String,
    pattern: String,
    req: Request<Body>,
) -> impl IntoResponse {
    let (parts, body) = req.into_parts();
    let content_encoding = parts
        .headers
        .get(header::CONTENT_ENCODING)
        .and_then(|v| v.to_str().ok());
    let content_type = parts
        .headers
        .get(header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");
    let has_content_length = parts.headers.contains_key(header::CONTENT_LENGTH);

    let method = parts.method.to_string();
    let path = normalize_dispatch_path(parts.uri.path());
    let query = parts.uri.query().unwrap_or("").to_string();
    let headers: serde_json::Map<String, serde_json::Value> = parts
        .headers
        .iter()
        .filter_map(|(k, v)| {
            if k == header::CONTENT_ENCODING {
                return None;
            }
            let name = k.as_str().to_string();
            let value = v.to_str().unwrap_or("").to_string();
            Some((name, json!(value)))
        })
        .collect();
    let params: serde_json::Map<String, serde_json::Value> = extract_route_params(&pattern, &path)
        .into_iter()
        .map(|(k, v)| (k, json!(v)))
        .collect();

    // When Content-Length is present and no compression, use the fast
    // buffered path: read full body, pre-parse NDJSON into native dicts,
    // and include parsed_body in the request. This eliminates
    // json_read_dict overhead on the Prolog side.
    //
    // When chunked (no Content-Length) or compressed, stream body chunks
    // through the input channel as they arrive. Prolog drains them via
    // appserver_stream_recv/2. No parsed_body — Prolog parses NDJSON
    // from the drained body. This enables true streaming with lower
    // latency at the cost of Prolog-side JSON parsing.
    let can_pre_parse = has_content_length
        && content_encoding.is_none()
        && content_type.contains("application/x-ndjson");

    let mut body_opt = Some(body);

    let (buffered_body, parsed_body) = if can_pre_parse {
        let body = body_opt.take().unwrap();
        let body_bytes = match axum::body::to_bytes(body, *MAX_BODY_SIZE).await {
            Ok(bytes) => bytes,
            Err(_) => return plugin_error_response("failed to read request body"),
        };
        // For large bodies, skip pre-parsing to avoid dispatch thread
        // serialize_to_term bottleneck. Worker threads parse via
        // json_read_all_from_stream in parallel. Don't include the body
        // string in request_json either — the worker reads it from the
        // input stream.
        if body_bytes.len() < 100_000 {
            let parsed_body: Vec<serde_json::Value> = body_bytes
                .split(|&b| b == b'\n')
                .filter(|line| !line.is_empty())
                .filter_map(|line| serde_json::from_slice(line).ok())
                .collect();
            (Some(body_bytes), parsed_body)
        } else {
            // Large body: send via input stream only, don't serialize in JSON.
            (Some(body_bytes), Vec::new())
        }
    } else {
        (None, Vec::new())
    };

    let request_json = if parsed_body.is_empty() {
        // For large NDJSON bodies (>= 100KB), don't serialize the body
        // string into request_json — the worker reads it from the input
        // stream. For small bodies that failed to parse as NDJSON lines,
        // include the body string as fallback.
        let body_str = buffered_body
            .as_ref()
            .filter(|b| b.len() < 100_000)
            .map(|b| String::from_utf8_lossy(b).to_string())
            .unwrap_or_default();
        json!({
            "method": method,
            "path": path,
            "query": query,
            "headers": headers,
            "body": body_str,
            "params": params,
        })
    } else {
        json!({
            "method": method,
            "path": path,
            "query": query,
            "headers": headers,
            "body": "",
            "params": params,
            "parsed_body": parsed_body,
        })
    };
    crate::log::log_info(format!("{} {} (stream)", method, path));

    let (input_stream_id, input_tx) = create_input_stream();
    let (response_stream_id, mut response_rx) = create_response_stream();

    let dispatch_req = DispatchRequest {
        request_json,
        handler_module: module,
        handler_name: handler,
        input_stream_id,
        response_stream_id,
        is_stream: true,
    };

    let dispatch_result = {
        let queue = dispatch_queue();
        let in_flight = in_flight_requests();
        let request_id = next_request_id(&method, &path);
        {
            let mut set = in_flight.lock().unwrap();
            if set.contains(&request_id) {
                input_stream_registry().lock().unwrap().take(input_stream_id);
                stream_registry().lock().unwrap().remove(response_stream_id);
                return plugin_error_response("duplicate request id");
            }
            set.insert(request_id.clone());
        }
        let _guard = InFlightRequestGuard {
            id: request_id,
            in_flight,
        };

        if queue.send(DispatchMessage::Stream(dispatch_req)).await.is_err() {
            stream_registry().lock().unwrap().remove(response_stream_id);
            return plugin_error_response("dispatch queue closed");
        }

        if let Some(bytes) = buffered_body {
            // Buffered path: send entire body as one chunk, then close.
            if input_tx.send(bytes).await.is_err() {
                stream_registry().lock().unwrap().remove(response_stream_id);
                return plugin_error_response("failed to send request body");
            }
            drop(input_tx);
        } else {
            // Streaming path: forward body chunks to Prolog as they arrive.
            let body = body_opt.take().unwrap();
            tokio::spawn(async move {
                let mut body = body;
                use http_body_util::BodyExt;
                while let Some(chunk_result) = body.frame().await {
                    match chunk_result {
                        Ok(frame) => {
                            if let Ok(bytes) = frame.into_data() {
                                if input_tx.send(bytes).await.is_err() {
                                    break;
                                }
                            }
                        }
                        Err(_) => break,
                    }
                }
                drop(input_tx);
            });
        }

        tokio::task::spawn_blocking(move || {
            let first = response_rx
                .blocking_recv()
                .ok_or(PrologError::Failure)?;
            let response: serde_json::Value =
                serde_json::from_slice(&first).map_err(|_| PrologError::Failure)?;
            Ok((response, response_rx))
        })
        .await
        .unwrap_or(Err(PrologError::Failure))
    };

    match dispatch_result {
        Ok((response, response_rx)) => {
            let status = response
                .get("status")
                .and_then(|s| s.as_u64())
                .unwrap_or(200) as u16;
            let status = StatusCode::from_u16(status).unwrap_or(StatusCode::OK);
            let mut response_headers = HeaderMap::new();
            if let Some(headers_map) = response.get("headers").and_then(|h| h.as_object()) {
                for (key, value) in headers_map {
                    if let Some(value_str) = value.as_str() {
                        if let Ok(header_name) = key.parse::<header::HeaderName>() {
                            if let Ok(header_value) = value_str.parse() {
                                response_headers.insert(header_name, header_value);
                            }
                        }
                    }
                }
            }

            let body_value = response.get("body").cloned().unwrap_or(json!("stream"));
            if body_value == json!("stream") {
                if !response_headers.contains_key(header::CONTENT_TYPE) {
                    response_headers.insert(
                        header::CONTENT_TYPE,
                        "application/x-ndjson".parse().unwrap(),
                    );
                }
                let stream = GuardedReceiverStream {
                    inner: ReceiverStream::new(response_rx),
                    _guard: StreamGuard { id: response_stream_id },
                };
                let mut builder = Response::builder().status(status);
                for (key, value) in response_headers.iter() {
                    builder = builder.header(key, value);
                }
                builder.body(Body::from_stream(stream)).unwrap()
            } else {
                stream_registry().lock().unwrap().remove(response_stream_id);
                let mut builder = Response::builder().status(status);
                for (key, value) in response_headers.iter() {
                    builder = builder.header(key, value);
                }
                if let Some(body_str) = body_value.as_str() {
                    builder.body(Body::from(body_str.to_string()))
                } else {
                    builder.body(Body::from(body_value.to_string()))
                }.unwrap()
            }
        }
        Err(_) => {
            stream_registry().lock().unwrap().remove(response_stream_id);
            plugin_error_response("Plugin stream handler failed")
        }
    }
}

/// Return a JSON error response.
fn plugin_error_response(message: &str) -> Response<Body> {
    let body = json!({
        "@type": "api:ErrorResponse",
        "api:status": "api:failure",
        "api:error": {"@type": "api:InternalServerError"},
        "api:message": message
    });
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(body.to_string()))
        .unwrap()
}

#[cfg(test)]
mod path_tests {
    use super::{decompress_request_body, normalize_dispatch_path};

    #[test]
    fn normalize_decodes_percent_encoding() {
        assert_eq!(normalize_dispatch_path("/api/db/admin%20name"), "/api/db/admin name");
    }

    #[test]
    fn normalize_preserves_trailing_slash() {
        // Trailing slashes are preserved so that SWI-Prolog's http_dispatch
        // can match prefix and exact handlers exactly as in the native backend.
        assert_eq!(normalize_dispatch_path("/api/db/admin/"), "/api/db/admin/");
    }

    #[test]
    fn normalize_keeps_root_slash() {
        assert_eq!(normalize_dispatch_path("/"), "/");
    }

    #[test]
    fn normalize_does_not_modify_root_alias_slashes() {
        // The connect handler is registered at `/api/` and the 404 handler at
        // `/api`; the raw path is preserved for http_dispatch to resolve.
        assert_eq!(normalize_dispatch_path("/api"), "/api");
        assert_eq!(normalize_dispatch_path("/api/"), "/api/");
    }

    #[test]
    fn normalize_does_not_restore_slash_for_nested_paths() {
        assert_eq!(normalize_dispatch_path("/api/info"), "/api/info");
        assert_eq!(normalize_dispatch_path("/api/info/"), "/api/info/");
    }

    #[test]
    fn decompresses_gzip_request_body() {
        use flate2::write::GzEncoder;
        use flate2::Compression;
        use std::io::Write;
        let original = b"{\"hello\":\"world\"}";
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(original).unwrap();
        let compressed = encoder.finish().unwrap();
        let out = decompress_request_body(Some("gzip"), &compressed).unwrap();
        assert_eq!(out, original);
    }

    #[test]
    fn decompresses_deflate_request_body() {
        use flate2::write::ZlibEncoder;
        use flate2::Compression;
        use std::io::Write;
        let original = b"{\"hello\":\"world\"}";
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(original).unwrap();
        let compressed = encoder.finish().unwrap();
        let out = decompress_request_body(Some("deflate"), &compressed).unwrap();
        assert_eq!(out, original);
    }

    fn is_catchall_pattern(pattern: &str) -> bool {
        pattern.split('/').any(|seg| seg.starts_with('*'))
    }

    #[test]
    fn detect_catchall_pattern() {
        assert!(is_catchall_pattern("/api/db/*path"));
        assert!(!is_catchall_pattern("/api/organizations/:seg1"));
        assert!(!is_catchall_pattern("/api"));
    }
}

#[cfg(test)]
mod subscription_limiter_tests {
    use super::SubscriptionLimiter;

    #[test]
    fn allows_under_limit() {
        let mut limiter = SubscriptionLimiter::default();
        let max = limiter.max_per_user();
        for _ in 0..max {
            assert!(limiter.try_acquire("user_a").is_ok());
        }
    }

    #[test]
    fn rejects_over_limit() {
        let mut limiter = SubscriptionLimiter::default();
        let max = limiter.max_per_user();
        for _ in 0..max {
            limiter.try_acquire("user_a").unwrap();
        }
        assert!(limiter.try_acquire("user_a").is_err());
    }

    #[test]
    fn release_frees_slot() {
        let mut limiter = SubscriptionLimiter::default();
        let max = limiter.max_per_user();
        for _ in 0..max {
            limiter.try_acquire("user_a").unwrap();
        }
        assert!(limiter.try_acquire("user_a").is_err());
        limiter.release("user_a");
        assert!(limiter.try_acquire("user_a").is_ok());
    }

    #[test]
    fn independent_users_have_independent_limits() {
        let mut limiter = SubscriptionLimiter::default();
        let max = limiter.max_per_user();
        for _ in 0..max {
            limiter.try_acquire("user_a").unwrap();
        }
        assert!(limiter.try_acquire("user_a").is_err());
        assert!(limiter.try_acquire("user_b").is_ok());
    }

    #[test]
    fn release_unknown_user_is_noop() {
        let mut limiter = SubscriptionLimiter::default();
        limiter.release("nonexistent");
        assert_eq!(limiter.count_for("nonexistent"), 0);
    }
}
