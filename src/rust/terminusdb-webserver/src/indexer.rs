//! Indexer push architecture: Rust-side IndexerRegistry and branch tasks.
//!
//! This module implements the Rust side of the indexer push architecture:
//! - `IndexerRegistry`: a global registry of per-branch indexing tasks
//! - `run_branch_task`: a tokio task per branch that queries tdb-search
//!   for the last-indexed commit, gets the commit chain from Prolog via
//!   FFI dispatch, and streams NDJSON to tdb-search `/push` one commit
//!   at a time via pipe-based streaming.
//!
//! FFI predicates registered for Prolog:
//! - `indexer_notify(+Path, +BranchName)`: notify that a commit happened
//! - `indexer_set_config(+TdbSearchUrl, +AuthHeader)`: set tdb-search URL/auth
//! - `indexer_progress(+Path, +BranchName, -Progress)`: query progress
//! - `indexer_abort_domain(+Domain)`: abort all tasks for a domain

use std::collections::HashMap;
use std::os::fd::IntoRawFd;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use swipl::prelude::*;
use tokio::sync::{oneshot, mpsc};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::unix::pipe::Receiver as PipeReceiver;

use crate::dispatch::dispatch_queue;

// ───────────────────────── Data structures ─────────────────────────

/// A branch identified by its resource path and branch name.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BranchKey {
    pub path: String,
    pub branch: String,
}

/// Work item sent from a tokio branch task to the Prolog dispatch loop
/// via `DispatchMessage::Indexer`.
pub struct IndexerWork {
    pub path: String,
    pub branch: String,
    pub commit_id: String,
    pub output_write_fd: i32,
}

/// Work item for requesting the commit chain from Prolog via
/// `DispatchMessage::IndexerCommitChain`.
pub struct IndexerCommitChainRequest {
    pub path: String,
    pub branch: String,
    pub start_commit: String,
    pub response_tx: oneshot::Sender<Result<Vec<String>, String>>,
}

/// Progress tracking for a branch task.
pub struct BranchProgress {
    completed: AtomicU64,
    total: AtomicU64,
    status: StdMutex<IndexStatus>,
}

impl BranchProgress {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            completed: AtomicU64::new(0),
            total: AtomicU64::new(0),
            status: StdMutex::new(IndexStatus::Indexing),
        })
    }

    fn set_total(&self, n: u64) {
        self.total.store(n, Ordering::SeqCst);
    }

    fn reset_to_indexing(&self) {
        let mut s = self.status.lock().unwrap();
        *s = IndexStatus::Indexing;
        self.completed.store(0, Ordering::SeqCst);
    }

    fn increment_completed(&self) {
        self.completed.fetch_add(1, Ordering::SeqCst);
    }

    fn set_error(&self, msg: String) {
        let mut s = self.status.lock().unwrap();
        *s = IndexStatus::Error(msg);
    }

    fn set_completed(&self) {
        let mut s = self.status.lock().unwrap();
        *s = IndexStatus::Completed;
    }

    fn snapshot(&self) -> (u64, u64, IndexStatus) {
        let completed = self.completed.load(Ordering::SeqCst);
        let total = self.total.load(Ordering::SeqCst);
        let status = self.status.lock().unwrap().clone();
        (completed, total, status)
    }
}

/// Status of an indexing task.
#[derive(Clone, Debug)]
pub enum IndexStatus {
    Indexing,
    Completed,
    Error(String),
}

/// A running branch task.
struct BranchTask {
    progress: Arc<BranchProgress>,
    notify_count: AtomicU64,
    /// Cancellation token — when set, the task should exit.
    cancel: tokio_util::sync::CancellationToken,
}

/// The global indexer registry.
pub struct IndexerRegistry {
    tasks: StdMutex<HashMap<BranchKey, Arc<BranchTask>>>,
    tdb_search_url: StdMutex<Option<String>>,
    auth_header: StdMutex<Option<String>>,
    http_client: reqwest::Client,
}

impl IndexerRegistry {
    fn new() -> Self {
        Self {
            tasks: StdMutex::new(HashMap::new()),
            tdb_search_url: StdMutex::new(None),
            auth_header: StdMutex::new(None),
            http_client: reqwest::Client::builder()
                .timeout(std::time::Duration::from_secs(300))
                .build()
                .expect("failed to build reqwest client"),
        }
    }

    fn set_config(&self, url: String, auth: String) {
        *self.tdb_search_url.lock().unwrap() = Some(url);
        *self.auth_header.lock().unwrap() = Some(auth);
    }

    fn get_config(&self) -> Result<(String, String), String> {
        let url = self.tdb_search_url.lock().unwrap().clone()
            .ok_or("tdb_search_url not set")?;
        let auth = self.auth_header.lock().unwrap().clone()
            .ok_or("auth_header not set")?;
        Ok((url, auth))
    }

    /// Notify that a commit happened on a branch. If a task already exists,
    /// increment its notify_count. If not, create a new task.
    fn notify(&self, path: String, branch: String) -> Result<(), String> {
        let key = BranchKey { path: path.clone(), branch: branch.clone() };
        let mut tasks = self.tasks.lock().unwrap();

        if let Some(task) = tasks.get(&key) {
            // Task exists — increment notify_count, return immediately.
            task.notify_count.fetch_add(1, Ordering::SeqCst);
            return Ok(());
        }

        // No task — create one.
        let progress = BranchProgress::new();
        let cancel = tokio_util::sync::CancellationToken::new();
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            cancel: cancel.clone(),
        });
        tasks.insert(key.clone(), task.clone());

        let (tdb_search_url, auth_header) = self.get_config()?;
        let registry = INDEXER_REGISTRY.get()
            .ok_or("indexer registry not initialized")?
            .clone();
        let http_client = self.http_client.clone();

        // Spawn the branch task on the tokio runtime.
        tokio::spawn(async move {
            run_branch_task(
                key,
                tdb_search_url,
                auth_header,
                http_client,
                progress,
                registry,
                cancel,
            ).await;
        });

        Ok(())
    }

    /// Abort all tasks for a domain.
    /// Matches paths that are exactly the domain or have the domain as a
    /// prefix followed by '/' (e.g. "admin/db" matches "admin/db" and
    /// "admin/db/local/branch/main" but not "admin/dbfoo").
    fn abort_domain(&self, domain: &str) {
        let mut tasks = self.tasks.lock().unwrap();
        let keys_to_remove: Vec<BranchKey> = tasks
            .keys()
            .filter(|k| {
                k.path == domain
                    || k.path.starts_with(&format!("{}/", domain))
            })
            .cloned()
            .collect();
        for key in keys_to_remove {
            if let Some(task) = tasks.remove(&key) {
                task.cancel.cancel();
            }
        }
    }

    /// Get progress for a branch.
    fn progress(&self, path: &str, branch: &str) -> Option<(u64, u64, IndexStatus)> {
        let key = BranchKey { path: path.to_string(), branch: branch.to_string() };
        let tasks = self.tasks.lock().unwrap();
        tasks.get(&key).map(|t| t.progress.snapshot())
    }

    /// Remove a task from the registry (called when the task exits).
    fn remove_task(&self, key: &BranchKey) {
        self.tasks.lock().unwrap().remove(key);
    }
}

/// Global indexer registry singleton.
static INDEXER_REGISTRY: std::sync::OnceLock<Arc<IndexerRegistry>> = std::sync::OnceLock::new();

/// Get the global indexer registry, initializing it if needed.
fn indexer_registry() -> &'static Arc<IndexerRegistry> {
    INDEXER_REGISTRY.get_or_init(|| Arc::new(IndexerRegistry::new()))
}

// ───────────────────────── DispatchMessage additions ─────────────────────────
// These are defined in dispatch.rs but we need to add the new variants there.
// The dispatch loop handlers call Prolog predicates:
//   Indexer(IndexerWork) → calls indexer_process_commit/4
//   IndexerCommitChain(IndexerCommitChainRequest) → calls indexer_commit_chain/4

// ───────────────────────── run_branch_task ─────────────────────────

/// The per-branch indexing task.
///
/// 1. GET /last-indexed from tdb-search to find the starting commit
/// 2. Get commit chain from Prolog via FFI dispatch
/// 3. For each commit: create pipe, dispatch to Prolog, read NDJSON, POST /push
/// 4. At HEAD: mark completed, loop back if new commits arrived
async fn run_branch_task(
    key: BranchKey,
    tdb_search_url: String,
    auth_header: String,
    http_client: reqwest::Client,
    progress: Arc<BranchProgress>,
    registry: Arc<IndexerRegistry>,
    cancel: tokio_util::sync::CancellationToken,
) {
    loop {
        if cancel.is_cancelled() {
            registry.remove_task(&key);
            return;
        }

        // Reset progress for this iteration (status may be Completed
        // from a previous loop iteration if new commits arrived).
        progress.reset_to_indexing();

        // 1. Get starting commit from tdb-search /last-indexed
        let start_commit = match get_last_indexed(
            &http_client, &tdb_search_url, &key, &auth_header
        ).await {
            Ok(c) => c,
            Err(e) => {
                crate::log::log_error(format!(
                    "[indexer] get_last_indexed failed for {} {}: {}",
                    key.path, key.branch, e
                ));
                progress.set_error(e);
                registry.remove_task(&key);
                return;
            }
        };

        // 2. Get commit chain from Prolog via FFI dispatch
        let commit_chain = match get_commit_chain_via_dispatch(
            &key, &start_commit, &cancel
        ).await {
            Ok(chain) => chain,
            Err(e) => {
                crate::log::log_error(format!(
                    "[indexer] get_commit_chain failed for {} {}: {}",
                    key.path, key.branch, e
                ));
                progress.set_error(e);
                registry.remove_task(&key);
                return;
            }
        };

        progress.set_total(commit_chain.len() as u64);

        if commit_chain.is_empty() {
            // Already at HEAD — nothing to index.
            progress.set_completed();
        }

        // 3. Process each commit: pipe → dispatch → read NDJSON → POST /push
        for (i, commit_id) in commit_chain.iter().enumerate() {
            if cancel.is_cancelled() {
                registry.remove_task(&key);
                return;
            }

            // Parent commit: previous in chain, or start_commit for first.
            // If start_commit is empty (never indexed), first push omits parent.
            let parent = if i > 0 {
                Some(commit_chain[i - 1].as_str())
            } else if !start_commit.is_empty() {
                Some(start_commit.as_str())
            } else {
                None
            };

            // Create pipe for NDJSON streaming (Prolog → tokio).
            let (output_read, output_write) = match nix::unistd::pipe() {
                Ok(p) => p,
                Err(e) => {
                    crate::log::log_error(format!(
                        "[indexer] pipe creation failed for {} {}: {}",
                        key.path, key.branch, e
                    ));
                    progress.set_error(format!("pipe creation failed: {}", e));
                    registry.remove_task(&key);
                    return;
                }
            };

            let output_write_fd = output_write.into_raw_fd();

            let work = IndexerWork {
                path: key.path.clone(),
                branch: key.branch.clone(),
                commit_id: commit_id.clone(),
                output_write_fd,
            };

            // Send to dispatch queue — the Prolog engine thread will pick
            // this up and call indexer_process_commit/4 which writes NDJSON
            // to the pipe write end.
            let queue = dispatch_queue();
            if queue.send(crate::dispatch::DispatchMessage::Indexer(work)).await.is_err() {
                crate::log::log_error(format!(
                    "[indexer] dispatch queue send failed for {} {} commit {}",
                    key.path, key.branch, commit_id
                ));
                progress.set_error("dispatch queue closed".to_string());
                registry.remove_task(&key);
                return;
            }

            // Read NDJSON from the pipe read end and POST to tdb-search /push.
            let pipe_receiver = match PipeReceiver::from_owned_fd(output_read) {
                Ok(r) => r,
                Err(e) => {
                    crate::log::log_error(format!(
                        "[indexer] from_owned_fd failed for {} {} commit {}: {}",
                        key.path, key.branch, commit_id, e
                    ));
                    // output_read (OwnedFd) was consumed by from_owned_fd
                    // and automatically closed when it was dropped on error.
                    // The write end was sent to dispatch and may still be
                    // open in Prolog. The dispatch error handler will close it.
                    progress.set_error(format!("from_owned_fd failed: {}", e));
                    registry.remove_task(&key);
                    return;
                }
            };
            let reader = BufReader::new(pipe_receiver);
            let lines = reader.lines();

            let resp = post_push_stream(
                &http_client, &tdb_search_url, &key, &commit_id, parent,
                &auth_header, lines, &cancel
            ).await;

            match resp {
                Ok(_) => {
                    progress.increment_completed();
                }
                Err(e) => {
                    crate::log::log_error(format!(
                        "[indexer] post_push_stream failed for {} {} commit {}: {}",
                        key.path, key.branch, commit_id, e
                    ));
                    progress.set_error(e);
                    registry.remove_task(&key);
                    return;
                }
            }
        }

        // 4. At HEAD — mark complete
        progress.set_completed();

        // 5. Check if new commits arrived while we were processing.
        //    Atomically decrement notify_count; if >0, loop back.
        let tasks = registry.tasks.lock().unwrap();
        if let Some(task) = tasks.get(&key) {
            let prev = task.notify_count.fetch_sub(1, Ordering::SeqCst);
            if prev > 1 {
                // New commits arrived — loop back to step 1.
                drop(tasks);
                continue;
            } else {
                // No new commits — remove task and exit.
                drop(tasks);
                registry.remove_task(&key);
                return;
            }
        } else {
            // Task was already removed (e.g. by abort_domain) — exit.
            return;
        }
    }
}

// ───────────────────────── HTTP helpers ─────────────────────────

/// GET /last-indexed from tdb-search.
/// Returns the commit ID string, or empty string if never indexed (null).
async fn get_last_indexed(
    http_client: &reqwest::Client,
    tdb_search_url: &str,
    key: &BranchKey,
    auth_header: &str,
) -> Result<String, String> {
    let url = format!(
        "{}/last-indexed?domain={}&branch={}",
        tdb_search_url.trim_end_matches('/'),
        urlencoding::encode(&key.path),
        urlencoding::encode(&key.branch),
    );

    let resp = http_client
        .get(&url)
        .header("Authorization", auth_header)
        .timeout(std::time::Duration::from_secs(5))
        .send()
        .await
        .map_err(|e| format!("GET /last-indexed failed: {}", e))?;

    let status = resp.status();
    if !status.is_success() {
        let body = resp.text().await.unwrap_or_default();
        return Err(format!("GET /last-indexed returned {}: {}", status, body));
    }

    let body_text = resp
        .text()
        .await
        .map_err(|e| format!("failed to read /last-indexed response: {}", e))?;
    let body: serde_json::Value = serde_json::from_str(&body_text)
        .map_err(|e| format!("failed to parse /last-indexed response: {}", e))?;

    let commit = body.get("commit").and_then(|c| c.as_str());
    Ok(commit.unwrap_or("").to_string())
}

/// Get the commit chain from Prolog via FFI dispatch.
/// Sends an `IndexerCommitChain` message to the dispatch queue and waits
/// for the response via a oneshot channel.
async fn get_commit_chain_via_dispatch(
    key: &BranchKey,
    start_commit: &str,
    cancel: &tokio_util::sync::CancellationToken,
) -> Result<Vec<String>, String> {
    let (response_tx, response_rx) = oneshot::channel();

    let request = IndexerCommitChainRequest {
        path: key.path.clone(),
        branch: key.branch.clone(),
        start_commit: start_commit.to_string(),
        response_tx,
    };

    let queue = dispatch_queue();
    queue
        .send(crate::dispatch::DispatchMessage::IndexerCommitChain(request))
        .await
        .map_err(|_| "dispatch queue closed".to_string())?;

    tokio::select! {
        result = response_rx => {
            result.map_err(|_| "ffi response dropped".to_string())?
        }
        _ = cancel.cancelled() => {
            Err("task cancelled".to_string())
        }
    }
}

/// POST NDJSON stream to tdb-search /push.
///
/// Reads lines from the pipe reader and streams them as the request body.
/// If an error marker (`{"op":"Error",...}`) is detected in the stream,
/// appends `{"op":"Abort"}` as a final line before closing the request body.
///
/// Returns Ok(()) on success (2xx or 409), Err on failure.
async fn post_push_stream(
    http_client: &reqwest::Client,
    tdb_search_url: &str,
    key: &BranchKey,
    commit_id: &str,
    parent: Option<&str>,
    auth_header: &str,
    lines: tokio::io::Lines<BufReader<PipeReceiver>>,
    cancel: &tokio_util::sync::CancellationToken,
) -> Result<(), String> {
    use futures::StreamExt;
    use http_body_util::StreamBody;
    use http_body::Frame;

    let mut url = format!(
        "{}/push?domain={}&branch={}&target_commit={}",
        tdb_search_url.trim_end_matches('/'),
        urlencoding::encode(&key.path),
        urlencoding::encode(&key.branch),
        urlencoding::encode(commit_id),
    );
    if let Some(p) = parent {
        url.push_str(&format!("&parent_commit={}", urlencoding::encode(p)));
    }

    // Convert lines into a stream of bytes for the request body.
    // We need to detect error markers and append abort if found.
    let (body_tx, body_rx) = mpsc::channel::<Result<axum::body::Bytes, std::io::Error>>(64);

    let cancel_clone = cancel.clone();
    tokio::spawn(async move {
        let mut lines = lines;
        let mut error_detected = false;

        loop {
            tokio::select! {
                _ = cancel_clone.cancelled() => {
                    break;
                }
                line_result = lines.next_line() => {
                    match line_result {
                        Ok(Some(line)) => {
                            // Check for error marker.
                            if line.contains("\"op\":\"Error\"") {
                                error_detected = true;
                            }
                            let mut bytes = line.into_bytes();
                            bytes.push(b'\n');
                            if body_tx.send(Ok(axum::body::Bytes::from(bytes))).await.is_err() {
                                break;
                            }
                        }
                        Ok(None) => {
                            // Pipe closed — if error was detected, send abort.
                            if error_detected {
                                let abort = axum::body::Bytes::from(
                                    b"{\"op\":\"Abort\"}\n".to_vec()
                                );
                                let _ = body_tx.send(Ok(abort)).await;
                            }
                            break;
                        }
                        Err(_) => {
                            // Pipe read error — send abort if we can.
                            if error_detected {
                                let abort = axum::body::Bytes::from(
                                    b"{\"op\":\"Abort\"}\n".to_vec()
                                );
                                let _ = body_tx.send(Ok(abort)).await;
                            }
                            break;
                        }
                    }
                }
            }
        }
        drop(body_tx);
    });

    // Convert the mpsc receiver into a streaming body.
    let stream = tokio_stream::wrappers::ReceiverStream::new(body_rx);
    let body = StreamBody::new(stream.map(|result| {
        result.map(|bytes| Frame::data(bytes))
    }));

    let resp = http_client
        .post(&url)
        .header("Authorization", auth_header)
        .header("Content-Type", "application/x-ndjson")
        .body(reqwest::Body::wrap(body))
        .timeout(std::time::Duration::from_secs(300))
        .send()
        .await
        .map_err(|e| format!("POST /push failed: {}", e))?;

    let status = resp.status();
    if status.is_success() {
        Ok(())
    } else if status.as_u16() == 409 {
        // Conflict — commit already indexed or in-flight. Treat as success.
        Ok(())
    } else {
        let body = resp.text().await.unwrap_or_default();
        Err(format!("POST /push returned {}: {}", status, body))
    }
}

// ───────────────────────── FFI Predicates ─────────────────────────

predicates! {
    /// Set the tdb-search URL and auth header for the IndexerRegistry.
    /// Called once at startup from Prolog.
    /// Signature: indexer_set_config(+TdbSearchUrl, +AuthHeader)
    #[module("$appserver")]
    pub semidet fn indexer_set_config(_context, url_term, auth_term) {
        let url: String = url_term.get_ex()?;
        let auth: String = auth_term.get_ex()?;
        indexer_registry().set_config(url, auth);
        Ok(())
    }

    /// Notify the indexer that a commit happened on a branch.
    /// If a task already exists, increments notify_count and returns immediately.
    /// If no task exists, creates one and spawns run_branch_task.
    /// Signature: indexer_notify(+Path, +BranchName)
    #[module("$appserver")]
    pub semidet fn indexer_notify(_context, path_term, branch_term) {
        let path: String = path_term.get_ex()?;
        let branch: String = branch_term.get_ex()?;
        indexer_registry().notify(path, branch).map_err(|e| {
            crate::log::log_error(format!("[indexer] notify failed: {}", e));
            PrologError::Failure
        })
    }

    /// Query indexing progress for a branch.
    /// Returns a dict with status, completed, and total fields.
    /// Signature: indexer_progress(+Path, +BranchName, -Progress)
    #[module("$appserver")]
    pub semidet fn indexer_progress(context, path_term, branch_term, progress_term) {
        let path: String = path_term.get_ex()?;
        let branch: String = branch_term.get_ex()?;

        let registry = indexer_registry();
        match registry.progress(&path, &branch) {
            Some((completed, total, status)) => {
                let (status_atom, error_msg) = match status {
                    IndexStatus::Indexing => ("indexing", "".to_string()),
                    IndexStatus::Completed => ("completed", "".to_string()),
                    IndexStatus::Error(msg) => ("error", msg),
                };

                let progress_json = serde_json::json!({
                    "status": status_atom,
                    "completed": completed,
                    "total": total,
                    "error": error_msg,
                });

                let frame = context.open_frame();
                let tmp = frame.new_term_ref();
                context.serialize_to_term(&tmp, &progress_json)
                    .map_err(|_| PrologError::Failure)?;
                let result = progress_term.unify(&tmp);
                frame.close();
                result.map_err(|_| PrologError::Failure)
            }
            None => {
                // No task — return not_found status.
                let progress_json = serde_json::json!({
                    "status": "not_found",
                    "completed": 0,
                    "total": 0,
                    "error": "",
                });

                let frame = context.open_frame();
                let tmp = frame.new_term_ref();
                context.serialize_to_term(&tmp, &progress_json)
                    .map_err(|_| PrologError::Failure)?;
                let result = progress_term.unify(&tmp);
                frame.close();
                result.map_err(|_| PrologError::Failure)
            }
        }
    }

    /// Abort all indexing tasks for a domain.
    /// Called from post_delete_db_hook after io_delete_domain/2 does the
    /// tdb-search DELETE.
    /// Signature: indexer_abort_domain(+Domain)
    #[module("$appserver")]
    pub semidet fn indexer_abort_domain(_context, domain_term) {
        let domain: String = domain_term.get_ex()?;
        indexer_registry().abort_domain(&domain);
        Ok(())
    }
}

/// Register all indexer FFI predicates.
pub fn register() {
    register_indexer_set_config();
    register_indexer_notify();
    register_indexer_progress();
    register_indexer_abort_domain();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_indexer_registry_new() {
        let registry = IndexerRegistry::new();
        assert!(registry.get_config().is_err(), "config should be unset");
    }

    #[test]
    fn test_indexer_registry_set_get_config() {
        let registry = IndexerRegistry::new();
        registry.set_config("http://localhost:8080".to_string(), "Basic abc".to_string());
        let (url, auth) = registry.get_config().expect("config should be set");
        assert_eq!(url, "http://localhost:8080");
        assert_eq!(auth, "Basic abc");
    }

    #[test]
    fn test_branch_progress_initial_state() {
        let progress = BranchProgress::new();
        assert_eq!(progress.completed.load(Ordering::Relaxed), 0);
        assert_eq!(progress.total.load(Ordering::Relaxed), 0);
        let (_, _, status) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Indexing));
    }

    #[test]
    fn test_branch_progress_update() {
        let progress = BranchProgress::new();
        progress.set_total(10);
        progress.increment_completed();
        progress.increment_completed();
        progress.increment_completed();
        progress.increment_completed();
        progress.increment_completed();
        let (completed, total, status) = progress.snapshot();
        assert_eq!(completed, 5);
        assert_eq!(total, 10);
        assert!(matches!(status, IndexStatus::Indexing));
    }

    #[test]
    fn test_branch_progress_error() {
        let progress = BranchProgress::new();
        progress.set_error("test error".to_string());
        let (_, _, status) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Error(ref msg) if msg == "test error"));
    }

    #[test]
    fn test_branch_progress_completed() {
        let progress = BranchProgress::new();
        progress.set_completed();
        let (_, _, status) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Completed));
    }

    #[test]
    fn test_branch_progress_reset_to_indexing() {
        let progress = BranchProgress::new();
        progress.set_total(5);
        progress.increment_completed();
        progress.increment_completed();
        progress.set_completed();
        let (completed, _, status) = progress.snapshot();
        assert_eq!(completed, 2);
        assert!(matches!(status, IndexStatus::Completed));

        progress.reset_to_indexing();
        let (completed, total, status) = progress.snapshot();
        assert_eq!(completed, 0);
        assert_eq!(total, 5);
        assert!(matches!(status, IndexStatus::Indexing));
    }

    #[test]
    fn test_branch_key_equality() {
        let a = BranchKey { path: "admin/db".to_string(), branch: "main".to_string() };
        let b = BranchKey { path: "admin/db".to_string(), branch: "main".to_string() };
        let c = BranchKey { path: "admin/db".to_string(), branch: "dev".to_string() };
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_indexer_work_fields() {
        let work = IndexerWork {
            path: "admin/db".to_string(),
            branch: "main".to_string(),
            commit_id: "abc123".to_string(),
            output_write_fd: -1,
        };
        assert_eq!(work.path, "admin/db");
        assert_eq!(work.branch, "main");
        assert_eq!(work.commit_id, "abc123");
        assert_eq!(work.output_write_fd, -1);
    }
}
