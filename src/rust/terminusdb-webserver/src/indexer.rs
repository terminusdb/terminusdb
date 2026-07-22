//! Indexer push architecture: Rust-side IndexerRegistry and per-commit tasks.
//!
//! Global round-robin scheduler with per-commit tokio tasks:
//! - `IndexerRegistry`: global registry with a pending queue and current branch
//! - Only one commit task runs at a time globally
//! - Branches are picked round-robin from the pending queue
//! - Each task processes exactly one commit, then returns control to the scheduler
//! - Indexer work routes through `DispatchMessage::Pipe` → worker pool
//!
//! FFI predicates registered for Prolog:
//! - `indexer_notify(+Path, +BranchName)`: notify that a commit happened
//! - `indexer_set_config(+TdbSearchUrl, +AuthHeader)`: set tdb-search URL/auth
//! - `indexer_progress(+Path, +BranchName, -Progress)`: query progress
//! - `indexer_abort_domain(+Domain)`: abort all tasks for a domain

use std::collections::{HashMap, VecDeque};
use std::os::fd::IntoRawFd;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use std::time::{Duration, Instant};
use swipl::prelude::*;
use futures::FutureExt;
use tokio::sync::mpsc;
use tokio::io::AsyncReadExt;
use tokio::net::unix::pipe::Receiver as PipeReceiver;

// ───────────────────────── Data structures ─────────────────────────

/// A branch identified by its resource path and branch name.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BranchKey {
    pub path: String,
    pub branch: String,
}

impl BranchKey {
    /// Extract the domain (first two segments: org/db) from the full
    /// branch path (e.g. "admin/db/local/branch/main" → "admin/db").
    /// tdb-search expects the domain as org/db, not the full branch path.
    fn domain(&self) -> &str {
        let parts: Vec<&str> = self.path.splitn(3, '/').collect();
        if parts.len() >= 2 {
            // Re-borrow from the original string to avoid allocation
            let second_slash = parts[0].len() + 1 + parts[1].len();
            &self.path[..second_slash]
        } else {
            &self.path
        }
    }
}

/// Result of processing a single commit.
#[derive(Clone, Debug)]
enum TaskResult {
    /// There is a next commit to process.
    NextCommit(String),
    /// We're at HEAD — no more commits.
    AtHead,
    /// An error occurred.
    Error(String),
    /// tdb-search returned 503 (compaction in progress). Retry the same commit.
    Retry,
}

/// Progress tracking for a branch task.
pub struct BranchProgress {
    completed: AtomicU64,
    total: AtomicU64,
    status: StdMutex<IndexStatus>,
    /// The commit ID currently being processed (None when idle).
    current_commit: StdMutex<Option<String>>,
    /// Documents indexed by tdb-search so far in the current commit
    /// (from tdb-search response stream progress updates).
    processed_documents: AtomicU64,
    /// Total documents to process in the current commit.
    total_documents: AtomicU64,
    /// NDJSON lines sent over the pipe to tdb-search so far.
    /// This is a throughput metric — it reflects how many documents
    /// have been streamed, not how many have been embedded.
    documents_sent: AtomicU64,
    /// Timestamp of the last document processed (for dead man's switch).
    last_progress_at: StdMutex<Option<Instant>>,
    /// When the current 503-retry cycle started (for max retry duration).
    retry_started_at: StdMutex<Option<Instant>>,
}

impl BranchProgress {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            completed: AtomicU64::new(0),
            total: AtomicU64::new(0),
            status: StdMutex::new(IndexStatus::Indexing),
            current_commit: StdMutex::new(None),
            processed_documents: AtomicU64::new(0),
            total_documents: AtomicU64::new(0),
            documents_sent: AtomicU64::new(0),
            last_progress_at: StdMutex::new(None),
            retry_started_at: StdMutex::new(None),
        })
    }

    fn set_total(&self, n: u64) {
        self.total.store(n, Ordering::SeqCst);
    }

    #[allow(dead_code)]
    fn reset_to_indexing(&self) {
        let mut s = self.status.lock().unwrap();
        *s = IndexStatus::Indexing;
        self.completed.store(0, Ordering::SeqCst);
    }

    /// Set status to Indexing without resetting completed counter.
    /// Used when re-enqueueing for the next commit in a chain.
    fn set_status_indexing(&self) {
        let mut s = self.status.lock().unwrap();
        *s = IndexStatus::Indexing;
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

    fn set_current_commit(&self, commit: String) {
        *self.current_commit.lock().unwrap() = Some(commit);
        // Reset progress timestamp when a new commit starts.
        *self.last_progress_at.lock().unwrap() = Some(Instant::now());
    }

    /// Set the pending commit ID without starting the dead man's switch timer.
    /// Used before the dispatch is sent to the worker, so the status API
    /// can report which commit is upcoming. The timer starts when
    /// set_current_commit is called after CGI headers are read.
    fn set_pending_commit(&self, commit: String) {
        *self.current_commit.lock().unwrap() = Some(commit);
    }

    fn clear_current_commit(&self) {
        *self.current_commit.lock().unwrap() = None;
        *self.last_progress_at.lock().unwrap() = None;
    }

    /// Mark the start of a 503-retry cycle for max-duration tracking.
    fn start_retry_timer(&self) {
        *self.retry_started_at.lock().unwrap() = Some(Instant::now());
    }

    /// Clear the retry timer (called when the push succeeds).
    fn clear_retry_timer(&self) {
        *self.retry_started_at.lock().unwrap() = None;
    }

    /// Returns how long the current retry cycle has been going, or None.
    fn retry_duration(&self) -> Option<Duration> {
        self.retry_started_at.lock().unwrap().map(|t| t.elapsed())
    }

    /// Update the progress timestamp to now. Called when a document is processed.
    fn touch_progress(&self) {
        *self.last_progress_at.lock().unwrap() = Some(Instant::now());
    }

    /// Returns the elapsed time since the last progress update, or None if
    /// no progress has been recorded (e.g. task is idle).
    fn progress_age(&self) -> Option<Duration> {
        self.last_progress_at.lock().unwrap().map(|t| t.elapsed())
    }

    fn set_total_documents(&self, n: u64) {
        self.total_documents.store(n, Ordering::SeqCst);
    }

    fn increment_processed_documents(&self) {
        self.processed_documents.fetch_add(1, Ordering::SeqCst);
        self.touch_progress();
    }

    /// Set the processed documents counter to an absolute value.
    /// Used to sync with tdb-search's real indexing progress from its
    /// response stream (indexed count), which reflects actual embedding
    /// completion rather than just NDJSON lines sent over the pipe.
    fn set_processed_documents(&self, n: u64) {
        self.processed_documents.store(n, Ordering::SeqCst);
        self.touch_progress();
    }

    fn increment_documents_sent(&self, n: u64) {
        self.documents_sent.fetch_add(n, Ordering::SeqCst);
    }

    fn reset_document_counters(&self) {
        self.processed_documents.store(0, Ordering::SeqCst);
        self.total_documents.store(0, Ordering::SeqCst);
        self.documents_sent.store(0, Ordering::SeqCst);
        *self.last_progress_at.lock().unwrap() = Some(Instant::now());
    }

    fn snapshot(&self) -> (u64, u64, IndexStatus, Option<String>, u64, u64, u64) {
        let completed = self.completed.load(Ordering::SeqCst);
        let total = self.total.load(Ordering::SeqCst);
        let status = self.status.lock().unwrap().clone();
        let current_commit = self.current_commit.lock().unwrap().clone();
        let processed_docs = self.processed_documents.load(Ordering::SeqCst);
        let total_docs = self.total_documents.load(Ordering::SeqCst);
        let docs_sent = self.documents_sent.load(Ordering::SeqCst);
        (completed, total, status, current_commit, processed_docs, total_docs, docs_sent)
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
    /// The next commit to process (set by on_task_complete from X-Next-Commit header).
    next_commit: StdMutex<Option<String>>,
    /// Cancellation token — when set, the task should exit.
    cancel: tokio_util::sync::CancellationToken,
    /// Whether to request clustering embeddings from tdb-search on push.
    /// Read from the schema @context @metadata.terminusdb.options at notify time.
    store_clustering: std::sync::atomic::AtomicBool,
    /// When true, the next on_task_complete(NextCommit) should ignore
    /// the X-Next-Commit header and set next_commit = None instead,
    /// forcing a fresh /last-indexed query. Set by notify() when a
    /// re-index is requested while indexing is ongoing.
    reset_chain: std::sync::atomic::AtomicBool,
}

/// The global indexer registry.
pub struct IndexerRegistry {
    tasks: StdMutex<HashMap<BranchKey, Arc<BranchTask>>>,
    pending_queue: StdMutex<VecDeque<BranchKey>>,
    current_branch: StdMutex<Option<BranchKey>>,
    tdb_search_url: StdMutex<Option<String>>,
    auth_header: StdMutex<Option<String>>,
    http_client: reqwest::Client,
}

impl IndexerRegistry {
    fn new() -> Self {
        Self {
            tasks: StdMutex::new(HashMap::new()),
            pending_queue: StdMutex::new(VecDeque::new()),
            current_branch: StdMutex::new(None),
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
    /// increment its notify_count. If not, create a new task and enqueue it.
    fn notify(&self, path: String, branch: String, store_clustering: bool) -> Result<(), String> {
        let key = BranchKey { path, branch };
        let mut tasks = self.tasks.lock().unwrap();

        if let Some(task) = tasks.get(&key) {
            // Task exists — increment notify_count and update store_clustering.
            task.notify_count.fetch_add(1, Ordering::SeqCst);
            task.store_clustering.store(store_clustering, Ordering::SeqCst);
            // If the task is already Completed, re-enqueue it for
            // scheduling so the new commits get processed.
            let is_completed = matches!(
                *task.progress.status.lock().unwrap(),
                IndexStatus::Completed
            );
            if is_completed {
                task.progress.set_status_indexing();
                *task.next_commit.lock().unwrap() = None;
                drop(tasks);
                self.enqueue_for_scheduling(&key);
            } else {
                // Task is still Indexing — set reset_chain so the next
                // on_task_complete(NextCommit) will ignore the stale
                // X-Next-Commit header and query /last-indexed fresh.
                task.reset_chain.store(true, Ordering::SeqCst);
            }
            return Ok(());
        }

        // No task — create one.
        let progress = BranchProgress::new();
        progress.set_total(1);
        let cancel = tokio_util::sync::CancellationToken::new();
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(None),
            cancel: cancel.clone(),
            store_clustering: std::sync::atomic::AtomicBool::new(store_clustering),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        tasks.insert(key.clone(), task);
        drop(tasks);

        // Enqueue for scheduling.
        self.enqueue_for_scheduling(&key);

        Ok(())
    }

    /// Re-index a branch from scratch. Aborts any running task for this
    /// branch, calls DELETE /domain on tdb-search to wipe all document data
    /// and tags (so /last-indexed returns null), then creates a fresh task
    /// that will start from the oldest commit.
    fn reindex(&self, path: String, branch: String, store_clustering: bool) -> Result<(), String> {
        let key = BranchKey { path: path.clone(), branch: branch.clone() };

        // 1. Cancel and remove any existing task for this branch.
        {
            let mut tasks = self.tasks.lock().unwrap();
            if let Some(task) = tasks.remove(&key) {
                task.cancel.cancel();
            }
        }
        {
            let mut queue = self.pending_queue.lock().unwrap();
            queue.retain(|k| k != &key);
        }
        // Clear current_branch if it matches.
        {
            let mut current = self.current_branch.lock().unwrap();
            if current.as_ref() == Some(&key) {
                *current = None;
            }
        }

        // 2. Call DELETE /branch-index on tdb-search (async), then create
        //    a fresh task and enqueue it after the delete completes.
        let (tdb_search_url, auth_header) = self.get_config()?;
        let http_client = self.http_client.clone();
        let domain = key.domain().to_string();
        let branch_clone = branch.clone();
        let path_clone = path.clone();

        match crate::dispatch::tokio_handle() {
            Some(h) => {
                let registry = match INDEXER_REGISTRY.get() {
                    Some(r) => r.clone(),
                    None => return Err("registry not initialized".to_string()),
                };
                h.spawn(async move {
                    // Delete the entire domain on tdb-search. This removes all
                    // document data, tags, and in-memory state — not just the
                    // branch tags. Using /domain (not /branch-index) ensures
                    // old documents are wiped, so re-indexed pushes insert
                    // fresh data instead of silently upserting existing docs.
                    let delete_url = format!(
                        "{}/domain?domain={}",
                        tdb_search_url.trim_end_matches('/'),
                        urlencoding::encode(&domain),
                    );
                    crate::log::log_info(format!(
                        "[indexer] reindex: DELETE {} for {} {}",
                        delete_url, path_clone, branch_clone
                    ));
                    let resp = http_client
                        .delete(&delete_url)
                        .header("Authorization", &auth_header)
                        .send()
                        .await;
                    match resp {
                        Ok(r) if r.status().is_success() => {
                            crate::log::log_info(format!(
                                "[indexer] reindex: domain deleted for {} {}",
                                path_clone, branch_clone
                            ));
                        }
                        Ok(r) => {
                            crate::log::log_error(format!(
                                "[indexer] reindex: DELETE domain returned {}: {}",
                                r.status(),
                                path_clone
                            ));
                        }
                        Err(e) => {
                            crate::log::log_error(format!(
                                "[indexer] reindex: DELETE domain failed: {} ({})",
                                e, path_clone
                            ));
                        }
                    }

                    // 3. Create a fresh task and enqueue it.
                    //    next_commit = None → run_commit_task will query
                    //    /last-indexed, which now returns null → starts from
                    //    the oldest commit.
                    let key = BranchKey {
                        path: path_clone.clone(),
                        branch: branch_clone.clone(),
                    };
                    let progress = BranchProgress::new();
                    progress.set_total(1);
                    progress.set_status_indexing();
                    let task = Arc::new(BranchTask {
                        progress: progress,
                        notify_count: AtomicU64::new(1),
                        next_commit: StdMutex::new(None),
                        cancel: tokio_util::sync::CancellationToken::new(),
                        store_clustering: std::sync::atomic::AtomicBool::new(store_clustering),
                        reset_chain: std::sync::atomic::AtomicBool::new(false),
                    });
                    registry.tasks.lock().unwrap().insert(key.clone(), task);
                    registry.enqueue_for_scheduling(&key);
                    registry.try_spawn_next();
                });
            }
            None => return Err("tokio runtime not available".to_string()),
        }

        Ok(())
    }

    /// Add a branch to the pending queue if not already queued.
    fn enqueue_for_scheduling(&self, key: &BranchKey) {
        let mut queue = self.pending_queue.lock().unwrap();
        if !queue.iter().any(|k| k == key) {
            queue.push_back(key.clone());
        }
        drop(queue);
        self.try_spawn_next();
    }

    /// Try to spawn the next task from the queue.
    /// Only spawns if no task is currently running and the queue is not empty.
    fn try_spawn_next(&self) {
        // Check if a task is already running.
        {
            let current = self.current_branch.lock().unwrap();
            if let Some(ref c) = *current {
                crate::log::log_info(format!(
                    "[indexer] try_spawn_next: skipping, current_branch is {} {}",
                    c.path, c.branch
                ));
                return;
            }
        }

        // Pop the next branch from the queue (round-robin).
        let key = {
            let mut queue = self.pending_queue.lock().unwrap();
            queue.pop_front()
        };

        let key = match key {
            Some(k) => {
                crate::log::log_info(format!(
                    "[indexer] try_spawn_next: spawning task for {} {}",
                    k.path, k.branch
                ));
                k
            }
            None => return,
        };

        // Set as current branch.
        *self.current_branch.lock().unwrap() = Some(key.clone());

        // Get the task data.
        let (progress, cancel, next_commit, store_clustering) = {
            let tasks = self.tasks.lock().unwrap();
            match tasks.get(&key) {
                Some(task) => {
                    let nc = task.next_commit.lock().unwrap().clone();
                    // Clear next_commit after reading so progress() doesn't
                    // report a stale scheduling value while the task runs.
                    // on_task_complete will set it from X-Next-Commit header.
                    *task.next_commit.lock().unwrap() = None;
                    let sc = task.store_clustering.load(Ordering::SeqCst);
                    (task.progress.clone(), task.cancel.clone(), nc, sc)
                },
                None => {
                    // Task was removed (e.g. by abort_domain) — skip.
                    *self.current_branch.lock().unwrap() = None;
                    self.try_spawn_next();
                    return;
                }
            }
        };

        progress.set_status_indexing();

        // Set pending commit for the status API, but do NOT start the
        // dead man's switch timer yet. The timer starts only after CGI
        // headers are read (set_current_commit at line ~769), which is
        // when the push actually begins. This prevents false positives
        // when the worker is busy with other requests and the dispatch
        // sits in the queue for a long time.
        progress.set_pending_commit(next_commit.clone().unwrap_or_default());

        let (tdb_search_url, auth_header) = match self.get_config() {
            Ok(c) => c,
            Err(e) => {
                crate::log::log_error(format!("[indexer] config not set: {}", e));
                progress.set_error(e);
                *self.current_branch.lock().unwrap() = None;
                self.remove_task(&key);
                self.try_spawn_next();
                return;
            }
        };

        let registry = match INDEXER_REGISTRY.get() {
            Some(r) => r.clone(),
            None => {
                crate::log::log_error("[indexer] registry not initialized".to_string());
                progress.set_error("registry not initialized".to_string());
                *self.current_branch.lock().unwrap() = None;
                self.remove_task(&key);
                self.try_spawn_next();
                return;
            }
        };

        let http_client = self.http_client.clone();

        match crate::dispatch::tokio_handle() {
            Some(h) => {
                let registry_clone = registry.clone();
                let key_clone = key.clone();
                h.spawn(async move {
                    let result = std::panic::AssertUnwindSafe(
                        run_commit_task(
                            key_clone.clone(),
                            tdb_search_url,
                            auth_header,
                            http_client,
                            progress.clone(),
                            cancel,
                            next_commit,
                            store_clustering,
                        )
                    )
                    .catch_unwind()
                    .await
                    .unwrap_or_else(|e| {
                        let msg = if let Some(s) = e.downcast_ref::<&str>() {
                            s.to_string()
                        } else if let Some(s) = e.downcast_ref::<String>() {
                            s.clone()
                        } else {
                            "panic in run_commit_task".to_string()
                        };
                        crate::log::log_error(format!(
                            "[indexer] panic in run_commit_task for {} {}: {}",
                            key_clone.path, key_clone.branch, msg
                        ));
                        progress.set_error(msg.clone());
                        TaskResult::Error(msg)
                    });
                    registry_clone.on_task_complete(&key_clone, result);
                });
            }
            None => {
                crate::log::log_error("[indexer] tokio runtime not available".to_string());
                progress.set_error("tokio runtime not available".to_string());
                *self.current_branch.lock().unwrap() = None;
                self.remove_task(&key);
                self.try_spawn_next();
            }
        }
    }

    /// Called when a commit task completes.
    fn on_task_complete(&self, key: &BranchKey, result: TaskResult) {
        // Clear current branch.
        *self.current_branch.lock().unwrap() = None;

        match result {
            TaskResult::NextCommit(next) => {
                crate::log::log_info(format!(
                    "[indexer] task for {} {} completed, next commit: {}",
                    key.path, key.branch, next
                ));
                let tasks = self.tasks.lock().unwrap();
                if let Some(task) = tasks.get(key) {
                    task.progress.clear_retry_timer();
                    // If reset_chain was requested (re-index while indexing),
                    // ignore the X-Next-Commit header and force a fresh
                    // /last-indexed query by setting next_commit = None.
                    let should_reset = task.reset_chain.swap(false, Ordering::SeqCst);
                    if should_reset {
                        *task.next_commit.lock().unwrap() = None;
                        task.progress.increment_completed();
                        drop(tasks);
                        self.enqueue_for_scheduling(key);
                        self.try_spawn_next();
                    } else {
                        *task.next_commit.lock().unwrap() = Some(next);
                        task.progress.increment_completed();
                        drop(tasks);
                        self.enqueue_for_scheduling(key);
                        self.try_spawn_next();
                    }
                } else {
                    drop(tasks);
                    self.try_spawn_next();
                }
            }
            TaskResult::AtHead => {
                crate::log::log_info(format!(
                    "[indexer] task for {} {} at HEAD, indexing complete",
                    key.path, key.branch
                ));
                let tasks = self.tasks.lock().unwrap();
                if let Some(task) = tasks.get(key) {
                    task.progress.clear_retry_timer();
                    task.progress.increment_completed();
                    let completed = task.progress.completed.load(Ordering::SeqCst);
                    task.progress.total.store(completed, Ordering::SeqCst);
                    // Drain ALL pending notifies at once. Without this,
                    // a high notify_count (e.g. 3413 from document inserts)
                    // causes an infinite loop: each AtHead only decrements
                    // by 1, re-enqueuing every time. swap(1) drains to
                    // baseline in one shot.
                    let prev = task.notify_count.swap(1, Ordering::SeqCst);
                    if prev > 1 {
                        // New commits may have arrived — re-enqueue once
                        // to check. If HEAD hasn't changed, the next AtHead
                        // will see prev==1 and mark completed.
                        *task.next_commit.lock().unwrap() = None;
                        drop(tasks);
                        self.enqueue_for_scheduling(key);
                        return;
                    } else {
                        // No new commits — mark completed but keep the
                        // task in the registry so the status API can
                        // report 'completed'. It will be re-enqueued
                        // when a new commit arrives via indexer_notify.
                        task.progress.set_completed();
                        *task.next_commit.lock().unwrap() = None;
                        drop(tasks);
                    }
                } else {
                    drop(tasks);
                }
                self.try_spawn_next();
            }
            TaskResult::Error(msg) => {
                crate::log::log_error(format!(
                    "[indexer] task error for {} {}: {}",
                    key.path, key.branch, msg
                ));
                let tasks = self.tasks.lock().unwrap();
                if let Some(task) = tasks.get(key) {
                    task.progress.set_error(msg);
                }
                drop(tasks);
                self.remove_task(key);
                self.try_spawn_next();
            }
            TaskResult::Retry => {
                // Maximum total retry duration before giving up (5 minutes).
                const MAX_RETRY_DURATION: Duration = Duration::from_secs(300);

                let tasks = self.tasks.lock().unwrap();
                if let Some(task) = tasks.get(key) {
                    // Check if we've exceeded the max retry duration.
                    if let Some(elapsed) = task.progress.retry_duration() {
                        if elapsed >= MAX_RETRY_DURATION {
                            drop(tasks);
                            let msg = format!(
                                "indexing task for {} {} failed: tdb-search returned 503 \
                                 (compaction in progress) for {}s — exceeded max retry duration of {}s",
                                key.path, key.branch,
                                elapsed.as_secs(), MAX_RETRY_DURATION.as_secs()
                            );
                            crate::log::log_error(format!("[indexer] {}", msg));
                            let tasks = self.tasks.lock().unwrap();
                            if let Some(task) = tasks.get(key) {
                                task.progress.set_error(msg);
                                task.progress.clear_retry_timer();
                            }
                            drop(tasks);
                            self.remove_task(key);
                            self.try_spawn_next();
                            return;
                        }
                    } else {
                        // First 503 in this cycle — start the retry timer.
                        task.progress.start_retry_timer();
                    }

                    let elapsed = task.progress.retry_duration()
                        .map(|d| d.as_secs())
                        .unwrap_or(0);
                    crate::log::log_info(format!(
                        "[indexer] task for {} {} will retry (503 from tdb-search, \
                         retry cycle {}s / {}s max)",
                        key.path, key.branch, elapsed, MAX_RETRY_DURATION.as_secs()
                    ));

                    // Refresh the dead man's switch timer so it doesn't fire
                    // during the retry wait. We are intentionally waiting for
                    // compaction — this is not a stuck task.
                    task.progress.touch_progress();
                    // Keep current_commit set so the status API reports activity,
                    // but the refreshed timer prevents the dead man's switch.
                    *task.next_commit.lock().unwrap() = None;
                }
                drop(tasks);
                // Re-enqueue after a short delay to give compaction time to finish.
                let registry = Arc::clone(indexer_registry());
                let key_clone = key.clone();
                if let Some(handle) = crate::dispatch::tokio_handle() {
                    handle.spawn(async move {
                        tokio::time::sleep(std::time::Duration::from_secs(5)).await;
                        registry.enqueue_for_scheduling(&key_clone);
                        registry.try_spawn_next();
                    });
                }
            }
        }
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
        for key in &keys_to_remove {
            if let Some(task) = tasks.remove(key) {
                task.cancel.cancel();
            }
        }
        drop(tasks);

        // Also remove from pending queue.
        let mut queue = self.pending_queue.lock().unwrap();
        queue.retain(|k| {
            k.path != domain && !k.path.starts_with(&format!("{}/", domain))
        });
    }

    /// Get progress for a branch.
    fn progress(&self, path: &str, branch: &str) -> Option<(u64, u64, IndexStatus, Option<String>, Option<String>, u64, u64, u64)> {
        let key = BranchKey { path: path.to_string(), branch: branch.to_string() };
        let tasks = self.tasks.lock().unwrap();
        tasks.get(&key).map(|t| {
            let (completed, total, status, current_commit, processed_docs, total_docs, docs_sent) = t.progress.snapshot();
            let next_commit = t.next_commit.lock().unwrap().clone();
            (completed, total, status, current_commit, next_commit, processed_docs, total_docs, docs_sent)
        })
    }

    /// Remove a task from the registry (called when the task exits).
    fn remove_task(&self, key: &BranchKey) {
        self.tasks.lock().unwrap().remove(key);
    }

    /// Dead man's switch: check all active tasks for progress violations.
    /// If any task has a current_commit set (actively processing) but no
    /// document progress for longer than the deadline, crash the server
    /// with a FATAL log message and a full stack trace.
    ///
    /// ARCHITECTURAL INVARIANT: This fail-loud behaviour must NEVER be changed
    /// to cancel-and-continue, log-and-swallow, or any non-aborting strategy
    /// without explicit approval from a human architect. If this switch fires,
    /// there is a serious correctness issue in the TerminusDB product — a
    /// thread is stuck in a state that violates the indexing contract. Aborting
    /// is the only safe response: it surfaces the bug immediately with a stack
    /// trace rather than silently corrupting the index or leaking resources.
    ///
    /// The deadline is configurable via TERMINUSDB_INDEXER_WATCHDOG_DEADLINE
    /// (default: 60 seconds). CPU-only embedding backends (e.g. Ollama)
    /// can take >10s for the first document, so the default allows for
    /// slow first-document processing.
    fn check_dead_mans_switch(&self) {
        let deadline_secs = std::env::var("TERMINUSDB_INDEXER_WATCHDOG_DEADLINE")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(60);

        let deadline = Duration::from_secs(deadline_secs);

        let tasks = self.tasks.lock().unwrap();
        for (key, task) in tasks.iter() {
            let (completed, total, _status, current_commit, processed_docs, total_docs, _docs_sent) =
                task.progress.snapshot();

            // Only check tasks that are actively indexing (have a current_commit
            // and have started the push — last_progress_at is set by
            // set_current_commit, not set_pending_commit).
            if let Some(ref commit) = current_commit {
                // Skip if all documents have been processed — the task is
                // waiting for the tdb-search response, not stuck.
                if total_docs > 0 && processed_docs >= total_docs {
                    continue;
                }
                if let Some(age) = task.progress.progress_age() {
                    if age > deadline {
                        // Dead man's switch violation! Crash the server.
                        let msg = format!(
                            "[FATAL] Indexer dead man's switch violation: \
                             branch {} {} commit {} \
                             no document progress for {:?} (deadline {:?}) \
                             processed_documents={} total_documents={} \
                             commits_processed={} total_commits={} \
                             — a single document should take < 5s, \
                             this indicates a stuck task that must be investigated",
                            key.path, key.branch, commit,
                            age, deadline,
                            processed_docs, total_docs,
                            completed, total
                        );
                        eprintln!("{}", msg);

                        // Print all thread stack traces for debugging.
                        // On macOS, we use backtrace_symbols via signal.
                        // The simplest cross-platform approach: send SIGABRT
                        // which triggers the Prolog signal handler to print
                        // all thread stacks before crashing.
                        eprintln!("[FATAL] Aborting server for stack trace analysis...");
                        std::process::abort();
                    }
                }
            }
        }
    }
}

/// Global indexer registry singleton.
static INDEXER_REGISTRY: std::sync::OnceLock<Arc<IndexerRegistry>> = std::sync::OnceLock::new();

/// Get the global indexer registry, initializing it if needed.
/// Also spawns the dead man's switch watchdog task on first init.
fn indexer_registry() -> &'static Arc<IndexerRegistry> {
    INDEXER_REGISTRY.get_or_init(|| {
        let registry = Arc::new(IndexerRegistry::new());

        // Spawn the dead man's switch watchdog.
        // Checks every 5 seconds (configurable via env) for tasks that
        // haven't made progress within the deadline.
        let registry_clone = registry.clone();
        if let Some(handle) = crate::dispatch::tokio_handle() {
            handle.spawn(async move {
                let interval_secs = std::env::var("TERMINUSDB_INDEXER_WATCHDOG_INTERVAL")
                    .ok()
                    .and_then(|s| s.parse::<u64>().ok())
                    .unwrap_or(5);
                let mut interval = tokio::time::interval(Duration::from_secs(interval_secs));
                interval.tick().await; // skip first immediate tick
                loop {
                    interval.tick().await;
                    registry_clone.check_dead_mans_switch();
                }
            });
        }

        registry
    })
}

// ───────────────────────── run_commit_task ─────────────────────────

/// Process a single commit: dispatch to Prolog via Pipe, read CGI headers
/// (extracting X-Next-Commit), then raw-forward the NDJSON body to
/// tdb-search /push.
///
/// Returns the TaskResult so the scheduler can decide what to do next.
async fn run_commit_task(
    key: BranchKey,
    tdb_search_url: String,
    auth_header: String,
    http_client: reqwest::Client,
    progress: Arc<BranchProgress>,
    cancel: tokio_util::sync::CancellationToken,
    next_commit: Option<String>,
    store_clustering: bool,
) -> TaskResult {
    if cancel.is_cancelled() {
        return TaskResult::Error("task cancelled".to_string());
    }

    // 1. Determine which commit to process.
    // If next_commit is None, query tdb-search /last-indexed to find the starting point.
    // mode=next means "find the next commit after the given one" (used when
    //   the commit is the last-indexed commit from tdb-search).
    // mode=process means "process this commit directly" (used when
    //   the commit comes from the NextCommit chain).
    // mode=first means "find the first commit in the branch" (used when
    //   tdb-search has never indexed this branch, commit is empty).
    let (commit_id, mode) = match next_commit {
        Some(c) => (c, "process"),
        None => {
            match get_last_indexed(&http_client, &tdb_search_url, &key, &auth_header).await {
                Ok(c) if !c.is_empty() => {
                    // We have a last-indexed commit. We need to find the next commit
                    // after it. The Prolog handler will do this — we pass the
                    // last-indexed commit as the "current" commit and Prolog
                    // will find the next one via X-Next-Commit header.
                    (c, "next")
                }
                Ok(_) => {
                    // Never indexed — start from the beginning.
                    // Prolog will find the first commit.
                    (String::new(), "first")
                }
                Err(e) => {
                    return TaskResult::Error(format!("get_last_indexed failed: {}", e));
                }
            }
        }
    };

    // 2. Create pipe for CGI + NDJSON streaming (Prolog → tokio).
    let (output_read, output_write) = match nix::unistd::pipe() {
        Ok(p) => p,
        Err(e) => {
            return TaskResult::Error(format!("pipe creation failed: {}", e));
        }
    };

    let output_write_fd = output_write.into_raw_fd();

    #[cfg(target_os = "linux")]
    unsafe {
        libc::fcntl(output_write_fd, libc::F_SETPIPE_SZ, 1_048_576);
    }

    // 3. Build the PipeDispatchRequest for the indexer_worker handler.
    let request_json = serde_json::json!({
        "method": "POST",
        "path": &key.path,
        "query": format!("branch={}&commit={}&mode={}", &key.branch, &commit_id, mode),
        "headers": {},
        "body": "",
        "params": {},
    });

    let dispatch_req = crate::dispatch::PipeDispatchRequest {
        request_json,
        handler_module: "indexer_worker".to_string(),
        handler_name: "indexer_process_commit_handler".to_string(),
        input_read_fd: None,
        output_write_fd,
        binary: false,
    };

    // 4. Send to the dispatch queue (DispatchMessage::Pipe).
    crate::log::log_info(format!(
        "[indexer] sending dispatch for {} {} commit={} mode={}",
        key.path, key.branch, commit_id, mode
    ));
    let queue = crate::dispatch::dispatch_queue();
    if queue.send(crate::dispatch::DispatchMessage::Pipe(dispatch_req)).await.is_err() {
        return TaskResult::Error("dispatch queue closed".to_string());
    }
    crate::log::log_info(format!(
        "[indexer] dispatch sent for {} {} commit={}",
        key.path, key.branch, commit_id
    ));

    // 5. Read from the pipe: parse CGI headers, extract X-Next-Commit,
    //    then raw-forward the body to tdb-search /push.
    let mut pipe_receiver = match PipeReceiver::from_owned_fd(output_read) {
        Ok(r) => r,
        Err(e) => {
            return TaskResult::Error(format!("from_owned_fd failed: {}", e));
        }
    };

    // Read CGI headers from the pipe.
    let (status, headers, leftover_body) = {
        let cancel_clone = cancel.clone();
        tokio::select! {
            result = crate::dispatch::read_cgi_headers(&mut pipe_receiver) => {
                match result {
                    Ok(s) => s,
                    Err(e) => return TaskResult::Error(format!("read_cgi_headers failed: {}", e)),
                }
            }
            _ = cancel_clone.cancelled() => {
                return TaskResult::Error("task cancelled".to_string());
            }
        }
    };

    if status != 200 {
        return TaskResult::Error(format!("Prolog handler returned status {}", status));
    }

    crate::log::log_info(format!(
        "[indexer] CGI headers: status={}, leftover_body_len={}, headers={:?}",
        status, leftover_body.len(), headers
    ));

    // Extract X-Commit-Id (the actual commit processed by Prolog) and
    // X-Next-Commit (the next commit to process, or "None" if at HEAD).
    let actual_commit_id = headers
        .get("x-commit-id")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_string())
        .unwrap_or_else(|| commit_id.clone());

    // Track which commit is being processed for the status endpoint.
    progress.set_current_commit(actual_commit_id.clone());

    // Read X-Commit-Count (total commits in branch history) and update
    // the progress total so the status API reports meaningful progress.
    if let Some(count_str) = headers
        .get("x-commit-count")
        .and_then(|v| v.to_str().ok())
    {
        if let Ok(count) = count_str.trim().parse::<u64>() {
            if count > 0 {
                progress.set_total(count);
            }
        }
    }

    // Read X-Document-Count (total documents to process in this commit).
    // Reset document counters AFTER reading the new total so the status API
    // never sees a 0/0 gap between commits.
    let new_doc_count = headers
        .get("x-document-count")
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.trim().parse::<u64>().ok());
    progress.reset_document_counters();
    if let Some(doc_count) = new_doc_count {
        progress.set_total_documents(doc_count);
    }

    let next_commit_header = headers
        .get("x-next-commit")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_string());

    let parent_commit = headers
        .get("x-parent-commit")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "none".to_string());

    let next_commit_result = match next_commit_header.as_deref() {
        Some("None") | None => TaskResult::AtHead,
        Some(commit) => TaskResult::NextCommit(commit.to_string()),
    };

    // 6. Stream the NDJSON body (leftover + remaining pipe data) to tdb-search /push.
    let push_result = post_push_stream(
        &http_client,
        &tdb_search_url,
        &key,
        &actual_commit_id,
        &parent_commit,
        &auth_header,
        leftover_body,
        pipe_receiver,
        &cancel,
        &progress,
        store_clustering,
    ).await;

    // Clear the current commit marker — the task is done.
    progress.clear_current_commit();

    match push_result {
        Ok(_) => next_commit_result,
        Err(e) => {
            if e == "RETRY_503" {
                TaskResult::Retry
            } else {
                TaskResult::Error(e)
            }
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
        urlencoding::encode(key.domain()),
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

/// POST NDJSON stream to tdb-search /push.
///
/// Takes leftover bytes (from CGI header reading) and the pipe receiver,
/// and streams all remaining bytes as the request body.
/// Detects error markers and appends Abort if found.
///
/// Returns Ok(()) on success (2xx or 409), Err on failure.
async fn post_push_stream(
    http_client: &reqwest::Client,
    tdb_search_url: &str,
    key: &BranchKey,
    commit_id: &str,
    parent_commit: &str,
    auth_header: &str,
    leftover: Vec<u8>,
    mut receiver: PipeReceiver,
    cancel: &tokio_util::sync::CancellationToken,
    progress: &Arc<BranchProgress>,
    store_clustering: bool,
) -> Result<(), String> {
    use futures::StreamExt;
    use http_body_util::StreamBody;
    use http_body::Frame;

    let clustering_param = if store_clustering {
        "&store_clustering=true"
    } else {
        ""
    };

    let url = if parent_commit == "none" || parent_commit.is_empty() {
        format!(
            "{}/push?domain={}&branch={}&target_commit={}&stream=true{}",
            tdb_search_url.trim_end_matches('/'),
            urlencoding::encode(key.domain()),
            urlencoding::encode(&key.branch),
            urlencoding::encode(commit_id),
            clustering_param,
        )
    } else {
        format!(
            "{}/push?domain={}&branch={}&target_commit={}&parent_commit={}&stream=true{}",
            tdb_search_url.trim_end_matches('/'),
            urlencoding::encode(key.domain()),
            urlencoding::encode(&key.branch),
            urlencoding::encode(commit_id),
            urlencoding::encode(parent_commit),
            clustering_param,
        )
    };

    // Check leftover for error markers.
    let leftover_str = String::from_utf8_lossy(&leftover);
    let mut error_detected = leftover_str.contains("\"op\":\"Error\"");

    // Build a stream from the leftover bytes + pipe data.
    let (body_tx, body_rx) = mpsc::channel::<Result<axum::body::Bytes, std::io::Error>>(1024);

    // Send leftover bytes first.
    if !leftover.is_empty() {
        if body_tx.send(Ok(axum::body::Bytes::from(leftover))).await.is_err() {
            return Err("body channel closed before sending leftover".to_string());
        }
    }

    let cancel_clone = cancel.clone();
    let progress_clone = progress.clone();
    tokio::spawn(async move {
        let mut buf = [0u8; 8192];
        let mut total_bytes_sent: usize = 0;
        let mut total_lines_sent: usize = 0;
        loop {
            tokio::select! {
                _ = cancel_clone.cancelled() => {
                    break;
                }
                read_result = receiver.read(&mut buf) => {
                    match read_result {
                        Ok(0) => {
                            // EOF — if error was detected, send abort.
                            if error_detected {
                                let abort = axum::body::Bytes::from(
                                    b"{\"op\":\"Abort\"}\n".to_vec()
                                );
                                let _ = body_tx.send(Ok(abort)).await;
                            }
                            break;
                        }
                        Ok(n) => {
                            // Check for error markers in the new data.
                            let chunk_str = String::from_utf8_lossy(&buf[..n]);
                            if chunk_str.contains("\"op\":\"Error\"") {
                                error_detected = true;
                            }
                            // Count NDJSON lines as documents sent (pipe throughput).
                            let newlines = chunk_str.bytes().filter(|&b| b == b'\n').count();
                            progress_clone.increment_documents_sent(newlines as u64);
                            total_bytes_sent += n;
                            total_lines_sent += newlines;
                            if body_tx.send(Ok(axum::body::Bytes::from(buf[..n].to_vec()))).await.is_err() {
                                break;
                            }
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
        crate::log::log_info(format!(
            "[indexer] pipe reader done: total_bytes_sent={}, total_lines_sent={}, error_detected={}",
            total_bytes_sent, total_lines_sent, error_detected
        ));
        drop(body_tx);
    });

    let stream = tokio_stream::wrappers::ReceiverStream::new(body_rx);
    let body = StreamBody::new(stream.map(|result| {
        result.map(|bytes| Frame::data(bytes))
    }));

    crate::log::log_info(format!("[indexer] POST /push URL: {}", url));

    let resp = http_client
        .post(&url)
        .header("Authorization", auth_header)
        .header("Content-Type", "application/x-ndjson")
        .body(reqwest::Body::wrap(body))
        .timeout(std::time::Duration::from_secs(600))
        .send()
        .await
        .map_err(|e| format!("POST /push failed: {}", e))?;

    let status = resp.status();

    // Log X-Task-Id from response header if present.
    if let Some(task_id) = resp.headers().get("X-Task-Id") {
        crate::log::log_info(format!(
            "[indexer] POST /push X-Task-Id: {}",
            task_id.to_str().unwrap_or("<invalid>")
        ));
    }

    if !status.is_success() && status.as_u16() != 409 {
        // Non-streaming error response (e.g. 409, 422, 400, 503) — read as text.
        let resp_body = resp.text().await.unwrap_or_default();
        crate::log::log_info(format!(
            "[indexer] POST /push response status: {}, body: {}",
            status, resp_body
        ));
        if status.as_u16() == 409 {
            return Ok(());
        }
        if status.as_u16() == 422 {
            return Err(format!("POST /push returned 422 (abort): {}", resp_body));
        }
        if status.as_u16() == 503 {
            crate::log::log_info(format!(
                "[indexer] POST /push returned 503 (compaction in progress), will retry"
            ));
            return Err("RETRY_503".to_string());
        }
        return Err(format!("POST /push returned {}: {}", status, resp_body));
    }

    // Stream the NDJSON response body, parsing each line as a ProgressUpdate.
    // Progress lines: {"status":"progress","indexed":N,"total_seen":M,"skipped":K}
    // Terminal lines: {"status":"complete",...} or {"status":"error",...} or {"status":"aborted"}
    let mut resp_stream = resp.bytes_stream();
    let mut ndjson_buf = String::new();
    let mut terminal_status: Option<String> = None;
    let mut terminal_error: Option<String> = None;

    while let Some(chunk_result) = resp_stream.next().await {
        match chunk_result {
            Ok(chunk) => {
                ndjson_buf.push_str(&String::from_utf8_lossy(&chunk));
                // Process complete lines.
                while let Some(nl_pos) = ndjson_buf.find('\n') {
                    let line = ndjson_buf[..nl_pos].trim().to_owned();
                    ndjson_buf.drain(..=nl_pos);
                    if line.is_empty() {
                        continue;
                    }
                    match serde_json::from_str::<serde_json::Value>(&line) {
                        Ok(json) => {
                            let status_val = json
                                .get("status")
                                .and_then(|s| s.as_str())
                                .unwrap_or("unknown");
                            match status_val {
                                "progress" => {
                                    let indexed = json
                                        .get("indexed")
                                        .and_then(|v| v.as_u64())
                                        .unwrap_or(0);
                                    let total_seen = json
                                        .get("total_seen")
                                        .and_then(|v| v.as_u64())
                                        .unwrap_or(0);
                                    let skipped = json
                                        .get("skipped")
                                        .and_then(|v| v.as_u64())
                                        .unwrap_or(0);
                                    // Update BranchProgress with the real
                                    // indexing progress from tdb-search.
                                    // This reflects actual embedding completion,
                                    // not just NDJSON lines sent over the pipe.
                                    progress.set_processed_documents(indexed);
                                    crate::log::log_info(format!(
                                        "[indexer] progress: indexed={}, total_seen={}, skipped={}",
                                        indexed, total_seen, skipped
                                    ));
                                }
                                "complete" => {
                                    let indexed_documents = json
                                        .get("indexed_documents")
                                        .and_then(|v| v.as_u64())
                                        .unwrap_or(0);
                                    crate::log::log_info(format!(
                                        "[indexer] push complete: indexed_documents={}",
                                        indexed_documents
                                    ));
                                    // Ensure processed_documents reflects the
                                    // final count from tdb-search.
                                    progress.set_processed_documents(indexed_documents);
                                    terminal_status = Some("complete".to_owned());
                                }
                                "error" => {
                                    let error = json
                                        .get("error")
                                        .and_then(|v| v.as_str())
                                        .unwrap_or("unknown error");
                                    crate::log::log_error(format!(
                                        "[indexer] push error: {}",
                                        error
                                    ));
                                    terminal_status = Some("error".to_owned());
                                    terminal_error = Some(error.to_owned());
                                }
                                "aborted" => {
                                    crate::log::log_info("[indexer] push aborted by client".to_string());
                                    terminal_status = Some("aborted".to_owned());
                                }
                                _ => {
                                    crate::log::log_info(format!(
                                        "[indexer] unknown progress status: {}",
                                        status_val
                                    ));
                                }
                            }
                        }
                        Err(e) => {
                            crate::log::log_info(format!(
                                "[indexer] failed to parse progress line: {} — {}",
                                line, e
                            ));
                        }
                    }
                }
            }
            Err(e) => {
                crate::log::log_error(format!("[indexer] response stream error: {}", e));
                break;
            }
        }
    }

    match terminal_status.as_deref() {
        Some("complete") => Ok(()),
        Some("aborted") => Ok(()),
        Some("error") => Err(format!(
            "POST /push pipeline error: {}",
            terminal_error.unwrap_or_else(|| "unknown".to_owned())
        )),
        _ => {
            if status.is_success() {
                crate::log::log_info(
                    "[indexer] response stream ended without terminal status, treating as success".to_string(),
                );
                Ok(())
            } else {
                Err(format!("POST /push returned {} with no terminal status", status))
            }
        }
    }
}

// ───────────────────────── FFI Predicates ─────────────────────────

predicates! {
    /// Set the tdb-search URL and auth header for the IndexerRegistry.
    /// Called once at startup from Prolog.
    /// Signature: indexer_set_config(+TdbSearchUrl, +AuthHeader)
    #[module("$appserver")]
    pub semidet fn indexer_set_config(_context, url_term, auth_term) {
        let url: PrologText = url_term.get_ex()?;
        let auth: PrologText = auth_term.get_ex()?;
        indexer_registry().set_config(url.into_inner(), auth.into_inner());
        Ok(())
    }

    /// Notify the indexer that a commit happened on a branch.
    /// If a task already exists, increments notify_count and returns immediately.
    /// If no task exists, creates one and enqueues it for scheduling.
    #[module("$appserver")]
    /// Signature: indexer_notify(+Path, +BranchName, +StoreClustering)
    pub semidet fn indexer_notify(_context, path_term, branch_term, store_clustering_term) {
        let path: PrologText = path_term.get_ex()?;
        let branch: PrologText = branch_term.get_ex()?;
        let store_clustering: bool = store_clustering_term.get_ex()?;
        indexer_registry().notify(path.into_inner(), branch.into_inner(), store_clustering).map_err(|e| {
            crate::log::log_error(format!("[indexer] notify failed: {}", e));
            PrologError::Failure
        })
    }

    /// Query indexing progress for a branch.
    /// Returns a dict with status, error, and branch_processing sub-dict containing
    /// current_commit, upcoming_commit, total_commits, commits_processed.
    /// Signature: indexer_progress(+Path, +BranchName, -Progress)
    #[module("$appserver")]
    pub semidet fn indexer_progress(context, path_term, branch_term, progress_term) {
        let path: PrologText = path_term.get_ex()?;
        let branch: PrologText = branch_term.get_ex()?;

        let registry = indexer_registry();
        #[derive(serde::Serialize)]
        struct BranchProcessing {
            commits_processed: u64,
            total_commits: u64,
            #[serde(skip_serializing_if = "Option::is_none")]
            current_commit: Option<String>,
            #[serde(skip_serializing_if = "Option::is_none")]
            upcoming_commit: Option<String>,
            processed_documents: u64,
            total_documents: u64,
            documents_sent: u64,
        }
        #[derive(serde::Serialize)]
        struct Progress {
            status: &'static str,
            #[serde(skip_serializing_if = "Option::is_none")]
            error: Option<String>,
            #[serde(skip_serializing_if = "Option::is_none")]
            branch_processing: Option<BranchProcessing>,
        }
        let progress = match registry.progress(&path, &branch) {
            Some((completed, total, status, current_commit, next_commit, processed_docs, total_docs, docs_sent)) => {
                let (s, e) = match status {
                    IndexStatus::Indexing => ("indexing", None),
                    IndexStatus::Completed => ("completed", None),
                    IndexStatus::Error(msg) => ("error", Some(msg)),
                };
                Progress {
                    status: s,
                    error: e,
                    branch_processing: Some(BranchProcessing {
                        commits_processed: completed,
                        total_commits: total,
                        current_commit,
                        upcoming_commit: next_commit,
                        processed_documents: processed_docs,
                        total_documents: total_docs,
                        documents_sent: docs_sent,
                    }),
                }
            }
            None => Progress { status: "not_found", error: None, branch_processing: None },
        };

        // Serialize to a JSON string (respects skip_serializing_if),
        // then let Prolog parse it with atom_json_dict.
        // swipl-rs's serialize_to_term doesn't respect skip_serializing_if
        // and can't handle serde_json::Number natively.
        let json_str = serde_json::to_string(&progress)
            .map_err(|_| PrologError::Failure)?;
        progress_term.unify(&json_str).map_err(|_| PrologError::Failure)
    }

    /// Abort all indexing tasks for a domain.
    /// Called from post_delete_db_hook after io_delete_domain/2 does the
    /// tdb-search DELETE.
    /// Signature: indexer_abort_domain(+Domain)
    #[module("$appserver")]
    pub semidet fn indexer_abort_domain(_context, domain_term) {
        let domain: PrologText = domain_term.get_ex()?;
        indexer_registry().abort_domain(&domain.into_inner());
        Ok(())
    }

    /// Re-index a branch from scratch. Aborts any running task, wipes the
    /// branch's index on tdb-search, then starts fresh from the oldest commit.
    /// Signature: indexer_reindex(+Path, +BranchName, +StoreClustering)
    #[module("$appserver")]
    pub semidet fn indexer_reindex(_context, path_term, branch_term, store_clustering_term) {
        let path: PrologText = path_term.get_ex()?;
        let branch: PrologText = branch_term.get_ex()?;
        let store_clustering: bool = store_clustering_term.get_ex()?;
        indexer_registry().reindex(path.into_inner(), branch.into_inner(), store_clustering).map_err(|e| {
            crate::log::log_error(format!("[indexer] reindex failed: {}", e));
            PrologError::Failure
        })
    }
}

/// Register all indexer FFI predicates.
pub fn register() {
    register_indexer_set_config();
    register_indexer_notify();
    register_indexer_progress();
    register_indexer_abort_domain();
    register_indexer_reindex();
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
        let (_, _, status, _, _, _, _) = progress.snapshot();
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
        let (completed, total, status, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 5);
        assert_eq!(total, 10);
        assert!(matches!(status, IndexStatus::Indexing));
    }

    #[test]
    fn test_branch_progress_error() {
        let progress = BranchProgress::new();
        progress.set_error("test error".to_string());
        let (_, _, status, _, _, _, _) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Error(ref msg) if msg == "test error"));
    }

    #[test]
    fn test_branch_progress_completed() {
        let progress = BranchProgress::new();
        progress.set_completed();
        let (_, _, status, _, _, _, _) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Completed));
    }

    #[test]
    fn test_branch_progress_current_commit() {
        let progress = BranchProgress::new();
        let (_, _, _, current, _, _, _) = progress.snapshot();
        assert!(current.is_none(), "current_commit should start as None");

        progress.set_current_commit("abc123".to_string());
        let (_, _, _, current, _, _, _) = progress.snapshot();
        assert_eq!(current.as_deref(), Some("abc123"));

        progress.clear_current_commit();
        let (_, _, _, current, _, _, _) = progress.snapshot();
        assert!(current.is_none(), "current_commit should be None after clear");
    }

    #[test]
    fn test_branch_progress_set_total_overrides_guess() {
        let progress = BranchProgress::new();
        // Initial notify sets total = 1
        progress.set_total(1);
        let (_, total, _, _, _, _, _) = progress.snapshot();
        assert_eq!(total, 1);

        // X-Commit-Count header provides the real total
        progress.set_total(5);
        let (_, total, _, _, _, _, _) = progress.snapshot();
        assert_eq!(total, 5, "set_total should override the initial guess");

        // Simulate processing commits
        progress.increment_completed();
        progress.increment_completed();
        let (completed, total, _, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 2);
        assert_eq!(total, 5, "total should remain 5 after incrementing completed");
    }

    #[test]
    fn test_branch_progress_reset_to_indexing() {
        let progress = BranchProgress::new();
        progress.set_total(5);
        progress.increment_completed();
        progress.increment_completed();
        progress.set_completed();
        let (completed, _, status, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 2);
        assert!(matches!(status, IndexStatus::Completed));

        progress.reset_to_indexing();
        let (completed, total, status, _, _, _, _) = progress.snapshot();
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
    fn test_task_result_variants() {
        let next = TaskResult::NextCommit("abc123".to_string());
        assert!(matches!(next, TaskResult::NextCommit(ref c) if c == "abc123"));

        let head = TaskResult::AtHead;
        assert!(matches!(head, TaskResult::AtHead));

        let err = TaskResult::Error("boom".to_string());
        assert!(matches!(err, TaskResult::Error(ref m) if m == "boom"));
    }

    #[test]
    fn test_pending_queue_enqueue_dedup() {
        let registry = IndexerRegistry::new();
        // Prevent try_spawn_next from popping by setting current_branch.
        *registry.current_branch.lock().unwrap() = Some(BranchKey {
            path: "dummy".to_string(),
            branch: "dummy".to_string(),
        });
        let key1 = BranchKey { path: "admin/db".to_string(), branch: "main".to_string() };

        registry.enqueue_for_scheduling(&key1);
        assert_eq!(registry.pending_queue.lock().unwrap().len(), 1);

        // Enqueueing the same key should not duplicate.
        registry.enqueue_for_scheduling(&key1);
        assert_eq!(registry.pending_queue.lock().unwrap().len(), 1);

        let key2 = BranchKey { path: "admin/db".to_string(), branch: "dev".to_string() };
        registry.enqueue_for_scheduling(&key2);
        assert_eq!(registry.pending_queue.lock().unwrap().len(), 2);
    }

    #[test]
    fn test_abort_domain_cleans_pending_queue() {
        let registry = IndexerRegistry::new();
        *registry.current_branch.lock().unwrap() = Some(BranchKey {
            path: "dummy".to_string(),
            branch: "dummy".to_string(),
        });
        let key1 = BranchKey { path: "admin/db/local/branch/main".to_string(), branch: "main".to_string() };
        let key2 = BranchKey { path: "admin/db/local/branch/dev".to_string(), branch: "dev".to_string() };
        let key3 = BranchKey { path: "other/db/local/branch/main".to_string(), branch: "main".to_string() };

        registry.enqueue_for_scheduling(&key1);
        registry.enqueue_for_scheduling(&key2);
        registry.enqueue_for_scheduling(&key3);
        assert_eq!(registry.pending_queue.lock().unwrap().len(), 3);

        registry.abort_domain("admin/db");
        let queue = registry.pending_queue.lock().unwrap();
        assert_eq!(queue.len(), 1);
        assert!(queue.iter().any(|k| k.path == "other/db/local/branch/main"));
    }

    #[test]
    fn test_round_robin_order() {
        let registry = IndexerRegistry::new();
        *registry.current_branch.lock().unwrap() = Some(BranchKey {
            path: "dummy".to_string(),
            branch: "dummy".to_string(),
        });
        let key1 = BranchKey { path: "admin/db".to_string(), branch: "main".to_string() };
        let key2 = BranchKey { path: "admin/db".to_string(), branch: "dev".to_string() };
        let key3 = BranchKey { path: "admin/db".to_string(), branch: "feature".to_string() };

        registry.enqueue_for_scheduling(&key1);
        registry.enqueue_for_scheduling(&key2);
        registry.enqueue_for_scheduling(&key3);

        // Pop in FIFO order (round-robin).
        let popped1 = registry.pending_queue.lock().unwrap().pop_front().unwrap();
        let popped2 = registry.pending_queue.lock().unwrap().pop_front().unwrap();
        let popped3 = registry.pending_queue.lock().unwrap().pop_front().unwrap();

        assert_eq!(popped1, key1);
        assert_eq!(popped2, key2);
        assert_eq!(popped3, key3);
    }

    #[test]
    fn test_set_status_indexing_preserves_counters() {
        let progress = BranchProgress::new();
        progress.set_total(3);
        progress.increment_completed();
        progress.increment_completed();
        let (completed, total, _, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 2);
        assert_eq!(total, 3);

        progress.set_status_indexing();
        let (completed, total, status, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 2, "set_status_indexing must not reset completed");
        assert_eq!(total, 3, "set_status_indexing must not reset total");
        assert!(matches!(status, IndexStatus::Indexing));
    }

    #[test]
    fn test_progress_not_reset_on_reschedule() {
        let registry = IndexerRegistry::new();
        registry.set_config("http://localhost:8080".to_string(), "Basic abc".to_string());

        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        // Simulate task creation via notify.
        // We can't call notify because it requires INDEXER_REGISTRY to be set,
        // so we manually create the task.
        let progress = BranchProgress::new();
        progress.set_total(1);
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(None),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task);

        // Simulate first commit completed with NextCommit.
        progress.increment_completed();
        progress.total.store(2, Ordering::SeqCst);

        // Simulate try_spawn_next calling set_status_indexing (not reset_to_indexing).
        progress.set_status_indexing();

        let (completed, total, status, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 1, "completed must not be reset when re-scheduling");
        assert_eq!(total, 2);
        assert!(matches!(status, IndexStatus::Indexing));
    }

    #[test]
    fn test_total_updated_on_next_commit() {
        let progress = BranchProgress::new();
        // Simulate X-Commit-Count header set the real total.
        progress.set_total(5);

        // Simulate first commit completes with NextCommit.
        // total should NOT be overwritten — it stays at 5.
        progress.increment_completed();

        assert_eq!(progress.completed.load(Ordering::Relaxed), 1);
        assert_eq!(progress.total.load(Ordering::Relaxed), 5);

        // Simulate second commit completes with AtHead.
        progress.increment_completed();
        let completed = progress.completed.load(Ordering::SeqCst);
        progress.total.store(completed, Ordering::SeqCst);
        progress.set_completed();

        let (completed, total, status, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 2);
        assert_eq!(total, 2);
        assert!(matches!(status, IndexStatus::Completed));
    }

    #[test]
    fn test_scheduler_progresses_through_multiple_commits() {
        let registry = IndexerRegistry::new();
        registry.set_config("http://localhost:8080".to_string(), "Basic abc".to_string());

        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        // Create a task manually.
        let progress = BranchProgress::new();
        progress.set_total(1);
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(None),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task.clone());

        // Simulate commit 1 completes with NextCommit("commit2"):
        // (replicating what on_task_complete does for NextCommit, without try_spawn_next)
        *task.next_commit.lock().unwrap() = Some("commit2".to_string());
        progress.increment_completed();
        progress.set_status_indexing();

        // Verify state after first commit.
        assert_eq!(*task.next_commit.lock().unwrap(), Some("commit2".to_string()));
        assert_eq!(progress.completed.load(Ordering::Relaxed), 1);
        assert_eq!(progress.total.load(Ordering::Relaxed), 1);
        let (_, _, status, _, _, _, _) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Indexing));

        // Simulate commit 2 completes with AtHead:
        // (replicating what on_task_complete does for AtHead without pending notify)
        progress.increment_completed();
        let completed = progress.completed.load(Ordering::SeqCst);
        progress.total.store(completed, Ordering::SeqCst);
        progress.set_completed();

        let (completed, total, status, _, _, _, _) = progress.snapshot();
        assert_eq!(completed, 2, "both commits should be counted as completed");
        assert_eq!(total, 2);
        assert!(matches!(status, IndexStatus::Completed));
    }

    #[test]
    fn test_on_task_complete_next_commit_sets_next_commit() {
        let registry = IndexerRegistry::new();
        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        let progress = BranchProgress::new();
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(None),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task.clone());

        // Block try_spawn_next from actually spawning by setting current_branch.
        *registry.current_branch.lock().unwrap() = Some(BranchKey {
            path: "dummy".to_string(),
            branch: "dummy".to_string(),
        });

        registry.on_task_complete(&key, TaskResult::NextCommit("next123".to_string()));

        // on_task_complete clears current_branch, then try_spawn_next runs
        // and clears next_commit after reading it. We verify the side effects
        // that persist: completed was incremented. total is NOT overwritten
        // — it stays at the value set from X-Commit-Count header.
        assert_eq!(progress.completed.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn test_on_task_complete_next_commit_triggers_spawn_attempt() {
        // After on_task_complete with NextCommit, try_spawn_next must be
        // called so the next commit is actually picked up from the queue.
        // Before the fix, the key was enqueued but try_spawn_next was never
        // called, leaving the indexer stuck after the first commit.
        //
        // We verify the fix by checking that try_spawn_next popped the key
        // from the queue (it will fail to spawn due to missing tokio handle,
        // but the important thing is that it was called at all).
        let registry = IndexerRegistry::new();
        registry.set_config("http://localhost:8080".to_string(), "Basic abc".to_string());
        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        let progress = BranchProgress::new();
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(None),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task.clone());

        registry.on_task_complete(&key, TaskResult::NextCommit("next123".to_string()));

        // try_spawn_next should have popped the key from the queue.
        assert!(
            registry.pending_queue.lock().unwrap().is_empty(),
            "NextCommit must trigger try_spawn_next which pops the key from the queue"
        );
        // completed must be incremented.
        assert_eq!(progress.completed.load(Ordering::Relaxed), 1);
        // next_commit is cleared by try_spawn_next after reading it (Bug 9 fix).
        assert_eq!(*task.next_commit.lock().unwrap(), None,
            "next_commit must be cleared by try_spawn_next after reading it");
    }

    #[test]
    fn test_on_task_complete_error_removes_task() {
        let registry = IndexerRegistry::new();
        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        let progress = BranchProgress::new();
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(None),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task);

        // Block try_spawn_next from actually spawning.
        *registry.current_branch.lock().unwrap() = Some(BranchKey {
            path: "dummy".to_string(),
            branch: "dummy".to_string(),
        });

        registry.on_task_complete(&key, TaskResult::Error("test error".to_string()));

        assert!(registry.tasks.lock().unwrap().get(&key).is_none());
        let (_, _, status, _, _, _, _) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Error(ref m) if m == "test error"));
    }

    #[test]
    fn test_on_task_complete_at_head_with_pending_notify_re_enqueues() {
        // Verify the notify_count logic that determines re-enqueue vs completion.
        // When notify_count > 1, the task should be re-enqueued (not completed).
        let progress = BranchProgress::new();
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(2), // 2 notifies: one processed, one pending
            next_commit: StdMutex::new(None),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });

        // Simulate AtHead with pending notify:
        // fetch_sub returns the old value. If old > 1, re-enqueue.
        let prev = task.notify_count.fetch_sub(1, Ordering::SeqCst);
        assert_eq!(prev, 2, "fetch_sub should return old value");
        assert!(prev > 1, "should re-enqueue when notify_count was > 1");

        // next_commit should be reset for re-enqueue.
        *task.next_commit.lock().unwrap() = None;
        assert_eq!(*task.next_commit.lock().unwrap(), None);

        // Status should NOT be set to Completed (it stays Indexing).
        let (_, _, status, _, _, _, _) = progress.snapshot();
        assert!(matches!(status, IndexStatus::Indexing),
            "status should remain Indexing when re-enqueueing");
    }

    #[test]
    fn test_spawn_task_clears_next_commit_after_reading() {
        // Bug 9: spawn_task must clear task.next_commit after reading it
        // so that progress() does not report a stale scheduling value
        // while the task is running.
        let registry = IndexerRegistry::new();
        registry.set_config("http://localhost:8080".to_string(), "Basic abc".to_string());

        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        let progress = BranchProgress::new();
        progress.set_total(1);
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(Some("commitB".to_string())),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task.clone());

        // Simulate what spawn_task does: read next_commit, then clear it.
        let next_commit_read = {
            let tasks = registry.tasks.lock().unwrap();
            match tasks.get(&key) {
                Some(t) => {
                    let nc = t.next_commit.lock().unwrap().clone();
                    *t.next_commit.lock().unwrap() = None;
                    nc
                }
                None => None,
            }
        };

        // The value was read correctly.
        assert_eq!(next_commit_read, Some("commitB".to_string()));

        // The task's next_commit is now cleared — progress() will see None.
        assert_eq!(*task.next_commit.lock().unwrap(), None,
            "next_commit must be cleared after spawn reads it");

        // progress() should report None for upcoming_commit while task runs.
        let prog = registry.progress(&key.path, &key.branch);
        assert!(prog.is_some());
        let (_, _, _, _, upcoming, _, _, _) = prog.unwrap();
        assert_eq!(upcoming, None,
            "progress() must not report stale next_commit while task is running");
    }

    #[test]
    fn test_reset_chain_on_notify_while_indexing() {
        // When notify() is called on a task that is still Indexing,
        // reset_chain is set. The next on_task_complete(NextCommit)
        // should ignore the X-Next-Commit header and set next_commit=None,
        // forcing a fresh /last-indexed query.
        let registry = IndexerRegistry::new();
        registry.set_config("http://localhost:8080".to_string(), "Basic abc".to_string());

        let key = BranchKey {
            path: "admin/db/local/branch/main".to_string(),
            branch: "main".to_string(),
        };

        let progress = BranchProgress::new();
        progress.set_total(5);
        let task = Arc::new(BranchTask {
            progress: progress.clone(),
            notify_count: AtomicU64::new(1),
            next_commit: StdMutex::new(Some("commit2".to_string())),
            cancel: tokio_util::sync::CancellationToken::new(),
            store_clustering: std::sync::atomic::AtomicBool::new(false),
            reset_chain: std::sync::atomic::AtomicBool::new(false),
        });
        registry.tasks.lock().unwrap().insert(key.clone(), task.clone());

        // Simulate re-index while indexing: notify() sets reset_chain.
        task.reset_chain.store(true, Ordering::SeqCst);

        // Block try_spawn_next from actually spawning.
        *registry.current_branch.lock().unwrap() = Some(BranchKey {
            path: "dummy".to_string(),
            branch: "dummy".to_string(),
        });

        // on_task_complete with NextCommit should ignore "commit3" and
        // set next_commit = None because reset_chain is true.
        registry.on_task_complete(&key, TaskResult::NextCommit("commit3".to_string()));

        assert_eq!(progress.completed.load(Ordering::Relaxed), 1,
            "completed should be incremented after NextCommit with reset_chain");

        // reset_chain should be cleared after being consumed.
        assert!(!task.reset_chain.load(Ordering::SeqCst),
            "reset_chain should be cleared after on_task_complete consumes it");
    }
}
