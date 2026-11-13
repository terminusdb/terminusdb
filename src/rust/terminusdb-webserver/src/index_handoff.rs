//! Index-task handoff queue: file-based work handoff between TerminusDB
//! instances sharing one vectorlink data directory.
//!
//! Only one process may hold the vectorlink embed-cache write lock; any
//! other instance on the same data dir runs read-only and cannot index
//! locally. Such instances hand work to the lock holder by dropping task
//! files into `<data_dir>/index_tasks/`:
//!
//! - The requester creates `<url-encoded ref>.<kind>.json` where kind is
//!   `index` (incremental, produced by indexer_notify) or `reindex`
//!   (full rebuild, produced by indexer_reindex / POST /api/index). The
//!   file is created O_EXCL and flock'd during the write so claimants
//!   never observe a partially written payload.
//! - A file that already exists is inspected, not rewritten: a locked
//!   file means *underway* (the writer holds an exclusive flock), an
//!   unlocked file means *pending*.
//! - The write-lock holder polls the directory, claims pending files by
//!   taking the exclusive lock, executes the task through the indexer
//!   registry, and deletes the file when the task completes. An unlocked
//!   leftover (crashed writer — the OS releases its locks) is re-claimed
//!   by the next poll: self-healing with no error path.
//!
//! The lock holder watches the directory for new files and sweeps it on
//! an interval as fallback, so pickup works on every filesystem.

use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use nix::fcntl::{Flock, FlockArg};
use serde::{Deserialize, Serialize};

/// How long a file may exist unparsed before it counts as poison. A file
/// younger than this may still be receiving its payload from a writer
/// that holds the lock only during the write itself.
pub const POISON_GRACE: Duration = Duration::from_secs(60);

/// What kind of index work a task file requests.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IndexTaskKind {
    /// Incremental indexing of new commits (indexer_notify semantics).
    Index,
    /// Full rebuild from the oldest commit (indexer_reindex semantics).
    Reindex,
    /// Drop all indexed data for a domain (DELETE /domain semantics).
    /// `path` on the task carries the domain (`org/db`), not a branch ref.
    Delete,
}

impl IndexTaskKind {
    pub fn as_str(self) -> &'static str {
        match self {
            IndexTaskKind::Index => "index",
            IndexTaskKind::Reindex => "reindex",
            IndexTaskKind::Delete => "delete",
        }
    }
}

/// One index-task payload. `path` is the 5-segment branch resource path
/// (`org/db/local/branch/name`) the registry already works with.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexTask {
    pub path: String,
    pub branch: String,
    /// Tasks always index to the branch head at execution time.
    /// Where the request originated: `api:index` for an explicit
    /// POST /api/index, `auto:notify` for commit/search nudges.
    pub requester: String,
    pub store_clustering: bool,
    pub kind: IndexTaskKind,
    /// Unix epoch seconds when the task was enqueued.
    pub requested_at: u64,
}

impl IndexTask {
    pub fn new(
        path: &str,
        branch: &str,
        store_clustering: bool,
        kind: IndexTaskKind,
        requester: &str,
    ) -> Self {
        Self {
            path: path.to_string(),
            branch: branch.to_string(),
            requester: requester.to_string(),
            store_clustering,
            kind,
            requested_at: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
        }
    }
}

/// File name for a task. The URL-encoded branch path keeps one file per
/// (ref, kind) pair, so duplicate requests for the same work collapse
/// onto a single file.
fn task_file_name(path: &str, kind: IndexTaskKind) -> String {
    format!("{}.{}.json", urlencoding::encode(path), kind.as_str())
}

/// The outcome of an enqueue attempt — what a requester reports about
/// the task file it found or created.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnqueueOutcome {
    /// The task file was created with this request's payload.
    Enqueued,
    /// The task file already exists and is not locked — already queued,
    /// no duplicate written.
    Pending,
    /// The task file exists and is locked — indexing is underway.
    Underway,
}

/// State of a task file as seen by a process that probes it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskFileState {
    Absent,
    Pending,
    Underway,
}

/// Take the exclusive nonblocking flock on a file. `Ok(Some)` means this
/// process now holds the lock (release by dropping the Flock). `Ok(None)`
/// means another file description holds it — the task is underway.
fn try_lock(file_path: &Path) -> io::Result<Option<Flock<File>>> {
    let file = OpenOptions::new().read(true).write(true).open(file_path)?;
    match Flock::lock(file, FlockArg::LockExclusiveNonblock) {
        Ok(lock) => Ok(Some(lock)),
        Err((_file, nix::errno::Errno::EWOULDBLOCK)) => Ok(None),
        Err((_file, e)) => Err(io::Error::from(e)),
    }
}

/// Write a task file into the queue directory.
///
/// The file is created O_EXCL and kept exclusively locked while its
/// payload is written, so a claimant can only ever observe a complete
/// file or a locked one — never a partial write. A pre-existing file is
/// probed, not rewritten.
pub fn enqueue(dir: &Path, task: &IndexTask) -> io::Result<EnqueueOutcome> {
    fs::create_dir_all(dir)?;
    let file_path = dir.join(task_file_name(&task.path, task.kind));
    // A claimant may take a transient lock on the just-created file to
    // read it — EWOULDBLOCK on our own file means exactly that. The lock
    // is bound to the inode, so removing and recreating leaves its stale
    // claim on a dead file while the payload lands on a fresh inode.
    // Bounded: three transient races in a row report underway instead.
    for _attempt in 0..3 {
        let file = match OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&file_path)
        {
            Ok(f) => f,
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {
                return Ok(match try_lock(&file_path)? {
                    Some(_file) => EnqueueOutcome::Pending,
                    None => EnqueueOutcome::Underway,
                });
            }
            Err(e) => return Err(e),
        };
        let mut lock = match Flock::lock(file, FlockArg::LockExclusiveNonblock) {
            Ok(lock) => lock,
            Err((_file, nix::errno::Errno::EWOULDBLOCK)) => {
                let _ = fs::remove_file(&file_path);
                continue;
            }
            Err((_file, e)) => return Err(io::Error::from(e)),
        };
        let body = serde_json::to_vec_pretty(task)
            .map_err(|e| io::Error::other(e.to_string()))?;
        lock.write_all(&body)?;
        lock.sync_all()?;
        // Dropping the lock releases the claim; the task is now pending.
        return Ok(EnqueueOutcome::Enqueued);
    }
    Ok(EnqueueOutcome::Underway)
}

/// Probe the queue state for a branch path across the two branch-level
/// task kinds — `Delete` files are domain-keyed and can never match a
/// branch path. `Underway` wins over `Pending` when both have files.
pub fn probe_task_file(dir: &Path, path: &str) -> TaskFileState {
    let mut saw_pending = false;
    for kind in [IndexTaskKind::Reindex, IndexTaskKind::Index] {
        let file_path = dir.join(task_file_name(path, kind));
        match try_lock(&file_path) {
            Ok(Some(_file)) => saw_pending = true,
            Ok(None) => return TaskFileState::Underway,
            Err(ref e) if e.kind() == io::ErrorKind::NotFound => {}
            Err(_) => {}
        }
    }
    if saw_pending {
        TaskFileState::Pending
    } else {
        TaskFileState::Absent
    }
}

/// Parse a task file's JSON payload. `file` must be positioned at the
/// start — the poller reads files it just opened.
pub fn read_task(file: &File) -> Result<IndexTask, String> {
    let mut buf = String::new();
    let mut reader = file;
    reader
        .read_to_string(&mut buf)
        .map_err(|e| format!("failed to read task file: {}", e))?;
    serde_json::from_str(&buf).map_err(|e| format!("failed to parse task file: {}", e))
}

/// A task file the poller successfully claimed: the Flock keeps the
/// exclusive lock alive until it is dropped.
pub struct ClaimedTaskFile {
    pub file_path: PathBuf,
    pub file: Flock<File>,
    pub task: IndexTask,
}

/// Scan the queue directory and claim every pending task file that is
/// not locked. Unparseable files older than `poison_grace` are deleted;
/// younger ones are left alone (a writer may still be producing them).
pub fn claim_pending(
    dir: &Path,
    poison_grace: Duration,
) -> io::Result<Vec<ClaimedTaskFile>> {
    let mut claimed = Vec::new();
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(claimed),
        Err(e) => return Err(e),
    };
    for entry in entries.flatten() {
        let file_path = entry.path();
        if file_path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let Some(file) = try_lock(&file_path)? else {
            continue;
        };
        match read_task(&file) {
            Ok(task) => claimed.push(ClaimedTaskFile {
                file_path,
                file,
                task,
            }),
            Err(e) => {
                let young = entry
                    .metadata()
                    .ok()
                    .and_then(|m| m.modified().ok())
                    .and_then(|t| t.elapsed().ok())
                    .map(|age| age < poison_grace)
                    .unwrap_or(false);
                if young {
                    // Possibly still being written — release and retry
                    // on the next poll.
                    drop(file);
                } else {
                    crate::log::log_error(format!(
                        "[indexer] handoff: removing unparseable task file {}: {}",
                        file_path.display(),
                        e
                    ));
                    let _ = fs::remove_file(&file_path);
                }
            }
        }
    }
    Ok(claimed)
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(super) fn test_task(kind: IndexTaskKind) -> IndexTask {
        IndexTask::new(
            "admin/db/local/branch/main",
            "main",
            false,
            kind,
            "test",
        )
    }

    #[test]
    fn task_file_name_encodes_ref_and_kind() {
        assert_eq!(
            task_file_name("admin/db/local/branch/main", IndexTaskKind::Index),
            "admin%2Fdb%2Flocal%2Fbranch%2Fmain.index.json"
        );
        assert_eq!(
            "admin%2Fdb.delete.json",
            task_file_name("admin/db", IndexTaskKind::Delete),
        );
        assert_eq!(
            "admin%2Fdb%2Flocal%2Fbranch%2Fmain.reindex.json",
            task_file_name("admin/db/local/branch/main", IndexTaskKind::Reindex),
            "admin%2Fdb%2Flocal%2Fbranch%2Fmain.reindex.json"
        );
        // Different branches → different files.
        assert_ne!(
            task_file_name("admin/db/local/branch/dev", IndexTaskKind::Index),
            task_file_name("admin/db/local/branch/main", IndexTaskKind::Index)
        );
    }

    #[test]
    fn enqueue_creates_complete_task_file() {
        let dir = tempfile::tempdir().unwrap();
        let outcome = enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        assert_eq!(outcome, EnqueueOutcome::Enqueued);

        let file_path = dir
            .path()
            .join("admin%2Fdb%2Flocal%2Fbranch%2Fmain.index.json");
        let body = fs::read_to_string(&file_path).unwrap();
        let parsed: IndexTask = serde_json::from_str(&body).unwrap();
        assert_eq!(parsed.path, "admin/db/local/branch/main");
        assert_eq!(parsed.branch, "main");
        assert_eq!(parsed.requester, "test");
        assert_eq!(parsed.kind, IndexTaskKind::Index);
        assert!(parsed.requested_at > 0);
    }

    #[test]
    fn enqueue_existing_unlocked_reports_pending() {
        let dir = tempfile::tempdir().unwrap();
        enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        // Second request for the same ref collapses onto the same file.
        let outcome = enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        assert_eq!(outcome, EnqueueOutcome::Pending);
    }

    #[test]
    fn enqueue_existing_locked_reports_underway() {
        let dir = tempfile::tempdir().unwrap();
        enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        let file_path = dir
            .path()
            .join("admin%2Fdb%2Flocal%2Fbranch%2Fmain.index.json");
        let _lock = try_lock(&file_path).unwrap().expect("file should be lockable");
        let outcome = enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        assert_eq!(outcome, EnqueueOutcome::Underway);
    }

    #[test]
    fn enqueue_different_kind_uses_separate_file() {
        let dir = tempfile::tempdir().unwrap();
        enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        let outcome = enqueue(dir.path(), &test_task(IndexTaskKind::Reindex)).unwrap();
        assert_eq!(outcome, EnqueueOutcome::Enqueued);
    }

    #[test]
    fn try_lock_blocks_second_claimant_until_released() {
        let dir = tempfile::tempdir().unwrap();
        enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        let file_path = dir
            .path()
            .join("admin%2Fdb%2Flocal%2Fbranch%2Fmain.index.json");
        let first = try_lock(&file_path).unwrap().expect("first claim");
        assert!(try_lock(&file_path).unwrap().is_none(), "held lock must block");
        drop(first);
        assert!(
            try_lock(&file_path).unwrap().is_some(),
            "released lock must be claimable again — self-healing after a dead writer"
        );
    }

    #[test]
    fn probe_task_file_reports_states() {
        let dir = tempfile::tempdir().unwrap();
        let path = "admin/db/local/branch/main";
        assert_eq!(probe_task_file(dir.path(), path), TaskFileState::Absent);

        enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        assert_eq!(probe_task_file(dir.path(), path), TaskFileState::Pending);

        let file_path = dir.path().join(task_file_name(path, IndexTaskKind::Index));
        let _lock = try_lock(&file_path).unwrap().unwrap();
        assert_eq!(probe_task_file(dir.path(), path), TaskFileState::Underway);
    }

    #[test]
    fn claim_pending_claims_unlocked_and_skips_locked() {
        let dir = tempfile::tempdir().unwrap();
        enqueue(dir.path(), &test_task(IndexTaskKind::Index)).unwrap();
        let mut other = test_task(IndexTaskKind::Index);
        other.path = "admin/otherdb/local/branch/main".to_string();
        enqueue(dir.path(), &other).unwrap();

        // Lock the second file — the poller must skip it.
        let locked_path = dir
            .path()
            .join(task_file_name(&other.path, IndexTaskKind::Index));
        let _lock = try_lock(&locked_path).unwrap().unwrap();

        let claimed = claim_pending(dir.path(), POISON_GRACE).unwrap();
        assert_eq!(claimed.len(), 1);
        assert_eq!(claimed[0].task.path, "admin/db/local/branch/main");
    }

    #[test]
    fn claim_pending_keeps_young_unparseable_file() {
        let dir = tempfile::tempdir().unwrap();
        let file_path = dir.path().join("garbage.index.json");
        fs::write(&file_path, b"not json").unwrap();
        let claimed = claim_pending(dir.path(), POISON_GRACE).unwrap();
        assert!(claimed.is_empty());
        assert!(file_path.exists(), "young garbage file may still be writing");
    }

    #[test]
    fn claim_pending_deletes_old_unparseable_file() {
        let dir = tempfile::tempdir().unwrap();
        let file_path = dir.path().join("garbage.index.json");
        fs::write(&file_path, b"not json").unwrap();
        // Zero grace: any unparseable file is immediately poison.
        let claimed = claim_pending(dir.path(), Duration::ZERO).unwrap();
        assert!(claimed.is_empty());
        assert!(!file_path.exists(), "stale garbage file must be removed");
    }

    #[test]
    fn claim_pending_empty_when_dir_missing() {
        let dir = tempfile::tempdir().unwrap();
        let missing = dir.path().join("does-not-exist");
        let claimed = claim_pending(&missing, POISON_GRACE).unwrap();
        assert!(claimed.is_empty());
    }
}

fn kind_rank(kind: IndexTaskKind) -> u8 {
    match kind {
        IndexTaskKind::Delete => 0,
        IndexTaskKind::Reindex => 1,
        IndexTaskKind::Index => 2,
    }
}

/// Execution order for a batch of claimed tasks: request order first —
/// a delete issued after an index must land after it, not be undone by
/// it. The kind rank only breaks same-second ties so a delete+index
/// pair issued together still wipes before it rebuilds.
pub fn sort_for_execution(claimed: &mut [ClaimedTaskFile]) {
    claimed.sort_by_key(|c| (c.task.requested_at, kind_rank(c.task.kind)));
}

#[cfg(test)]
mod execution_order_tests {
    use super::tests::test_task;
    use super::*;

    fn claimed_batch(dir: &Path, tasks: &[IndexTask]) -> Vec<ClaimedTaskFile> {
        for t in tasks {
            enqueue(dir, t).unwrap();
        }
        claim_pending(dir, POISON_GRACE).unwrap()
    }

    #[test]
    fn sorts_by_request_order_across_kinds() {
        let dir = tempfile::tempdir().unwrap();
        let mut index_first = test_task(IndexTaskKind::Index);
        index_first.requested_at = 100;
        let mut delete_later = test_task(IndexTaskKind::Delete);
        delete_later.path = "admin/db".to_string();
        delete_later.requested_at = 200;

        let mut claimed = claimed_batch(dir.path(), &[index_first, delete_later]);
        sort_for_execution(&mut claimed);

        let kinds: Vec<_> = claimed.iter().map(|c| c.task.kind).collect();
        assert_eq!(
            kinds,
            [IndexTaskKind::Index, IndexTaskKind::Delete],
            "a delete issued after an index must run after it"
        );
    }

    #[test]
    fn same_second_ties_fall_back_to_kind_rank() {
        let dir = tempfile::tempdir().unwrap();
        let mut index = test_task(IndexTaskKind::Index);
        index.requested_at = 100;
        let mut delete = test_task(IndexTaskKind::Delete);
        delete.path = "admin/db".to_string();
        delete.requested_at = 100;

        let mut claimed = claimed_batch(dir.path(), &[index, delete]);
        sort_for_execution(&mut claimed);

        let kinds: Vec<_> = claimed.iter().map(|c| c.task.kind).collect();
        assert_eq!(
            kinds,
            [IndexTaskKind::Delete, IndexTaskKind::Index],
            "a same-second delete+index pair still wipes first"
        );
    }
}
