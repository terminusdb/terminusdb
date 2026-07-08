use std::sync::mpsc::{self, Sender, Receiver};
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};
use swipl::prelude::*;

/// A log message with its capture-time timestamp.
///
/// The timestamp is recorded at the moment `log()` is called (send time),
/// not when the dispatcher thread drains the channel. This ensures that
/// log timestamps reflect when the event actually occurred, not when the
/// dispatcher happened to process it — which may be much later if the
/// dispatcher is busy with a request.
pub struct LogEntry {
    pub severity: String,
    pub message: String,
    /// Wall-clock seconds since UNIX_EPOCH, captured at send time.
    pub timestamp: f64,
}

/// Channel for sending log messages from any thread to the dispatcher thread,
/// which has a proper Prolog engine that can call json_log:json_log/2.
static LOG_CHANNEL: OnceLock<Sender<LogEntry>> = OnceLock::new();

/// Initialize the logging channel. The receiver end is returned to the
/// dispatcher thread, which processes log messages alongside dispatch requests.
pub fn init_log_channel() -> Receiver<LogEntry> {
    let (tx, rx) = mpsc::channel::<LogEntry>();
    LOG_CHANNEL.set(tx).ok();
    rx
}

/// Capture the current wall-clock time as seconds since UNIX_EPOCH.
fn now_timestamp() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

fn log(severity: &str, msg: &str) {
    let entry = LogEntry {
        severity: severity.to_string(),
        message: msg.to_string(),
        timestamp: now_timestamp(),
    };
    if let Some(tx) = LOG_CHANNEL.get() {
        // Send to the dispatcher thread's engine for proper Prolog logging.
        // If the channel is closed, fall back to stderr.
        if tx.send(entry).is_err() {
            eprintln!("[{}] {} (log channel closed)", severity, msg);
        }
    } else {
        // Channel not initialized yet — fall back to stderr.
        eprintln!("[{}] {} (log channel not initialized)", severity, msg);
    }
}

/// Process pending log messages using the given Prolog context.
/// Called from the dispatcher thread's engine loop.
pub fn drain_log_messages<CT: QueryableContextType>(context: &Context<CT>, rx: &Receiver<LogEntry>) {
    while let Ok(entry) = rx.try_recv() {
        let _ = log_to_context(context, &entry);
    }
}

fn log_to_context<CT: QueryableContextType>(context: &Context<CT>, entry: &LogEntry) -> PrologResult<()> {
    let f = context.open_frame();
    // Call json_log:json_log_rust/3 which accepts a pre-captured timestamp
    // (as a float of seconds since epoch) and formats it with the same
    // ISO 8601 format used by generate_time/1. This ensures the timestamp
    // reflects when the log entry was created, not when it was drained.
    let p = pred!("json_log:json_log_rust/3");
    let [severity_term, msg_term, ts_term] = f.new_term_refs();
    severity_term.unify(Atom::new(&entry.severity))?;
    msg_term.unify(&entry.message)?;
    ts_term.unify(entry.timestamp)?;
    let result = f.call_once(p, [&severity_term, &msg_term, &ts_term]);
    f.close();
    result
}

pub fn log_error(msg: String) {
    log("ERROR", &msg);
}

pub fn log_warning(msg: String) {
    log("WARNING", &msg);
}

pub fn log_info(msg: String) {
    log("INFO", &msg);
}

pub fn log_debug(msg: String) {
    log("DEBUG", &msg);
}
