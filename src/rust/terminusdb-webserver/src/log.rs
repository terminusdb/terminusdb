use std::sync::mpsc::{self, Sender, Receiver};
use std::sync::OnceLock;
use swipl::prelude::*;

/// Channel for sending log messages from any thread to the dispatcher thread,
/// which has a proper Prolog engine that can call json_log:json_log/2.
static LOG_CHANNEL: OnceLock<Sender<(String, String)>> = OnceLock::new();

/// Initialize the logging channel. The receiver end is returned to the
/// dispatcher thread, which processes log messages alongside dispatch requests.
pub fn init_log_channel() -> Receiver<(String, String)> {
    let (tx, rx) = mpsc::channel::<(String, String)>();
    LOG_CHANNEL.set(tx).ok();
    rx
}

fn log(severity: &str, msg: &str) {
    if let Some(tx) = LOG_CHANNEL.get() {
        // Send to the dispatcher thread's engine for proper Prolog logging.
        // If the channel is closed, fall back to stderr.
        if tx.send((severity.to_string(), msg.to_string())).is_err() {
            eprintln!("[{}] {} (log channel closed)", severity, msg);
        }
    } else {
        // Channel not initialized yet — fall back to stderr.
        eprintln!("[{}] {} (log channel not initialized)", severity, msg);
    }
}

/// Process pending log messages using the given Prolog context.
/// Called from the dispatcher thread's engine loop.
pub fn drain_log_messages<CT: QueryableContextType>(context: &Context<CT>, rx: &Receiver<(String, String)>) {
    while let Ok((severity, msg)) = rx.try_recv() {
        let _ = log_to_context(context, &severity, &msg);
    }
}

fn log_to_context<CT: QueryableContextType>(context: &Context<CT>, severity: &str, msg: &str) -> PrologResult<()> {
    let f = context.open_frame();
    let p = pred!("json_log:json_log/2");
    let [severity_term, msg_term] = f.new_term_refs();
    severity_term.unify(Atom::new(severity))?;
    msg_term.unify(msg)?;
    let result = f.call_once(p, [&severity_term, &msg_term]);
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
