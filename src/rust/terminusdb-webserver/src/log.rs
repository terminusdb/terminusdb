use std::cell::RefCell;
use swipl::prelude::*;

thread_local! {
    static LOG_ENGINE: RefCell<Option<Engine>> = const { RefCell::new(None) };
}

fn log(severity: &str, msg: &str) {
    let result: PrologResult<()> = if Engine::some_engine_active() {
        // We are already running in a Prolog context (e.g. the main thread during
        // appserver_start). Use the active engine directly instead of creating a
        // new one, which would fail to activate.
        unsafe {
            let context = unmanaged_engine_context();
            log_to_context(&context, severity, msg)
        }
    } else {
        LOG_ENGINE.with(|engine_cell| {
            let mut engine_ref = engine_cell.borrow_mut();
            if engine_ref.is_none() {
                *engine_ref = Some(Engine::new());
            }
            let engine = engine_ref.as_ref().unwrap();
            let activation = engine.activate();
            let context: Context<_> = activation.into();
            log_to_context(&context, severity, msg)
        })
    };
    if let Err(e) = result {
        panic!(
            "terminusdb-webserver: unable to log message ({severity}: {msg}): {e:?}"
        );
    }
}

fn log_to_context<CT: QueryableContextType>(context: &Context<CT>, severity: &str, msg: &str) -> PrologResult<()> {
    let f = context.open_frame();
    let p = pred!("json_log:json_log/2");
    let [severity_term, msg_term] = f.new_term_refs();
    severity_term.unify(Atom::new(severity))?;
    msg_term.unify(msg)?;
    f.call_once(p, [&severity_term, &msg_term])?;
    f.close();
    Ok(())
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
