use std::cell::RefCell;
use swipl::prelude::*;

thread_local! {
    static LOG_ENGINE: RefCell<Option<Engine>> = const { RefCell::new(None) };
}

fn log(severity: &str, msg: &str) {
    let result: PrologResult<()> = LOG_ENGINE.with(|engine_cell| {
        let mut engine_ref = engine_cell.borrow_mut();
        if engine_ref.is_none() {
            *engine_ref = Some(Engine::new());
        }
        let engine = engine_ref.as_ref().unwrap();
        let activation = engine.activate();
        let context: Context<_> = activation.into();
        let f = context.open_frame();
        let p = pred!("json_log:json_log/2");
        let [severity_term, msg_term] = f.new_term_refs();
        severity_term.unify(Atom::new(severity))?;
        msg_term.unify(msg)?;
        f.call_once(p, [&severity_term, &msg_term])?;
        f.close();
        Ok(())
    });
    if let Err(e) = result {
        panic!(
            "terminusdb-webserver: unable to log message ({severity}: {msg}): {e:?}"
        );
    }
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
