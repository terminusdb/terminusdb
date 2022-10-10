#![allow(unused)]
use swipl::prelude::*;

fn log<C: FrameableContextType>(
    context: &Context<C>,
    severity: Atom,
    msg: &str,
) -> PrologResult<()> {
    let f = context.open_frame();
    let p = pred!("json_log:json_log/2");
    let [severity_term, msg_term] = f.new_term_refs();
    severity_term.unify(severity)?;
    msg_term.unify(msg)?;

    f.call_once(p, [&severity_term, &msg_term])?;

    f.close();

    Ok(())
}

pub fn log_error<C: FrameableContextType>(context: &Context<C>, msg: &str) -> PrologResult<()> {
    let severity = atom!("ERROR");
    log(context, severity, msg)
}

macro_rules! log_error {
    ($context:ident, $msg:expr) => {
        crate::log::log_error($context, &format!("{}", $msg))
    };
    ($context:ident, $format:expr, $($args:tt),*) => {
        crate::log::log_error($context, &format!($format, $($args),*))
    }
}

pub fn log_warning<C: FrameableContextType>(context: &Context<C>, msg: &str) -> PrologResult<()> {
    let severity = atom!("WARNING");
    log(context, severity, msg)
}

macro_rules! log_warning {
    ($context:ident, $msg:expr) => {
        crate::log::log_warning($context, &format!("{}", $msg))
    };
    ($context:ident, $format:expr, $($args:tt),*) => {
        crate::log::log_warning($context, &format!($format, $($args),*))
    }
}

pub fn log_notice<C: FrameableContextType>(context: &Context<C>, msg: &str) -> PrologResult<()> {
    let severity = atom!("NOTICE");
    log(context, severity, msg)
}

macro_rules! log_notice {
    ($context:ident, $msg:expr) => {
        crate::log::log_notice($context, &format!("{}", $msg))
    };
    ($context:ident, $format:expr, $($args:tt),*) => {
        crate::log::log_notice($context, &format!($format, $($args),*))
    }
}

pub fn log_info<C: FrameableContextType>(context: &Context<C>, msg: &str) -> PrologResult<()> {
    let severity = atom!("INFO");
    log(context, severity, msg)
}

macro_rules! log_info {
    ($context:ident, $msg:expr) => {
        crate::log::log_info($context, &format!("{}", $msg))
    };
    ($context:ident, $format:expr, $($args:tt),*) => {
        crate::log::log_info($context, &format!($format, $($args),*))
    }
}

pub fn log_debug<C: FrameableContextType>(context: &Context<C>, msg: &str) -> PrologResult<()> {
    let severity = atom!("DEBUG");
    log(context, severity, msg)
}

macro_rules! log_debug {
    ($context:ident, $msg:expr) => {
        crate::log::log_debug($context, &format!("{}", $msg))
    };
    ($context:ident, $format:expr, $($args:tt),*) => {
        crate::log::log_debug($context, &format!($format, $($args),*))
    }
}
