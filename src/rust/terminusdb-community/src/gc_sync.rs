//! GC Synchronization for Rust-Prolog FFI
//!
//! This module provides synchronization primitives to prevent SWI-Prolog's
//! garbage collector from running while Rust code is creating Prolog terms.
//!
//! ## Problem
//! SWI-Prolog's GC can run concurrently with Rust FFI code. If GC scans the
//! term stack while Rust is in the middle of creating terms, it can encounter
//! partially-constructed data structures, leading to "relocation cells" crashes.
//!
//! ## Solution
//! - Rust code acquires a READ lock before creating terms (multiple concurrent OK)
//! - GC acquires a WRITE lock via `gc_safe/0` (exclusive, waits for all Rust)
//! - Automatic GC is disabled; only explicit `gc_safe/0` calls trigger GC
//!
//! ## Usage
//! ```ignore
//! let _guard = TermCreationGuard::new();
//! // ... create Prolog terms safely ...
//! // guard dropped, GC can run
//! ```
//!
//! This code should probably migrate to swipl-rs
//!
//! ## Requirements
//! The auto-optimize plugin must be loaded to:
//! 1. Disable `gc_thread` and `gc` flags
//! 2. Call `gc_safe/0` instead of `garbage_collect/0`

use std::sync::RwLock;
use once_cell::sync::Lazy;
use swipl::prelude::*;

predicates! {
    /// Safe garbage collection that waits for all Rust term creation to complete.
    /// Acquires WRITE lock, blocking until all Rust threads release READ locks.
    #[module("$gc_sync")]
    pub semidet fn gc_safe(context) {
        gc_safe_impl(context)
    }
}

pub fn register() {
    register_gc_safe();
}

/// RwLock for GC synchronization:
/// - Rust term creation: acquires READ lock (multiple concurrent OK)
/// - GC: acquires WRITE lock (exclusive, waits for all Rust to finish)
pub static GC_RWLOCK: Lazy<RwLock<()>> = Lazy::new(|| RwLock::new(()));

/// RAII guard that holds a READ lock during Rust term creation.
/// Multiple threads can hold read locks concurrently.
/// GC must wait for all read locks to be released.
pub struct TermCreationGuard<'a> {
    _lock: std::sync::RwLockReadGuard<'a, ()>,
}

impl TermCreationGuard<'_> {
    /// Acquire a read lock for term creation.
    /// This blocks if GC is currently running (write lock held).
    pub fn new() -> Self {
        let lock = GC_RWLOCK.read().unwrap();
        Self { _lock: lock }
    }
}

impl Default for TermCreationGuard<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// Safe garbage collection that waits for all Rust term creation to complete.
///
/// Acquires WRITE lock on GC_RWLOCK, blocking until all Rust threads
/// release their READ locks. Then temporarily enables GC, runs it, and
/// disables it again.
///
/// This is exposed as `$json_preserve:gc_safe/0` for Prolog to call.
pub fn gc_safe_impl<C: QueryableContextType>(context: &Context<'_, C>) -> PrologResult<()> {
    // Acquire WRITE lock - waits for all Rust term creation to finish
    let _guard = GC_RWLOCK.write().unwrap();
    
    // Temporarily enable GC
    let gc_flag = context.new_term_ref();
    gc_flag.unify(Atom::new("gc"))?;
    let true_atom = context.new_term_ref();
    true_atom.unify(Atom::new("true"))?;
    let false_atom = context.new_term_ref();
    false_atom.unify(Atom::new("false"))?;
    
    context.call_once(pred!(set_prolog_flag/2), [&gc_flag, &true_atom])?;
    
    // Run GC - safe because we hold exclusive write lock
    let result = context.call_once(pred!(garbage_collect/0), []);
    
    // Disable GC again
    let _ = context.call_once(pred!(set_prolog_flag/2), [&gc_flag, &false_atom]);
    
    result
}
