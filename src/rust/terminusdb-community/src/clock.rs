//! Process-wide monotonic clock for commit timestamps.
//!
//! Commit stamps must never repeat or go backwards: two commits written
//! within the same wall-clock tick still need distinct, ordered values,
//! and NTP adjustments must not rewind history. A single process-global
//! atomic keeps the last emitted nanosecond and hands out the maximum of
//! the wall clock and `last + 1`, so values are strictly increasing and
//! always carry nanosecond digits regardless of the OS timer's real
//! granularity.

use std::sync::atomic::{AtomicI64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use swipl::predicates;

static COMMIT_CLOCK_LAST_NS: AtomicI64 = AtomicI64::new(i64::MIN);

/// Nanoseconds since the Unix epoch as a signed count — negative values
/// represent pre-1970 historic time.
fn wall_now_ns() -> i64 {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => i64::try_from(d.as_nanos()).unwrap_or(i64::MAX),
        Err(e) => -i64::try_from(e.duration().as_nanos()).unwrap_or(i64::MAX),
    }
}

/// Nanoseconds since the Unix epoch, strictly increasing for the life of
/// the process. Lock-free: a compare-exchange retries until this call has
/// claimed the next value.
fn next_ns(wall: i64) -> i64 {
    let mut prev = COMMIT_CLOCK_LAST_NS.load(Ordering::Relaxed);
    loop {
        let candidate = wall.max(prev.saturating_add(1));
        match COMMIT_CLOCK_LAST_NS.compare_exchange_weak(
            prev,
            candidate,
            Ordering::SeqCst,
            Ordering::Relaxed,
        ) {
            Ok(_) => return candidate,
            Err(actual) => prev = actual,
        }
    }
}

fn commit_now_ns() -> i64 {
    next_ns(wall_now_ns())
}

predicates! {
    /// Deterministic predicate returning a strictly-increasing
    /// nanosecond-resolution Unix epoch timestamp for commit stamping.
    ///
    /// Signature: now_nanos(-Nanos) — Nanos is a non-negative integer.
    #[module("$clock")]
    semidet fn now_nanos(_context, term) {
        term.unify(commit_now_ns())
    }
}

pub fn register() {
    register_now_nanos();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commit_now_ns_is_strictly_increasing() {
        let mut prev = commit_now_ns();
        for _ in 0..10_000 {
            let next = commit_now_ns();
            assert!(next > prev, "{next} not greater than {prev}");
            prev = next;
        }
    }

    #[test]
    fn commit_now_ns_stays_monotonic_across_signed_values() {
        // A negative wall (pre-1970 historic time, or a rewound system
        // clock) must still yield a strictly-increasing stamp.
        let base = commit_now_ns();
        let historic = next_ns(base - 60_000_000_000);
        assert_eq!(historic, base + 1);
        assert!(commit_now_ns() > historic);
    }

    #[test]
    fn commit_now_ns_tracks_wall_clock() {
        let before = wall_now_ns();
        let stamped = commit_now_ns();
        let after = wall_now_ns();
        assert!(stamped >= before, "stamp {stamped} behind wall {before}");
        assert!(
            stamped <= after + 1_000_000,
            "stamp {stamped} more than 1ms ahead of wall {after}"
        );
    }
}
