#[macro_use]
mod log;

#[macro_use]
mod dict_lookup;

mod changes;
mod consts;
mod doc;
mod embedding;
mod jwt;
mod graphql;
mod json_preserve;
mod path;
mod prefix;
mod schema;
mod template;
mod types;
mod value;

pub use swipl;
use swipl::prelude::*;
pub use terminusdb_store_prolog::terminus_store;

use rand::Rng;
use std::sync::{LazyLock, Mutex};
use uuid::{ContextV7, Timestamp, Uuid};

/// Shared v7 counter context: UUIDs stay monotonically ordered across
/// Prolog threads (12-bit counter per millisecond before the timestamp
/// is bumped ahead).
static UUID_V7_CONTEXT: LazyLock<Mutex<ContextV7>> =
    LazyLock::new(|| Mutex::new(ContextV7::new()));

predicates! {
    /// Temporary predicate to demonstrate and test the embedded
    /// module. This should go away as soon as some real predicates
    /// are added here.
    #[module("$rustnative")]
    semidet fn hello(_context, term) {
        term.unify("Hello world")
    }

    #[module("$lcs")]
    semidet fn list_diff(_context, list1_term, list2_term, diff) {
        let list1: Vec<Atom> = list1_term.get()?;
        let list2: Vec<Atom> = list2_term.get()?;

        let table = lcs::LcsTable::new(&list1, &list2);
        let table_diff = table.diff();
        let mut vec = Vec::with_capacity(table_diff.len());
        let unchanged = Atom::new("unchanged");
        let deleted = Atom::new("deleted");
        let inserted = Atom::new("inserted");
        for elt in table_diff {
            let atomic =
                match elt {
                    lcs::DiffComponent::Unchanged(_x,_y) => &unchanged,
                    lcs::DiffComponent::Deletion(_x) => &deleted,
                    lcs::DiffComponent::Insertion(_x) => &inserted
                };
            vec.push(atomic);
        }

        diff.unify(vec.as_slice())
    }

    #[module("utils")]
    semidet fn random_string(_context, s_term) {
        let mut buf = [0_u8;31];
        let mut rng = rand::rng();

        for item in &mut buf {
            let r = rng.random_range(0..36);
            if r < 10 {
                *item = b'0' + r;
            }
            else {
                *item = b'a' - 10 + r;
            }
        }

        let s = unsafe { std::str::from_utf8_unchecked(&buf) };

        s_term.unify(s)
    }

    #[module("utils")]
    semidet fn random_base64(_context, size_term, s_term) {
        let size: u64 = size_term.get()?;
        let mut buf = Vec::with_capacity(size as usize);
        let mut rng = rand::rng();

        for _ in 0..size {
            let r = rng.random_range(0..64);
            let item = base64char(r);
            buf.push(item)
        }

        let s = unsafe { std::str::from_utf8_unchecked(&buf) };

        s_term.unify(s)
    }

    #[module("utils")]
    semidet fn uuid_v7(_context, s_term) {
        let id = {
            let context = UUID_V7_CONTEXT.lock().unwrap();
            Uuid::new_v7(Timestamp::now(&*context))
        };
        s_term.unify(id.to_string().as_str())
    }

}

// implements RFC4648 encoding
#[inline]
fn base64char(r: u8) -> u8 {
    if r < 26 {
        b'A' + r
    } else if r < 52 {
        b'a' + (r - 26)
    } else if r < 62 {
        b'0' + (r - 52)
    } else if r == 62 {
        b'-'
    } else {
        b'_'
    }
}

pub fn install() {
    register_list_diff();
    register_random_string();
    register_random_base64();
    register_uuid_v7();
    doc::register();
    graphql::register();
    json_preserve::register();
    template::register();
    changes::register();
    embedding::register();
    jwt::register();
}

#[cfg(test)]
mod tests {
    use super::UUID_V7_CONTEXT;
    use std::hint::black_box;
    use std::time::Instant;
    use uuid::{Timestamp, Uuid};

    /// Per-iteration lock + generate + format: the same work the
    /// utils:uuid_v7/1 predicate does per foreign call.
    /// Run: cargo test --release -p terminusdb-community bench_uuid_v7 -- --ignored --nocapture
    #[test]
    #[ignore]
    fn bench_uuid_v7() {
        const N: usize = 10_000_000;
        let mut acc = 0u64;
        let start = Instant::now();
        for _ in 0..N {
            let id = {
                let context = UUID_V7_CONTEXT.lock().unwrap();
                Uuid::new_v7(Timestamp::now(&*context))
            };
            let s = id.to_string();
            acc ^= black_box(s.as_bytes()[0]) as u64;
        }
        let elapsed = start.elapsed();
        println!(
            "{N} uuid_v7 (lock+gen+to_string) in {elapsed:?}: {:.1} ns/call",
            elapsed.as_nanos() as f64 / N as f64
        );
        black_box(acc);
    }

    /// Same work but the mutex is taken once for the whole loop:
    /// bench_uuid_v7 minus this = per-call lock overhead.
    #[test]
    #[ignore]
    fn bench_uuid_v7_single_lock() {
        const N: usize = 10_000_000;
        let context = UUID_V7_CONTEXT.lock().unwrap();
        let mut acc = 0u64;
        let start = Instant::now();
        for _ in 0..N {
            let id = Uuid::new_v7(Timestamp::now(&*context));
            let s = id.to_string();
            acc ^= black_box(s.as_bytes()[0]) as u64;
        }
        let elapsed = start.elapsed();
        println!(
            "{N} uuid_v7 (gen+to_string, single lock) in {elapsed:?}: {:.1} ns/call",
            elapsed.as_nanos() as f64 / N as f64
        );
        black_box(acc);
    }
}
