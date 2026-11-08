#[macro_use]
mod log;

#[macro_use]
mod dict_lookup;

mod change_window;
pub mod changes;
mod clock;
pub mod consts;
mod doc;
mod embedding;
mod expanded_name;
mod graphql;
mod json_preserve;
mod jwt;
mod path;
mod prefix;
mod schema;
mod template;
mod types;
mod value;

pub use swipl;
use swipl::prelude::*;
pub use value::register_decimal_lexical_hook;

use tdb_succinct::tfc::lexical_interval::{
    decode_lexical_entry, derived_period, encode_lexical_interval,
    extend_component, parse_lexical_interval, render_lexical_interval,
    Component, FLAG_DECLARED_PERIOD, FLAG_DURATION_END, FLAG_POINT,
    FLAG_START_DURATION,
};
pub use terminusdb_store_prolog::terminus_store;

use rand::Rng;
use std::sync::{LazyLock, Mutex};
use uuid::{ContextV7, Timestamp, Uuid};

/// Shared v7 counter context: UUIDs stay monotonically ordered across
/// Prolog threads (12-bit counter per millisecond before the timestamp
/// is bumped ahead).
static UUID_V7_CONTEXT: LazyLock<Mutex<ContextV7>> = LazyLock::new(|| Mutex::new(ContextV7::new()));

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

    #[module("utils")]
    semidet fn sha1_hex(_context, data_term, s_term) {
        use sha1::{Digest, Sha1};
        let data: String = data_term.get()?;
        let digest = Sha1::digest(data.as_bytes());
        s_term.unify(format!("{:x}", digest).as_str())
    }

    /// Parse an xdd:dateTimeInterval literal with the storage parser and
    /// unify Out with its canonical rendered form. Fails on malformed input;
    /// the caller maps failure to a cast error.
    #[module("$xdd")]
    semidet fn interval_parse(_context, lit_term, out_term) {
        // literals arrive as atoms from documents and as strings from WOQL
        let lit: String = lit_term
            .get::<String>()
            .or_else(|_| lit_term.get::<Atom>().map(|a| a.name().to_string()))?;
        match parse_lexical_interval(&lit) {
            Ok(parsed) => {
                let entry = encode_lexical_interval(&parsed);
                let decoded = decode_lexical_entry(&entry).unwrap();
                let rendered = render_lexical_interval(&decoded);
                out_term.unify(rendered.as_str())
            }
            Err(_) => Err(PrologError::Failure),
        }
    }

    /// Decompose an xdd:dateTimeInterval literal into its supplied side
    /// spellings (ISO extended form; a duration side yields its `P…`
    /// spelling, specials yield `..`/`X*`/empty), the interval flag, and
    /// the bounds-derived canonical duration. Fails on malformed input.
    #[module("$xdd")]
    semidet fn interval_components(
        _context,
        lit_term,
        s1_term,
        s2_term,
        flag_term,
        period_term,
    ) {
        let lit: String = lit_term
            .get::<String>()
            .or_else(|_| lit_term.get::<Atom>().map(|a| a.name().to_string()))?;
        match parse_lexical_interval(&lit) {
            Ok(iv) => {
                // declared `T/T/P` rebuilds the same term shape the legacy
                // parser produced: explicit flag, supplied period.
                let flag = match iv.flag {
                    FLAG_POINT => "point",
                    FLAG_START_DURATION => "start_duration",
                    FLAG_DURATION_END => "duration_end",
                    _ => "explicit",
                };
                let parts: Vec<&str> = iv.canonical.split('/').collect();
                let side = |s: &str, c: &Component, is_left: bool| {
                    // a duration side carries its `P…` spelling, not a component
                    if (is_left && iv.flag == FLAG_DURATION_END)
                        || (!is_left && iv.flag == FLAG_START_DURATION)
                    {
                        s.to_string()
                    } else {
                        match c {
                            Component::Finite(_) => extend_component(s),
                            _ => c.canonical_str().to_string(),
                        }
                    }
                };
                let (s1, s2, period) = match iv.flag {
                    FLAG_POINT => (
                        extend_component(parts[0]),
                        String::new(),
                        derived_period(&iv),
                    ),
                    FLAG_DECLARED_PERIOD => (
                        side(parts[0], &iv.start, true),
                        side(parts[1], &iv.end, false),
                        parts.get(2).copied().unwrap_or("").to_string(),
                    ),
                    _ => (
                        side(parts[0], &iv.start, true),
                        side(parts.get(1).copied().unwrap_or(""), &iv.end, false),
                        derived_period(&iv),
                    ),
                };
                s1_term.unify(s1.as_str())?;
                s2_term.unify(s2.as_str())?;
                flag_term.unify(flag)?;
                period_term.unify(period.as_str())
            }
            Err(_) => Err(PrologError::Failure),
        }
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
    register_sha1_hex();
    register_interval_parse();
    register_interval_components();
    doc::register();
    graphql::register();
    json_preserve::register();
    template::register();
    changes::register();
    clock::register();
    change_window::register();
    embedding::register();
    jwt::register();
    terminusdb_webserver::install();
}

#[cfg(test)]
mod tests {
    use super::{base64char, UUID_V7_CONTEXT};
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

    /// The work utils:random_base64/2 does per call (16 chars, thread RNG,
    /// no lock) — the Random key's generation cost.
    #[test]
    #[ignore]
    fn bench_random_base64() {
        use rand::Rng;
        const N: usize = 10_000_000;
        let mut rng = rand::rng();
        let mut acc = 0u64;
        let start = Instant::now();
        for _ in 0..N {
            let mut buf = Vec::with_capacity(16);
            for _ in 0..16 {
                buf.push(base64char(rng.random_range(0..64)));
            }
            let s = unsafe { std::str::from_utf8_unchecked(&buf) };
            acc ^= black_box(s.as_bytes()[0]) as u64;
        }
        let elapsed = start.elapsed();
        println!(
            "{N} random_base64(16) in {elapsed:?}: {:.1} ns/call",
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

    /// The work utils:sha1_hex/2 does per foreign call.
    /// Run: cargo test --release -p terminusdb-community bench_sha1_hex -- --ignored --nocapture
    #[test]
    #[ignore]
    fn bench_sha1_hex() {
        use sha1::{Digest, Sha1};
        // known-answer: sha1("abc")
        assert_eq!(
            format!("{:x}", Sha1::digest(b"abc")),
            "a9993e364706816aba3e25717850c26c9cd0d89d"
        );
        let input = "Dict(\"name\"-e3b0c44298fc1c149afbf4c8996fb92427ae41e4-\"value\"-da39a3ee5e6b4b0d3255bfef95601890afd80709)";
        const N: usize = 10_000_000;
        let mut acc = 0u64;
        let start = Instant::now();
        for _ in 0..N {
            let digest = Sha1::digest(input.as_bytes());
            let s = format!("{:x}", digest);
            acc ^= black_box(s.as_bytes()[0]) as u64;
        }
        let elapsed = start.elapsed();
        println!(
            "{N} sha1_hex(~100B) in {elapsed:?}: {:.1} ns/call",
            elapsed.as_nanos() as f64 / N as f64
        );
        black_box(acc);
    }
}
