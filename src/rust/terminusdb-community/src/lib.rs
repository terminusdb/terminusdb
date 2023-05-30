#[macro_use]
mod log;

mod consts;
mod doc;
mod graphql;
mod path;
mod prefix;
mod schema;
mod types;
mod value;

pub use swipl;
use swipl::prelude::*;
pub use terminusdb_store_prolog::terminus_store;

use rand::{thread_rng, Rng};

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
        let mut rng = thread_rng();

        for item in &mut buf {
            let r = rng.gen_range(0..36);
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
    semidet fn random_base62(_context, size_term, s_term) {
        let size: u64 = size_term.get()?;
        let mut buf = Vec::with_capacity(size as usize);
        let mut rng = thread_rng();

        for _ in 0..size {
            let r = rng.gen_range(0..62);
            let item = if r < 10 { b'0' + r }
            else if r < 36 { b'A' - 10 + r }
            else { b'a' - 36 + r };
            buf.push(item)
        }

        let s = unsafe { std::str::from_utf8_unchecked(&buf) };

        s_term.unify(s)
    }

}

pub fn install() {
    register_list_diff();
    register_random_string();
    register_random_base62();
    doc::register();
    graphql::register();
}
