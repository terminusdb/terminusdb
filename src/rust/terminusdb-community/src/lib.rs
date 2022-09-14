mod consts;
mod doc;
mod prefix;
mod schema;
mod types;
mod value;

use lcs;
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

        for i in 0..31 {
            let r = rng.gen_range(0..36);
            if r < 10 {
                buf[i] = '0' as u8 + r;
            }
            else {
                buf[i] = 'a' as u8 - 10 + r;
            }
        }

        let s = unsafe { std::str::from_utf8_unchecked(&buf) };

        s_term.unify(s)
    }
}

pub fn install() {
    register_list_diff();
    register_random_string();
    doc::register();
}
