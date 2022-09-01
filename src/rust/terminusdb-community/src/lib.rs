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

use terminusdb_store_prolog::layer::*;

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
}

pub fn install() {
    register_list_diff();
    doc::register();
}
