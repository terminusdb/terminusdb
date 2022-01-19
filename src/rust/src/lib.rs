use swipl::prelude::*;

predicates! {
    /// Temporary predicate to demonstrate and test the embedded
    /// module. This should go away as soon as some real predicates
    /// are added here.
    #[module("rustnative")]
    semidet fn hello(_context, term) {
        term.unify("Hello world")
    }
}

#[no_mangle]
pub extern "C" fn install() {
    register_hello();
}
