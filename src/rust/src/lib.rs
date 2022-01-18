use swipl::prelude::*;

predicates! {
    #[module("rustnative")]
    semidet fn hello(_context, term) {
        term.unify("Hello world")
    }
}

#[no_mangle]
pub extern "C" fn install() {
    register_hello();
}
