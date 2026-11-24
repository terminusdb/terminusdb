use swipl::prelude::*;

predicates! {
    /// Hello world predicate registered by the Rust plugin loader.
    ///
    /// Signature: `hello_rust(-Message)` where Message is unified with
    /// a greeting string. Demonstrates that the plugin's shared object
    /// was loaded and its `install()` entry point was called.
    #[module("$rustnative")]
    semidet fn hello_rust(_context, term) {
        term.unify("Hello from Rust plugin!")
    }
}

/// Entry point called by SWI-Prolog's `load_foreign_library/1`.
///
/// Registers all predicates declared in the `predicates!` block above.
#[no_mangle]
pub extern "C" fn install() {
    register_hello_rust();
}
