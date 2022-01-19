use swipl::prelude::*;
use alcs;
use std::sync::Arc;

predicates! {
    /// Temporary predicate to demonstrate and test the embedded
    /// module. This should go away as soon as some real predicates
    /// are added here.
    #[module("rustnative")]
    semidet fn hello(_context, term) {
        term.unify("Hello world")
    }

    #[module("alcs")]
    semidet fn compute_alcs(_context, list1_term, list2_term, computed_term) {
        let list1: Vec<String> = list1_term.get()?;
        let list2: Vec<String> = list2_term.get()?;

        let (ih,iv,ig,vg,dg)=alcs::alcs_mat(&list1, &list2);
        let data = AlcsData { ih,iv,ig, vg, dg };

        computed_term.unify(Arc::new(data))
    }

    #[module("alcs")]
    semidet fn do_something_with_alcs(_context, alcs_data_term, result_term) {
        let _data: Arc<AlcsData> = alcs_data_term.get()?;

        result_term.unify(true)
    }
}

#[arc_blob("Alcs", defaults)]
#[derive(Clone)]
struct AlcsData {
    ih: Vec<Vec<usize>>,
    iv: Vec<Vec<usize>>,
    ig: Vec<usize>,
    vg: Vec<Option<usize>>,
    dg: Vec<Option<usize>>
}

#[no_mangle]
pub extern "C" fn install() {
    register_hello();
    register_compute_alcs();
    register_do_something_with_alcs();
}
