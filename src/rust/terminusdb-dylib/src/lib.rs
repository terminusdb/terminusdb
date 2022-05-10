use terminusdb_community;
use terminusdb_store_prolog;

#[no_mangle]
pub extern "C" fn install() {
    terminusdb_community::install();
    terminusdb_store_prolog::install(Some("terminus_store"));
}
