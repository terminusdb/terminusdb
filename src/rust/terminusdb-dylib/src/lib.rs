use terminusdb_community;

#[no_mangle]
pub extern "C" fn install() {
    terminusdb_community::install();
}
