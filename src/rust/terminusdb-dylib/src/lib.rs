use terminusdb_community;
use terminusdb_store_prolog;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

#[no_mangle]
pub extern "C" fn install() {
    terminusdb_community::install();
    terminusdb_store_prolog::install(Some("terminus_store"));
}
