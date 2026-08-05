:- module(plugin_api_indexer, [
    indexer_notify/4,
    indexer_reindex/3,
    indexer_set_config/2,
    indexer_progress/3,
    indexer_abort_domain/1,
    indexer_available/0
]).

/** <module> Indexer FFI predicate wrappers

Thin wrappers around the Rust FFI predicates registered in the
`$appserver` module by the terminusdb-webserver crate.  These wrappers
provide proper module qualification so that callers can import them
normally instead of reaching into `$appserver` directly.

The Rust FFI predicates use `PrologText` which accepts both Prolog
atoms and strings, so callers can pass either form.

Each wrapper guards with `current_predicate('$appserver':Name/Arity)`
so that the module works correctly even when the Rust runtime is not
loaded (e.g. in unit tests or when the swipl backend is used).

@see plugin_api for the umbrella re-export module.
*/

%% indexer_available is semidet.
%
%  Succeeds iff the Rust indexer FFI predicates are registered.
%  Use this to gate code that requires the indexer without calling it.
indexer_available :-
    current_predicate('$appserver':indexer_notify/4),
    current_predicate('$appserver':indexer_reindex/3).

%% indexer_notify(+Path, +BranchName, +StoreClustering, +HasEmbeddings) is det.
%
%  Notify the Rust IndexerRegistry that a commit happened on a branch.
%  StoreClustering is a boolean indicating whether the schema has
%  store_clustering enabled in @metadata.terminusdb.options.
%  HasEmbeddings is a boolean indicating whether the caller has verified
%  that the schema has embedding metadata. When false, the Rust-side
%  no_embedding_cache may skip the nudge instantly for domains without
%  embedding types. When true, the cache is cleared so indexing proceeds.
%  Throws `indexer_ffi_not_loaded` if the Rust runtime is not available.
indexer_notify(Path, BranchName, StoreClustering, HasEmbeddings) :-
    (   current_predicate('$appserver':indexer_notify/4)
    ->  '$appserver':indexer_notify(Path, BranchName, StoreClustering, HasEmbeddings)
    ;   throw(error(indexer_ffi_not_loaded(indexer_notify), _))
    ).

%% indexer_reindex(+Path, +BranchName, +StoreClustering) is det.
%
%  Re-index a branch from scratch. Aborts any running indexing task for
%  this branch, wipes the branch's index on tdb-search (DELETE /branch-index),
%  then starts a fresh indexing pass from the oldest commit.
%  Throws `indexer_ffi_not_loaded` if the Rust runtime is not available.
indexer_reindex(Path, BranchName, StoreClustering) :-
    (   current_predicate('$appserver':indexer_reindex/3)
    ->  '$appserver':indexer_reindex(Path, BranchName, StoreClustering)
    ;   throw(error(indexer_ffi_not_loaded(indexer_reindex), _))
    ).

%% indexer_set_config(+TdbSearchUrl, +AuthHeader) is det.
%
%  Set the tdb-search URL and auth header for the IndexerRegistry.
%  Called once at startup from the post_server_startup_hook.
indexer_set_config(Url, AuthHeader) :-
    (   current_predicate('$appserver':indexer_set_config/2)
    ->  '$appserver':indexer_set_config(Url, AuthHeader)
    ;   throw(error(indexer_ffi_not_loaded(indexer_set_config), _))
    ).

%% indexer_progress(+Path, +BranchName, -Progress) is semidet.
%
%  Query indexing progress for a branch.
indexer_progress(Path, BranchName, Progress) :-
    (   current_predicate('$appserver':indexer_progress/3)
    ->  '$appserver':indexer_progress(Path, BranchName, Progress)
    ;   throw(error(indexer_ffi_not_loaded(indexer_progress), _))
    ).

%% indexer_abort_domain(+Domain) is det.
%
%  Abort all indexer tasks for a domain.
indexer_abort_domain(Domain) :-
    (   current_predicate('$appserver':indexer_abort_domain/1)
    ->  '$appserver':indexer_abort_domain(Domain)
    ;   throw(error(indexer_ffi_not_loaded(indexer_abort_domain), _))
    ).
