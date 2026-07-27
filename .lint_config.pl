% Linter ignore rules for SWI-Prolog prolog_xref.
% Predicates and modules that can't be statically resolved are listed here.

% --- ignore_predicate/1 ---

% Unknown date/time predicates
ignore_predicate("dateTimeStamp/11").
ignore_predicate("time/8").
ignore_predicate("dateTime/11").

% woql_compile/n predicates are not actually real. The linter
% mysteriously complains about them though, so we have to ignore them
% until we can figure out what actually causes this.
ignore_predicate("woql_compile/5").
ignore_predicate("woql_compile/3").

% Module prefix used in meta-calls (query_response:(...)) - not an actual predicate
ignore_predicate("query_response/0").

% plunit assertions
ignore_predicate("assertion/1").
ignore_predicate("assertion/2").

% thread library predicates linter can't resolve
ignore_predicate("mutex_trylock/1").
ignore_predicate("thread_at_exit/1").

% gensym library predicates linter can't resolve
ignore_predicate("gensym/2").

% Module-qualified calls such as commit_queue:enqueue_commit/2 are sometimes
% misread by the linter as a call to commit_queue/0.
ignore_predicate("commit_queue/0").

% Predicates from library(http/json) - linter can't follow re-exports in SWI-Prolog 10
ignore_predicate("atom_json_dict/3").
ignore_predicate("atom_json_term/3").
ignore_predicate("json_read/2").
ignore_predicate("json_read_dict/2").
ignore_predicate("json_read_dict/3").
ignore_predicate("json_write/2").
ignore_predicate("json_write/3").
ignore_predicate("json_write_dict/2").
ignore_predicate("json_write_dict/3").

% aggregate_all/3 from library(aggregate) — linter can't resolve in SWI-Prolog 10
ignore_predicate("aggregate_all/3").

% SWI-Prolog 10 setup_call_cleanup/4 — valid but linter doesn't resolve it
ignore_predicate("setup_call_cleanup/4").

% Rust FFI predicates in $doc module: xref_called strips the '$doc': prefix,
% so the linter sees these as unqualified calls and flags them.
ignore_predicate("get_document_context/2").
ignore_predicate("print_all_documents_json/8").
ignore_predicate("par_print_all_documents_json/8").
ignore_predicate("print_all_documents_json_by_type/9").
ignore_predicate("par_print_all_documents_json_by_type/9").
ignore_predicate("print_documents_json_by_id/9").
ignore_predicate("par_print_documents_json_by_id/9").

% Re-exported FFI predicates: plugin_api re-exports from plugin_api/indexer
% which calls '$appserver':indexer_*. The linter cannot follow reexport chains.
ignore_predicate("indexer_notify/3").
ignore_predicate("indexer_set_config/2").
ignore_predicate("indexer_progress/3").
ignore_predicate("indexer_abort_domain/1").

% --- ignore_module/1 ---

% Rust FFI predicates registered at runtime in $-prefixed modules.
% Suppresses warnings for calls where xref_called preserves the module prefix.
% (ignore_predicate entries above cover cases where the prefix is stripped.)
ignore_module("$appserver").
ignore_module("$doc").
ignore_module("$terminus_store").

% --- ignore_file/1 ---

ignore_file("./src/library").
ignore_file("./x").

:- if(\+ getenv("TERMINUSDB_ENTERPRISE", true)).
ignore_file("./terminusdb-enterprise").
:- endif.
