% We have to find out about those
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
% These predicates exist in both SWI-Prolog 9 and 10, but prolog_xref doesn't resolve them
ignore_predicate("atom_json_dict/3").
ignore_predicate("atom_json_term/3").
ignore_predicate("json_read_dict/2").
ignore_predicate("json_read_dict/3").
ignore_predicate("json_write/2").
ignore_predicate("json_write/3").
ignore_predicate("json_write_dict/2").
ignore_predicate("json_write_dict/3").

% These predicates come from rust. We need a better way to ignore those
ignore_predicate("get_document_context/2").
ignore_predicate("print_all_documents_json/8").
ignore_predicate("par_print_all_documents_json/8").
ignore_predicate("print_all_documents_json_by_type/9").
ignore_predicate("par_print_all_documents_json_by_type/9").
ignore_predicate("print_documents_json_by_id/9").
ignore_predicate("par_print_documents_json_by_id/9").


ignore_file("./src/library").
ignore_file("./x").

:- if(\+ getenv("TERMINUSDB_ENTERPRISE", true)).
ignore_file("./terminusdb-enterprise").
:- endif.
