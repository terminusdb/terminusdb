% We have to find out about those
ignore_predicate("dateTimeStamp/11").
ignore_predicate("time/8").
ignore_predicate("dateTime/11").

% woql_compile/n predicates are not actually real. The linter
% mysteriously complains about them though, so we have to ignore them
% until we can figure out what actually causes this.
ignore_predicate("woql_compile/5").
ignore_predicate("woql_compile/3").

% These predicates come from rust. We need a better way to ignore those
ignore_predicate("get_document_context/5").
ignore_predicate("print_all_documents_json/5").
ignore_predicate("par_print_all_documents_json/5").
ignore_predicate("print_all_documents_json_by_type/6").
ignore_predicate("par_print_all_documents_json_by_type/6").

ignore_file("./src/library").
