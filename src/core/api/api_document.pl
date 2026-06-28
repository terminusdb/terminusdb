:- module(api_document, [
              api_can_read_document/6,
              api_insert_documents/9,
              api_delete_documents/9,
              api_delete_document/8,
              api_delete_documents_by_type/8,
              api_replace_documents/9,
              api_nuke_documents/7,
              api_generate_document_ids/4,
              call_catch_document_mutation/2,
              api_document_error_wrapper/3,
              api_read_document_selector/12,
              api_generate_document_ids/4,
              api_get_documents/4,
              api_get_document/5,
              api_full_replace_schema/2,
              idlists_duplicates_toplevel/3,
              nonground_captures/2,

              api_insert_documents_core_string/8,
              api_replace_documents_core_string/7,
              api_delete_documents_by_ids/3,

              document_input_format/1,
              document_output_format/1,
              detect_input_format/2,
              detect_output_format/3,
              convert_input_to_json_stream/4,
              document_stream_headers/3,
              document_stream_start/3,
              pre_branch_commit_id/2,
              document_stream_write/4,
              document_stream_end/2,

              run_commit_package/1,
              execute_commit_package/2,
              commit_package_test_handler/1
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(transaction/database), [
                  reset_transaction_objects_graph_descriptors/1,
                  query_context_transaction_objects/2
              ]).
:- use_module(core(document)).
:- use_module(core(document/commit_queue)).
:- use_module(core(document/json), [
                  insert_document_expanded/4,
                  insert_document_expanded/5,
                  replace_document_expanded/5,
                  insert_backlinks/2,
                  extract_return_ids/2
              ]).
:- use_module(core(document/parallel_elaboration), [
                  elaborate_insert_request_db/4,
                  elaborate_insert_request_db_with_contracts/4,
                  chunk_size/1,
                  with_commit_window_guard/4,
                  acquire_commit_window_guard/3,
                  acquire_commit_window_guard/2,
                  release_commit_window_guard/0,
                  has_commit_window_guard/0,
                  branch_key_from_transaction/2,
                  first_commit_candidate/2
              ]).
:- use_module(core(util/json_preserve), [close_list_tails/1]).
:- use_module(core(account)).
:- use_module(config(terminus_config)).

:- use_module(library(terminus_store)).
:- use_module(library(option)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(pcre), [re_match/3]).
:- use_module(library(pprint), [print_term/2]).

%%% Document Format Extension Points (multifile hooks)
%%%
%%% Enterprise can register additional formats (jsonld, rdfxml) by adding
%%% clauses to these predicates. Community only supports 'json'.

:- multifile document_input_format/1.
:- multifile document_output_format/1.
:- multifile convert_input_to_json_stream/4.
:- multifile document_stream_headers/3.
:- multifile document_stream_start/3.
:- multifile document_stream_write/4.
:- multifile document_stream_end/2.

document_input_format(json).
document_output_format(json).

detect_input_format(ContentType, Format) :-
    (   re_match('^application/json', ContentType, [])
    ->  Format = json
    ;   re_match('^application/ld\\+json', ContentType, [])
    ->  (   document_input_format(jsonld)
        ->  Format = jsonld
        ;   throw(error(unsupported_document_format(jsonld), _))
        )
    ;   re_match('^application/rdf\\+xml', ContentType, [])
    ->  (   document_input_format(rdfxml)
        ->  Format = rdfxml
        ;   throw(error(unsupported_document_format(rdfxml), _))
        )
    ;   re_match('^text/turtle', ContentType, [])
    ->  (   document_input_format(turtle)
        ->  Format = turtle
        ;   throw(error(unsupported_document_format(turtle), _))
        )
    ;   throw(error(bad_content_type(ContentType, 'application/json'), _))
    ).

detect_output_format(Search, Request, Format) :-
    (   memberchk(format=FormatParam, Search),
        FormatParam \= ''
    ->  atom_string(FormatAtom, FormatParam),
        (   document_output_format(FormatAtom)
        ->  Format = FormatAtom
        ;   throw(error(unsupported_document_format(FormatAtom), _))
        )
    ;   memberchk(accept(AcceptList), Request),
        member(media(Type/SubType, _, _, _), AcceptList),
        accept_to_format(Type/SubType, AcceptFormat),
        document_output_format(AcceptFormat)
    ->  Format = AcceptFormat
    ;   Format = json
    ).

accept_to_format(application/json, json).
accept_to_format(application/'ld+json', jsonld).
accept_to_format(application/'rdf+xml', rdfxml).
accept_to_format(text/turtle, turtle).

before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction) :-
    do_or_die(
        open_descriptor(Descriptor, Transaction),
        error(unresolvable_collection(Descriptor), _)),

    transaction_data_version(Transaction, Actual_Data_Version),
    compare_data_versions(Requested_Data_Version, Actual_Data_Version).

before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction) :-
    Commit_Info0 = commit_info{author: Author, message: Message},
    maybe_inject_auth_user(Auth, Commit_Info0, Commit_Info),
    do_or_die(
        create_context(Descriptor, Commit_Info, Context),
        error(unresolvable_collection(Descriptor), _)),
    do_or_die(
        query_default_collection(Context, Transaction),
        error(query_default_collection_failed_unexpectedly(Context), _)),
    transaction_data_version(Transaction, Actual_Data_Version),
    compare_data_versions(Requested_Data_Version, Actual_Data_Version).

api_generate_document_ids(instance, Transaction, Config, Id) :-
    (   Config.unfold = true
    ->  Include_Subdocuments = false
    ;   Include_Subdocuments = true),
    skip_generate_nsols(
        get_document_uri(Transaction, Include_Subdocuments, Id),
        Config.skip,
        Config.count).
api_generate_document_ids(schema, Transaction, Config, Id) :-
    skip_generate_nsols(
        get_schema_document_uri(Transaction, Id),
        Config.skip,
        Config.count).

api_get_documents(Transaction, Graph_Type, Config, Document) :-
    api_document:api_generate_document_ids(Graph_Type, Transaction, Config, Id),
    api_document:api_get_document(Graph_Type, Transaction, Id, Config, Document).

api_generate_document_ids_by_type(instance, Transaction, Type, Config, Id) :-
    skip_generate_nsols(
        get_document_uri_by_type(Transaction, Type, Id),
        Config.skip,
        Config.count).
api_generate_document_ids_by_type(schema, Transaction, Type, Config, Id) :-
    skip_generate_nsols(
        get_schema_document_uri_by_type(Transaction, Type, Id),
        Config.skip,
        Config.count).

api_get_documents_by_type(Transaction, Graph_Type, Type, Config, Document) :-
    api_document:api_generate_document_ids_by_type(Graph_Type, Transaction, Type, Config, Id),
    api_document:api_get_document(Graph_Type, Transaction, Id, Config, Document).


api_generate_document_ids_by_query(instance, Transaction, Type, Query, Config, Id) :-
    skip_generate_nsols(
        match_expanded_query_document_uri(Transaction, Type, Query, Id),
        Config.skip,
        Config.count).
api_generate_document_ids_by_query(schema, _Transaction, _Type, _Query, _Config, _Id) :-
    throw(error(query_is_only_supported_for_instance_graphs, _)).

api_print_documents_by_query(Transaction, Type, Query, Config, Stream_Started) :-
    '$doc':get_document_context(Transaction, Context),
    forall(api_document:api_generate_document_ids_by_query(instance, Transaction, Type, Query, Config, Id),
           (   Stream_Started = started(Started),
               do_or_die('$doc':print_document_json(current_output, Context, Id, Config.as_list, (Config.compress), (Config.unfold), (Config.minimized), Started),
                         error(document_not_found(Id), _)),
               nb_setarg(1, Stream_Started, true)
           )).

api_get_documents_by_query(Transaction, Graph_Type, Type, Query, Config, Document) :-
    api_document:api_generate_document_ids_by_query(Graph_Type, Transaction, Type, Query, Config, Id),
    api_document:api_get_document(Graph_Type, Transaction, Id, Config, Document).

api_document_exists(schema, Transaction, Id) :-
    schema_document_exists(Transaction, Id).
api_document_exists(instance, Transaction, Id) :-
    document_exists(Transaction, Id).

api_print_document(Graph_Type, Transaction, Id, Config) :-
    api_print_document(Graph_Type, Transaction, Id, Config, started(true)).
api_print_document(instance, Transaction, Id, Config, Stream_Started) :-
    '$doc':get_document_context(Transaction, Context),
    Stream_Started = started(Started),
    database_prefixes(Transaction, Prefixes),
    prefix_expand(Id, Prefixes, Id_Ex),
    do_or_die('$doc':print_document_json(current_output, Context, Id_Ex, Config.as_list, (Config.compress), (Config.unfold), (Config.minimized), Started),
              error(document_not_found(Id), _)),
   nb_setarg(1, Stream_Started, true).
api_print_document(schema, Transaction, Id, Config, Stream_Started) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)),
    json_stream_write_dict(Config, Stream_Started, Document).

api_print_documents(schema, Transaction, Config, Stream_Started) :-
    forall(api_get_documents(Transaction, schema, Config, Document),
           json_stream_write_dict(Config, Stream_Started, Document)).
api_print_documents(instance, Transaction, Config, _Stream_Started) :-
    '$doc':get_document_context(Transaction, Context),
    (   parallelize_enabled
    ->  '$doc':par_print_all_documents_json(current_output, Context, (Config.skip), (Config.count), (Config.as_list), (Config.compress), (Config.unfold), (Config.minimized))
    ;   '$doc':print_all_documents_json(current_output, Context, (Config.skip), (Config.count), (Config.as_list), (Config.compress), (Config.unfold), (Config.minimized))).

api_print_documents_by_type(schema, Transaction, Config, Type, Stream_Started) :-
    forall(api_get_documents_by_type(Transaction, schema, Type, Config, Document),
           json_stream_write_dict(Config, Stream_Started, Document)).
api_print_documents_by_type(instance, Transaction, Config, Type, _Stream_Started) :-
    '$doc':get_document_context(Transaction, Context),
    database_and_default_prefixes(Transaction,Prefixes),
    % TODO errors on unknown prefix
    prefix_expand_schema(Type, Prefixes, Type_Ex),

    (   parallelize_enabled
    ->  '$doc':par_print_all_documents_json_by_type(current_output, Context, Type_Ex, (Config.skip), (Config.count), (Config.as_list), (Config.compress), (Config.unfold), (Config.minimized))
    ;   '$doc':print_all_documents_json_by_type(current_output, Context, Type_Ex, (Config.skip), (Config.count), (Config.as_list), (Config.compress), (Config.unfold), (Config.minimized))).

api_print_documents_by_id(schema, Transaction, Config, Ids, Stream_Started) :-
    forall((member(Id, Ids),
            api_get_document(schema, Transaction, Id, Config, Document)),
           json_stream_write_dict(Config, Stream_Started, Document)).
api_print_documents_by_id(instance, Transaction, Config, Ids, _Stream_Started) :-
    '$doc':get_document_context(Transaction, Context),
    database_and_default_prefixes(Transaction, Prefixes),
    maplist({Prefixes}/[Id, Id_Ex]>>prefix_expand(Id, Prefixes, Id_Ex),
            Ids,
            Ids_Ex),
    (   parallelize_enabled
    ->  '$doc':par_print_documents_json_by_id(current_output, Context, Ids_Ex, (Config.skip), (Config.count), (Config.as_list), (Config.compress), (Config.unfold), (Config.minimized))
    ;   '$doc':print_documents_json_by_id(current_output, Context, Ids, (Config.skip), (Config.count), (Config.as_list), (Config.compress), (Config.unfold), (Config.minimized))).

api_get_document(instance, Transaction, Id, Config, Document) :-
    Options = options{compress_ids: Config.compress, unfold: Config.unfold, keep_json_type: false},
    do_or_die(get_document(Transaction, Id, Document, Options),
              error(document_not_found(Id), _)).
api_get_document(schema, Transaction, Id, _Config, Document) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

embed_document_in_error(Error, Document, New_Error) :-
    Error =.. Error_List,
    append(Error_List, [Document], New_Error_List),
    New_Error =.. New_Error_List.

known_document_error(type_not_found(_)).
known_document_error(can_not_insert_existing_object_with_id(_)).
known_document_error(unrecognized_property(_,_,_)).
known_document_error(casting_error(_,_)).
known_document_error(submitted_id_does_not_match_generated_id(_,_)).
known_document_error(submitted_document_id_does_not_have_expected_prefix(_,_)).
known_document_error(document_key_type_unknown(_)).
known_document_error(document_key_type_missing(_)).
known_document_error(subdocument_key_missing).
known_document_error(key_missing_required_field(_)).
known_document_error(document_key_not_object(_)).
known_document_error(empty_key).
known_document_error(bad_field_value(_, _)).
known_document_error(key_missing_fields(_)).
known_document_error(key_fields_not_an_array(_)).
known_document_error(key_fields_is_empty).
known_document_error(unable_to_assign_ids).
known_document_error(inserted_subdocument_as_document).
known_document_error(capture_already_bound(_)).
known_document_error(wrong_array_dimensions(_,_)).
known_document_error(not_a_unit_type(_)).
known_document_error(unknown_language_tag(_)).
known_document_error(no_language_tag_for_multilingual).
known_document_error(language_tags_repeated(_)).
known_document_error(no_property_specified_in_link(_)).
known_document_error(no_ref_or_id_in_link(_)).
known_document_error(no_ref_in_link(_)).
known_document_error(link_id_specified_but_not_valid(_)).
known_document_error(not_one_parent_of_subdocument(_)).
known_document_error(embedded_subdocument_has_linked_by).
known_document_error(back_links_not_supported_in_replace).
known_document_error(prefix_does_not_resolve(_)).
known_document_error(stored_document_is_not_a_json(_)).
known_document_error(at_prefixed_properties_not_supported(_)).
known_document_error(at_prefixed_properties_not_supported(_,_)).
known_document_error(invalid_jsondocument_at_id_must_be_iri(_)).
known_document_error(unable_to_elaborate_schema_document(_)).
known_document_error(elaboration_stale(_)).

:- meta_predicate call_catch_document_mutation(+, :).
api_document_error_wrapper(error(E, Context), Document, error(New_E, Context)) :-
    !,
    (   known_document_error(E)
    ->  embed_document_in_error(E, Document, New_E)
    ;   New_E = E
    ).
api_document_error_wrapper(Error, _, Error).

call_catch_document_mutation(Document, Goal) :-
    catch_with_backtrace(Goal,
          error(E, Context),
          (   known_document_error(E)
          ->  embed_document_in_error(E, Document, New_E),
              throw(error(New_E, _))
          ;   throw(error(E, Context)))).

api_insert_document_(schema, _Raw_JSON, _Overwrite, Transaction, Document, Captures, [Id], Captures, T-T) :-
    call_catch_document_mutation(
        Document,
        do_or_die(insert_schema_document(Transaction, Document),
                  error(document_insertion_failed_unexpectedly(Document), _))),
    do_or_die(Id = (Document.get('@id')),
              error(document_has_no_id_somehow, _)).
api_insert_document_(instance, Raw_JSON, Overwrite, Transaction, Document, Captures_In, Id, Captures_Out, BLH-BLT) :-
    call_catch_document_mutation(
        Document,
        do_or_die(insert_document(Transaction, Document, Raw_JSON, Overwrite, Captures_In, Id, BLH-BLT, Captures_Out),
                  error(document_insertion_failed_unexpectedly(Document), _))).

api_insert_document_unsafe_(schema, _, Transaction, Prefixes, Document, Captures, [Id], Captures, T-T) :-
    call_catch_document_mutation(
        Document,
        (   do_or_die(
                insert_schema_document_unsafe(Transaction, Prefixes, Document),
                error(document_insertion_failed_unexpectedly(Document), _)),
            do_or_die(
                get_dict('@id', Document, Id),
                error(document_has_no_id_somehow, _)))
    ).
api_insert_document_unsafe_(instance, Raw_JSON, Transaction, Prefixes, Document, Captures_In, Id, Captures_Out, SH-ST) :-
    call_catch_document_mutation(
        Document,
        do_or_die(
            insert_document_unsafe(Transaction, Prefixes, Document, Raw_JSON, Captures_In, Id, SH-ST, Captures_Out),
            error(document_insertion_failed_unexpectedly(Document), _))
    ).

insert_documents_(true, Graph_Type, Raw_JSON, _Overwrite, Stream, Transaction, Captures_In, Captures_Out, BackLinks, Ids) :-
    api_nuke_documents_(Graph_Type, Transaction),
    (   Graph_Type = schema
    ->  % For a schema full replace, read the context and replace the existing one.
        do_or_die(
            (   stream_to_lazy_docs(Stream,Result),
                Result = [Prefixes|Lazy_List],
                is_dict(Prefixes),
                get_dict('@type', Prefixes, "@context")
            ),
            error(no_context_found_in_schema, _)),
        call_catch_document_mutation(
            Prefixes,
            replace_context_document(Transaction, Prefixes)
        )
    ;   % Otherwise, do nothing
        database_prefixes(Transaction, Prefixes),
        stream_to_lazy_docs(Stream, Lazy_List)
    ),
    api_insert_document_from_lazy_list_unsafe(Lazy_List, Graph_Type, Raw_JSON, Transaction, Prefixes, Captures_In, Captures_Out, BackLinks-[], Ids).
insert_documents_(false, Graph_Type, Raw_JSON, Overwrite, Stream, Transaction, Captures_In, Captures_Out, BackLinks, Ids) :-
    stream_to_lazy_docs(Stream, Lazy_List),
    api_insert_document_from_lazy_list(Lazy_List, Graph_Type, Raw_JSON, Overwrite, Transaction, Captures_In, Captures_Out, BackLinks-[], Ids).

api_insert_document_from_lazy_list_unsafe([Document|Rest], Graph_Type, Raw_JSON, Transaction, Prefixes, Captures_In, Captures_Out, BLH-BLT, [Ids|New_Ids]) :-
    !,
    api_insert_document_unsafe_(Graph_Type, Raw_JSON, Transaction, Prefixes, Document, Captures_In, Ids, Captures_Mid, BLH-BLM),
    api_insert_document_from_lazy_list_unsafe(Rest, Graph_Type, Raw_JSON, Transaction, Prefixes, Captures_Mid, Captures_Out, BLM-BLT, New_Ids).
api_insert_document_from_lazy_list_unsafe([], _, _, _, _, Captures, Captures, T-T, []).

% /8 wrapper - backward compatibility (default overwrite=false)
api_insert_document_from_lazy_list(List, Graph_Type, Raw_JSON, Transaction, Captures_In, Captures_Out, BackLinks, Ids) :-
    api_insert_document_from_lazy_list(List, Graph_Type, Raw_JSON, false, Transaction, Captures_In, Captures_Out, BackLinks, Ids).

api_insert_document_from_lazy_list([Document|Rest], Graph_Type, Raw_JSON, Overwrite, Transaction, Captures_In, Captures_Out, BLH-BLT, [Ids|New_Ids]) :-
    !,
    api_insert_document_(Graph_Type, Raw_JSON, Overwrite, Transaction, Document, Captures_In, Ids, Captures_Mid, BLH-BLM),
    api_insert_document_from_lazy_list(Rest, Graph_Type, Raw_JSON, Overwrite, Transaction, Captures_Mid, Captures_Out, BLM-BLT, New_Ids).
api_insert_document_from_lazy_list([], _, _, _, _, Captures, Captures, T-T, []).

api_replace_document_from_lazy_list([Document|Rest], Graph_Type, Raw_JSON, Transaction, Create,
                                    Captures_In, Captures_Out, [Ids|New_Ids]) :-
    !,
    call_catch_document_mutation(
        Document,
        api_replace_document_(Graph_Type,
                              Raw_JSON,
                              Transaction,
                              Document,
                              Create,
                              Captures_In,
                              Ids,
                              Captures_Mid)
    ),
    api_replace_document_from_lazy_list(Rest, Graph_Type, Raw_JSON, Transaction, Create, Captures_Mid, Captures_Out, New_Ids).
api_replace_document_from_lazy_list([], _, _, _, _, Captures, Captures, []).

xor(true,false).
xor(false,true).

insert_documents_default_options(
    options{
        graph_type: instance,
        full_replace: false,
        raw_json: false,
        merge_repeats: false,
        overwrite: false,
        input_format: json
    }).

api_insert_documents(SystemDB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Transaction_Meta_Data, Ids, Options_New) :-
    insert_documents_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type), Options),
    option_or_die(author(Author), Options),
    option_or_die(message(Message), Options),
    option(full_replace(Full_Replace), Options),
    option(raw_json(Raw_JSON), Options),
    option(merge_repeats(Doc_Merge), Options),
    option(overwrite(Overwrite), Options),
    option(input_format(InputFormat), Options),
    die_if(
        (   Graph_Type = schema,
            Raw_JSON = true
        ),
        error(raw_json_and_schema_disallowed,_)
    ),
    (   commit_queue_available,
        Graph_Type = instance,
        Raw_JSON = false,
        Full_Replace = false,
        InputFormat = json,
        stream_property(Stream, position(Pos)),
        setup_call_cleanup(
            set_stream_position(Stream, Pos),
            (   stream_to_lazy_docs(Stream, LazyDocs),
                docs_parallel_eligible(LazyDocs)
            ),
            set_stream_position(Stream, Pos)
        )
    ->  api_insert_documents_queued(SystemDB, Auth, Path, Stream, Requested_Data_Version,
                                   New_Data_Version, Transaction_Meta_Data, Ids, Options)
    ;   resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
        before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
        stream_property(Stream, position(Pos)),
        with_transaction(Context,
                         (   set_stream_position(Stream, Pos),
                             branch_key_from_transaction(Transaction, BranchKey),
                             (   parallel_elaboration_enabled(Graph_Type, Raw_JSON, Full_Replace, Options, Stream)
                             ->  (   branch_descriptor{} :< Transaction.descriptor
                                 ->  with_commit_window_guard(
                                         Transaction, BranchKey, PreBranchCommitId,
                                         setup_call_cleanup(
                                             parallel_elaborate_stream(Transaction, Stream, Queue, Thread),
                                             (   CoreStream = queue(Queue),
                                                 CoreRaw_JSON = true,
                                                 (   InputFormat = json
                                                 ->  InsertStream = CoreStream
                                                 ;   convert_input_to_json_stream(InputFormat, CoreStream, Transaction, InsertStream)
                                                 ),
                                                 api_insert_documents_core(Transaction, InsertStream, Graph_Type, CoreRaw_JSON, Full_Replace, Doc_Merge, Overwrite, PreBranchCommitId, Ids)
                                             ),
                                             cleanup_parallel_stream(Queue, Thread)))
                                 ;   % Non-branch descriptors (e.g. system_descriptor) have no
                                     % advancing branch commit, so a commit-window guard is
                                     % neither applicable nor acquireable. Fall back to the
                                     % synchronous path without a guard.
                                     CoreStream = Stream,
                                     CoreRaw_JSON = Raw_JSON,
                                     (   InputFormat = json
                                     ->  InsertStream = CoreStream
                                     ;   convert_input_to_json_stream(InputFormat, CoreStream, Transaction, InsertStream)
                                     ),
                                     api_insert_documents_core(Transaction, InsertStream, Graph_Type, CoreRaw_JSON, Full_Replace, Doc_Merge, Overwrite, Ids)
                                 )
                             ;   CoreStream = Stream,
                                 CoreRaw_JSON = Raw_JSON,
                                 (   InputFormat = json
                                 ->  InsertStream = CoreStream
                                 ;   convert_input_to_json_stream(InputFormat, CoreStream, Transaction, InsertStream)
                                 ),
                                 api_insert_documents_core(Transaction, InsertStream, Graph_Type, CoreRaw_JSON, Full_Replace, Doc_Merge, Overwrite, Ids)
                             )
                         ),
                         Meta_Data,
                         Options),
        meta_data_version(Transaction, Meta_Data, New_Data_Version),
        Transaction_Meta_Data = Meta_Data
    ).

pre_branch_commit_id(Transaction, CommitId) :-
    (   catch(transaction_data_version(Transaction, data_version(branch, CommitId)),
              error(data_version_not_found(_), _),
              fail)
    ->  true
    ;   CommitId = none).

parallel_elaboration_enabled(Graph_Type, Raw_JSON, Full_Replace, Options, Stream) :-
    Graph_Type = instance,
    Raw_JSON = false,
    Full_Replace = false,
    option(input_format(json), Options),
    current_predicate(parallel_elaboration:elaborate_insert_request_db/4),
    \+ parallel_elaboration_disabled,
    stream_property(Stream, position(Pos)),
    setup_call_cleanup(
        set_stream_position(Stream, Pos),
        (   stream_to_lazy_docs(Stream, LazyDocs),
            docs_parallel_eligible(LazyDocs)
        ),
        set_stream_position(Stream, Pos)
    ).

parallel_replace_enabled(Graph_Type, Raw_JSON, Options, Stream) :-
    Graph_Type = instance,
    Raw_JSON = false,
    option(input_format(json), Options),
    current_predicate(parallel_elaboration:elaborate_insert_request_db/4),
    \+ parallel_elaboration_disabled,
    stream_property(Stream, position(Pos)),
    setup_call_cleanup(
        set_stream_position(Stream, Pos),
        (   stream_to_lazy_docs(Stream, LazyDocs),
            docs_parallel_eligible(LazyDocs)
        ),
        set_stream_position(Stream, Pos)
    ).

commit_queue_available :-
    current_predicate(commit_queue:start_workers/1),
    current_predicate(commit_queue:enqueue_commit/2),
    commit_queue:multi_purpose_worker_thread(_).

docs_parallel_eligible([]).
docs_parallel_eligible([Doc|Rest]) :-
    doc_parallel_eligible(Doc),
    docs_parallel_eligible(Rest).

doc_parallel_eligible(Doc) :-
    \+ contains_ineligible_marker(Doc).

contains_ineligible_marker(Doc) :-
    is_dict(Doc),
    !,
    (   get_dict('@capture', Doc, _)
    ;   get_dict('@linked-by', Doc, _)
    ;   dict_pairs(Doc, _, Pairs),
        member(_Key-Value, Pairs),
        contains_ineligible_marker(Value)
    ).
contains_ineligible_marker(List) :-
    is_list(List),
    member(Item, List),
    contains_ineligible_marker(Item).

parallel_elaboration_disabled :-
    (   getenv('TERMINUSDB_PARALLEL_ELABORATION', Value)
    ->  (   atom(Value)
        ->  downcase_atom(Value, Lower),
            member(Lower, ['0', false, off])
        ;   Value = 0
        )
    ;   false
    ).

parallel_elaborate_stream(DB, Stream, Queue, Thread) :-
    stream_to_lazy_docs(Stream, LazyDocs),
    message_queue_create(Queue, []),
    thread_create(elaborate_stream_producer(DB, LazyDocs, Queue), Thread, []).

cleanup_parallel_stream(Queue, Thread) :-
    catch(thread_signal(Thread, abort), _, true),
    catch(thread_join(Thread, _), _, true),
    catch(message_queue_destroy(Queue), _, true).

take_chunk_lazy(_, 0, [], Rest) :- !, Rest = [].
take_chunk_lazy(LazyDocs, N, Chunk, Rest) :-
    N > 0,
    (   LazyDocs = [Doc|Rest0]
    ->  N1 is N - 1,
        Chunk = [Doc|ChunkRest],
        take_chunk_lazy(Rest0, N1, ChunkRest, Rest)
    ;   Chunk = [],
        Rest = LazyDocs
    ).

elaborate_stream_producer(DB, LazyDocs, Queue) :-
    chunk_size(ChunkSize),
    take_chunk_lazy(LazyDocs, ChunkSize, Chunk, Rest),
    (   Chunk == []
    ->  thread_send_message(Queue, done)
    ;   catch(
            parallel_elaboration:elaborate_insert_request_db_with_contracts(DB, Chunk, Pairs, [workers(4), wrap_api_errors(true)]),
            Error,
            (   thread_send_message(Queue, error(Error)),
                throw(Error)
            )
        )
    ->  thread_send_message(Queue, docs(Pairs)),
        elaborate_stream_producer(DB, Rest, Queue)
    ;   % Fallback: the catch above re-throws detailed errors, but if a
        % failure path reaches here without a detailed error, send a bare
        % error message so the consumer's defensive guard can react.
        thread_send_message(Queue, error)
    ).

consume_elaborated_queue(Queue, Transaction, Overwrite, CommitId, Ids, Captures_Out) :-
    (   has_commit_window_guard
    ->  WaitForGuard = true
    ;   WaitForGuard = false
    ),
    consume_elaborated_queue_(Queue, Transaction, Overwrite, CommitId, WaitForGuard, Ids, Captures_Out).

consume_elaborated_queue_(Queue, Transaction, Overwrite, CommitId, WaitForGuard, Ids, Captures_Out) :-
    (   WaitForGuard = true,
        \+ first_commit_candidate(Transaction, CommitId)
    ->  release_commit_window_guard
    ;   true
    ),
    thread_get_message(Queue, Message),
    (   WaitForGuard = true
    ->  branch_key_from_transaction(Transaction, BranchKey),
        acquire_commit_window_guard(BranchKey, CommitId)
    ;   true
    ),
    (   Message = done
    ->  Ids = [],
        empty_assoc(Captures_Out)
    ;   Message = error
    % Defensive guard: the producer's fallback branch (taken when the
    % catch around chunk elaboration fails without re-throwing a detailed
    % error) can still send a bare `error` message. Keep this clause so
    % the consumer does not hang on an unexpected queue message.
    ->  throw(error(parallel_elaboration_failed, _))
    ;   Message = error(Error)
    ->  throw(Error)
    ;   Message = docs(Pairs)
    ->  pairs_to_insert_ids_and_captures(Pairs, Transaction, Overwrite, Ids_Mid, Captures_Mid),
        consume_elaborated_queue_(Queue, Transaction, Overwrite, CommitId, WaitForGuard, Ids_Tail, Captures_Tail),
        append(Ids_Mid, Ids_Tail, Ids),
        merge_captures(Captures_Mid, Captures_Tail, Captures_Out)
    ).

pairs_to_insert_ids_and_captures(Pairs, Transaction, Overwrite, Ids, Captures_Out) :-
    pairs_to_insert_ids_and_captures_(Pairs, Transaction, Overwrite, Ids, Captures_Out).

verify_contract(Transaction, Contract) :-
    is_dict(Contract, verification_contract),
    (   get_dict(pre_schema_layer_id, Contract, PreSchemaLayerId),
        PreSchemaLayerId \= none
    ->  [SchemaObject] = Transaction.schema_objects,
        layer_to_id(SchemaObject.read, CurrentSchemaLayerId),
        (   CurrentSchemaLayerId \= PreSchemaLayerId
        ->  throw(error(elaboration_stale(schema_layer_changed), _))
        ;   true)
    ;   true),
    (   get_dict(pre_branch_commit_id, Contract, PreBranchCommitId),
        branch_key_from_transaction(Transaction, BranchKey),
        get_dict(must_exist, Contract, MustExist),
        get_dict(must_not_exist, Contract, MustNotExist),
        (   catch(transaction_data_version(Transaction, data_version(branch, CurrentBranchCommitId)),
                  error(data_version_not_found(_), _),
                  fail)
        ->  true
        ;   CurrentBranchCommitId = none),
        (   CurrentBranchCommitId == PreBranchCommitId
        ->  true
        ;   CurrentBranchCommitId \= none,
            '$change_window':intersects(BranchKey, PreBranchCommitId, CurrentBranchCommitId,
                                        MustExist, MustNotExist, _)
        ->  throw(fail_transaction)
        ;   true
        )
    ;   true).

pairs_to_insert_ids_and_captures_([], _, _, [], empty_assoc).
pairs_to_insert_ids_and_captures_([Doc-Contract], Transaction, Overwrite, [Ids], Captures_Out) :-
    enrich_pair_for_operation(insert, Contract, EnrichedContract),
    verify_contract(Transaction, EnrichedContract),
    insert_document_expanded(Transaction, Doc, EnrichedContract, Overwrite, _),
    is_dict(EnrichedContract, verification_contract),
    get_dict(id_pairs, EnrichedContract, Id_Pairs),
    extract_return_ids(Id_Pairs, Ids),
    get_dict(captures_out, EnrichedContract, Captures_Out).
pairs_to_insert_ids_and_captures_([Doc-Contract|Rest], Transaction, Overwrite, [Ids|Ids_Rest], Captures_Out) :-
    Rest \= [],
    enrich_pair_for_operation(insert, Contract, EnrichedContract),
    verify_contract(Transaction, EnrichedContract),
    insert_document_expanded(Transaction, Doc, EnrichedContract, Overwrite, _),
    is_dict(EnrichedContract, verification_contract),
    get_dict(id_pairs, EnrichedContract, Id_Pairs),
    extract_return_ids(Id_Pairs, Ids),
    get_dict(captures_out, EnrichedContract, Captures_First),
    pairs_to_insert_ids_and_captures_(Rest, Transaction, Overwrite, Ids_Rest, Captures_Rest),
    merge_captures(Captures_First, Captures_Rest, Captures_Out).

merge_captures(Captures1, Captures2, Merged) :-
    (   empty_assoc(Captures1)
    ->  Merged = Captures2
    ;   empty_assoc(Captures2)
    ->  Merged = Captures1
    ;   assoc_to_list(Captures2, Pairs2),
        foldl(merge_capture_pair, Pairs2, Captures1, Merged)
    ).

merge_capture_pair(Key-Value, Captures_In, Captures_Out) :-
    (   get_assoc(Key, Captures_In, Existing)
    ->  (   Existing == Value
        ->  Captures_Out = Captures_In
        ;   (   unify_with_occurs_check(Existing, Value)
            ->  Captures_Out = Captures_In
            ;   throw(error(capture_already_bound(Key, Existing), _))
            )
        )
    ;   put_assoc(Key, Captures_In, Value, Captures_Out)
    ).

consume_replaced_queue(Queue, Transaction, Create, CommitId, Ids, Captures_Out) :-
    (   has_commit_window_guard
    ->  WaitForGuard = true
    ;   WaitForGuard = false
    ),
    consume_replaced_queue_(Queue, Transaction, Create, CommitId, WaitForGuard, Ids, Captures_Out).

consume_replaced_queue_(Queue, Transaction, Create, CommitId, WaitForGuard, Ids, Captures_Out) :-
    (   WaitForGuard = true,
        \+ first_commit_candidate(Transaction, CommitId)
    ->  release_commit_window_guard
    ;   true
    ),
    thread_get_message(Queue, Message),
    (   WaitForGuard = true
    ->  branch_key_from_transaction(Transaction, BranchKey),
        acquire_commit_window_guard(BranchKey, CommitId)
    ;   true
    ),
    (   Message = done
    ->  Ids = [],
        empty_assoc(Captures_Out)
    ;   Message = error
    % Defensive guard: see the matching clause in consume_elaborated_queue_/6.
    % The producer's fallback branch can still send a bare `error` message,
    % so this clause prevents the consumer from hanging silently.
    ->  throw(error(parallel_elaboration_failed, _))
    ;   Message = error(Error)
    ->  throw(Error)
    ;   Message = docs(Pairs)
    ->  pairs_to_replace_ids_and_captures(Pairs, Transaction, Create, Ids_Mid, Captures_Mid),
        consume_replaced_queue_(Queue, Transaction, Create, CommitId, WaitForGuard, Ids_Tail, Captures_Tail),
        append(Ids_Mid, Ids_Tail, Ids),
        merge_captures(Captures_Mid, Captures_Tail, Captures_Out)
    ).

pairs_to_replace_ids_and_captures([], _, _, [], empty_assoc).
pairs_to_replace_ids_and_captures([Doc-Contract], Transaction, Create, [[Id]], Captures_Out) :-
    enrich_pair_for_operation(replace, Contract, EnrichedContract),
    verify_contract(Transaction, EnrichedContract),
    replace_document_expanded(Transaction, Doc, EnrichedContract, Create, Id),
    is_dict(EnrichedContract, verification_contract),
    get_dict(captures_out, EnrichedContract, Captures_Out).
pairs_to_replace_ids_and_captures([Doc-Contract|Rest], Transaction, Create, [[Id]|Ids], Captures_Out) :-
    Rest \= [],
    enrich_pair_for_operation(replace, Contract, EnrichedContract),
    verify_contract(Transaction, EnrichedContract),
    replace_document_expanded(Transaction, Doc, EnrichedContract, Create, Id),
    is_dict(EnrichedContract, verification_contract),
    get_dict(captures_out, EnrichedContract, Captures_First),
    pairs_to_replace_ids_and_captures(Rest, Transaction, Create, Ids, Captures_Rest),
    merge_captures(Captures_First, Captures_Rest, Captures_Out).

api_insert_documents_core(Transaction, queue(Queue), Graph_Type, _Raw_JSON, _Full_Replace, Doc_Merge, Overwrite, CommitId, Ids) :-
    ensure_transaction_has_builder(Graph_Type, Transaction),
    consume_elaborated_queue(Queue, Transaction, Overwrite, CommitId, Ids_List, Captures_Out),
    die_if(nonground_captures(Captures_Out, Nonground),
           error(not_all_captures_found(Nonground), _)),
    idlists_duplicates_toplevel(Ids_List, Duplicates, Ids),
    (   Doc_Merge = true
    ->  true
    ;   die_if(Duplicates \= [],
               error(same_ids_in_one_transaction(Duplicates), _))
    ).

api_insert_documents_core(Transaction, pairs([]), Graph_Type, _Raw_JSON, _Full_Replace, _Doc_Merge, _Overwrite, []) :-
    !,
    ensure_transaction_has_builder(Graph_Type, Transaction).
api_insert_documents_core(Transaction, pairs(Pairs), Graph_Type, _Raw_JSON, _Full_Replace, Doc_Merge, Overwrite, Ids) :-
    !,
    ensure_transaction_has_builder(Graph_Type, Transaction),
    pairs_to_insert_ids_and_captures_(Pairs, Transaction, Overwrite, Ids_List, Captures_Out),
    die_if(nonground_captures(Captures_Out, Nonground),
           error(not_all_captures_found(Nonground), _)),
    idlists_duplicates_toplevel(Ids_List, Duplicates, Ids),
    (   Doc_Merge = true
    ->  true
    ;   die_if(Duplicates \= [],
               error(same_ids_in_one_transaction(Duplicates), _))
    ).

api_insert_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Overwrite, Ids) :-
    empty_assoc(Captures_In),
    ensure_transaction_has_builder(Graph_Type, Transaction),
    insert_documents_(Full_Replace, Graph_Type, Raw_JSON, Overwrite, Stream, Transaction, Captures_In, Captures_Out, BackLinks, Ids_List),
    die_if(nonground_captures(Captures_Out, Nonground),
           error(not_all_captures_found(Nonground), _)),
    database_instance(Transaction, [Instance]),
    insert_backlinks(BackLinks, Instance),
    idlists_duplicates_toplevel(Ids_List, Duplicates, Ids),
    (   Doc_Merge = true
    ->  true
    ;   die_if(Duplicates \= [],
               error(same_ids_in_one_transaction(Duplicates), _))
    ).

api_insert_documents_core_string(Transaction, String, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Overwrite, Ids) :-
    open_string(String, Stream),
    api_insert_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Overwrite, Ids_Atoms),
    % Convert atoms to strings for Rust FFI compatibility
    maplist(atom_string, Ids_Atoms, Ids).


idlists_duplicates_toplevel(Ids, Duplicates, Toplevel) :-
    append(Ids,All_Ids),
    length(All_Ids, N),
    sort(All_Ids, Sorted),
    (   length(Sorted, N)
    ->  Duplicates = []
    ;   has_duplicates(All_Ids, Duplicates)
    ),
    maplist([[Id|_],Id]>>true, Ids, Toplevel).

nonground_captures(Captures, Nonground) :-
    findall(Ref,
            (   gen_assoc(Ref, Captures, Var),
                var(Var)),
            Nonground),
    Nonground \= [].

api_delete_document_(schema, Transaction, Id) :-
    delete_schema_document(Transaction, Id).
api_delete_document_(instance, Transaction, Id) :-
    delete_document(Transaction, Id).

% Bulk deletion with reference counting support for sys:JSON
api_delete_documents_bulk_(schema, Transaction, Ids) :-
    forall(member(Id, Ids),
           delete_schema_document(Transaction, Id)).
api_delete_documents_bulk_(instance, Transaction, Ids) :-
    'document/json':delete_documents_bulk(Transaction, Ids).

delete_documents_default_options(
    options{
        graph_type: instance
    }).

api_delete_documents(SystemDB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Transaction_Meta_Data, Ids, Options_New) :-
    die_if(
        nonvar(Ids),
        error(unexpected_argument_instantiation(api_delete_documents, Ids), _)),
    delete_documents_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type), Options),
    option(author(Author), Options),
    option(message(Message), Options),

    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         % First pass: collect all IDs to delete
                         findall(Id,
                                 (   json_read_list_stream(Stream, ID_Unchecked),
                                     param_check_json(non_empty_string, id, ID_Unchecked, Id)
                                 ),
                                 Ids_To_Delete),
                         % Second pass: delete all documents with the full deletion set
                         api_delete_documents_bulk_(Graph_Type, Transaction, Ids_To_Delete)
                     ),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version),
    Transaction_Meta_Data = Meta_Data.

api_delete_documents_by_ids(Transaction, Graph_Type, Ids) :-
    forall(
        member(Id, Ids),
        (   atom_string(Id_Atom, Id),
            api_delete_document_(Graph_Type, Transaction, Id_Atom)
        )
    ).

api_delete_document(SystemDB, Auth, Path, ID, Requested_Data_Version, New_Data_Version, Transaction_Meta_Data, Options) :-
    option(graph_type(Graph_Type), Options),
    option(author(Author), Options),
    option(message(Message), Options),

    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_delete_document_(Graph_Type, Transaction, ID),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version),
    Transaction_Meta_Data = Meta_Data.

api_delete_documents_by_type_for_graph(schema, Transaction, Type) :-
    forall(api_generate_document_ids_by_type(schema, Transaction, Type, _{skip: 0, count: unlimited}, ID),
           api_delete_document_(schema, Transaction, ID)).
api_delete_documents_by_type_for_graph(instance, Transaction, Type) :-
    delete_documents_by_type(Transaction, Type, true).

api_delete_documents_by_type(SystemDB, Auth, Path, Type, Requested_Data_Version, New_Data_Version, Transaction_Meta_Data, Options) :-
    option(graph_type(Graph_Type), Options),
    option(author(Author), Options),
    option(message(Message), Options),

    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_delete_documents_by_type_for_graph(Graph_Type, Transaction, Type),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version),
    Transaction_Meta_Data = Meta_Data.

api_nuke_documents_(schema, Transaction) :-
    nuke_schema_documents(Transaction).
api_nuke_documents_(instance, Transaction) :-
    nuke_documents(Transaction).

nuke_documents_default_options(
    options{
        graph_type: instance
    }).

api_nuke_documents(SystemDB, Auth, Path, Requested_Data_Version, New_Data_Version, Transaction_Meta_Data, Options_New) :-
    nuke_documents_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type),Options),
    option_or_die(author(Author),Options),
    option_or_die(message(Message),Options),
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_nuke_documents_(Graph_Type, Transaction),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version),
    Transaction_Meta_Data = Meta_Data.

api_replace_document_(instance, Raw_JSON, Transaction, Document, Create, Captures_In, Ids, Captures_Out):-
    replace_document(Transaction, Document, Create, Raw_JSON, Captures_In, Ids, _Dependencies, Captures_Out).
api_replace_document_(schema, _Raw_JSON, Transaction, Document, Create, Captures_In, [Id], Captures_In):-
    replace_schema_document(Transaction, Document, Create, Id).


replace_document_default_options(
    options{
        graph_type: instance,
        create: false,
        raw_json: false,
        merge_repeats: false,
        input_format: json
    }).

api_replace_documents(SystemDB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Transaction_Meta_Data, Ids, Options_New) :-
    replace_document_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type),Options),
    option_or_die(create(Create),Options),
    option_or_die(author(Author),Options),
    option(message(Message),Options),
    option(raw_json(Raw_JSON),Options,false),
    option(merge_repeats(Doc_Merge), Options),
    option(input_format(InputFormat), Options),
    (   commit_queue_available,
        Graph_Type = instance,
        Raw_JSON = false,
        InputFormat = json,
        stream_property(Stream, position(Pos)),
        setup_call_cleanup(
            set_stream_position(Stream, Pos),
            (   stream_to_lazy_docs(Stream, LazyDocs),
                docs_parallel_eligible(LazyDocs)
            ),
            set_stream_position(Stream, Pos)
        )
    ->  api_replace_documents_queued(SystemDB, Auth, Path, Stream, Requested_Data_Version,
                                    New_Data_Version, Transaction_Meta_Data, Ids, Options)
    ;   resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
        before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
        stream_property(Stream, position(Pos)),
        with_transaction(Context,
                         (   set_stream_position(Stream, Pos),
                             branch_key_from_transaction(Transaction, BranchKey),
                             (   InputFormat = json
                             ->  ReplaceStream = Stream
                             ;   convert_input_to_json_stream(InputFormat, Stream, Transaction, ReplaceStream)
                             ),
                             (   parallel_replace_enabled(Graph_Type, Raw_JSON, Options, ReplaceStream)
                             ->  with_commit_window_guard(
                                     Transaction, BranchKey, PreBranchCommitId,
                                     setup_call_cleanup(
                                         parallel_elaborate_stream(Transaction, ReplaceStream, Queue, Thread),
                                         api_replace_documents_core(Transaction, queue(Queue), Graph_Type, Raw_JSON, Create, Doc_Merge, PreBranchCommitId, Ids),
                                         cleanup_parallel_stream(Queue, Thread)))
                             ;   api_replace_documents_core(Transaction, ReplaceStream, Graph_Type, Raw_JSON, Create, Doc_Merge, Ids)
                             )
                         ),
                         Meta_Data,
                         Options),
        meta_data_version(Transaction, Meta_Data, New_Data_Version),
        Transaction_Meta_Data = Meta_Data
    ).

api_replace_documents_core(Transaction, queue(Queue), Graph_Type, _Raw_JSON, Create, Doc_Merge, CommitId, Ids) :-
    !,
    ensure_transaction_has_builder(Graph_Type, Transaction),
    consume_replaced_queue(Queue, Transaction, Create, CommitId, Ids_List, Captures_Out),
    die_if(nonground_captures(Captures_Out, Nonground),
           error(not_all_captures_found(Nonground), _)),
    idlists_duplicates_toplevel(Ids_List, Duplicates, Ids),
    (   Doc_Merge = true
    ->  true
    ;   die_if(Duplicates \= [],
               error(same_ids_in_one_transaction(Duplicates), _))
    ).
api_replace_documents_core(Transaction, pairs(Pairs), Graph_Type, _Raw_JSON, Create, Doc_Merge, Ids) :-
    !,
    ensure_transaction_has_builder(Graph_Type, Transaction),
    pairs_to_replace_ids_and_captures(Pairs, Transaction, Create, Ids_List, Captures_Out),
    die_if(nonground_captures(Captures_Out, Nonground),
           error(not_all_captures_found(Nonground), _)),
    idlists_duplicates_toplevel(Ids_List, Duplicates, Ids),
    (   Doc_Merge = true
    ->  true
    ;   die_if(Duplicates \= [],
               error(same_ids_in_one_transaction(Duplicates), _))
    ).
api_replace_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Create, Doc_Merge, Ids) :-
    empty_assoc(Captures),
    ensure_transaction_has_builder(Graph_Type, Transaction),
    stream_to_lazy_docs(Stream, Lazy_List),
    api_replace_document_from_lazy_list(Lazy_List,
                                        Graph_Type,
                                        Raw_JSON,
                                        Transaction,
                                        Create,
                                        Captures,
                                        Captures_Out,
                                        Ids_List),
    die_if(nonground_captures(Captures_Out, Nonground),
           error(not_all_captures_found(Nonground), _)),
    idlists_duplicates_toplevel(Ids_List, Duplicates, Ids),
    (   Doc_Merge = true
    ->  true
    ;   die_if(Duplicates \= [],
               error(same_ids_in_one_transaction(Duplicates), _))
    ).

api_replace_documents_core_string(Transaction, String, Graph_Type, Raw_JSON, Create, Doc_Merge, Ids) :-
    open_string(String, Stream),
    api_replace_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Create, Doc_Merge, Ids_Atoms),
    % Convert atoms to strings for Rust FFI compatibility
    maplist(atom_string, Ids_Atoms, Ids).

api_insert_documents_queued(SystemDB, Auth, Path, Stream, Requested_Data_Version,
                            New_Data_Version, Transaction_Meta_Data, Ids, Options) :-
    option(graph_type(Graph_Type), Options),
    option_or_die(author(Author), Options),
    option_or_die(message(Message), Options),
    option(overwrite(Overwrite), Options),
    option(input_format(InputFormat), Options),
    option(merge_repeats(Doc_Merge), Options),
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
    (   branch_descriptor{} :< Transaction.descriptor
    ->  branch_key_from_transaction(Transaction, BranchKey),
        stream_property(Stream, position(Pos)),
        set_stream_position(Stream, Pos),
        (   InputFormat = json
        ->  DocsStream = Stream
        ;   convert_input_to_json_stream(InputFormat, Stream, Transaction, DocsStream)
        ),
        stream_to_lazy_docs(DocsStream, LazyDocs),
        collect_stream_docs(LazyDocs, Docs),
        ElaborationOptions = [workers(4), wrap_api_errors(true)],
        parallel_elaboration:elaborate_insert_request_db_with_contracts(Transaction, Docs, Pairs, ElaborationOptions),
        query_context_transaction_objects(Context, TransactionObjects),
        message_queue_create(ReplyQueue, []),
        gensym(request, RequestId),
        producer_timeout(ProducerTimeout),
        setup_call_cleanup(
            true,
            (   Package = commit_package{
                    branch_key: BranchKey,
                    all_branches: [BranchKey],
                    transaction_objects: TransactionObjects,
                    operation: insert,
                    branch_operations: [branch_operation{
                                           branch_key: BranchKey,
                                           doc_contract_pairs: Pairs,
                                           delete_ids: [],
                                           raw_docs: Docs
                                       }],
                    commit_info: Context.commit_info,
                    options: options{overwrite: Overwrite, merge_repeats: Doc_Merge},
                    graph_type: Graph_Type,
                    reply_queue: ReplyQueue,
                    request_id: RequestId
                },
                commit_queue:enqueue_commit(BranchKey, Package),
                (   catch(thread_get_message(ReplyQueue,
                                            commit_result(Result, RequestId),
                                            [timeout(ProducerTimeout)]),
                          error(existence_error(message_queue, _), _),
                          (Result = timeout))
                ->  true
                ;   Result = timeout
                )
            ),
            catch(message_queue_destroy(ReplyQueue), _, true)
        ),
        (   Result = success(Meta_Data, Ids)
        ->  meta_data_version(Transaction, Meta_Data, New_Data_Version),
            Transaction_Meta_Data = Meta_Data
        ;   handle_commit_result(Result, _Meta_Data, Ids)
        )
    ;   % Non-branch descriptors (e.g. system_descriptor) have no advancing
        % branch commit, so the commit queue cannot serialize them. Fall back
        % to the synchronous path inside the already-open transaction.
        stream_property(Stream, position(Pos)),
        with_transaction(Context,
                         (   set_stream_position(Stream, Pos),
                             (   InputFormat = json
                             ->  CoreStream = Stream
                             ;   convert_input_to_json_stream(InputFormat, Stream, Transaction, CoreStream)
                             ),
                             api_insert_documents_core(Transaction, CoreStream, Graph_Type, false, false,
                                                       Doc_Merge, Overwrite, Ids)
                         ),
                         Meta_Data,
                         Options),
        meta_data_version(Transaction, Meta_Data, New_Data_Version),
        Transaction_Meta_Data = Meta_Data
    ).

api_replace_documents_queued(SystemDB, Auth, Path, Stream, Requested_Data_Version,
                             New_Data_Version, Transaction_Meta_Data, Ids, Options) :-
    option(graph_type(Graph_Type), Options),
    option_or_die(create(Create), Options),
    option_or_die(author(Author), Options),
    option_or_die(message(Message), Options),
    option(merge_repeats(Doc_Merge), Options),
    option(input_format(InputFormat), Options),
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Auth, Author, Message, Requested_Data_Version, Context, Transaction),
    (   branch_descriptor{} :< Transaction.descriptor
    ->  branch_key_from_transaction(Transaction, BranchKey),
        stream_property(Stream, position(Pos)),
        set_stream_position(Stream, Pos),
        (   InputFormat = json
        ->  ReplaceStream = Stream
        ;   convert_input_to_json_stream(InputFormat, Stream, Transaction, ReplaceStream)
        ),
        stream_to_lazy_docs(ReplaceStream, LazyDocs),
        collect_stream_docs(LazyDocs, Docs),
        ElaborationOptions = [workers(4), wrap_api_errors(true)],
        parallel_elaboration:elaborate_insert_request_db_with_contracts(Transaction, Docs, Pairs, ElaborationOptions),
        query_context_transaction_objects(Context, TransactionObjects),
        message_queue_create(ReplyQueue, []),
        gensym(request, RequestId),
        producer_timeout(ProducerTimeout),
        setup_call_cleanup(
            true,
            (   Package = commit_package{
                    branch_key: BranchKey,
                    all_branches: [BranchKey],
                    transaction_objects: TransactionObjects,
                    operation: replace,
                    branch_operations: [branch_operation{
                                           branch_key: BranchKey,
                                           doc_contract_pairs: Pairs,
                                           delete_ids: [],
                                           raw_docs: Docs
                                       }],
                    commit_info: Context.commit_info,
                    options: options{create: Create, merge_repeats: Doc_Merge},
                    graph_type: Graph_Type,
                    reply_queue: ReplyQueue,
                    request_id: RequestId
                },
                commit_queue:enqueue_commit(BranchKey, Package),
                (   catch(thread_get_message(ReplyQueue,
                                            commit_result(Result, RequestId),
                                            [timeout(ProducerTimeout)]),
                          error(existence_error(message_queue, _), _),
                          (Result = timeout))
                ->  true
                ;   Result = timeout
                )
            ),
            catch(message_queue_destroy(ReplyQueue), _, true)
        ),
        (   Result = success(Meta_Data, Ids)
        ->  meta_data_version(Transaction, Meta_Data, New_Data_Version),
            Transaction_Meta_Data = Meta_Data
        ;   handle_commit_result(Result, _Meta_Data, Ids)
        )
    ;   % Non-branch descriptors have no advancing branch commit; fall back to
        % the synchronous path.
        stream_property(Stream, position(Pos)),
        with_transaction(Context,
                         (   set_stream_position(Stream, Pos),
                             (   InputFormat = json
                             ->  CoreStream = Stream
                             ;   convert_input_to_json_stream(InputFormat, Stream, Transaction, CoreStream)
                             ),
                             api_replace_documents_core(Transaction, CoreStream, Graph_Type, false, Create,
                                                       Doc_Merge, Ids)
                         ),
                         Meta_Data,
                         Options),
        meta_data_version(Transaction, Meta_Data, New_Data_Version),
        Transaction_Meta_Data = Meta_Data
    ).

producer_timeout(Timeout) :-
    (   getenv('TERMINUSDB_COMMIT_QUEUE_TIMEOUT', Value),
        catch(number_string(Timeout, Value), _, fail)
    ->  (   Timeout > 0
        ->  true
        ;   Timeout = 30
        )
    ;   Timeout = 30
    ).

handle_commit_result(success(Meta_Data, Ids), Meta_Data, Ids).
handle_commit_result(reject(Reason), _Meta_Data, _Ids) :-
    throw(error(commit_rejected(Reason), _)).
handle_commit_result(error(Exception), _Meta_Data, _Ids) :-
    throw(error(Exception, _)).
handle_commit_result(timeout, _Meta_Data, _Ids) :-
    throw(error(commit_queue_timeout, _)).

collect_stream_docs(LazyDocs, Docs) :-
    (   get_attr(LazyDocs, 'util/lazy_docs', lazy_input(Stream, _))
    ->  read_all_docs_from_stream(Stream, Docs)
    ;   collect_stream_docs_(LazyDocs, Docs)
    ).

collect_stream_docs_([], []) :- !.
collect_stream_docs_([Doc|Rest], [Doc|Docs]) :-
    collect_stream_docs_(Rest, Docs).

read_all_docs_from_stream(Stream, Docs) :-
    read_all_docs_from_stream_(Stream, Docs).

read_all_docs_from_stream_(Stream, Docs) :-
    (   json_preserve:json_read_dict(Stream, Doc, [default_tag(json), end_of_file(eof)])
    ->  (   Doc = eof
        ->  Docs = []
        ;   is_list(Doc)
        ->  append(Doc, Rest, Docs),
            read_all_docs_from_stream_(Stream, Rest)
        ;   Docs = [Doc|Rest],
            read_all_docs_from_stream_(Stream, Rest)
        )
    ;   Docs = []
    ).

api_can_read_document(System_DB, Auth, Path, Graph_Type, Requested_Data_Version, Actual_Data_Version) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, _).

format_read_documents(Format, Transaction, Graph_Type, Id, Ids, Type, Query, Config, DataVersion) :-
    database_context_object(Transaction, SchemaContext),
    database_prefixes(Transaction, InternalPrefixes),
    put_dict(schema_context, Config, SchemaContext, Config1),
    put_dict(internal_prefixes, Config1, InternalPrefixes, ConfigWithCtx),

    Request = ConfigWithCtx.request,
    document_stream_headers(Format, Request, DataVersion),

    document_stream_start(Format, ConfigWithCtx, StreamState),

    (   nonvar(Query)
    ->  die_if(Graph_Type \= instance,
               error(query_is_only_supported_for_instance_graphs, _)),
        expand_query_document(Transaction, Type, Query, Query_Ex, Type_Ex),
        forall(api_get_documents_by_query(Transaction, instance, Type_Ex, Query_Ex, ConfigWithCtx, Document),
               document_stream_write(Format, ConfigWithCtx, StreamState, Document))
    ;   ground(Id)
    ->  api_get_document(Graph_Type, Transaction, Id, ConfigWithCtx, Document),
        document_stream_write(Format, ConfigWithCtx, StreamState, Document)
    ;   ground(Ids)
    ->  forall(
            (   member(OneId, Ids),
                api_get_document(Graph_Type, Transaction, OneId, ConfigWithCtx, Document)
            ),
            document_stream_write(Format, ConfigWithCtx, StreamState, Document))
    ;   ground(Type)
    ->  forall(api_get_documents_by_type(Transaction, Graph_Type, Type, ConfigWithCtx, Document),
               document_stream_write(Format, ConfigWithCtx, StreamState, Document))
    ;   forall(api_get_documents(Transaction, Graph_Type, ConfigWithCtx, Document),
               document_stream_write(Format, ConfigWithCtx, StreamState, Document))
    ),

    document_stream_end(Format, ConfigWithCtx).


:- meta_predicate api_read_document_selector(+,+,+,+,+,+,+,+,+,+,+,1).
api_read_document_selector(System_DB, Auth, Path, Graph_Type, Id, Ids, Type, Query, Config, Requested_Data_Version, Actual_Data_Version, _Initial_Goal) :-
    get_dict(format, Config, Format),
    Format \= json,
    !,
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    format_read_documents(Format, Transaction, Graph_Type, Id, Ids, Type, Query, Config, Actual_Data_Version).
api_read_document_selector(System_DB, Auth, Path, Graph_Type, _Id, _Ids, Type, Query, Config, Requested_Data_Version, Actual_Data_Version, Initial_Goal) :-
    nonvar(Query),
    !,
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),

    die_if(Graph_Type \= instance,
           error(query_is_only_supported_for_instance_graphs, _)),

    expand_query_document(Transaction, Type, Query, Query_Ex, Type_Ex),

    % At this point we know we can open the stream. Any exit conditions have triggered by now.
    call(Initial_Goal, Config.as_list),
    json_stream_start(Config, Stream_Started),

    api_print_documents_by_query(Transaction, Type_Ex, Query_Ex, Config, Stream_Started),

    json_stream_end(Config).
api_read_document_selector(System_DB, Auth, Path, Graph_Type, Id, _Ids, _Type, _Query, Config, Requested_Data_Version, Actual_Data_Version, Initial_Goal) :-
    ground(Id),
    !,
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    get_dict(as_list, Config, As_List),
    (   As_List = true
    ->  % as_list mode: return empty array if document doesn't exist
        % (handles TOCTOU race with cascade deletes in pre_commit_hook)
        % Write Link header if schema @context has a string URI
        (   catch(
                (   get_schema_document(Transaction, '@context', ContextDoc),
                    get_dict('@context', ContextDoc, ContextURI),
                    (atom(ContextURI) ; string(ContextURI))
                ),
                _,
                fail
            )
        ->  routes:write_json_ld_context_link_header(some(ContextURI))
        ;   true
        ),
        call(Initial_Goal, true),
        json_stream_start(Config, Stream_Started),
        (   catch(
                api_print_document(Graph_Type, Transaction, Id, Config, Stream_Started),
                error(document_not_found(_), _),
                true  % Silently succeed — document was cascade-deleted
            )
        ->  true
        ;   true
        ),
        json_stream_end(Config)
    ;   % Non-as_list mode: standard 404 on missing document
        do_or_die(api_document_exists(Graph_Type, Transaction, Id),
                  error(document_not_found(Id), _)),
        % Write Link header if schema @context has a string URI
        (   catch(
                (   get_schema_document(Transaction, '@context', ContextDoc),
                    get_dict('@context', ContextDoc, ContextURI),
                    (atom(ContextURI) ; string(ContextURI))
                ),
                _,
                fail
            )
        ->  routes:write_json_ld_context_link_header(some(ContextURI))
        ;   true
        ),
        call(Initial_Goal, false),
        json_stream_start(Config, Stream_Started),
        api_print_document(Graph_Type, Transaction, Id, Config, Stream_Started),
        json_stream_end(Config)
    ).
api_read_document_selector(System_DB, Auth, Path, Graph_Type, _Id, Ids, _Type, _Query, Config, Requested_Data_Version, Actual_Data_Version, Initial_Goal) :-
    ground(Ids),
    !,
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    do_or_die(api_document_exists(Graph_Type, Transaction, Id),
              error(document_not_found(Id), _)),
    get_dict(as_list, Config, As_List),
    call(Initial_Goal, As_List),
    json_stream_start(Config, Stream_Started),

    api_print_documents_by_id(Graph_Type, Transaction, Config, Ids, Stream_Started),

    json_stream_end(Config).
api_read_document_selector(System_DB, Auth, Path, Graph_Type, _Id, _Ids, Type, _Query, Config, Requested_Data_Version, Actual_Data_Version, Initial_Goal) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    % At this point we know we can open the stream. Any exit conditions have triggered by now.
    get_dict(as_list, Config, As_List),
    call(Initial_Goal, As_List),
    json_stream_start(Config, Stream_Started),

    (   ground(Type)
    ->  api_print_documents_by_type(Graph_Type, Transaction, Config, Type, Stream_Started)
    ;   api_print_documents(Graph_Type, Transaction, Config, Stream_Started)
    ),

    json_stream_end(Config).

api_full_replace_schema(Transaction, Schema) :-
    empty_assoc(Captures_In),
    nuke_schema_documents(Transaction),
    Schema = [Context|Classes],
    call_catch_document_mutation(
        Context,
        replace_context_document(Transaction, Context)
    ),
    api_insert_document_from_lazy_list_unsafe(Classes, schema, false, Transaction, Context,
                                             Captures_In, _Captures_Out, _BackLinks-[], _Ids).

default_insert_options(Options) :-
    Options = [graph_type(instance),
               author("author"),
               message("message"),
               full_replace(false),
               raw_json(false),
               merge_repeats(false)
              ].

default_schema_insert_options(Options) :-
    Options = [graph_type(schema),
               author("author"),
               message("message"),
               full_replace(false),
               raw_json(false),
               merge_repeats(false)
              ].

% ---------------------------------------------------------------------------
% Commit-queue worker support
% ---------------------------------------------------------------------------

:- dynamic execute_commit_package/2.
:- dynamic commit_package_test_handler/1.

run_commit_package(Package) :-
    execute_commit_package(Package, Result),
    deliver_commit_result(Package, Result).

execute_commit_package(Package, Result) :-
    commit_package_test_handler(Handler),
    !,
    once(call(Handler, Package, Result)).
execute_commit_package(Package, Result) :-
    Package.all_branches = [_BranchKey],
    !,
    reset_transaction_objects_graph_descriptors(Package.transaction_objects),
    open_commit_windows_for_transactions(Package.transaction_objects, GuardIds),
    (   catch(
            (   apply_package_changes(Package, Ids),
                run_transactions(Package.transaction_objects, _Witnesses, MetaData, Package.options),
                Result = success(MetaData, Ids)
            ),
            Exception,
            (   (   Exception = fail_transaction
                ->  catch(
                        run_synchronous_reelaboration_and_commit(Package, Result),
                        SyncError,
                        Result = error(SyncError)
                    )
                ;   Exception = error(elaboration_stale(_), _)
                ->  Result = reject(schema_changed)
                ;   Result = error(Exception)
                )
            )
        )
    ->  true
    ;   Result = error(unexpected_commit_failure)
    ),
    close_commit_windows(GuardIds),
    (   Result = success(_, _),
        schema_change_in_package(Package)
    ->  commit_queue:invalidate_pending_after_schema_change(Package.branch_key)
    ;   true
    ).
execute_commit_package(_Package, Result) :-
    Result = error(error(multi_branch_commit_not_implemented, _)).

% run_synchronous_reelaboration_and_commit/2 is called from run_commit_package
% when the worker already holds the branch lock, so it must NOT re-acquire it.
run_synchronous_reelaboration_and_commit(Package, Result) :-
    get_dict(branch_operations, Package, BranchOperations),
    BranchOperations = [BranchOp],
    get_dict(raw_docs, BranchOp, RawDocs),
    get_dict(transaction_objects, Package, TransactionObjects),
    [Transaction] = TransactionObjects,
    get_dict(operation, Package, Operation),
    get_dict(options, Package, Options),
    get_dict(graph_type, Package, Graph_Type),
    reset_transaction_objects_graph_descriptors(TransactionObjects),
    parallel_elaboration:elaborate_insert_request_db_with_contracts(Transaction, RawDocs, NewPairs,
                                                                    [workers(4), wrap_api_errors(true)]),
    (   Operation = insert
    ->  option(overwrite(Overwrite), Options, false),
        option(merge_repeats(Doc_Merge), Options, false),
        api_insert_documents_core(Transaction, pairs(NewPairs), Graph_Type, false, false, Doc_Merge, Overwrite, Ids)
    ;   Operation = replace
    ->  option(create(Create), Options, false),
        option(merge_repeats(Doc_Merge), Options, false),
        api_replace_documents_core(Transaction, pairs(NewPairs), Graph_Type, false, Create, Doc_Merge, Ids)
    ),
    run_transactions([Transaction], _Witnesses, MetaData, Options),
    Result = success(MetaData, Ids).

apply_package_changes(Package, Ids) :-
    get_dict(operation, Package, Operation),
    get_dict(transaction_objects, Package, TransactionObjects),
    get_dict(branch_operations, Package, BranchOperations),
    get_dict(options, Package, Options),
    get_dict(graph_type, Package, Graph_Type),
    put_dict(_{graph_type: Graph_Type}, Options, BranchOptions),
    [Transaction] = TransactionObjects,
    BranchOperations = [BranchOperation],
    get_dict(doc_contract_pairs, BranchOperation, Pairs),
    get_dict(delete_ids, BranchOperation, DeleteIds),
    apply_branch_operation(Operation, Transaction, Pairs, DeleteIds, BranchOptions, Ids).

apply_branch_operation(insert, Transaction, Pairs, [], Options, Ids) :- !,
    option(graph_type(Graph_Type), Options),
    option(overwrite(Overwrite), Options, false),
    option(merge_repeats(Doc_Merge), Options, false),
    enrich_pairs_for_operation(Pairs, insert, EnrichedPairs),
    api_insert_documents_core(Transaction, pairs(EnrichedPairs), Graph_Type, false, false, Doc_Merge, Overwrite, Ids).
apply_branch_operation(replace, Transaction, Pairs, [], Options, Ids) :- !,
    option(graph_type(Graph_Type), Options),
    option(create(Create), Options, false),
    option(merge_repeats(Doc_Merge), Options, false),
    enrich_pairs_for_operation(Pairs, replace, EnrichedPairs),
    api_replace_documents_core(Transaction, pairs(EnrichedPairs), Graph_Type, false, Create, Doc_Merge, Ids).
apply_branch_operation(delete, Transaction, [], DeleteIds, _Options, Ids) :- !,
    findall(Id, (member(Id, DeleteIds), delete_document(Transaction, Id)), Ids).

enrich_pairs_for_operation(Pairs, Operation, EnrichedPairs) :-
    maplist({Operation}/[Doc-Contract, Doc-EnrichedContract]>>(
                enrich_pair_for_operation(Operation, Contract, EnrichedContract)
            ),
            Pairs, EnrichedPairs).

enrich_pair_for_operation(insert, Contract, EnrichedContract) :-
    !,
    contract_conflict_sets(insert, Contract, MustNotExist, MustExist),
    EnrichedContract = Contract.put(_{must_exist: MustExist, must_not_exist: MustNotExist}).
enrich_pair_for_operation(replace, Contract, EnrichedContract) :-
    !,
    contract_conflict_sets(replace, Contract, MustNotExist, MustExist),
    EnrichedContract = Contract.put(_{must_exist: MustExist, must_not_exist: MustNotExist}).
enrich_pair_for_operation(delete, Contract, Contract).

contract_conflict_sets(insert, Contract, MustNotExist, []) :-
    get_dict(id_pairs, Contract, IdPairs),
    maplist([Id-_, Id]>>true, IdPairs, MustNotExist).
contract_conflict_sets(replace, Contract, MustNotExist, MustExist) :-
    get_dict(id_pairs, Contract, IdPairs),
    include([_-normal]>>true, IdPairs, NormalPairs),
    maplist([Id-_, Id]>>true, NormalPairs, MustExist),
    created_ids(IdPairs, MustNotExist).
contract_conflict_sets(delete, _Contract, [], []).

created_ids(IdPairs, CreatedIds) :-
    include([_-Variety]>>(member(Variety, [created, value_hash])), IdPairs, CreatedPairs),
    maplist([Id-_, Id]>>true, CreatedPairs, CreatedIds).

branch_transaction_object(TransactionObjects, BranchKey, Transaction) :-
    member(Transaction, TransactionObjects),
    branch_key_from_transaction(Transaction, BranchKey),
    !.

open_commit_windows_for_transactions([], []).
open_commit_windows_for_transactions([Transaction|Rest], [GuardId|GuardIds]) :-
    branch_key_from_transaction(Transaction, BranchKey),
    pre_branch_commit_id(Transaction, CommitId),
    parallel_elaboration:open_commit_window_for_branch_head(Transaction, BranchKey, CommitId, 200, GuardId),
    open_commit_windows_for_transactions(Rest, GuardIds).

close_commit_windows(GuardIds) :-
    forall(member(GuardId, GuardIds),
           catch('$change_window':close_commit_window(GuardId), _, true)).

deliver_commit_result(Package, Result) :-
    get_dict(reply_queue, Package, ReplyQueue),
    get_dict(request_id, Package, RequestId),
    !,
    catch(thread_send_message(ReplyQueue,
                             commit_result(Result, RequestId)),
          error(existence_error(message_queue, _), _),
          true).
deliver_commit_result(_Package, _Result).

schema_change_in_package(Package) :-
    Package.graph_type = schema,
    !.

:- begin_tests(delete_document, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).

insert_some_cities(System, Path) :-
    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Dublin" }
{ "@type": "City",
  "@id" : "City/Pretoria",
  "name" : "Pretoria" }
{ "@type": "City",
  "@id" : "City/Utrecht",
  "name" : "Utrecht" }',
                Stream),
    default_insert_options(Options),
    api_insert_documents(System, 'User/admin', Path, Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(delete_objects_with_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),

    insert_some_cities(System, 'admin/foo'),

    open_string('"City/Dublin" "City/Pretoria"', Stream),
    Options = [
        graph_type(instance),
        author("author"),
        message("message")
    ],

    api_delete_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _, _Ids, Options),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),
    Ids = ['City/Utrecht'].

test(delete_objects_with_string,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('["City/Dublin", "City/Pretoria"]', Stream),
    Options = [
        graph_type(instance),
        author("author"),
        message("message")
    ],
    api_delete_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _, _Ids, Options),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),

    Ids = ['City/Utrecht'].

test(delete_objects_with_mixed_string_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('"City/Dublin"\n["City/Pretoria"]', Stream),
    Options = [
        author("author"),
        message("message")
    ],
    api_delete_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _, _Ids, Options),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),

    Ids = ['City/Utrecht'].

:- end_tests(delete_document).

:- begin_tests(replace_document, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).

insert_some_cities(System, Path) :-
    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Dublin" }
{ "@type": "City",
  "@id" : "City/Pretoria",
  "name" : "Pretoria" }
{ "@type": "City",
  "@id" : "City/Utrecht",
  "name" : "Utrecht" }',
                Stream),
    default_insert_options(Options),
    api_insert_documents(System, 'User/admin', Path, Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(replace_objects_with_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Baile Atha Cliath" }
{ "@type": "City",
  "@id" : "City/Pretoria",
  "name" : "Tshwane" }', Stream),
    Options = [graph_type(instance),
               author("author"),
               message("message"),
               create(false)],
    api_replace_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _, Ids, Options),

    Ids = ['http://example.com/data/world/City/Dublin','http://example.com/data/world/City/Pretoria'].

test(replace_duplicate_documents_with_merge_repeats,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Baile Atha Cliath" }
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Baile Atha Cliath" }', Stream),
    Options = [graph_type(instance),
               author("author"),
               message("message"),
               create(false),
               merge_repeats(true)],
    api_replace_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _, Ids, Options),

    length(Ids, 2).

test(replace_duplicate_documents_without_merge_repeats,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      error(same_ids_in_one_transaction(_))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Baile Atha Cliath" }
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Baile Atha Cliath" }', Stream),
    Options = [graph_type(instance),
               author("author"),
               message("message"),
               create(false)],
    api_replace_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(replace_document_with_explicit_id_for_no_key_class, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Simple",
              name: "xsd:string"})
    ),

    open_string('{"@type":"Simple","@id":"Simple/doc1","name":"original"}', Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version,
                         _New_Data_Version_1, _, Ids, Options),
    Ids = [Id],

    atom_string(Id, Id_String),
    format(string(Replace_JSON), '{"@type":"Simple","@id":"~s","name":"replaced"}', [Id_String]),
    open_string(Replace_JSON, Stream_2),

    Options1 = [graph_type(instance),
                author("author"),
                message("message"),
                create(false)],
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version,
                          _New_Data_Version_2, _, [Id], Options1),

    open_descriptor(Desc, T),
    get_document(T, Id, Doc),
    get_dict(name, Doc, "replaced").

test(explicit_id_document_is_parallel_eligible_for_replace) :-
    Doc = json{'@id': "Simple/doc1", '@type': "Simple", name: "original"},
    docs_parallel_eligible([Doc]).

:- end_tests(replace_document).


:- begin_tests(document_error_reporting).

:- use_module(core(util/test_utils)).
:- use_module(core(document)).
:- use_module(core(api/api_error)).

test(key_missing, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Thing",
              '@key': _{'@type': "Lexical",
                        '@fields': ["field"]},
              field: "xsd:string"})
    ),

    Document = _{'@type': "Thing"},

    % GMG: this is clearly too elaborate to be an effective test...
    catch(
        call_catch_document_mutation(
            Document,
            with_test_transaction(
                Desc,
                C2,
                insert_document(
                    C2,
                    Document,
                    _)
            )
        ),
        Error,
        api_error_jsonld(
            insert_documents,
            Error,
            JSON
        )
    ),

    JSON = _{'@type':'api:InsertDocumentErrorResponse',
             'api:error':_{'@type':'api:SchemaCheckFailure',
                           'api:witnesses':[
                               json{'@type':required_field_does_not_exist_in_document,
                                    document:
                                    json{'@type':'http://somewhere.for.now/schema#Thing'},
                                    field:'http://somewhere.for.now/schema#field'}]},
             'api:message':"Schema check failure",
             'api:status':"api:failure"}.

test(back_links_not_supported_in_replace, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "CycleDoc",
              other: _{'@type': "Set",
                       '@class': "CycleDoc"}})
    ),

    open_string('{"@type": "CycleDoc"}', Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version,
                         _New_Data_Version_1, _, Ids, Options),
    Ids = [Id],

    atom_string(Id, Id_String),
    format(string(Replace_JSON), '[{"@type":"CycleDoc","@id":"~s","@linked-by":{"@id":"~s","@property":"other"}}]', [Id_String, Id_String]),
    open_string(Replace_JSON, Stream_2),

    catch(
        api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version,
                              _New_Data_Version_2, _, _, [graph_type(instance),
                                                           author("author"),
                                                           message("message"),
                                                           create(false)]),
        Error,
        api_error_jsonld(replace_documents, Error, JSON)
    ),

    get_dict('@type', JSON, 'api:ReplaceDocumentErrorResponse'),
    get_dict('api:error', JSON, ErrorDict),
    get_dict('@type', ErrorDict, 'api:LinksInReplaceError'),
    get_dict('api:status', JSON, "api:failure").

:- end_tests(document_error_reporting).
:- begin_tests(document_id_capture, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(document)).

test(basic_capture, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
}
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _, _Ids, Options),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).

test(capture_missing, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(not_all_captures_found(["C_Ernie"]))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
}',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(double_capture, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(capture_already_bound("Capture",_))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "@capture": "Capture",
  "name" : "Bert",
  "friends" : []
}
{ "@type": "Person",
  "@capture": "Capture",
  "name" : "Ernie",
  "friends" : []
}
',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(basic_capture_list, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
[{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
},
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}]',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _, _Ids, Options),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).

test(basic_capture_replace, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),

    open_string('
{ "@type": "Person",
  "name" : "Bert",
  "friends" : []
}
{ "@type": "Person",
  "name" : "Ernie",
  "friends" : []
}',
                Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version, _New_Data_Version_1, _, _Ids_1, Options),

    open_string('
{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
}
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}',
                Stream_2),
    Options1 = [graph_type(instance),
                author("author"),
                message("message"),
                create(false)],
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version, _New_Data_Version_2, _, _Ids_2, Options1),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).

test(basic_capture_list_replace, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "name" : "Bert",
  "friends" : []
}
{ "@type": "Person",
  "name" : "Ernie",
  "friends" : []
}',
                Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version, _New_Data_Version_1, _, _Ids_1, Options),

    open_string('
[{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
},
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}]',
                Stream_2),
    Options1 = [graph_type(instance),
                author("author"),
                message("message"),
                create(false)],
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version, _New_Data_Version_2, _, _Ids_2, Options1),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).


:- end_tests(document_id_capture).


:- begin_tests(subdocument_as_document, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(document)).

test(insert_subdocument_as_document, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(inserted_subdocument_as_document(_{'@type':"Thing"}))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Thing",
              '@key': _{
                          '@type': "Random"
                      },
              '@subdocument': []})
    ),


    open_string('
{"@type": "Thing"}
',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(replace_nonexisting_subdocument_as_document, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(inserted_subdocument_as_document(_{'@type':"Thing"}))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Thing",
              '@key': _{
                          '@type': "Random"
                      },
              '@subdocument': []})
    ),


    open_string('
{"@type": "Thing"}
',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [graph_type(instance),
               author("author"),
               message("message"),
               create(true)],
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _, _Ids, Options).

test(replace_existing_subdocument_as_document, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         % Right now, idgen will generate ids of submitted documents
         % without taking into account that they may be
         % subdocuments. For the replace case, this means we cannot
         % directly replace a subdocument, as the submitted id will
         % never match the generated id. idgen fix is planned but not
         % yet implemented. Unblock when it is here.
         blocked('idgen prevents subdocument replace')
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        (   insert_schema_document(
                C1,
                _{'@type': "Class",
                  '@id': "Thing",
                  '@key': _{
                              '@type': "Random"
                          },
                  '@subdocument': [],
                  'name': "xsd:string"}),
            insert_schema_document(
                C1,
                _{'@type': "Class",
                  '@id': "Outer",
                  '@key': _{
                              '@type': "Random"
                          },
                  'thing': "Thing"}))
    ),


    open_string('
{"@type": "Outer",
 "thing": {"@type": "Thing", "name": "Foo"}
}
',
                Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("author"),
               message("message")],
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version, _New_Data_Version_1, _, [Outer_Id], Options),

    get_document(Desc, Outer_Id, Outer_Document),
    Id = (Outer_Document.thing.'@id'),

    format(string(Replace_String),
           '{"@type": "Thing", "@id": "~q", "name": "Bar"}',
           [Id]),
    open_string(Replace_String, Stream_2),

    Options = [graph_type(instance),
               author("author"),
               message("message"),
               create(false)],
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version, _New_Data_Version_2, _, _Ids_2, Options),

    get_document(Desc, Id, _Inner_Document).


:- end_tests(subdocument_as_document).

:- begin_tests(full_replace).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(document)).

% Regression in 10.0.23 caused schema replace to use the previous
% context for expanding type and property names. This caused the test
% below to fail.
test(full_replace_schema, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
{ "@type": "@context",
  "@schema": "http://some.weird.place#",
  "@base": "http://some.weird.place/"
}

{ "@type": "Class",
  "@id": "Thing"
}', Stream),
    Options = [author("test"),
               full_replace(true),
               graph_type(schema),
               message("test")],
    api_insert_documents(System, Auth, "admin/testdb", Stream, no_data_version, _, _, _, Options),

    resolve_absolute_string_descriptor("admin/testdb", TestDB),
    open_descriptor(TestDB, T),

    get_schema_document(T, "Thing", _Document).

test(full_replace_instance, [
         setup((setup_temp_store(State),
                create_db_with_test_schema("admin", "testdb"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
{"@type": "City", "name": "Utrecht"}
', Stream),

    Options = [author("test"),
               graph_type(instance),
               full_replace(true),
               message("test")],
    api_insert_documents(System, Auth, "admin/testdb", Stream, no_data_version, _, _, [Id], Options),

    resolve_absolute_string_descriptor("admin/testdb", TestDB),
    open_descriptor(TestDB, T),

    get_document(T, Id, Document),
    _{'@type': 'City', 'name': "Utrecht"} :< Document.
test(full_replace_instance_no_author, [
         setup((setup_temp_store(State),
                create_db_with_test_schema("admin", "testdb"))),
         cleanup(teardown_temp_store(State)),
         error(required_option_unspecified(author),_)
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
{"@type": "City", "name": "Utrecht"}
', Stream),
    Options = [],
    api_insert_documents(System, Auth, "admin/testdb", Stream, no_data_version, _, _, [Id], Options),

    resolve_absolute_string_descriptor("admin/testdb", TestDB),
    open_descriptor(TestDB, T),

    get_document(T, Id, Document),
    _{'@type': 'City', 'name': "Utrecht"} :< Document.
:- end_tests(full_replace).

:- begin_tests(document_format_detection).
:- use_module(config(terminus_config)).

test(detect_input_json) :-
    detect_input_format('application/json', json).

test(detect_input_json_with_charset) :-
    detect_input_format('application/json; charset=UTF-8', json).

test(detect_input_jsonld_community_rejects,
     [condition(\+ is_enterprise),
      error(unsupported_document_format(jsonld), _)]) :-
    detect_input_format('application/ld+json', _).

test(detect_input_rdfxml_community_rejects,
     [condition(\+ is_enterprise),
      error(unsupported_document_format(rdfxml), _)]) :-
    detect_input_format('application/rdf+xml', _).

test(detect_input_turtle_community_rejects,
     [condition(\+ is_enterprise),
      error(unsupported_document_format(turtle), _)]) :-
    detect_input_format('text/turtle', _).

test(detect_input_unknown_rejects,
     [error(bad_content_type('text/plain', 'application/json'), _)]) :-
    detect_input_format('text/plain', _).

test(detect_output_default_json) :-
    detect_output_format([], [accept(accept(application/json))], json).

test(detect_output_format_param_json) :-
    detect_output_format([format=json], [], json).

test(detect_output_format_param_jsonld_community_rejects,
     [condition(\+ is_enterprise),
      error(unsupported_document_format(jsonld), _)]) :-
    detect_output_format([format=jsonld], [], _).

test(detect_output_format_param_rdfxml_community_rejects,
     [condition(\+ is_enterprise),
      error(unsupported_document_format(rdfxml), _)]) :-
    detect_output_format([format=rdfxml], [], _).

test(detect_output_format_param_turtle_community_rejects,
     [condition(\+ is_enterprise),
      error(unsupported_document_format(turtle), _)]) :-
    detect_output_format([format=turtle], [], _).

test(detect_output_format_param_unknown_rejects,
     [error(unsupported_document_format(csv), _)]) :-
    detect_output_format([format=csv], [], _).

test(detect_output_no_accept_defaults_json) :-
    detect_output_format([], [], json).

test(document_input_format_json) :-
    document_input_format(json).

test(document_output_format_json) :-
    document_output_format(json).

:- end_tests(document_format_detection).

:- begin_tests(parallel_initial_writes, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(triple), [with_triple_store/2]).

insert_and_report(Store, SystemDB, Auth, Path, Stream, Options, Queue) :-
    catch(
        (   with_triple_store(Store,
                              api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _New_Data_Version, _, Ids, Options))
        ->  thread_send_message(Queue, done(Ids))
        ;   thread_send_message(Queue, error(failure))
        ),
        Error,
        thread_send_message(Queue, error(Error))
    ).

test(two_parallel_initial_writes_are_serialised, [
         setup((setup_temp_store(State),
                create_db_with_test_schema(admin, foo))),
         cleanup(teardown_temp_store(State))
     ]) :-
    State = Store-_,
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    Doc1 = '{"@type":"City","name":"Alice"}',
    Doc2 = '{"@type":"City","name":"Bob"}',
    open_string(Doc1, Stream1),
    open_string(Doc2, Stream2),
    message_queue_create(Queue, []),
    thread_create(insert_and_report(Store, SystemDB, Auth, 'admin/foo', Stream1, Options, Queue), Thread1),
    thread_create(insert_and_report(Store, SystemDB, Auth, 'admin/foo', Stream2, Options, Queue), Thread2),
    thread_get_message(Queue, Result1, [timeout(5)]),
    thread_get_message(Queue, Result2, [timeout(5)]),
    thread_join(Thread1, _),
    thread_join(Thread2, _),
    Result1 = done(Ids1),
    Result2 = done(Ids2),
    append(Ids1, Ids2, Ids),
    length(Ids, 2),
    with_triple_store(Store,
                      (   resolve_absolute_string_descriptor('admin/foo', Desc),
                          open_descriptor(Desc, T),
                          forall(member(Id, Ids), get_document(T, Id, _))
                      )),
    Ids = [Id1, Id2],
    Id1 \= Id2.

:- end_tests(parallel_initial_writes).

:- begin_tests(system_document_inserts, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(triple), [with_triple_store/2]).

system_document_inserts_setup_low_retry_limit :-
    nb_setval(test_old_max_retries, ''),
    (   getenv('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES', OldRetries)
    ->  nb_setval(test_old_max_retries, OldRetries)
    ;   true),
    setenv('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES', '2'),
    abolish_table_subgoals(max_transaction_retries(_)).

system_document_inserts_restore_retry_limit :-
    nb_getval(test_old_max_retries, OldRetries),
    (   OldRetries = ''
    ->  unsetenv('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES')
    ;   setenv('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES', OldRetries)
    ),
    abolish_table_subgoals(max_transaction_retries(_)).

% Regression test for the integration-test hang after the
% "lists organization users" capability test. Sequential system
% document inserts must not hold a commit-window guard across
% transactions and must not exhaust the transaction retry budget.
% We cap the retry budget to 2 so that a guard leak fails fast and
% deterministically, instead of waiting for the default retry ceiling.
test(sequential_system_document_inserts_release_commit_window_guard, [
         setup((setup_temp_store(State),
                system_document_inserts_setup_low_retry_limit)),
         cleanup((system_document_inserts_restore_retry_limit,
                  teardown_temp_store(State)))
     ]) :-
    State = Store-_,
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    default_insert_options(Options),
    User1 = '{"@type":"User","name":"Alice"}',
    User2 = '{"@type":"User","name":"Bob"}',
    open_string(User1, Stream1),
    open_string(User2, Stream2),
    with_triple_store(Store,
                      (   api_insert_documents(SystemDB, Auth, '_system', Stream1, no_data_version, _DV1, _, Ids1, Options),
                          api_insert_documents(SystemDB, Auth, '_system', Stream2, no_data_version, _DV2, _, Ids2, Options)
                      )),
    append(Ids1, Ids2, Ids),
    length(Ids, 2).

:- end_tests(system_document_inserts).


api_document_test_handler(P, success(test_meta, [test_id])) :-
    get_dict(test_marker, P, true).
api_document_test_handler(P, error(test_error)) :-
    get_dict(test_marker_error, P, true).

:- begin_tests(commit_queue_helpers, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).

% Regression test: enrich_pairs_for_operation must preserve the elaborated
% document in the output pair, not replace it with an anonymous variable.
% Otherwise the worker-side insert path loses the document and returns no IDs.
test(enrich_pairs_for_operation_preserves_doc) :-
    Doc = json{'@id': "http://example.com/Person/1",
               '@type': "Person",
               'http://example.com/schema#name': "Alice"},
    Contract = verification_contract{
        submitted_id: "http://example.com/Person/1",
        generated_id: "http://example.com/Person/1",
        id_pairs: ["http://example.com/Person/1"-normal],
        dependencies: [],
        backlinks: [],
        captures_out: t
    },
    Pairs = [Doc-Contract],
    enrich_pairs_for_operation(Pairs, insert, EnrichedPairs),
    EnrichedPairs = [OutDoc-OutContract],
    OutDoc == Doc,
    is_dict(OutContract, verification_contract),
    get_dict(must_exist, OutContract, []),
    get_dict(must_not_exist, OutContract, ["http://example.com/Person/1"]).

test(enrich_pairs_for_operation_preserves_doc_replace) :-
    Doc = json{'@id': "http://example.com/Person/1",
               '@type': "Person",
               'http://example.com/schema#name': "Alice"},
    Contract = verification_contract{
        submitted_id: "http://example.com/Person/1",
        generated_id: "http://example.com/Person/1",
        id_pairs: ["http://example.com/Person/1"-normal],
        dependencies: [],
        backlinks: [],
        captures_out: t
    },
    Pairs = [Doc-Contract],
    enrich_pairs_for_operation(Pairs, replace, EnrichedPairs),
    EnrichedPairs = [OutDoc-OutContract],
    OutDoc == Doc,
    is_dict(OutContract, verification_contract),
    get_dict(must_exist, OutContract, ["http://example.com/Person/1"]),
    get_dict(must_not_exist, OutContract, []).

test(contract_conflict_sets_insert) :-
    Contract = verification_contract{
        id_pairs: ["http://example.com/Person/1"-created,
                   "http://example.com/Person/2"-normal,
                   "http://example.com/Person/3"-value_hash]
    },
    contract_conflict_sets(insert, Contract, MustNotExist, MustExist),
    MustNotExist = ["http://example.com/Person/1", "http://example.com/Person/2", "http://example.com/Person/3"],
    MustExist = [].

test(contract_conflict_sets_replace) :-
    Contract = verification_contract{
        id_pairs: ["http://example.com/Person/1"-normal,
                   "http://example.com/Person/2"-created]
    },
    contract_conflict_sets(replace, Contract, MustNotExist, MustExist),
    MustExist = ["http://example.com/Person/1"],
    MustNotExist = ["http://example.com/Person/2"].

test(collect_stream_docs_preserves_nested_list) :-
    JSON = '{"@type":"Group","@id":"Group/0","people":[{"@type":"Person","name":"Immanuel Kant","age":"79","order":"3"},{"@type":"Person","name":"Karl Popper","age":"92","order":"5"}]}',
    open_string(JSON, Stream),
    stream_to_lazy_docs(Stream, LazyDocs),
    collect_stream_docs(LazyDocs, Docs),
    Docs = [Doc],
    get_dict(people, Doc, People),
    People = [Person1, Person2],
    get_dict(name, Person1, "Immanuel Kant"),
    get_dict(name, Person2, "Karl Popper"),
    \+ contains_ineligible_marker(People).

test(collect_stream_docs_handles_empty_object) :-
    JSON = '{}',
    open_string(JSON, Stream),
    stream_to_lazy_docs(Stream, LazyDocs),
    collect_stream_docs(LazyDocs, Docs),
    Docs = [Doc],
    is_dict(Doc, json).

test(collect_stream_docs_handles_empty_stream) :-
    open_string('', Stream),
    stream_to_lazy_docs(Stream, LazyDocs),
    collect_stream_docs(LazyDocs, Docs),
    Docs = [].

test(apply_package_changes_empty_pairs, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                write_schema_string('{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n{"@id":"Thing","@type":"Class"}\n', Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, Transaction),
    create_context(Transaction, commit_info{author: "test", message: "test"}, Context),
    query_context_transaction_objects(Context, TransactionObjects),
    Package = commit_package{
        branch_key: "test_branch",
        all_branches: ["test_branch"],
        transaction_objects: TransactionObjects,
        operation: insert,
        branch_operations: [branch_operation{
            branch_key: "test_branch",
            doc_contract_pairs: [],
            delete_ids: [],
            raw_docs: []
        }],
        commit_info: Context.commit_info,
        options: options{overwrite: false, merge_repeats: false},
        graph_type: instance,
        reply_queue: _,
        request_id: empty_test
    },
    apply_package_changes(Package, Ids),
    Ids = [].

test(apply_package_changes_duplicate_ids, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                write_schema_string('{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n{"@id":"Thing","@type":"Class","@key":{"@type":"Lexical","@fields":["name"]},"name":"xsd:string"}\n', Desc),
                open_descriptor(Desc, Transaction),
                create_context(Transaction, commit_info{author: "test", message: "test"}, Context),
                query_context_transaction_objects(Context, TransactionObjects),
                Doc = _{'@type':'Thing', name:"A"},
                parallel_elaboration:json_elaborate_with_contract(Transaction, Doc, Elaborated, Contract),
                Pairs = [Elaborated-Contract, Elaborated-Contract])),
         cleanup(teardown_temp_store(State))
     ]) :-
    Package = commit_package{
        branch_key: "test_branch",
        all_branches: ["test_branch"],
        transaction_objects: TransactionObjects,
        operation: insert,
        branch_operations: [branch_operation{
            branch_key: "test_branch",
            doc_contract_pairs: Pairs,
            delete_ids: [],
            raw_docs: [Doc, Doc]
        }],
        commit_info: Context.commit_info,
        options: options{overwrite: false, merge_repeats: false},
        graph_type: instance,
        reply_queue: _,
        request_id: duplicate_test
    },
    catch(
        apply_package_changes(Package, _),
        error(same_ids_in_one_transaction([DuplicateId]), _),
        true
    ),
    DuplicateId = 'terminusdb:///data/Thing/A'.

test(collect_stream_docs_preserves_simple_string_names, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                write_schema_string('{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n{"@id":"Simple","@type":"Class","name":"xsd:string"}\n', Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    JSON = '[{"@type":"Simple","@id":"Simple/w0-d0-abc123","name":"Simple/w0-d0-abc123-xyz456"}]',
    open_string(JSON, Stream),
    stream_to_lazy_docs(Stream, LazyDocs),
    collect_stream_docs(LazyDocs, Docs),
    Docs = [Doc],
    get_dict(name, Doc, Name),
    string(Name),
    Name = "Simple/w0-d0-abc123-xyz456".

% run_commit_package/1 tests. These verify that the package is routed to a
% handler and that the result is delivered to the reply queue, without
% requiring a real transaction commit.

test(run_commit_package_delivers_success, [
         setup(asserta(api_document:commit_package_test_handler(api_document:api_document_test_handler))),
         cleanup(retractall(api_document:commit_package_test_handler(_)))
     ]) :-
    message_queue_create(ReplyQueue, []),
    Package = commit_package{
        branch_key: "test_branch",
        all_branches: ["test_branch"],
        reply_queue: ReplyQueue,
        request_id: req_success,
        test_marker: true
    },
    run_commit_package(Package),
    thread_get_message(ReplyQueue, commit_result(success(test_meta, [test_id]), req_success), [timeout(1)]),
    message_queue_destroy(ReplyQueue).

test(run_commit_package_delivers_error, [
         setup(asserta(api_document:commit_package_test_handler(api_document:api_document_test_handler))),
         cleanup(retractall(api_document:commit_package_test_handler(_)))
     ]) :-
    message_queue_create(ReplyQueue, []),
    Package = commit_package{
        branch_key: "test_branch",
        all_branches: ["test_branch"],
        reply_queue: ReplyQueue,
        request_id: req_error,
        test_marker_error: true
    },
    run_commit_package(Package),
    thread_get_message(ReplyQueue, commit_result(error(test_error), req_error), [timeout(1)]),
    message_queue_destroy(ReplyQueue).

test(execute_commit_package_default_rejects_multi_branch) :-
    Package = commit_package{
        branch_key: "test_branch",
        all_branches: ["branch1", "branch2"],
        reply_queue: _,
        request_id: req_multi
    },
    execute_commit_package(Package, Result),
    Result = error(error(multi_branch_commit_not_implemented, _)).

test(deliver_commit_result_ignores_missing_reply_queue) :-
    Package = commit_package{
        reply_queue: missing_reply_queue,
        request_id: req_missing
    },
    deliver_commit_result(Package, success(test_meta, [test_id])).

:- end_tests(commit_queue_helpers).


