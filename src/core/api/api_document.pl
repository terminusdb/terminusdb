:- module(api_document, [
              api_can_read_document/6,
              api_insert_documents/8,
              api_delete_documents/8,
              api_delete_document/7,
              api_replace_documents/8,
              api_nuke_documents/6,
              api_generate_document_ids/4,
              call_catch_document_mutation/2,
              api_read_document_selector/12,
              api_generate_document_ids/4,
              api_get_documents/4,
              api_get_document/5,
              api_full_replace_schema/2,
              idlists_duplicates_toplevel/3,
              nonground_captures/2,

              api_insert_documents_core_string/7,
              api_replace_documents_core_string/6,
              api_delete_documents_by_ids/3
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(config(terminus_config)).

:- use_module(library(option)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(pprint), [print_term/2]).

before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction) :-
    do_or_die(
        open_descriptor(Descriptor, Transaction),
        error(unresolvable_collection(Descriptor), _)),

    transaction_data_version(Transaction, Actual_Data_Version),
    compare_data_versions(Requested_Data_Version, Actual_Data_Version).

before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction) :-
    do_or_die(
        create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
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
    do_or_die(get_document(Transaction, Config.compress, Config.unfold, Id, Document),
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

:- meta_predicate call_catch_document_mutation(+, :).
call_catch_document_mutation(Document, Goal) :-
    catch_with_backtrace(Goal,
          error(E, Context),
          (   known_document_error(E)
          ->  embed_document_in_error(E, Document, New_E),
              throw(error(New_E, _))
          ;   throw(error(E, Context)))).

api_insert_document_(schema, _Raw_JSON, Transaction, Document, Captures, [Id], Captures, T-T) :-
    call_catch_document_mutation(
        Document,
        do_or_die(insert_schema_document(Transaction, Document),
                  error(document_insertion_failed_unexpectedly(Document), _))),
    do_or_die(Id = (Document.get('@id')),
              error(document_has_no_id_somehow, _)).
api_insert_document_(instance, Raw_JSON, Transaction, Document, Captures_In, Id, Captures_Out, BLH-BLT) :-
    call_catch_document_mutation(
        Document,
        do_or_die(insert_document(Transaction, Document, Raw_JSON, Captures_In, Id, BLH-BLT, Captures_Out),
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

insert_documents_(true, Graph_Type, Raw_JSON, Stream, Transaction, Captures_In, Captures_Out, BackLinks, Ids) :-
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
insert_documents_(false, Graph_Type, Raw_JSON, Stream, Transaction, Captures_In, Captures_Out, BackLinks, Ids) :-
    stream_to_lazy_docs(Stream, Lazy_List),
    api_insert_document_from_lazy_list(Lazy_List, Graph_Type, Raw_JSON, Transaction, Captures_In, Captures_Out, BackLinks-[], Ids).

api_insert_document_from_lazy_list_unsafe([Document|Rest], Graph_Type, Raw_JSON, Transaction, Prefixes, Captures_In, Captures_Out, BLH-BLT, [Ids|New_Ids]) :-
    !,
    api_insert_document_unsafe_(Graph_Type, Raw_JSON, Transaction, Prefixes, Document, Captures_In, Ids, Captures_Mid, BLH-BLM),
    api_insert_document_from_lazy_list_unsafe(Rest, Graph_Type, Raw_JSON, Transaction, Prefixes, Captures_Mid, Captures_Out, BLM-BLT, New_Ids).
api_insert_document_from_lazy_list_unsafe([], _, _, _, _, Captures, Captures, T-T, []).

api_insert_document_from_lazy_list([Document|Rest], Graph_Type, Raw_JSON, Transaction, Captures_In, Captures_Out, BLH-BLT, [Ids|New_Ids]) :-
    !,
    api_insert_document_(Graph_Type, Raw_JSON, Transaction, Document, Captures_In, Ids, Captures_Mid, BLH-BLM),
    api_insert_document_from_lazy_list(Rest, Graph_Type, Raw_JSON, Transaction, Captures_Mid, Captures_Out, BLM-BLT, New_Ids).
api_insert_document_from_lazy_list([], _, _, _, Captures, Captures, T-T, []).

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
        merge_repeats: false
    }).

api_insert_documents(SystemDB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Ids, Options_New) :-
    insert_documents_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type), Options),
    option_or_die(author(Author), Options),
    option_or_die(message(Message), Options),
    option(full_replace(Full_Replace), Options),
    option(raw_json(Raw_JSON), Options),
    option(merge_repeats(Doc_Merge), Options),
    die_if(
        (   Graph_Type = schema,
            Raw_JSON = true
        ),
        error(raw_json_and_schema_disallowed,_)
    ),
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         api_insert_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Ids)
                     ),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_insert_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Ids) :-
    empty_assoc(Captures_In),
    ensure_transaction_has_builder(Graph_Type, Transaction),
    insert_documents_(Full_Replace, Graph_Type, Raw_JSON, Stream, Transaction, Captures_In, Captures_Out, BackLinks, Ids_List),
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

api_insert_documents_core_string(Transaction, String, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Ids) :-
    open_string(String, Stream),
    api_insert_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Full_Replace, Doc_Merge, Ids).


idlists_duplicates_toplevel(Ids, Duplicates, Toplevel) :-
    append(Ids,All_Ids),
    length(All_Ids, N),
    sort(All_Ids, Sorted),
    (   length(Sorted, N)
    ->  Duplicates = []
    ;   has_duplicates(All_Ids, Duplicates)
    ),
    maplist([[Id|_],Id]>>true, Ids, Toplevel).

insert_backlinks(Links, Graph) :-
    nb_link_dict(backlinks,Graph,Links),
    insert_backlinks_(Links, Graph).

insert_backlinks_([], _).
insert_backlinks_([link(S,P,O)|T], Instance) :-
    insert(Instance, S, P, O, _),
    insert_backlinks_(T, Instance).

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

delete_documents_default_options(
    options{
        graph_type: instance
    }).

api_delete_documents(SystemDB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Ids, Options_New) :-
    die_if(
        nonvar(Ids),
        error(unexpected_argument_instantiation(api_delete_documents, Ids), _)),
    delete_documents_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type), Options),
    option(author(Author), Options),
    option(message(Message), Options),

    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         forall(
                             json_read_list_stream(Stream, ID_Unchecked),
                             (   param_check_json(non_empty_string, id, ID_Unchecked, Id),
                                 api_delete_document_(Graph_Type, Transaction, Id)
                             )
                         )
                     ),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_delete_documents_by_ids(Transaction, Graph_Type, Ids) :-
    forall(
        member(Id, Ids),
        (   atom_string(Id_Atom, Id),
            api_delete_document_(Graph_Type, Transaction, Id_Atom)
        )
    ).

api_delete_document(SystemDB, Auth, Path, ID, Requested_Data_Version, New_Data_Version, Options) :-
    option(graph_type(Graph_Type), Options),
    option(author(Author), Options),
    option(message(Message), Options),

    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_delete_document_(Graph_Type, Transaction, ID),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_nuke_documents_(schema, Transaction) :-
    nuke_schema_documents(Transaction).
api_nuke_documents_(instance, Transaction) :-
    nuke_documents(Transaction).

nuke_documents_default_options(
    options{
        graph_type: instance
    }).

api_nuke_documents(SystemDB, Auth, Path, Requested_Data_Version, New_Data_Version, Options_New) :-
    nuke_documents_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type),Options),
    option_or_die(author(Author),Options),
    option_or_die(message(Message),Options),
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_nuke_documents_(Graph_Type, Transaction),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_replace_document_(instance, Raw_JSON, Transaction, Document, Create, Captures_In, Ids, Captures_Out):-
    replace_document(Transaction, Document, Create, Raw_JSON, Captures_In, Ids, _Dependencies, Captures_Out).
api_replace_document_(schema, _Raw_JSON, Transaction, Document, Create, Captures_In, [Id], Captures_In):-
    replace_schema_document(Transaction, Document, Create, Id).


replace_document_default_options(
    options{
        graph_type: instance,
        create: false,
        raw_json: false
    }).

api_replace_documents(SystemDB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Ids, Options_New) :-
    replace_document_default_options(Default),
    merge_options(Options_New,Default,Options),
    option(graph_type(Graph_Type),Options),
    option_or_die(create(Create),Options),
    option_or_die(author(Author),Options),
    option(message(Message),Options),
    option(raw_json(Raw_JSON),Options,false),
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         api_replace_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Create, Ids)
                     ),
                     Meta_Data,
                     Options),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_replace_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Create, Ids) :-
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
    die_if(Duplicates \= [], error(same_ids_in_one_transaction(Duplicates), _)).

api_replace_documents_core_string(Transaction, String, Graph_Type, Raw_JSON, Create, Ids) :-
    open_string(String, Stream),
    api_replace_documents_core(Transaction, Stream, Graph_Type, Raw_JSON, Create, Ids).

api_can_read_document(System_DB, Auth, Path, Graph_Type, Requested_Data_Version, Actual_Data_Version) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, _).


:- meta_predicate api_read_document_selector(+,+,+,+,+,+,+,+,+,+,+,1).
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
    do_or_die(api_document_exists(Graph_Type, Transaction, Id),
              error(document_not_found(Id), _)),
    get_dict(as_list, Config, As_List),
    call(Initial_Goal, As_List),
    json_stream_start(Config, Stream_Started),
    api_print_document(Graph_Type, Transaction, Id, Config, Stream_Started),
    json_stream_end(Config).
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
    api_insert_documents(System, 'User/admin', Path, Stream, no_data_version, _New_Data_Version, _Ids, Options).

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

    api_delete_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _Ids, Options),

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
    api_delete_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _Ids, Options),

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
    api_delete_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, _Ids, Options),

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
    api_insert_documents(System, 'User/admin', Path, Stream, no_data_version, _New_Data_Version, _Ids, Options).

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
    api_replace_documents(System, 'User/admin', 'admin/foo', Stream, no_data_version, _New_Data_Version, Ids, Options),

    Ids = ['http://example.com/data/world/City/Dublin','http://example.com/data/world/City/Pretoria'].

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _Ids, Options),

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _Ids, Options).

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _Ids, Options).

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _Ids, Options),

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version, _New_Data_Version_1, _Ids_1, Options),

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
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version, _New_Data_Version_2, _Ids_2, Options1),

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version, _New_Data_Version_1, _Ids_1, Options),

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
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version, _New_Data_Version_2, _Ids_2, Options1),

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _Ids, Options).

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
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream, no_data_version, _New_Data_Version, _Ids, Options).

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
    api_insert_documents(SystemDB, Auth, "admin/testdb", Stream_1, no_data_version, _New_Data_Version_1, [Outer_Id], Options),

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
    api_replace_documents(SystemDB, Auth, "admin/testdb", Stream_2, no_data_version, _New_Data_Version_2, _Ids_2, Options),

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
    api_insert_documents(System, Auth, "admin/testdb", Stream, no_data_version, _, _, Options),

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
    api_insert_documents(System, Auth, "admin/testdb", Stream, no_data_version, _, [Id], Options),

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
    api_insert_documents(System, Auth, "admin/testdb", Stream, no_data_version, _, [Id], Options),

    resolve_absolute_string_descriptor("admin/testdb", TestDB),
    open_descriptor(TestDB, T),

    get_document(T, Id, Document),
    _{'@type': 'City', 'name': "Utrecht"} :< Document.
:- end_tests(full_replace).

