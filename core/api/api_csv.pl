:- module(api_csv, [csv_load/6,csv_update/6,csv_dump/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

csv_load(System_DB, Auth, Path, Commit_Info, Files, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

    do_or_die(
        Files = [Name=CSV_Path],
        error(single_files_only(Files))),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           commit_info: Commit_Info,
           files : Files,
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Context),

    assert_write_access(Context),

    csv_load_into_context(Name,CSV_Path,Context,Options).

csv_load_into_context(_Name, Path, Context, Options) :-
    get_dict(prefixes,Context, Prefixes),

    (   option(data_prefix(_), Options)
    ->  Options_1 = Options
    ;   get_dict(doc,Prefixes, Data_Prefix),
        merge_options(Options, [data_prefix(Data_Prefix)], Options_1)),

    (   option(schema_prefix(_), Options_1)
    ->  Final_Options = Options_1
    ;   get_dict(scm,Prefixes, Schema_Prefix),
        merge_options(Options_1, [schema_prefix(Schema_Prefix)], Final_Options)),

    with_transaction(
        Context,
        (   query_default_write_graph(Context, Read_Write_Obj),
            read_write_obj_builder(Read_Write_Obj, Builder),
            csv_builder(Path, Builder, Final_Options)
        ),
        _).

csv_update(System_DB, Auth, Path, Commit_Info, Files, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

    do_or_die(
        Files = [Name=CSV_Path],
        error(single_files_only(Files))),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           files : Files,
           commit_info: Commit_Info,
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Context),

    assert_write_access(Context),

    csv_update_into_context(Name,CSV_Path,Context,Options).

csv_update_into_context(_Name, Path, Context, Options) :-
    get_dict(prefixes,Context, Prefixes),

    (   option(data_prefix(_), Options)
    ->  Options_1 = Options
    ;   get_dict(doc, Prefixes, Data_Prefix),
        merge_options(Options, [data_prefix(Data_Prefix)], Options_1)),

    (   option(schema_prefix(_), Options_1)
    ->  Final_Options = Options_1
    ;   get_dict(scm, Prefixes, Schema_Prefix),
        merge_options(Options_1, [schema_prefix(Schema_Prefix)], Final_Options)),

    open_memory_store(Store),
    open_write(Store, Builder),
    csv_builder(Path, Builder, Final_Options),
    nb_commit(Builder, Layer),

    with_transaction(
        Context,
        (   query_default_write_graph(Context, Read_Write_Obj),
            read_write_obj_builder(Read_Write_Obj, CSV_Builder),
            nb_apply_diff(CSV_Builder,Layer)
        ),
        _).

csv_dump(System_DB, Auth, Path, [csv=Filename], _Options) :-
    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Context),

    tmp_file_stream(Filename, Stream, [encoding(utf8)]),

    query_default_write_graph(Context, Read_Write_Obj),
    read_write_obj_reader(Read_Write_Obj, Layer),

    get_dict(prefixes, Context, Prefixes),

    % collect header predicates
    findall(Predicate-P_Name,
            (   predicate_id(Layer, Predicate, _P_Id),
                uri_to_prefixed(Predicate, Prefixes, Prefix:P_Name_Enc),
                Prefix \= rdf,
                uri_encoded(query_value, P_Name, P_Name_Enc)
            ),
            Columns),

    % write headers
    zip(_, Column_Names, Columns),
    Header =.. [row|Column_Names],
    csv_write_stream(Stream, [Header], []),

    % write rows
    forall(
        (   ask(Context,
                t(Row, rdf:type, scm:'Row')),
            findall(RowDatum,
                    (   member(P-P_Name, Columns),
                        ask(Context,
                            t(Row, P, RowDatumTyped)),
                        RowDatum^^_ = RowDatumTyped
                    ),
                    RowData),
            RowValue =.. [row|RowData]),
        csv_write_stream(Stream, [RowValue], [])
    ),
    close(Stream).

:- begin_tests(csv_api).

:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).


test(csv_load,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "some,header~n", []),
    format(Stream, "1,2~n", []),
    format(Stream, "3,4~n", []),
    close(Stream),

    Path = 'admin/testdb',
    Files = ['csv'=Filename],
    Commit_Info = _{ author : "me", message : "a message"},
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    csv_load(System_DB, Auth, Path, Commit_Info, Files, []),
    resolve_absolute_string_descriptor(Path, Desc),
    findall(X-Y-Z, ask(Desc,t(X, Y, Z)), Triples),

    Triples = [
        (doc:Row1)-(scm:header)-("2"^^xsd:string),
        (doc:Row1)-(scm:some)-("1"^^xsd:string),
        (doc:Row1)-(rdf:type)-(scm:'Row'),
        (doc:Row2)-(scm:header)-("4"^^xsd:string),
        (doc:Row2)-(scm:some)-("3"^^xsd:string),
        (doc:Row2)-(rdf:type)-(scm:'Row')
    ].

test(csv_update,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "some,header~n", []),
    format(Stream, "1,2~n", []),
    format(Stream, "3,4~n", []),
    close(Stream),

    Path = 'admin/testdb',
    Files = ['csv'=Filename],
    Commit_Info = _{ author : "me", message : "a message"},
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    csv_load(System_DB, Auth, Path, Commit_Info, Files, []),

    tmp_file_stream(Filename2, Stream2, [encoding(utf8)]),
    format(Stream2, "some,header~n", []),
    format(Stream2, "1,9~n", []),
    format(Stream2, "3,4~n", []),
    close(Stream2),

    Files2 = ['csv'=Filename2],
    open_descriptor(system_descriptor{}, System_DB2),
    csv_update(System_DB2, Auth, Path, Commit_Info, Files2, []),

    resolve_absolute_string_descriptor(Path, Desc),
    findall(X-Y-Z, ask(Desc,t(X, Y, Z)), Triples),
    Triples = [
        (doc:Row1)-(scm:header)-("4"^^xsd:string),
        (doc:Row1)-(scm:some)-("3"^^xsd:string),
        (doc:Row1)-(rdf:type)-(scm:'Row'),
        (doc:Row2)-(scm:header)-("9"^^xsd:string),
        (doc:Row2)-(scm:some)-("1"^^xsd:string),
        (doc:Row2)-(rdf:type)-(scm:'Row')
    ].

test(csv_dump,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "some,header~n", []),
    format(Stream, "1,2~n", []),
    format(Stream, "3,4~n", []),
    close(Stream),

    Path = 'admin/testdb',
    Files = ['csv'=Filename],
    Commit_Info = _{ author : "me", message : "a message"},
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    csv_load(System_DB, Auth, Path, Commit_Info, Files, []),

    csv_dump(System_DB, Auth, Path, CSV_Files, []),
    member(csv=CSV_Filename, CSV_Files),
    open(CSV_Filename, read, Read_Stream),
    read_string(Read_Stream, _, String),
    nl,
    writeq(String),
    nl,
    String = "header,some\r\n2,1\r\n4,3\r\n".

:- end_tests(csv_api).
