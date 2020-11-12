:- module(api_csv, [csv_load/6,csv_update/6,csv_dump/6,csv_delete/6]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

csv_delete(System_DB, Auth, Path, Commit_Info, Name, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           commit_info: Commit_Info,
           files : [],
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Context),

    assert_write_access(Context),

    csv_delete_into_context(Name,Context,Options).

csv_delete_into_context(File, Context, _Options) :-
    get_dict(prefixes, Context, Prefixes),

    do_or_die(
        query_default_schema_write_graph(Context, _),
        error(no_schema(Context.default_descriptor))),

    with_transaction(
        Context,
        delete_csvs(Context,[File],Prefixes),
        _).

csv_load(System_DB, Auth, Path, Commit_Info, Files, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

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

    csv_load_into_context(Files,Context,Options).

csv_load_into_context(Files, Context, Options) :-

    get_dict(prefixes, Context, Prefixes),
    default_options(Options, Prefixes, Default_Options),

    with_transaction(
        Context,
        (   query_default_write_graph(Context, Read_Write_Obj),
            read_write_obj_builder(Read_Write_Obj, Builder),
            (   query_default_schema_write_graph(Context, Schema_Read_Write_Obj)
            ->  read_write_obj_builder(Schema_Read_Write_Obj, Schema_Builder)
            ;   Schema_Builder = false
            ),
            forall(
                member(Name=Path, Files),
                (   Schema_Builder = false
                ->  csv_builder(Name, Path, Builder, Default_Options)
                ;   csv_builder(Name, Path, Builder, Schema_Builder, Default_Options)
                )
            )
        ),
        _).

csv_update(System_DB, Auth, Path, Commit_Info, Files, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

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

    csv_update_into_context(Files,Context,Options).

csv_update_into_context(Files, Context, Options) :-
    get_dict(prefixes, Context, Prefixes),

    default_options(Options, Prefixes, Default_Options),

    open_memory_store(Store),
    open_write(Store, Builder),

    do_or_die(
        query_default_schema_write_graph(Context, _),
        error(no_schema(Context.default_collection))),

    read_write_obj_builder(Schema_Read_Write_Obj, Schema_Builder),

    forall(
        member(Name=Path, Files),
        csv_builder(Name, Path, Builder, Schema_Builder, Default_Options)
    ),
    nb_commit(Builder, Layer),
    nb_commit(Schema_Builder, Schema_Layer),

    with_transaction(
        Context,
        (   query_default_write_graph(Context, Read_Write_Obj),
            query_default_schema_write_graph(Context, Schema_Read_Write_Obj),
            read_write_obj_builder(Read_Write_Obj, CSV_Builder),
            read_write_obj_builder(Schema_Read_Write_Obj, CSV_Schema_Builder),
            % delete every csv object
            maplist([File=_,File]>>true,Files,File_Names),
            delete_csvs(Context,File_Names,Prefixes),
            nb_apply_delta(CSV_Builder,Layer),
            nb_apply_delta(CSV_Schema_Builder,Schema_Layer)
        ),
        _).

delete_csvs(Context, File_Names, Prefixes) :-
    forall(
        member(Name, File_Names),
        (   get_dict(doc,Prefixes,Prefix),
            csv_iri(Name,Prefix,Iri),
            atom_string(Node,Iri),
            % Delete instance data
            delete_object(Node,Context),
            % Delete schema data ...
            true % * delete_schema_rows(Iri,Context)
        )
    ).


csv_dump(System_DB, Auth, Path, Name, Filename, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Pre_Context),

    get_dict(prefixes, Pre_Context, Prefixes),
    default_options(Options, Prefixes, Default_Options),

    % Set up csv prefix
    option(schema_prefix(Schema_Prefix), Default_Options),

    New_Prefixes = (Prefixes.put(_{ csv: Schema_Prefix })),
    Context = (Pre_Context.put(_{ prefixes : New_Prefixes})),

    tmp_file_stream(Filename, Stream, [encoding(utf8)]),

    csv_columns(Name, Context, Columns),
    % collect header predicates

    % write headers
    maplist([_-Name-_,Name]>>true,Columns,Column_Names),
    Header =.. [row|Column_Names],
    csv_write_stream(Stream, [Header], []),

    % write rows
    forall(
        (   ask(Context,
                (   t(CSV, rdfs:label, Name@en),
                    t(CSV, csv:csv_row, Row)
                )),

            findall(RowDatum,
                    (   member(Predicate-_-_, Columns),
                        ask(Context,
                            t(Row, Predicate, RowDatumTyped)),
                        RowDatum^^_ = RowDatumTyped
                    ),
                    RowData),

            RowValue =.. [row|RowData]),
        csv_write_stream(Stream, [RowValue], [])
    ),

    close(Stream).

csv_columns(Name, Context, Sorted_Columns) :-
    get_dict(prefixes, Context, Prefixes),

    findall(Predicate-Column_Name-Column_Index,
            (   ask(Context,
                    (   t(CSV, rdfs:label, Name@en),
                        t(CSV, csv:csv_column, ColumnObject),
                        t(ColumnObject, csv:csv_column_name, Column_Name^^_),
                        t(ColumnObject, csv:csv_column_index, Column_Index^^_)
                    )),
                uri_encoded(query_value, Column_Name, Column_Encoded),
                atomic_list_concat([column_, Column_Encoded], Col),
                prefixed_to_uri(csv:Col, Prefixes, Predicate)
            ),
            Columns),

    predsort([Cmp,_-_-O,_-_-I]>>compare(Cmp,O,I), Columns, Sorted_Columns).


default_options(Options, Prefixes, Default_Options) :-
    (   option(data_prefix(_), Options)
    ->  Options_1 = Options
    ;   Data_Prefix = (Prefixes.doc),
        merge_options(Options, [data_prefix(Data_Prefix)], Options_1)),

    (   option(schema_prefix(_), Options_1)
    ->  Default_Options = Options_1
    ;   Schema_Prefix = (Prefixes.scm),
        merge_options(Options_1, [schema_prefix(Schema_Prefix)], Default_Options)).


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
        (Row1)-(scm:column_header)-("2"^^xsd:string),
        (Row1)-(scm:column_some)-("1"^^xsd:string),
        (Row1)-(rdf:type)-(Row_Type2),
        (Row2)-(scm:column_header)-("4"^^xsd:string),
        (Row2)-(scm:column_some)-("3"^^xsd:string),
        (Row2)-(rdf:type)-(Row_Type2),
        (doc:'CSV_csv')-(scm:csv_column)-(doc:'ColumnObject_csv_header'),
        (doc:'CSV_csv')-(scm:csv_column)-(doc:'ColumnObject_csv_some'),
        (doc:'CSV_csv')-(scm:csv_row)-(Row1),
        (doc:'CSV_csv')-(scm:csv_row)-(Row2),
        (doc:'CSV_csv')-(rdf:type)-(scm:'CSV'),
        (doc:'CSV_csv')-(rdfs:label)-("csv"@en),
        (doc:'ColumnObject_csv_header')-(scm:csv_column_index)-(1^^xsd:integer),
        (doc:'ColumnObject_csv_header')-(scm:csv_column_name)-("header"^^xsd:string),
        (doc:'ColumnObject_csv_header')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv_some')-(scm:csv_column_index)-(0^^xsd:integer),
        (doc:'ColumnObject_csv_some')-(scm:csv_column_name)-("some"^^xsd:string),
        (doc:'ColumnObject_csv_some')-(rdf:type)-(scm:'Column')
    ].

test(csv_update,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
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
    writeq(Triples),
    Triples = [
        (Row2)-(scm:column_header)-("4"^^xsd:string),
        (Row2)-(scm:column_some)-("3"^^xsd:string),
        (Row2)-(rdf:type)-(Row_Type1),
        (doc:'CSV_csv')-(scm:csv_column)-(doc:'ColumnObject_csv_header'),
        (doc:'CSV_csv')-(scm:csv_column)-(doc:'ColumnObject_csv_some'),
        (doc:'CSV_csv')-(scm:csv_row)-(Row2),
        (doc:'CSV_csv')-(scm:csv_row)-(Row3),
        (doc:'CSV_csv')-(rdf:type)-(scm:'CSV'),
        (doc:'CSV_csv')-(rdfs:label)-("csv"@en),
        (doc:'ColumnObject_csv_header')-(scm:csv_column_index)-(1^^xsd:integer),
        (doc:'ColumnObject_csv_header')-(scm:csv_column_name)-("header"^^xsd:string),
        (doc:'ColumnObject_csv_header')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv_some')-(scm:csv_column_index)-(0^^xsd:integer),
        (doc:'ColumnObject_csv_some')-(scm:csv_column_name)-("some"^^xsd:string),
        (doc:'ColumnObject_csv_some')-(rdf:type)-(scm:'Column'),
        (Row3)-(scm:column_header)-("9"^^xsd:string),
        (Row3)-(scm:column_some)-("1"^^xsd:string),
        (Row3)-(rdf:type)-(Row_Type1)
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
    Files = [csv=Filename],
    Commit_Info = _{ author : "me", message : "a message"},
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    csv_load(System_DB, Auth, Path, Commit_Info, Files, []),

    csv_dump(System_DB, Auth, Path, csv, CSV_Filename, []),

    open(CSV_Filename, read, Read_Stream),
    read_string(Read_Stream, _, String),

    % newline depends on platform
    re_match("some,header(\r\n|\n)1,2(\r\n|\n)3,4(\r\n|\n)",String).


test(csv_load_multiple,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = 'admin/testdb',

    tmp_file_stream(Filename1, Stream1, [encoding(utf8)]),
    format(Stream1, "some,header~n", []),
    format(Stream1, "1,2~n", []),
    format(Stream1, "3,4~n", []),
    close(Stream1),


    tmp_file_stream(Filename2, Stream2, [encoding(utf8)]),
    format(Stream2, "another,one~n", []),
    format(Stream2, "888.8,Goofball~n", []),
    format(Stream2, "99.9,Fuzzbucket~n", []),
    close(Stream2),

    Files = [csv1=Filename1, csv2=Filename2],

    Commit_Info = _{ author : "me", message : "a message"},
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),

    csv_load(System_DB, Auth, Path, Commit_Info, Files, []),

    resolve_absolute_string_descriptor(Path, Desc),

    findall(X-Y-Z, ask(Desc,t(X, Y, Z)), Triples),

    Prototype_Triples = [
        (Row4)-(scm:column_another)-("888.8"^^xsd:string),
        (Row4)-(scm:column_one)-("Goofball"^^xsd:string),
        (Row4)-(rdf:type)-(Row_Type3),
        (Row1)-(scm:column_header)-("2"^^xsd:string),
        (Row1)-(scm:column_some)-("1"^^xsd:string),
        (Row1)-(rdf:type)-(Row_Type1),
        (Row2)-(scm:column_header)-("4"^^xsd:string),
        (Row2)-(scm:column_some)-("3"^^xsd:string),
        (Row2)-(rdf:type)-(Row_Type1),
        (Row5)-(scm:column_another)-("99.9"^^xsd:string),
        (Row5)-(scm:column_one)-("Fuzzbucket"^^xsd:string),
        (Row5)-(rdf:type)-(Row_Type3),
        (doc:'CSV_csv1')-(scm:csv_column)-(doc:'ColumnObject_csv1_header'),
        (doc:'CSV_csv1')-(scm:csv_column)-(doc:'ColumnObject_csv1_some'),
        (doc:'CSV_csv1')-(scm:csv_row)-(Row1),
        (doc:'CSV_csv1')-(scm:csv_row)-(Row2),
        (doc:'CSV_csv1')-(rdf:type)-(scm:'CSV'),
        (doc:'CSV_csv1')-(rdfs:label)-("csv1"@en),
        (doc:'CSV_csv2')-(scm:csv_column)-(doc:'ColumnObject_csv2_another'),
        (doc:'CSV_csv2')-(scm:csv_column)-(doc:'ColumnObject_csv2_one'),
        (doc:'CSV_csv2')-(scm:csv_row)-(Row4),
        (doc:'CSV_csv2')-(scm:csv_row)-(Row5),
        (doc:'CSV_csv2')-(rdf:type)-(scm:'CSV'),
        (doc:'CSV_csv2')-(rdfs:label)-("csv2"@en),
        (doc:'ColumnObject_csv1_header')-(scm:csv_column_index)-(1^^xsd:integer),
        (doc:'ColumnObject_csv1_header')-(scm:csv_column_name)-("header"^^xsd:string),
        (doc:'ColumnObject_csv1_header')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv1_some')-(scm:csv_column_index)-(0^^xsd:integer),
        (doc:'ColumnObject_csv1_some')-(scm:csv_column_name)-("some"^^xsd:string),
        (doc:'ColumnObject_csv1_some')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv2_another')-(scm:csv_column_index)-(0^^xsd:integer),
        (doc:'ColumnObject_csv2_another')-(scm:csv_column_name)-("another"^^xsd:string),
        (doc:'ColumnObject_csv2_another')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv2_one')-(scm:csv_column_index)-(1^^xsd:integer),
        (doc:'ColumnObject_csv2_one')-(scm:csv_column_name)-("one"^^xsd:string),
        (doc:'ColumnObject_csv2_one')-(rdf:type)-(scm:'Column')
    ],

    forall(member(Triple, Triples),
           member(Triple,Prototype_Triples)).

test(csv_update_multiple,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = 'admin/testdb',

    tmp_file_stream(Filename1, Stream1, [encoding(utf8)]),
    format(Stream1, "some,header~n", []),
    format(Stream1, "1,2~n", []),
    format(Stream1, "3,4~n", []),
    close(Stream1),


    tmp_file_stream(Filename2, Stream2, [encoding(utf8)]),
    format(Stream2, "another,one~n", []),
    format(Stream2, "888.8,Goofball~n", []),
    format(Stream2, "99.9,Fuzzbucket~n", []),
    close(Stream2),

    Files = [csv1=Filename1, csv2=Filename2],
    Commit_Info = _{ author : "me", message : "a message"},
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    csv_load(System_DB, Auth, Path, Commit_Info, Files, []),

    tmp_file_stream(Filename3, Stream3, [encoding(utf8)]),
    format(Stream3, "some,header~n", []),
    format(Stream3, "1,9~n", []),
    format(Stream3, "3,4~n", []),
    close(Stream3),

    tmp_file_stream(Filename4, Stream4, [encoding(utf8)]),
    format(Stream4, "another,one~n", []),
    format(Stream4, "666,Goofball~n", []),
    format(Stream4, "99.9,Fuzzbucket~n", []),
    close(Stream4),

    Files2 = [csv1=Filename3, csv2=Filename4],
    open_descriptor(system_descriptor{}, System_DB2),
    csv_update(System_DB2, Auth, Path, Commit_Info, Files2, []),

    resolve_absolute_string_descriptor(Path, Desc),
    findall(X-Y-Z, ask(Desc,t(X, Y, Z)), Triples),
    Expected_Triples = [
        (Row2)-(scm:column_header)-("4"^^xsd:string),
        (Row2)-(scm:column_some)-("3"^^xsd:string),
        (Row2)-(rdf:type)-(Row_Type1),
        (Row5)-(scm:column_another)-("99.9"^^xsd:string),
        (Row5)-(scm:column_one)-("Fuzzbucket"^^xsd:string),
        (Row5)-(rdf:type)-(Row_Type3),
        (doc:'CSV_csv1')-(scm:csv_column)-(doc:'ColumnObject_csv1_header'),
        (doc:'CSV_csv1')-(scm:csv_column)-(doc:'ColumnObject_csv1_some'),
        (doc:'CSV_csv1')-(scm:csv_row)-(Row2),
        (doc:'CSV_csv1')-(scm:csv_row)-(Row3),
        (doc:'CSV_csv1')-(rdf:type)-(scm:'CSV'),
        (doc:'CSV_csv1')-(rdfs:label)-("csv1"@en),
        (doc:'CSV_csv2')-(scm:csv_column)-(doc:'ColumnObject_csv2_another'),
        (doc:'CSV_csv2')-(scm:csv_column)-(doc:'ColumnObject_csv2_one'),
        (doc:'CSV_csv2')-(scm:csv_row)-(Row5),
        (doc:'CSV_csv2')-(scm:csv_row)-(Row6),
        (doc:'CSV_csv2')-(rdf:type)-(scm:'CSV'),
        (doc:'CSV_csv2')-(rdfs:label)-("csv2"@en),
        (doc:'ColumnObject_csv1_header')-(scm:csv_column_index)-(1^^xsd:integer),
        (doc:'ColumnObject_csv1_header')-(scm:csv_column_name)-("header"^^xsd:string),
        (doc:'ColumnObject_csv1_header')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv1_some')-(scm:csv_column_index)-(0^^xsd:integer),
        (doc:'ColumnObject_csv1_some')-(scm:csv_column_name)-("some"^^xsd:string),
        (doc:'ColumnObject_csv1_some')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv2_another')-(scm:csv_column_index)-(0^^xsd:integer),
        (doc:'ColumnObject_csv2_another')-(scm:csv_column_name)-("another"^^xsd:string),
        (doc:'ColumnObject_csv2_another')-(rdf:type)-(scm:'Column'),
        (doc:'ColumnObject_csv2_one')-(scm:csv_column_index)-(1^^xsd:integer),
        (doc:'ColumnObject_csv2_one')-(scm:csv_column_name)-("one"^^xsd:string),
        (doc:'ColumnObject_csv2_one')-(rdf:type)-(scm:'Column'),
        (Row6)-(scm:column_another)-("666"^^xsd:string),
        (Row6)-(scm:column_one)-("Goofball"^^xsd:string),
        (Row6)-(rdf:type)-(Row_Type3),
        (Row3)-(scm:column_header)-("9"^^xsd:string),
        (Row3)-(scm:column_some)-("1"^^xsd:string),
        (Row3)-(rdf:type)-(Row_Type1)
    ],

    forall(member(Triple, Triples),
           member(Triple, Expected_Triples)).

test(csv_load_with_schema,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("admin", "testdb")
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

    once(ask(Desc,
             t((scm:'CSV'),(rdfs:comment),("CSV object"@en), schema))).

:- end_tests(csv_api).
