:- module(graph_dump, [graph_dump/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(triple/turtle_utils)).

:- use_module(library(plunit)).
:- use_module(library(pcre)).
:- use_module(library(readutil)).

graph_dump(System_DB, Auth, Path, Format, String) :-
    do_or_die(
        resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path), _)),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           filter : type_name_filter{ type: (Graph.type)}},
        Context),

    assert_read_access(Context),

    % We can extend formats here..
    (   Format = "turtle"
    ->  dump_turtle_graph(Context,String)
    ;   throw(error(unknown_format(Format), _))).

:- begin_tests(graph_dump).

:- use_module(core(util/test_utils)).
:- use_module(graph_load).

test(worlds_graph, [
         setup((setup_temp_store(State),
                create_db_with_test_schema("admin", "test"))),
         cleanup(teardown_temp_store(State)),
         fixme('no longer use the same ontology as the world.ttl can support')
     ]) :-

    expand_file_search_path(test('world.ttl'), File),
    read_file_to_string(File, String, []),
    super_user_authority(Auth),
    System_DB = system_descriptor{},
    Path = "admin/test/local/branch/main/instance",
    Commit_Info = commit_info{ author : "Me", message : "none" },
    Format = "turtle",
    graph_insert(System_DB, Auth, Path, Commit_Info, Format, String),

    resolve_absolute_string_descriptor("admin/test", Desc),
    once(
        ask(Desc,
            t(doc:scipioJrs, scm:birthday, Date))
    ),

    Date = date_time(-228,10,10,0,0,0.0)^^xsd:dateTime,

    graph_dump(System_DB, Auth, Path, "turtle", Output),

    re_match('-228-10-10T00:00:00.000Z', Output).

:- end_tests(graph_dump).
