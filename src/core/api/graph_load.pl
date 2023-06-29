:- module(graph_load, [graph_update/6,graph_insert/6]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(triple/turtle_utils)).

graph_update(System_DB, Auth, Path, Commit_Info, Format, String) :-
    do_or_die(
        resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path), _)),

    do_or_die(
        open_descriptor(Descriptor, Transaction),
        error(unresolvable_absolute_descriptor(Descriptor),_)),

    askable_settings_context(
        Transaction,
        _{  commit_info : Commit_Info,
            system: System_DB,
            authorization : Auth,
            write_graph : Graph
        }, Context),

    assert_write_access(Context),

    % We can extend formats here..
    (   Format = "turtle"
    ->  update_turtle_graph(Context,String)
    ;   throw(error(unknown_format(Format), _))).


graph_insert(System_DB, Auth, Path, Commit_Info, Format, String) :-
    do_or_die(
        resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path), _)),

    do_or_die(
        open_descriptor(Descriptor, Transaction),
        error(unresolvable_absolute_descriptor(Descriptor),_)),

    askable_settings_context(
        Transaction,
        _{  commit_info : Commit_Info,
            system: System_DB,
            authorization : Auth,
            write_graph : Graph
        }, Context),

    assert_write_access(Context),

    % We can extend formats here..
    (   Format = "turtle"
    ->  insert_turtle_graph(Context,String)
    ;   throw(error(unknown_format(Format), _))).
