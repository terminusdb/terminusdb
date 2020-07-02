:- module(graph_dump, [graph_dump/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(triple/turtle_utils)).

graph_dump(System_DB, Auth, Path, Format, String) :-
    do_or_die(
        resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path), _)),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Context),

    assert_read_access(Context),

    % We can extend formats here..
    (   Format = "turtle"
    ->  dump_turtle_graph(Context,String)
    ;   Format = "md"
    ->  dump_md(Context,String)
    ;   throw(error(unknown_format(Format), _))).
