:- module(api_csv, [csv_load/6]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

csv_load(System_DB, Auth, Path, Commit_Info, Files, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_default_graph(Path, Descriptor, Graph),
        error(invalid_graph_descriptor(Path),_)),

    do_or_die(
        Files = [Name=Path],
        error(single_files_only(Files))),

    askable_settings_context(
        Descriptor,
        _{ system : System_DB,
           authorization : Auth,
           commit_info: Commit_Info,
           filter : type_name_filter{ type: (Graph.type),
                                      names : [Graph.name]}},
        Context),

    assert_write_access(Context),

    csv_load_into_context(Name,Path,Context,Options).

csv_load_into_context(Name, Path, Context, Options) :-
    (   option(data_prefix(_), Options, _)
    ->  Final_Options = Options
    ;   atomic_list_concat(['csv:///',Name,'/data/'], Data),
        merge_options(Options, [data_prefix(Data)], Final_Options)),

    query_default_write_graph(Context, Read_Write_Obj),
    read_write_obj_builder(Read_Write_Obj, Builder),
    csv_builder_to_layer(Path, Builder, _Layer, Final_Options).
