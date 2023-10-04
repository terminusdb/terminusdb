:- module(api_woql, [woql_query_json/8]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).

:- use_module(library(option)).

woql_query_json(System_DB, Auth, Path_Option, Query, Context, New_Data_Version, JSON, Options) :-

    option(commit_info(Commit_Info), Options),
    option(all_witnesses(All_Witnesses), Options),
    option(files(Files), Options),
    option(data_version(Requested_Data_Version), Options),

    % No descriptor to work with until the query sets one up
    (   some(Path) = Path_Option
    ->  do_or_die(resolve_absolute_string_descriptor(Path, Descriptor),
                  error(invalid_absolute_path(Path),_)),
        do_or_die(askable_context(Descriptor, System_DB, Auth, Commit_Info, Context0),
                  error(unresolvable_collection(Descriptor),_)),
        Context = (Context0.put(_{files:Files,
                                  all_witnesses : All_Witnesses
                                 }))
    ;   none = Path_Option
    ->  empty_context(Empty),
        Context = (Empty.put(_{system: System_DB,
                               files: Files,
                               authorization : Auth,
                               all_witnesses : All_Witnesses,
                               commit_info : Commit_Info}))
    ;   throw(error(unexpected_path_option(Path_Option), _))
    ),

    (   Query = json_query(JSON_Query)
    ->  json_woql(JSON_Query, AST)
    ;   Query = atom_query(Atom_Query)
    ->  atom_woql(Atom_Query, AST)
    ;   throw(error(unexpected_query_type(Query), _))
    ),
    run_context_ast_jsonld_response(Context, AST, Requested_Data_Version, New_Data_Version, JSON, Options).

bind_vars([],_).
bind_vars([Name=Var|Tail],AST) :-
    Var = v(Name),
    bind_vars(Tail,AST).

atom_woql(Query, AST) :-
    read_term_from_atom(Query, AST, [variable_names(Names)]),
    bind_vars(Names,AST).
