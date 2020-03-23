:- module(query_response,[
              ask_ast_jsonld_response/3
          ]).

:- use_module(core(api)).
:- use_module(core(query/ask)).
:- use_module(core(query/jsonld)).
:- use_module(core(util)).
:- use_module(core(transaction)).

/** <module> Query Response
 *
 * Code to generate bindings for query response in JSON-LD
 */
ask_ast_jsonld_response(Context, AST, JSON) :-
    findall(JSON_Binding-Output_Context,
            (   ask_ast(Context, AST, Output_Context),
                get_dict(bindings, Output_Context, Bindings),
                json_transform_binding_set(Context, Bindings, JSON_Binding)),
            Binding_Set_And_Context),

    (   Binding_Set_And_Context = []
    ->  Final_Context = Context,
        Binding_Set = []
    ;   zip(Binding_Set, Contexts, Binding_Set_And_Context),
        last(Contexts, Final_Context)),

    (   Final_Context.transaction_objects \= []
    ->  run_transactions(Final_Context.transaction_objects)
    ;   true),

    (   Inserts = Context.get(inserts)
    ->  true
    ;   Inserts = 0),
    (   Deletes = Context.get(inserts)
    ->  true
    ;   Deletes = 0),

    JSON = _{'bindings' : Binding_Set,
             'inserts' : Inserts,
             'deletes' : Deletes}.

json_transform_binding_set(_Context, Binding, JSON) :-
    % TODO: We probably want to "compress" the URIs using the context
    maplist([Record,Var_Name=Term]>>(
                get_dict(var_name, Record, Var_Name),
                get_dict(woql_var, Record, Prolog_Var),
                (   var(Prolog_Var)
                ->  Term = "terminus:unknown"
                ;   term_jsonld(Prolog_Var, Term))),
            Binding,
            Data),
    dict_create(JSON, _, Data).
