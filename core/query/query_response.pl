:- module(query_response,[
              run_context_ast_jsonld_response/3
          ]).

:- use_module(woql_compile).
:- use_module(ask).
:- use_module(jsonld).

:- use_module(core(api)).
:- use_module(core(util)).
:- use_module(core(transaction)).

/** <module> Query Response
 *
 * Code to generate bindings for query response in JSON-LD
 */
run_context_ast_jsonld_response(Context, AST, JSON) :-
    compile_query(AST,Prog,Context,Output_Context),

    with_transaction(
        Output_Context,
        (
            findall(JSON_Binding,
                    (   woql_compile:Prog,
                        get_dict(bindings, Output_Context, Bindings),
                        json_transform_binding_set(Output_Context, Bindings, JSON_Binding)),
                    Binding_Set),

            query_context_transaction_objects(Output_Context,Transactions),
            run_transactions(Transactions),

            (   Inserts = Output_Context.get(inserts)
            ->  (   var(Inserts) % debugging - this should never happen.
                ->  Inserts = 0
                ;   true)
            ;   Inserts = 0),
            (   Deletes = Output_Context.get(deletes)
            ->  (   var(Deletes) % debugging - this should never happen.
                ->  Inserts = 0
                ;   true)
            ;   Deletes = 0),

            JSON = _{'bindings' : Binding_Set,
                     'inserts' : Inserts,
                     'deletes' : Deletes}
        )
    ).

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
