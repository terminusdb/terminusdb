:- module(query_response,[
              ask_ast_jsonld_response/3
          ]).

:- use_module(core(api)).
:- use_module(core(query/ask)).
:- use_module(core(query/jsonld)).

/** <module> Query Response
 *
 * Code to generate bindings for query response in JSON-LD
 */
ask_ast_jsonld_response(Context, AST, JSON) :-
    findall(JSON_Binding,
            (   ask_ast(Context, AST, Binding),
                json_transform_binding_set(Context, Binding, JSON_Binding)),
            Binding_Set),
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
