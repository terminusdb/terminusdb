:- module(query_response,[
              ask_ast_jsonld_response/3
          ]).

/** <module> Query Response
 *
 * Code to generate bindings for query response in JSON-LD
 */
ask_ast_jsonld_response(Context, AST, JSON) :-
    findall(JSON_Binding,
            (ask_ast(Context, AST, Binding),
             (   % Now we have a binding set
                 json_transform_binding_set(Context, Binding, JSON_Binding),
             )),
            JSON
           ).

json_transform_binding_set(Context, Binding, JSON) :-
    maplist([Record,Var_Name=Term]>>(get_dict(Record, var_name, Var_Name),
                                     get_dict(Record, prolog_var, Prolog_Var),
                                     (   var(Prolog_Var)
                                     ->  JSON = "terminus:unknown"
                                     ;   term_jsonld(Prolog_Var, Term))),
            Binding,
            Data),
    dict_create(JSON, _, Data).
