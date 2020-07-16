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
        query_response:(
            findall(JSON_Binding,
                    (   woql_compile:Prog,
                        get_dict(bindings, Output_Context, Bindings),
                        * http_log('~N[Bindings] ~q~n', [Bindings]),
                        json_transform_binding_set(Output_Context, Bindings, JSON_Binding)),
                    Binding_Set),
            * http_log('~N[Binding Set] ~q~n', [Binding_Set])
        ),
        Meta_Data
    ),
    context_variable_names(Output_Context, Names),
    Binding_JSON = _{'@type' : 'api:WoqlResponse',
                     'api:status' : 'api:success',
                     'api:variable_names' : Names,
                     'bindings' : Binding_Set},
    put_dict(Meta_Data, Binding_JSON, JSON).

context_variable_names(Context, Header) :-
    get_dict(bindings, Context, Bindings),
    maplist([Record, Name]>>get_dict(var_name, Record, Name),
            Bindings, Rev),
    reverse(Rev,Header).

json_transform_binding_set(_Context, Binding, JSON) :-
    % TODO: We probably want to "compress" the URIs using the context
    maplist([Record,Var_Name=Term]>>(
                get_dict(var_name, Record, Var_Name),
                get_dict(woql_var, Record, Prolog_Var),
                (   var(Prolog_Var)
                ->  Term = "system:unknown"
                ;   term_jsonld(Prolog_Var, Term))),
            Binding,
            Data),
    dict_create(JSON, _, Data).
