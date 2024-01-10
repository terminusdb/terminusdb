:- module(query_response,[
              run_context_ast_jsonld_response/5,
              run_context_ast_jsonld_response/6,
              pretty_print_query_response/3
          ]).

:- use_module(woql_compile).
:- use_module(ask).
:- use_module(jsonld).

:- use_module(core(api)).
:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(triple/literals)).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).

run_context_ast_jsonld_response(Context, AST, Requested_Data_Version, New_Data_Version, Binding_JSON) :-
    Options = _{ optimize: true, all_witnesses: false},
    run_context_ast_jsonld_response(Context, AST, Requested_Data_Version, New_Data_Version, Binding_JSON, Options).

/** <module> Query Response
 *
 * Code to generate bindings for query response in JSON-LD
 */
run_context_ast_jsonld_response(Context, AST, Requested_Data_Version, New_Data_Version, Binding_JSON, Options) :-
    compile_query(AST,Prog,Context,Output_Context,Options),
    do_or_die(
        query_default_collection(Output_Context, Transaction),
        error(query_default_collection_failed_unexpectedly(Output_Context), _)),
    transaction_data_version(Transaction, Actual_Data_Version),
    compare_data_versions(Requested_Data_Version, Actual_Data_Version),
    with_transaction(
        Output_Context,
        query_response:(
            findall(JSON_Binding,
                    (   woql_compile:Prog,
                        get_dict(bindings, Output_Context, Bindings),
                        * json_log_info_formatted('~N[Bindings] ~q~n', [Bindings]),
                        json_transform_binding_set(Output_Context, Bindings, JSON_Binding)),
                    Binding_Set),
            * json_log_info_formatted('~N[Binding Set] ~q~n', [Binding_Set])
        ),
        Meta_Data
    ),
    meta_data_version(Transaction, Meta_Data, New_Data_Version),
    context_variable_names(Output_Context, Names),
    Binding_JSON = _{'@type' : 'api:WoqlResponse',
                     'api:status' : 'api:success',
                     'api:variable_names' : Names,
                     'bindings' : Binding_Set,
                     % These are whitelisted metadata fields for serialization.
                     % Other fields (e.g. data_versions) cannot be serialized.
                     inserts : Meta_Data.inserts,
                     deletes : Meta_Data.deletes,
                     transaction_retry_count : Meta_Data.transaction_retry_count }.

context_variable_names(Context, Header) :-
    get_dict(bindings, Context, Bindings),
    maplist([Record, Name]>>get_dict(var_name, Record, Name),
            Bindings, Rev),
    reverse(Rev,Header).

json_transform_binding_set(Context, Binding, JSON) :-
    % TODO: We probably want to "compress" the URIs using the context
    Prefixes = (Context.prefixes),
    maplist({Prefixes}/[Record,Var_Name=Term]>>(
                get_dict(var_name, Record, Var_Name),
                get_dict(woql_var, Record, Prolog_Var),
                (   var(Prolog_Var)
                ->  Term = null
                ;   term_jsonld(Prolog_Var, Prefixes, Term))),
            Binding,
            Data),
    dict_create(JSON, _, Data).

field_separator(true,"",_Size).
field_separator(false,Sep,Size) :-
    atomic_list_concat(["~` t~",Size,"| | "],Sep).

cumulative_([],_,[]).
cumulative_([X|Res],Acc,[Sum|Remainder]) :-
    Sum is X + Acc,
    cumulative_(Res,Sum,Remainder).

cumulative(List,Cum) :-
    cumulative_(List,0,Cum).

column_size(Name,Bindings,Size) :-
    atom_length(Name,Name_Length),
    foldl({Name}/[Binding,R,Max]>>(
              get_dict(Name,Binding,Res),
              format(atom(Val),"~w", Res),
              atom_length(Val,Length),
              Max is max(R,Length)
          ),
          Bindings,
          Name_Length,
          Size).

pretty_print_value(Value,Prefixes,Compressed) :-
    once(   atom(Value)
        ;   string(Value)),
    !,
    uri_to_prefixed(Value,Prefixes,Term),
    format(atom(Compressed), "~w", [Term]).
pretty_print_value(Literal,Prefixes,Compressed) :-
    is_dict(Literal),
    !,
    get_dict('@value',Literal,Value),
    (   get_dict('@type', Literal, Type)
    ->  uri_to_prefixed(Type,Prefixes,Type_Prefixed),
        format(atom(Compressed),'~q^^~q',[Value,Type_Prefixed])
    ;   get_dict('@language', Literal, Lang),
        format(atom(Compressed),'~q@~q',[Value,Lang])
    ).
pretty_print_value(List,Prefixes,Atom) :-
    is_list(List),
    !,
    maplist({Prefixes}/[Literal,Printed]>>pretty_print_value(Literal,Prefixes,Printed),
            List, Atoms),
    format(atom(Atom), '~w', [Atoms]).
pretty_print_value(Value,_Prefixes,_Compressed) :-
    throw(error(no_case_coverage_for_pretty_printer(Value))).

pretty_print_query_response(Response, Prefixes, String) :-

    Names = (Response.'api:variable_names'),
    Bindings = (Response.bindings),
    maplist({Prefixes,Names}/[Binding,New_Binding]>>(
                maplist({Prefixes,Names,Binding}/[Name,Name-New_Value]>>(
                            get_dict(Name,Binding,Value),
                            pretty_print_value(Value,Prefixes,New_Value)
                        ),
                        Names,
                        Dict_Values),
                dict_create(New_Binding,_,Dict_Values)
            ),Bindings,Format_Bindings),
    maplist({Format_Bindings}/[Name,Size]>>column_size(Name,Format_Bindings,Size),Names,Sizes),
    maplist([Size,New_Size]>>(New_Size is Size + 1),Sizes,New_Sizes),

    (   append(Offset_Sizes,[_],New_Sizes)

    ->  maplist([Size,Format]>>atomic_list_concat(['~w~` t~',Size,'+'],Format),Offset_Sizes,Offset_Formats),

        append(Offset_Formats,['~w'],Formats),
        atomic_list_concat(Formats,Format_String)
    ;   Format_String = 'No results'),

    with_output_to(
        string(String),
        (   format(current_output,Format_String, Names),
            format(current_output,"~n",[]),
            forall(
                member(Binding, Format_Bindings),
                (
                    maplist({Binding}/[Name,Value]>>
                            get_dict(Name,Binding,Value), Names, Values),
                    (   Values \= []
                    ->  format(current_output,Format_String, Values),
                        format(current_output,"~n",[])
                    ;   true)
                )
            ),

            get_dict(inserts, Response, Inserts),
            (   Inserts > 0
            ->  format(current_output, "Inserts: ~d~n", [Inserts])
            ;   true),

            get_dict(deletes, Response, Deletes),
            (   Deletes > 0
            ->  format(current_output, "Deletes: ~d~n", [Deletes])
            ;   true)
        )
    ).
