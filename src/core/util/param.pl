/*
 * This is a collection of utilities for finding, checking, and converting
 * values from JSON and search (a.k.a. query string parameters).
 *
 * All JSON and search values should involve one of these predicates so that we
 * can have consistent and safe input validation along with minimal redundancy.
 */
:- module(param, [
              param_check_json/4,
              param_check_search/4,

              param_value_search_required/4,
              param_value_search_optional/5,

              param_value_json_required/4,
              param_value_json_optional/5,

              param_value_search_or_json/5,
              param_value_search_or_json_required/5,
              param_value_search_or_json_optional/6,

              param_value_search_author/2,
              param_value_search_message/2,
              param_value_search_graph_type/2
          ]).

:- use_module(utils).

/*
 * Check a JSON dict parameter value against an expected type. If it's wrong,
 * throw an error. Otherwise, convert it if necessary.
 */
param_check_json(Type, Param, Value_In, Value_Out) :-
    (   param_check_json_(Type, Value_In, Value_Out)
    ->  true
    ;   param_check_common_(Type, Value_In, Value_Out)
    ->  true
    ;   throw(error(bad_parameter_type(Param, Type, Value_In), _))).

param_check_json_(graph, "schema", schema).
param_check_json_(graph, "instance", instance).
param_check_json_(object, Value_In, Value_Out) :-
    is_dict(Value_In),
    Value_Out = Value_In.
param_check_json_(non_empty_string, Value_In, Value_Out) :-
    string(Value_In),
    Value_In \= "",
    Value_Out = Value_In.
param_check_json_(string, Value_In, Value_Out) :-
    string(Value_In),
    Value_Out = Value_In.
param_check_json_(non_empty_atom, Value_In, Value_Out) :-
    % In JSON, all inputs must be strings but we want an atom as output.
    param_check_json_(non_empty_string, Value_In, Value_String),
    atom_string(Value_Out, Value_String).

/*
 * Check a search list (a.k.a. query string) parameter value against an expected
 * type. If it's wrong, throw an error. Otherwise, convert it if necessary.
 */
param_check_search(Type, Param, Value_In, Value_Out) :-
    (   param_check_search_(Type, Value_In, Value_Out)
    ->  true
    ;   param_check_common_(Type, Value_In, Value_Out)
    ->  true
    ;   throw(error(bad_parameter_type(Param, Type, Value_In), _))).

param_check_search_(graph, schema, schema).
param_check_search_(graph, instance, instance).
param_check_search_(non_empty_atom, Value_In, Value_Out) :-
    % In search lists, all strings are given as atoms.
    atom(Value_In),
    Value_In \= '',
    Value_Out = Value_In.

/* Check parameters common to both JSON dicts and stream lists. */
param_check_common_(boolean, false, false).
param_check_common_(boolean, true, true).
param_check_common_(integer, Value_In, Value_Out) :-
    input_to_integer(Value_In, Value_Out).
param_check_common_(nonnegative_integer, Value_In, Value_Out) :-
    input_to_integer(Value_In, Value_Out),
    Value_Out >= 0.
param_check_common_(text, Value_In, Value_In) :-
    text(Value_In).

param_value_search_(Search, Param, Type, Value) :-
    memberchk(Param=Value_Unchecked, Search),
    param_check_search(Type, Param, Value_Unchecked, Value).

/*
 * Get the value of a required parameter from a search list (a.k.a. query string
 * parameters), check its type, and convert if necessary.
 */
param_value_search_required(Search, Param, Type, Value) :-
    (   param_value_search_(Search, Param, Type, Value)
    ->  true
    ;   throw(error(missing_parameter(Param), _))).

/*
 * Get the value of an optional parameter from a search list, or use Default if
 * not found.
 */
param_value_search_optional(Search, Param, Type, Default, Value) :-
    (   param_value_search_(Search, Param, Type, Value)
    ->  true
    ;   Value = Default).

/*
 * Get the value of the required parameter 'author' from a search list.
 */
param_value_search_author(Search, Author) :-
    param_value_search_required(Search, author, non_empty_atom, Author).

/*
 * Get the value of the required parameter 'message' from a search list.
 */
param_value_search_message(Search, Message) :-
    param_value_search_required(Search, message, non_empty_atom, Message).

/*
 * Get the value of the optional parameter 'graph_type' from a search list.
 */
param_value_search_graph_type(Search, Graph_Type) :-
    param_value_search_optional(Search, graph_type, graph, instance, Graph_Type).

param_value_json_(JSON, Param, Type, Value) :-
    get_dict(Param, JSON, Value_Unchecked),
    !,
    param_check_json(Type, Param, Value_Unchecked, Value).

/*
 * Get the value of a required parameter from a JSON dict, check it, and convert
 * if necessary.
 */
param_value_json_required(JSON, Param, Type, Value) :-
    (   param_value_json_(JSON, Param, Type, Value)
    ->  true
    ;   throw(error(missing_parameter(Param), _))).

/*
 * Get the value of an optional parameter from a JSON dict, or use Default if
 * not found.
 */
param_value_json_optional(JSON, Param, Type, Default, Value) :-
    (   param_value_json_(JSON, Param, Type, Value)
    ->  true
    ;   Value = Default).

/*
 * Get the value of a required parameter from a search list or JSON dict, check
 * it, and convert if necessary.
 */
param_value_search_or_json(Search, JSON, Param, Type, Value) :-
    (   param_value_search_(Search, Param, Type, Value)
    ->  true
    ;   param_value_json_(JSON, Param, Type, Value)
    ->  true).

/*
 * Get the value of a required parameter from a search list or JSON dict, check
 * it, and convert if necessary.
 */
param_value_search_or_json_required(Search, JSON, Param, Type, Value) :-
    (   param_value_search_or_json(Search, JSON, Param, Type, Value)
    ->  true
    ;   throw(error(missing_parameter(Param), _))).

/*
 * Get the value of an optional parameter from a search list or JSON dict, check
 * it, and convert if necessary.
 */
param_value_search_or_json_optional(Search, JSON, Param, Type, Default, Value) :-
    (   param_value_search_or_json(Search, JSON, Param, Type, Value)
    ->  true
    ;   Value = Default).
