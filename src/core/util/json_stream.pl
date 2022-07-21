:- module('util/json_stream',[
              json_stream_start/2,
              json_stream_end/1,
              json_stream_write_dict/4
          ]).

:- use_module(library(http/json)).

/**
 * json_stream_start(+As_List) is det.
 *
 * Starts writing a json stream. Effectively, this just writes a list opener if As_List is true.
 */
json_stream_start(As_List, Stream_Started) :-
    (   As_List = true
    ->  format("[~n")
    ;   true),

    Stream_Started = started(false).

/**
 * json_stream_end(+As_List) is det.
 *
 * Finalize writing a json stream. Effectively, this jsut writes a list closer if As_List is true.
 */
:- meta_predicate json_stream_end(1,+,?).
json_stream_end(As_List) :-
    (   As_List = true
    ->  format("~n]~n")
    ;   true).

/**
 * json_stream_write_dict(+As_List, +Pretty, +Stream_Started, +JSON) is det.
 *
 * Write a single JSON dictionary to the stream or list with the appropriate
 * separators.
 *
 * After writing all the JSON dictionaries to the stream, use
 * cors_json_stream_end to end it.
 */
:- meta_predicate json_stream_write_dict(+,+,?,+).
json_stream_write_dict(As_List, Minimized, Stream_Started, JSON) :-
    Stream_Started = started(Started),
    % Write the list separator (comma).
    (   Started = true,
        As_List = true
    ->  format(",~n")
    ;   true),

    % Set list started to true
    (   Started = false
    ->  nb_setarg(1, Stream_Started, true)
    ;   true),

    % Write the JSON dictionary.
    (   Minimized = true
    ->  JSON_Options = [width(0)]
    ;   JSON_Options = []),
    json_write_dict(current_output, JSON, JSON_Options),

    % Write the stream separator (newline).
    (   As_List = true
    ->  true
    ;   format("~n")).
