:- module('util/json_stream',[
              json_stream_start/1,
              json_stream_end/3,
              json_stream_write_dict/5,
              json_stream_write_string/4,
              json_stream_write_start/3
          ]).

:- use_module(library(http/json)).

/**
 * json_stream_start(-Stream_Started) is det.
 *
 * Initialize the sequence of predicates used for writing a JSON stream to
 * output.
 *
 * After this, use json_stream_write_dict to write JSON dictionaries to the
 * stream.
 *
 * (The implemntation is simple, but the predicate name is good documentation.)
 */
json_stream_start(Stream_Started) :-
    var(Stream_Started),
    !,
    Stream_Started = stream_started(false).
json_stream_start(Stream_Started) :-
    throw(error(unexpected_argument_instantiation(json_stream_start, Stream_Started), _)).

/**
 * json_stream_end(+Intial_Goal, +As_List, +Stream_Started) is det.
 *
 * Finalize the sequence of predicates used for writing a JSON stream to output.
 *
 * This is the last thing to do after writing all the JSON dictionaries to the
 * stream.
 */
:- meta_predicate json_stream_end(1,+,?).
json_stream_end(Initial_Goal, As_List, stream_started(Started)) :-
    !,
    % Write the headers in case they weren't written.
    (   Started = true
    ->  true
    ;   call(Initial_Goal, As_List)
    ),

    % Write the list end character (right square bracket).
    (   As_List = true
    ->  format("]~n")
    ;   true).
json_stream_end(_Initial_Goal, _As_List, Stream_Started) :-
    throw(error(unexpected_argument_instantiation(json_stream_end, Stream_Started), _)).

json_stream_write_start(Initial_Goal, As_List, Stream_Started) :-
    % Get the current value of Stream_Started before we possibly update it with
    % nb_setarg.
    Stream_Started = stream_started(Started),

    % Write the headers in case they weren't written. Update Stream_Started, so
    % that we don't write them again.
    (   Started = true
    ->  true
    ;   nb_setarg(1, Stream_Started, true),
        call(Initial_Goal, As_List)
    ).

/**
 * json_stream_write_dict(+Request, +As_List, +Data_Version, +Stream_Started, +JSON, +JSON_Options) is det.
 *
 * Write a single JSON dictionary to the stream or list with the appropriate
 * separators. If this is the first dictionary in the stream, write the headers
 * before writing the dictionary.
 *
 * After writing all the JSON dictionaries to the stream, use
 * cors_json_stream_end to end it.
 */
:- meta_predicate json_stream_write_dict(1,+,?,+,+).
json_stream_write_dict(Initial_Goal, As_List, Stream_Started, JSON, JSON_Options) :-
    % Get the current value of Stream_Started before we possibly update it with
    % nb_setarg.
    Stream_Started = stream_started(Started),

    % Write the headers in case they weren't written. Update Stream_Started, so
    % that we don't write them again.
    (   Started = true
    ->  true
    ;   nb_setarg(1, Stream_Started, true),
        call(Initial_Goal, As_List)
    ),

    % Write the list separator (comma).
    (   Started = true,
        As_List = true
    ->  format(",")
    ;   true),

    % Write the JSON dictionary.
    json_write_dict(current_output, JSON, JSON_Options),

    % Write the stream separator (newline).
    (   As_List = true
    ->  true
    ;   format("~n")).

:- meta_predicate json_stream_write_dict(1,+,?,+,+).
json_stream_write_string(Initial_Goal, As_List, Stream_Started, String) :-
    % Get the current value of Stream_Started before we possibly update it with
    % nb_setarg.
    Stream_Started = stream_started(Started),

    % Write the headers in case they weren't written. Update Stream_Started, so
    % that we don't write them again.
    (   Started = true
    ->  true
    ;   nb_setarg(1, Stream_Started, true),
        call(Initial_Goal, As_List)
    ),

    % Write the list separator (comma).
    (   Started = true,
        As_List = true
    ->  format(",")
    ;   true),

    % Write the String
    write(current_output, String),

    % Write the stream separator (newline).
    (   As_List = true
    ->  true
    ;   format("~n")).
