:- module('util/json_stream',[
              json_stream_start/2,
              json_stream_end/1,
              json_stream_write/3,
              json_stream_write_dict/3
          ]).

:- use_module(library(json)).

/**
 * json_stream_start(+Config) is det.
 *
 * Starts writing a json stream. Effectively, this just writes a list opener if As_List is true.
 */
json_stream_start(Config, Stream_Started) :-
    (   Config.as_list = true
    ->  format("[~n")
    ;   true),

    Stream_Started = started(false).

/**
 * json_stream_end(+Config) is det.
 *
 * Finalize writing a json stream. Effectively, this jsut writes a list closer if As_List is true.
 */
json_stream_end(Config) :-
    (   Config.as_list = true
    ->  format("~n]~n")
    ;   true).

/**
 * json_stream_write_dict(+Config, +Stream_Started, +JSON) is det.
 *
 * Write a single JSON dictionary to the stream or list with the appropriate
 * separators.
 *
 * After writing all the JSON dictionaries to the stream, use
 * cors_json_stream_end to end it.
 */
json_stream_write_dict(Config, Stream_Started, JSON) :-
    Stream_Started = started(Started),
    % Write the list separator (comma).
    (   Started = true,
        Config.as_list = true
    ->  format(",~n")
    ;   true),

    % Set list started to true
    (   Started = false
    ->  nb_setarg(1, Stream_Started, true)
    ;   true),

    % Write the JSON dictionary.
    (   Config.minimized = true
    ->  JSON_Options = [width(0)]
    ;   JSON_Options = []),
    json_write_dict(current_output, JSON, JSON_Options),

    % Write the stream separator (newline).
    (   Config.as_list = true
    ->  true
    ;   format("~n")).

/**
 * json_stream_write(+Config, +Stream_Started, +JSON) is det.
 *
 * Write a single JSON term to the stream or list with the appropriate
 * separators — the same framing as json_stream_write_dict/3 for values
 * that are not dicts, such as document id strings.
 */
json_stream_write(Config, Stream_Started, JSON) :-
    Stream_Started = started(Started),
    (   Started = true,
        Config.as_list = true
    ->  format(",~n")
    ;   true),

    (   Started = false
    ->  nb_setarg(1, Stream_Started, true)
    ;   true),

    (   Config.minimized = true
    ->  JSON_Options = [width(0)]
    ;   JSON_Options = []),
    json_write(current_output, JSON, JSON_Options),

    (   Config.as_list = true
    ->  true
    ;   format("~n")).
