:- module(trace_context, [
    parse_traceparent/3,
    format_traceparent/3
]).

/** <module> W3C Trace Context Support
 *
 * This module provides parsing and formatting for W3C Trace Context headers,
 * enabling integration with distributed tracing systems like Jaeger and OpenTelemetry.
 *
 * W3C Trace Context Format: version-trace_id-span_id-flags
 * Example: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
 *
 * See: https://www.w3.org/TR/trace-context/
 */

/**
 * parse_traceparent(+Traceparent, -TraceId, -SpanId) is semidet.
 *
 * Parse a W3C Trace Context traceparent header into its components.
 *
 * @param Traceparent The traceparent header value (atom or string)
 * @param TraceId The 32-character hexadecimal trace ID
 * @param SpanId The 16-character hexadecimal span ID
 *
 * Examples:
 *   ?- parse_traceparent('00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01', T, S).
 *   T = "4bf92f3577b34da6a3ce929d0e0e4736",
 *   S = "00f067aa0ba902b7".
 */
parse_traceparent(Traceparent, TraceId, SpanId) :-
    % Convert to string if atom
    (   atom(Traceparent)
    ->  atom_string(Traceparent, String)
    ;   String = Traceparent
    ),
    % Split by hyphen
    split_string(String, "-", "", Parts),
    % Must have at least 4 parts: version-trace_id-span_id-flags
    Parts = [_Version, TraceId, SpanId|_Rest],
    % Validate trace_id (32 hex chars)
    string_length(TraceId, 32),
    % Validate span_id (16 hex chars)
    string_length(SpanId, 16),
    !.

/**
 * format_traceparent(+TraceId, +SpanId, -Traceparent) is det.
 *
 * Format a W3C Trace Context traceparent header from components.
 * Uses version '00' and flags '01' (sampled).
 *
 * @param TraceId The 32-character hexadecimal trace ID
 * @param SpanId The 16-character hexadecimal span ID
 * @param Traceparent The formatted traceparent header
 *
 * Examples:
 *   ?- format_traceparent("4bf92f3577b34da6a3ce929d0e0e4736", "00f067aa0ba902b7", TP).
 *   TP = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'.
 */
format_traceparent(TraceId, SpanId, Traceparent) :-
    format(atom(Traceparent), '00-~w-~w-01', [TraceId, SpanId]).

:- begin_tests(trace_context).

test(parse_valid_traceparent, []) :-
    parse_traceparent('00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01', TraceId, SpanId),
    TraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
    SpanId = "00f067aa0ba902b7".

test(parse_traceparent_atom, []) :-
    parse_traceparent('00-abcdef0123456789abcdef0123456789-0123456789abcdef-00', TraceId, SpanId),
    TraceId = "abcdef0123456789abcdef0123456789",
    SpanId = "0123456789abcdef".

test(parse_invalid_traceparent_fails, [fail]) :-
    parse_traceparent('invalid-format', _, _).

test(parse_short_trace_id_fails, [fail]) :-
    parse_traceparent('00-short-00f067aa0ba902b7-01', _, _).

test(format_traceparent, []) :-
    format_traceparent("4bf92f3577b34da6a3ce929d0e0e4736", "00f067aa0ba902b7", TP),
    TP = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'.

test(parse_then_format_roundtrip, []) :-
    Original = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01',
    parse_traceparent(Original, TraceId, SpanId),
    format_traceparent(TraceId, SpanId, Formatted),
    atom_string(Formatted, FormattedString),
    atom_string(Original, OriginalString),
    FormattedString = OriginalString.

:- end_tests(trace_context).
