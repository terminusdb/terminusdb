:- module(plugin_api_stream, [
    raw_out_stream/2,
    stream_ndjson/2
]).

:- use_module(library(http/http_stream)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

%% raw_out_stream(+Stream, -Out) is det.
%
%  Resolve a stream pair to its output side (avoids "ambiguous operation
%  on stream pair" from http_open's returned pair).
raw_out_stream(Stream, Out) :-
    (   is_stream(Stream),
        stream_pair(Stream, _In, PairOut),
        PairOut \== []
    ->  Out = PairOut
    ;   Out = Stream
    ).

%% stream_ndjson(+Out, :Producer) is det.
%
%  Writes NDJSON content to Out using chunked transfer encoding.
%  Producer is called with a chunked output stream and should write
%  NDJSON lines to it. The Content-Type and Transfer-Encoding headers
%  are written before the chunked stream is opened.
%
%  The raw output stream is set to UTF-8 encoding before opening the
%  chunked stream. This ensures the chunked stream inherits UTF-8
%  encoding, so multi-byte characters (e.g. ® = \xc2\xae) are properly
%  encoded. Without this, the stream defaults to octet encoding and
%  characters above 127 are written as single bytes, producing invalid
%  UTF-8 that HTTP servers will reject.
%
%  This predicate is intended for use with http:post_data_hook/3.
%  The typical pattern is:
%
%    http:post_data_hook(ndjson_push(Producer), Out, _) :-
%        stream_ndjson(Out, Producer).
stream_ndjson(Out, Producer) :-
    raw_out_stream(Out, RawOut),
    set_stream(RawOut, encoding(utf8)),
    format(RawOut, "Content-Type: application/x-ndjson\r\n", []),
    format(RawOut, "Transfer-Encoding: chunked\r\n", []),
    format(RawOut, "\r\n", []),
    flush_output(RawOut),
    setup_call_cleanup(
        http_chunked_open(RawOut, Chunked, []),
        call(Producer, Chunked),
        close(Chunked)).

:- begin_tests(stream_ndjson_utf8).

% Test: octet mode writes multi-byte chars as single bytes (invalid UTF-8)
test(octet_mode_produces_invalid_utf8) :-
    tmp_file_stream(binary, Tmp, Raw),
    http_chunked_open(Raw, Chunked, []),
    stream_property(Chunked, encoding(Enc)),
    assertion(Enc == octet),
    format(Chunked, '~s~n', ['hello \u00ae world']),
    flush_output(Chunked),
    close(Chunked),
    close(Raw),
    open(Tmp, read, In, [type(binary)]),
    read_string(In, _, Bytes),
    close(In),
    string_codes(Bytes, Codes),
    % In octet mode, ® (code 174) is written as single byte 174,
    % NOT as the 2-byte UTF-8 sequence [194,174]
    \+ append(_, [194,174|_], Codes).

% Test: UTF-8 mode produces valid UTF-8 for multi-byte chars
test(utf8_mode_produces_valid_utf8) :-
    tmp_file_stream(binary, Tmp, Raw),
    set_stream(Raw, encoding(utf8)),
    http_chunked_open(Raw, Chunked, []),
    stream_property(Chunked, encoding(Enc)),
    assertion(Enc == utf8),
    format(Chunked, '~s~n', ['hello \u00ae world']),
    flush_output(Chunked),
    close(Chunked),
    close(Raw),
    open(Tmp, read, In, [type(binary)]),
    read_string(In, _, Bytes),
    close(In),
    string_codes(Bytes, Codes),
    % In UTF-8 mode, ® is correctly encoded as [194,174]
    assertion(append(_, [194,174|_], Codes)).

% Test: UTF-8 mode keeps multi-byte chars intact for large strings (>4096 bytes)
test(utf8_mode_large_string) :-
    tmp_file_stream(binary, Tmp, Raw),
    set_stream(Raw, encoding(utf8)),
    http_chunked_open(Raw, Chunked, []),
    findall(120, between(1, 4093, _), Xs),
    append(Xs, [174,101,110,100], CharCodes),
    string_codes(Big, CharCodes),
    format(Chunked, '~s~n', [Big]),
    flush_output(Chunked),
    close(Chunked),
    close(Raw),
    open(Tmp, read, In, [type(binary)]),
    read_string(In, _, Bytes),
    close(In),
    string_codes(Bytes, Codes),
    assertion(append(_, [194,174|_], Codes)).

% Test: UTF-8 mode keeps multi-byte chars intact for very large strings (100K+)
test(utf8_mode_very_large_string) :-
    tmp_file_stream(binary, Tmp, Raw),
    set_stream(Raw, encoding(utf8)),
    http_chunked_open(Raw, Chunked, []),
    findall(120, between(1, 99990, _), Xs),
    append(Xs, [174,101,110,100], CharCodes),
    string_codes(Big, CharCodes),
    format(Chunked, '~s~n', [Big]),
    flush_output(Chunked),
    close(Chunked),
    close(Raw),
    open(Tmp, read, In, [type(binary)]),
    read_string(In, _, Bytes),
    close(In),
    string_codes(Bytes, Codes),
    assertion(append(_, [194,174|_], Codes)).

% Test: multiple lines with flush after each produce valid UTF-8
test(utf8_mode_multi_line_flush) :-
    tmp_file_stream(binary, Tmp, Raw),
    set_stream(Raw, encoding(utf8)),
    http_chunked_open(Raw, Chunked, []),
    forall(between(1, 100, _),
           (   format(Chunked, '~s~n', ['line \u00ae test']),
               flush_output(Chunked)
           )),
    close(Chunked),
    close(Raw),
    open(Tmp, read, In, [type(binary)]),
    read_string(In, _, Bytes),
    close(In),
    string_codes(Bytes, Codes),
    assertion(append(_, [194,174|_], Codes)).

:- end_tests(stream_ndjson_utf8).
