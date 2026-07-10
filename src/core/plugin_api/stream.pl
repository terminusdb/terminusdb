:- module(plugin_api_stream, [
    raw_out_stream/2,
    stream_ndjson/2
]).

:- use_module(library(http/http_stream)).

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
%  This predicate is intended for use with http:post_data_hook/3.
%  The typical pattern is:
%
%    http:post_data_hook(ndjson_push(Producer), Out, _) :-
%        stream_ndjson(Out, Producer).
stream_ndjson(Out, Producer) :-
    raw_out_stream(Out, RawOut),
    format(RawOut, "Content-Type: application/x-ndjson\r\n", []),
    format(RawOut, "Transfer-Encoding: chunked\r\n", []),
    format(RawOut, "\r\n", []),
    flush_output(RawOut),
    setup_call_cleanup(
        http_chunked_open(RawOut, Chunked, []),
        call(Producer, Chunked),
        close(Chunked)).
