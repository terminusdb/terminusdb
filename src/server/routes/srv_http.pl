:- module(srv_http, [
                  build_swi_request/4,
                  capture_http_output/3,
                  parse_http_response/2,
                  cgi_capture_hook/2
              ]).

:- use_module(library(uri)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_stream)).
:- use_module(library(base64)).
:- use_module(library(memfile)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(core(util)).
:- use_module(core(util/test_utils)).

%% capture_http_output(+Request, :Goal, -Text) is det.
%%
%%  Run Goal with current_output redirected to a fresh CGI stream backed by a
%%  UTF-8 memory file. The handler is expected to write a CGI-style response
%%  (headers followed by a blank line and then the body). The captured text is
%%  read back as raw UTF-8 bytes so Unicode characters are preserved.
capture_http_output(Request, Goal, Text) :-
    new_memory_file(MemFile),
    setup_call_cleanup(
        open_memory_file(MemFile, write, OutStream, [encoding(utf8)]),
        setup_call_cleanup(
            cgi_open(OutStream, CGI, srv_http:cgi_capture_hook, [request(Request)]),
            (   set_stream(CGI, encoding(utf8)),
                with_output_to(
                    CGI,
                    call(Goal)
                )
            ),
            close(CGI)
        ),
        close(OutStream)
    ),
    setup_call_cleanup(
        open_memory_file(MemFile, read, ReadStream, [type(binary), encoding(octet)]),
        read_string(ReadStream, _, ByteString),
        close(ReadStream)
    ),
    utf8_bytes_to_string(ByteString, Text).

%% cgi_capture_hook(+Event, +CGI) is det.
%%
%%  Minimal CGI hook for the capture memory file. The header text collected by
%%  the CGI stream is written to the underlying memory file followed by a blank
%%  line; the body is written by the CGI stream itself.
cgi_capture_hook(send_header, CGI) :-
    cgi_property(CGI, header_codes(HeadText)),
    cgi_property(CGI, client(Out)),
    format(Out, '~s', [HeadText]),
    !.
cgi_capture_hook(_, _).

%% build_swi_request(+RequestDict, -SWIRequest, -BodyStream, -MemoryFile) is det.
%%
%%  Convert the JSON request dict produced by the Rust dispatcher into a
%%  SWI-Prolog HTTP request list. Only the fields that the API handlers use are
%%  populated. The body is stored in a binary memory file so the HTTP library can
%%  read the exact UTF-8 byte sequence.
build_swi_request(Request, SWIRequest, BodyStream, MemoryFile) :-
    get_dict(method, Request, MethodString),
    string_lower(MethodString, MethodStr),
    atom_string(Method, MethodStr),
    get_dict(path, Request, PathString),
    atom_string(Path, PathString),
    (   get_dict(query, Request, QueryString),
        QueryString \= ""
    ->  uri_query_components(QueryString, Query)
    ;   Query = []
    ),
    get_dict(headers, Request, HeadersDict),
    build_swi_headers(HeadersDict, HeaderTerms),
    get_dict(body, Request, Body),
    string_to_utf8_bytes(Body, BodyByteString, BodyLen),
    new_memory_file(MemoryFile),
    open_memory_file(MemoryFile, write, WriteStream, [type(binary), encoding(octet)]),
    format(WriteStream, "~s", [BodyByteString]),
    close(WriteStream),
    open_memory_file(MemoryFile, read, BodyStream, [type(binary), encoding(octet)]),
    SWIRequest = [
        method(Method),
        path(Path),
        request_uri(Path),
        search(Query),
        content_length(BodyLen),
        input(BodyStream),
        peer(ip(127,0,0,1)),
        http_version(1-1)
        | HeaderTerms
    ].

string_to_utf8_bytes(String, ByteString, Length) :-
    new_memory_file(MF),
    open_memory_file(MF, write, W, [encoding(utf8)]),
    format(W, '~s', [String]),
    close(W),
    open_memory_file(MF, read, R, [type(binary), encoding(octet)]),
    read_string(R, _, ByteString),
    close(R),
    free_memory_file(MF),
    string_length(ByteString, Length).

utf8_byte_length(String, Length) :-
    string_to_utf8_bytes(String, _, Length).

utf8_bytes_to_string(ByteString, String) :-
    string_codes(ByteString, Bytes),
    new_memory_file(MF),
    open_memory_file(MF, write, W, [type(binary), encoding(octet)]),
    write_bytes(W, Bytes),
    close(W),
    open_memory_file(MF, read, R, [encoding(utf8)]),
    read_string(R, _, String),
    close(R),
    free_memory_file(MF).

write_bytes(_, []).
write_bytes(Stream, [Byte | Rest]) :-
    put_byte(Stream, Byte),
    write_bytes(Stream, Rest).

build_swi_headers(HeadersDict, HeaderTerms) :-
    dict_pairs(HeadersDict, _, Pairs),
    maplist(build_swi_header, Pairs, HeaderTerms).

build_swi_header(Key-Value, Term) :-
    atom_string(KeyAtom, Key),
    downcase_atom(KeyAtom, KeyLower),
    atom_string(ValueAtom, Value),
    (   KeyLower == authorization
    ->  Term = authorization(ValueAtom)
    ;   KeyLower == 'content-type'
    ->  Term = content_type(ValueAtom)
    ;   KeyLower == origin
    ->  Term = origin(ValueAtom)
    ;   KeyLower == 'x-http-method-override'
    ->  Term = x_http_method_override(ValueAtom)
    ;   KeyLower == 'x-terminusdb-api-base'
    ->  Term = x_terminusdb_api_base(ValueAtom)
    ;   KeyLower == 'x-terminusdb-api-user'
    ->  Term = x_terminusdb_api_user(ValueAtom)
    ;   KeyLower == 'terminusdb-data-version'
    ->  Term = terminusdb_data_version(ValueAtom)
    ;   KeyLower == 'x-terminusdb-data-version'
    ->  Term = terminusdb_data_version(ValueAtom)
    ;   KeyLower == 'x-operation-id'
    ->  Term = x_operation_id(ValueAtom)
    ;   KeyLower == 'x-request-id'
    ->  Term = x_request_id(ValueAtom)
    ;   KeyLower == 'traceparent'
    ->  Term = traceparent(ValueAtom)
    ;   KeyLower == accept
    ->  (   catch(http_parse_header_value(accept, ValueAtom, Parsed), _, fail)
        ->  Term = accept(Parsed)
        ;   Term = header(accept, ValueAtom))
    ;   Term = header(KeyAtom, ValueAtom)
    ).

%% parse_http_response(+Text, -ResponseDict) is det.
%%
%%  Parse the SWI-Prolog HTTP response text produced by a CGI-style handler into
%%  a response dict with status, body, and headers keys.
parse_http_response(Text, Response) :-
    open_string(Text, Stream),
    read_header_lines(Stream, 200, Status, HeaderPairs),
    read_string(Stream, _, Body0),
    (   Body0 == end_of_file
    ->  Body = ""
    ;   Body = Body0
    ),
    strip_hop_headers(HeaderPairs, CleanPairs),
    dict_create(Headers, _, CleanPairs),
    Response = _{
        status: Status,
        body: Body,
        headers: Headers
    }.

read_header_lines(Stream, DefaultStatus, Status, Pairs) :-
    read_line_to_string(Stream, Line),
    (   Line == ""
    ->  Status = DefaultStatus,
        Pairs = []
    ;   Line == end_of_file
    ->  Status = DefaultStatus,
        Pairs = []
    ;   re_matchsub("^Status:\\s*(?<code>\\d+)", Line, StatusDict, [])
    ->  atom_string(StatusCode, StatusDict.code),
        atom_number(StatusCode, Status),
        read_header_lines(Stream, DefaultStatus, _, Pairs)
    ;   re_matchsub("^(?<key>[^:]+):\\s*(?<value>.*)$", Line, HeaderDict, [])
    ->  atom_string(KeyAtom, HeaderDict.key),
        atom_string(ValueAtom, HeaderDict.value),
        Pairs = [KeyAtom-ValueAtom | Rest],
        read_header_lines(Stream, DefaultStatus, Status, Rest)
    ;   read_header_lines(Stream, DefaultStatus, Status, Pairs)
    ).

strip_hop_headers([], []).
strip_hop_headers([Key-Value|Rest], Clean) :-
    downcase_atom(Key, KeyLower),
    (   is_hop_header(KeyLower)
    ->  strip_hop_headers(Rest, Clean)
    ;   Clean = [Key-Value|RestClean],
        strip_hop_headers(Rest, RestClean)
    ).

is_hop_header('transfer-encoding').
is_hop_header(connection).
is_hop_header('keep-alive').
is_hop_header('proxy-authenticate').
is_hop_header('proxy-authorization').
is_hop_header(te).
is_hop_header(trailer).

:- begin_tests(srv_http, [concurrent(false)]).

test(build_swi_headers) :-
    Dict = _{accept: "application/json", 'content-type': "application/json"},
    build_swi_headers(Dict, Headers),
    memberchk(content_type('application/json'), Headers),
    memberchk(accept([media(application/json, [], 1.0, [])]), Headers).

test(build_swi_headers_traceparent) :-
    Dict = _{traceparent: "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"},
    build_swi_headers(Dict, Headers),
    memberchk(traceparent('00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'), Headers).

test(build_swi_headers_accept) :-
    Dict = _{accept: "text/turtle"},
    build_swi_headers(Dict, Headers),
    memberchk(accept([media(text/turtle, [], 1.0, [])]), Headers).

test(strip_hop_headers) :-
    strip_hop_headers(['Content-Type'-"application/json", 'Transfer-Encoding'-chunked], Clean),
    Clean = ['Content-Type'-"application/json"].

test(parse_http_response) :-
    Text = "Status: 200 OK\nContent-Type: application/json\nTransfer-Encoding: chunked\n\n{\"ok\":true}",
    parse_http_response(Text, Response),
    Response = _{status: 200, body: "{\"ok\":true}", headers: Headers},
    \+ get_dict('Transfer-Encoding', Headers, _),
    get_dict('Content-Type', Headers, 'application/json').

:- end_tests(srv_http).
