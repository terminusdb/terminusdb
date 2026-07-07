:- module(srv_http, [
                  build_swi_request/4,
                  build_swi_headers/2,
                  capture_http_output/3,
                  capture_http_output_raw/3,
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
:- use_module(library(readutil)).
:- use_module(library(pcre)).
:- use_module(library(plunit)).
:- use_module(core(util)).
:- use_module(core(util/test_utils)).

%% capture_http_output(+Request, :Goal, -Text) is det.
%%
%%  Run Goal with current_output redirected to a fresh CGI stream backed by a
%%  UTF-8 memory file. The handler is expected to write a CGI-style response
%%  (headers followed by a blank line and then the body). The captured text is
%%  read back as raw octet bytes and then decoded via utf8_bytes_to_string,
%%  which round-trips both Unicode text and binary payloads correctly:
%%  UTF-8 encoding writes bytes 128-255 as 2-byte sequences, and
%%  utf8_bytes_to_string reverses this to recover the original byte values.
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

%% capture_http_output_utf8(+Request, :Goal, -Text) is det.
%%
%%  Run Goal with current_output redirected to a fresh CGI stream backed by a
%%  temporary file whose encoding is UTF-8. This captures the full response text
%%  (headers and body) as a Prolog string, preserving Unicode characters that
%%  are written by foreign predicates which bypass the CGI stream encoding when
%%  the backing store is a binary/octet memory file.
%%
%%  This is intended for the streaming document path, which always produces
%%  text (JSON or NDJSON) and never binary payloads.
capture_http_output_utf8(Request, Goal, Text) :-
    tmp_file('cgi', File),
    setup_call_cleanup(
        open(File, write, OutStream, [encoding(utf8)]),
        setup_call_cleanup(
            cgi_open(OutStream, CGI, srv_http:cgi_capture_hook, [request(Request)]),
            (   set_stream(CGI, encoding(utf8)),
                set_stream(OutStream, encoding(utf8)),
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
        open(File, read, ReadStream, [encoding(utf8)]),
        read_string(ReadStream, _, Text),
        (   close(ReadStream),
            catch(delete_file(File), _, true)
        )
    ).

%% capture_http_output_raw(+Request, :Goal, -OctetBytes) is det.
%%
%%  Run Goal with current_output redirected to a fresh CGI stream backed by a
%%  UTF-8 memory file. The handler writes a CGI-style response (headers
%%  followed by a blank line and then the body). The captured output is read
%%  back as raw octet bytes with NO encoding conversion — the bytes are
%%  exactly what the CGI stream produced. These bytes can be sent directly
%%  via appserver_stream_send_raw/2 for 8-bit clean pass-through to the
%%  HTTP client.
%%
%%  UTF-8 encoding on the write side ensures that reply_json writes raw
%%  UTF-8 multibyte sequences (no \uXXXX escapes) for non-ASCII characters.
%%  Octet encoding on the read side preserves every byte value 0-255
%%  unchanged, including binary payloads like gzip data.
capture_http_output_raw(Request, Goal, OctetBytes) :-
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
        read_string(ReadStream, _, OctetBytes),
        close(ReadStream)
    ),
    free_memory_file(MemFile).

%% cgi_capture_hook(+Event, +CGI) is det.
%%
%%  CGI hook for the capture memory file. The header text collected by
%%  the CGI stream is written to the underlying memory file, with
%%  Content-Length inserted from the CGI stream's content_length property
%%  when the handler has not already set Transfer-Encoding: chunked or
%%  Content-Length. This matches the SWI-Prolog built-in HTTP server
%%  behavior, enabling HTTP keep-alive for regular JSON responses while
%%  preserving chunked transfer encoding for streaming responses (NDJSON,
%%  WOQL streaming).
cgi_capture_hook(send_header, CGI) :-
    cgi_property(CGI, header_codes(HeadCodes)),
    cgi_property(CGI, client(Out)),
    %% header_codes may return a list of codes or a string depending on
    %% the SWI-Prolog version. Normalize to a string.
    text_to_string(HeadCodes, HeadText),
    (   \+ header_has_field(HeadText, 'transfer-encoding'),
        \+ header_has_field(HeadText, 'content-length'),
        cgi_property(CGI, content_length(Len))
    ->  insert_content_length(HeadText, Len, FinalText),
        format(Out, '~s', [FinalText])
    ;   format(Out, '~s', [HeadText])
    ),
    !.
cgi_capture_hook(_, _).

%% header_has_field(+HeadText, +FieldName) is semidet.
%%
%%  True if HeadText contains a header line whose name matches FieldName
%%  (case-insensitive). FieldName is a lowercase atom. HeadText is a string.
header_has_field(HeadText, FieldName) :-
    string_lower(HeadText, Lower),
    atom_string(FieldName, FieldStr),
    string_concat(FieldStr, ":", Prefix),
    sub_string(Lower, _, _, _, Prefix),
    % cut for semidet: we only need existence, not all match positions.
    !.

%% insert_content_length(+HeadText, +Len, -FinalText) is det.
%%
%%  Insert a Content-Length header line before the blank line that
%%  separates headers from body. HeadText is a string ending with \n\n
%%  (or \r\n\r\n). The \n\n separator is: one \n terminates the last
%%  header line, and the second \n is the blank line. So Before does
%%  NOT include the last header's line terminator — we must add it
%%  back before inserting Content-Length.
insert_content_length(HeadText, Len, FinalText) :-
    (   string_concat(Before, "\r\n\r\n", HeadText)
    ->  !, format(string(FinalText), '~s\r\nContent-Length: ~w\r\n\r\n', [Before, Len])
    ;   string_concat(Before, "\n\n", HeadText)
    ->  !, format(string(FinalText), '~s\nContent-Length: ~w\n\n', [Before, Len])
    ;   format(string(FinalText), '~sContent-Length: ~w\n\n', [HeadText, Len])
    ).

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
    dict_pairs(HeadersDict, _, Pairs0),
    exclude(is_skip_header, Pairs0, Pairs),
    maplist(build_swi_header, Pairs, HeaderTerms).

%% is_skip_header(+Pair) is semidet.
%%
%%  True for headers that are already provided separately in the SWI
%%  request list and must not be duplicated as header terms. The body
%%  length is added as content_length(BodyLen) by build_swi_request/4
%%  and build_swi_request_from_dict/4, so a Content-Length header would
%%  create a duplicate key that causes dict_create to fail.
%%  Pair is a Key-Value pair from dict_pairs/3.
is_skip_header(Key-_) :-
    atom_string(KeyAtom, Key),
    downcase_atom(KeyAtom, KeyLower),
    KeyLower == 'content-length'.

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
        ;   Term = accept(ValueAtom))
    ;   KeyLower == 'content-length'
    ->  Term = content_length(ValueAtom)
    ;   KeyLower == 'content-encoding'
    ->  Term = content_encoding(ValueAtom)
    ;   KeyLower == 'accept-encoding'
    ->  Term = accept_encoding(ValueAtom)
    ;   KeyLower == 'accept-language'
    ->  Term = accept_language(ValueAtom)
    ;   KeyLower == 'x-forwarded-for'
    ->  Term = x_forwarded_for(ValueAtom)
    ;   KeyLower == 'x-forwarded-proto'
    ->  Term = x_forwarded_proto(ValueAtom)
    ;   KeyLower == 'x-forwarded-host'
    ->  Term = x_forwarded_host(ValueAtom)
    ;   Term = header(KeyAtom, ValueAtom)
    ).

%% parse_http_response(+Text, -ResponseDict) is det.
%%
%%  Parse the SWI-Prolog HTTP response text produced by a CGI-style handler into
%%  a response dict with status, body, and headers keys. The input Text is a
%%  Prolog string produced by capture_http_output's utf8_bytes_to_string, so
%%  Unicode characters are proper code points and binary bytes are code points
%%  0-255. For binary content types the body is converted back to raw octet
%%  bytes so it can be sent via appserver_stream_send_raw without corruption.
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
    (   is_binary_content_type(Headers)
    ->  string_to_utf8_bytes(Body, OctetBody, _),
        Response = _{status: Status, body: OctetBody, headers: Headers}
    ;   Response = _{status: Status, body: Body, headers: Headers}
    ).

%% is_binary_content_type(+Headers) is semidet.
is_binary_content_type(Headers) :-
    (   get_dict('Content-Type', Headers, ContentType)
    ;   get_dict('Content-type', Headers, ContentType)
    ;   get_dict('content-type', Headers, ContentType)
    ),
    downcase_atom(ContentType, Lower),
    (   sub_atom(Lower, _, _, _, 'application/octets')
    ;   sub_atom(Lower, _, _, _, 'application/octet-stream')
    ;   sub_atom(Lower, _, _, _, 'application/x-gzip')
    ).

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
        Pairs = [KeyAtom-HeaderDict.value | Rest],
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

%% --- Segment 5: Stream path CGI capture encoding ---

test(capture_http_output_utf8_simple_ascii) :-
    SimpleGoal = (format(current_output, 'Status: 200 OK~n', []),
                  format(current_output, 'Content-Type: application/json~n~n', []),
                  format(current_output, '{"ok":true}', [])),
    capture_http_output([], SimpleGoal, Text),
    assertion(sub_string(Text, _, _, _, 'Status: 200 OK')),
    assertion(sub_string(Text, _, _, _, '{"ok":true}')).

test(capture_http_output_utf8_multibyte_body) :-
    %% Handler writes a body containing the character \u00f6 (o with diaeresis)
    Utf8Goal = (format(current_output, 'Status: 200 OK~n', []),
                format(current_output, 'Content-Type: application/json~n~n', []),
                format(current_output, '"Kurt G\u00f6del"', [])),
    capture_http_output([], Utf8Goal, Text),
    parse_http_response(Text, Response),
    get_dict(body, Response, Body),
    assertion(string(Body)),
    assertion(Body == "\"Kurt Gödel\"").

test(capture_http_output_utf8_japanese_body) :-
    JapaneseGoal = (format(current_output, 'Status: 200 OK~n', []),
                    format(current_output, 'Content-Type: application/json~n~n', []),
                    format(current_output, '"\u65e5\u672c\u8a9e"', [])),
    capture_http_output([], JapaneseGoal, Text),
    parse_http_response(Text, Response),
    get_dict(body, Response, Body),
    assertion(Body == "\"日本語\"").

test(capture_http_output_preserves_utf8_in_text) :-
    %% Directly verify that capture_http_output preserves the \u00f6 character
    %% in the captured Text string (before parse_http_response).
    Utf8Goal = (format(current_output, 'Status: 200 OK~n', []),
                format(current_output, 'Content-Type: application/json~n~n', []),
                format(current_output, 'G\u00f6del', [])),
    capture_http_output([], Utf8Goal, Text),
    %% The Text should contain the character \u00f6, not \ufffd
    assertion(sub_string(Text, _, _, _, "Gödel")),
    assertion(\+ sub_string(Text, _, _, _, "\ufffd")).

test(utf8_bytes_to_string_roundtrip_multibyte) :-
    %% Verify the utf8_bytes_to_string conversion itself works
    Original = "Gödel",
    string_to_utf8_bytes(Original, Bytes, _),
    utf8_bytes_to_string(Bytes, Result),
    assertion(Result == Original).

test(utf8_bytes_to_string_roundtrip_japanese) :-
    Original = "日本語",
    string_to_utf8_bytes(Original, Bytes, _),
    utf8_bytes_to_string(Bytes, Result),
    assertion(Result == Original).

test(parse_http_response_utf8_body) :-
    %% parse_http_response should preserve UTF-8 characters in the body
    Text = "Status: 200 OK\nContent-Type: application/json\n\n\"Kurt Gödel\"",
    parse_http_response(Text, Response),
    get_dict(body, Response, Body),
    assertion(Body == "\"Kurt Gödel\"").

test(parse_http_response_japanese_body) :-
    Text = "Status: 200 OK\nContent-Type: application/json\n\n\"日本語\"",
    parse_http_response(Text, Response),
    get_dict(body, Response, Body),
    assertion(Body == "\"日本語\"").

test(capture_http_output_reply_json_utf8) :-
    %% Test with reply_json which is what the actual document API uses.
    %% reply_json writes JSON with UTF-8 characters directly (not \uXXXX escapes)
    %% when the stream encoding is utf8.
    JsonGoal = (reply_json(_{name: "Kurt Gödel"}, [status(200)])),
    capture_http_output([], JsonGoal, Text),
    parse_http_response(Text, Response),
    get_dict(body, Response, Body),
    assertion(string(Body)),
    assertion(sub_string(Body, _, _, _, "Gödel")),
    assertion(\+ sub_string(Body, _, _, _, "\ufffd")).

test(capture_http_output_reply_json_japanese) :-
    JsonGoal = (reply_json(_{name: "日本語"}, [status(200)])),
    capture_http_output([], JsonGoal, Text),
    parse_http_response(Text, Response),
    get_dict(body, Response, Body),
    assertion(string(Body)),
    assertion(sub_string(Body, _, _, _, "日本語")),
    assertion(\+ sub_string(Body, _, _, _, "\ufffd")).

test(send_response_json_serialization_preserves_utf8) :-
    %% Test the JSON serialization step that send_response uses.
    %% The response dict has a body string containing ö (U+00F6).
    %% json_write_dict with as(string) should preserve the character.
    Response = _{status: 200, body: "{\"name\":\"Kurt Gödel\"}", headers: _{'Content-Type': 'application/json'}},
    with_output_to(string(JsonString), json_write_dict(current_output, Response, [as(string)])),
    assertion(string(JsonString)),
    assertion(sub_string(JsonString, _, _, _, "Gödel")),
    assertion(\+ sub_string(JsonString, _, _, _, "\ufffd")),
    %% Also check that ö is not escaped as \u00f6 in the JSON
    assertion(\+ sub_string(JsonString, _, _, _, "\\u00f6")).

test(send_response_json_serialization_japanese) :-
    Response = _{status: 200, body: "{\"name\":\"日本語\"}", headers: _{'Content-Type': 'application/json'}},
    with_output_to(string(JsonString), json_write_dict(current_output, Response, [as(string)])),
    assertion(string(JsonString)),
    assertion(sub_string(JsonString, _, _, _, "日本語")),
    assertion(\+ sub_string(JsonString, _, _, _, "\ufffd")).

test(json_string_byte_representation) :-
    %% Check the actual bytes in the JsonString to understand what
    %% appserver_stream_send will see.
    Response = _{status: 200, body: "{\"name\":\"Kurt Gödel\"}", headers: _{'Content-Type': 'application/json'}},
    with_output_to(string(JsonString), json_write_dict(current_output, Response, [as(string)])),
    %% Convert to UTF-8 bytes and check that ö is encoded as 0xC3 0xB6
    string_to_utf8_bytes(JsonString, Bytes, _),
    %% Find the ö byte sequence (0xC3 0xB6) in the byte list
    assertion(memberchk(0xC3, Bytes)),
    assertion(memberchk(0xB6, Bytes)),
    %% Also check via string_codes what Prolog sees
    string_codes(JsonString, Codes),
    assertion(memberchk(246, Codes)).

%% --- Content-Length header handling in cgi_capture_hook ---

test(header_has_field_case_insensitive) :-
    header_has_field("Status: 200\nContent-Type: application/json\n\n", 'content-type'),
    header_has_field("Status: 200\nCONTENT-TYPE: application/json\n\n", 'content-type'),
    \+ header_has_field("Status: 200\nContent-Type: application/json\n\n", 'transfer-encoding').

test(header_has_field_transfer_encoding) :-
    header_has_field("Status: 200\nTransfer-Encoding: chunked\n\n", 'transfer-encoding'),
    \+ header_has_field("Status: 200\nContent-Type: application/json\n\n", 'transfer-encoding').

test(insert_content_length_before_blank_line) :-
    HeadText = "Status: 200\nContent-Type: application/json\n\n",
    insert_content_length(HeadText, 42, FinalText),
    % Make semidet: existence check only.
    sub_string(FinalText, _, _, _, "Content-Length: 42\n\n"),
    !,
    %% Content-Length must be on its own line, not concatenated to Content-Type
    \+ sub_string(FinalText, _, _, _, "application/jsonContent-Length"),
    %% The original headers must still be present
    sub_string(FinalText, _, _, _, "Content-Type: application/json\n"),
    !.

test(insert_content_length_with_crlf_line_endings) :-
    HeadText = "Status: 200\r\nContent-Type: application/json\r\n\r\n",
    insert_content_length(HeadText, 42, FinalText),
    sub_string(FinalText, _, _, _, "Content-Length: 42\r\n\r\n"),
    \+ sub_string(FinalText, _, _, _, "application/jsonContent-Length").

test(cgi_capture_hook_inserts_content_length_for_reply_json) :-
    %% reply_json produces a CGI stream with content_length set.
    %% The hook should insert Content-Length into the header output.
    JsonGoal = (reply_json(_{ok: true}, [status(200)])),
    capture_http_output([], JsonGoal, Text),
    parse_http_response(Text, Response),
    get_dict(headers, Response, Headers),
    get_dict('Content-Length', Headers, _),
    get_dict(body, Response, Body),
    assertion(string(Body)).

test(cgi_capture_hook_inserts_content_length_zero_for_empty_body) :-
    %% A handler that writes only headers (no body) should get
    %% Content-Length: 0 inserted by the hook.
    EmptyGoal = (format(current_output, 'Status: 200 OK~n', []),
                 format(current_output, 'Content-Type: application/octet-stream~n~n', [])),
    capture_http_output([], EmptyGoal, Text),
    parse_http_response(Text, Response),
    get_dict(headers, Response, Headers),
    get_dict('Content-Length', Headers, '0'),
    get_dict(body, Response, Body),
    assertion(Body == "").

test(cgi_capture_hook_no_content_length_when_transfer_encoding_present) :-
    %% When the handler sets Transfer-Encoding: chunked, the hook should
    %% NOT insert Content-Length (chunked and Content-Length are mutually
    %% exclusive per HTTP spec).
    ChunkedGoal = (format(current_output, 'Status: 200 OK~n', []),
                   format(current_output, 'Transfer-Encoding: chunked~n~n', []),
                   format(current_output, '4\r\nWiki\r\n0\r\n\r\n', [])),
    capture_http_output([], ChunkedGoal, Text),
    %% The Transfer-Encoding header should be present in the raw output.
    % Make semidet: existence check only.
    sub_string(Text, _, _, _, 'Transfer-Encoding: chunked'),
    !,
    %% Content-Length should NOT be inserted
    \+ sub_string(Text, _, _, _, 'Content-Length').

test(is_skip_header_content_length) :-
    is_skip_header('Content-Length'-"42"),
    is_skip_header('content-length'-"42"),
    \+ is_skip_header('Content-Type'-"application/json").

test(build_swi_headers_skips_content_length) :-
    %% Content-Length is provided separately by build_swi_request as
    %% content_length(BodyLen), so it must be excluded from the header
    %% terms to avoid a duplicate key in dict_create.
    Dict = _{'Content-Type': "application/json", 'Content-Length': "42"},
    build_swi_headers(Dict, Headers),
    \+ memberchk(content_length('42'), Headers),
    memberchk(content_type('application/json'), Headers).

:- end_tests(srv_http).
