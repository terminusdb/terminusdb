:- module(json_preserve, [
    json_read_dict/3,
    json_read_dict_stream/3,
    sys_json_parse_value/2
]).

:- use_module(library(option)).

/** <module> Fast JSON Parser using Rust
 *
 * Drop-in replacement for library(http/json) using serde_json via Rust FFI.
 * Provides significant performance improvements AND preserves numeric precision.
 *
 * CRITICAL: Must use Rust parser to preserve arbitrary precision decimals.
 * Standard library converts numbers to floats, losing precision.
 *
 * Usage:
 *   :- use_module(core(util/json_preserve)).
 *   json_read_dict(Stream, Dict, Options).
 */

%! json_read_dict(+Stream, -Dict, +Options) is det.
%
% Parse JSON from Stream into Dict using fast Rust parser.
% Preserves arbitrary precision for numeric values.
%
% @param Stream Input stream or atom/string containing JSON
% @param Dict Parsed JSON as Prolog dict
% @param Options Supported: end_of_file(Value), default_tag(Tag)
json_read_dict(Stream, Dict, Options) :-
    (   is_stream(Stream)
    ->  % Use Rust streaming parser if we know the stream length
        json_read_dict_stream(Stream, Dict, Options)
    ;   % For atoms/strings, use fast Rust parser
        (   atom(Stream)
        ->  atom_string(Stream, JSONText),
            '$json_preserve':json_read_string(JSONText, Dict)
        ;   string(Stream)
        ->  '$json_preserve':json_read_string(Stream, Dict)
        ;   throw(error(type_error(stream_or_atom, Stream), _))
        )
    ).

%! json_read_dict_stream(+Stream, -Dict, +Options) is det.
%
% Read JSON from a stream using Rust-backed parser for precision and UTF-8 safety.
% Uses json_read_one_from_stream for true one-at-a-time streaming.
%
% IMPORTANT: All HTTP request bodies are buffered into string streams (see routes.pl)
% because transaction replay (with_transaction) requires seekable streams to reset
% position on retry. This means:
% - HTTP body is already in memory (buffered once)
% - Stream is seekable (supports set_stream_position)
% - Rust parser reads one JSON value at a time from the buffered string
% - Stream advances after each read for lazy document processing
%
% Future: When true streaming without transaction replay is available,
% the buffering in routes.pl could be eliminated.
%
% @param Stream Input stream (must be seekable for transaction replay)
% @param Dict Parsed JSON value or 'eof'
% @param Options end_of_file(Value) - value to return on EOF, default_tag(Tag)
json_read_dict_stream(Stream, Dict, Options) :-
    % Use Rust parser for one-at-a-time streaming
    % All streams are now string streams (buffered from HTTP)
    (   catch('$json_preserve':json_read_one_from_stream(Stream, Value), _Error, fail)
    ->  Dict = Value
    ;   % Rust parser returned nothing or failed - must be EOF
        handle_eof(Options, Dict)
    ).

%! handle_eof(+Options, -Dict) is det.
%
% Handle end-of-file based on options.
handle_eof(Options, Dict) :-
    (   memberchk(end_of_file(EOF), Options)
    ->  Dict = EOF
    ;   fail  % No eof option - fail on empty stream
    ).

%! sys_json_parse_value(+JSONString, -Value) is det.
%
% Parse a JSON value string using Rust parser with rational support.
% This is specifically for sys:JSON fields that need arbitrary precision.
%
% Numbers with decimal points become Prolog rationals for precision.
% Integers remain as integers.
%
% Example:
%   sys_json_parse_value('123.456', X)  => X = 15432r125 (rational)
%   sys_json_parse_value('42', X)        => X = 42 (integer)
%
sys_json_parse_value(JSONString, Value) :-
    atom_string(JSONString, JSONText),
    '$json_preserve':json_read_string(JSONText, Value).

% ============================================================================
% Unit Tests - Verify compatibility with library(http/json)
% ============================================================================

:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(plunit)).

:- begin_tests(json_preserve).

test(basic_object) :-
    JSON = '{"name": "Alice", "age": 30}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust).

test(integer_values) :-
    JSON = '{"a": 42, "b": 0, "c": -100}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(a, DictRust, A),
    assertion(integer(A)),
    assertion(A == 42).

test(float_values) :-
    JSON = '{"x": 3.14, "y": -2.5, "z": 0.0}',
    json_read_dict(JSON, DictRust, []),
    % Rust parser converts decimals to rationals for precision
    get_dict(x, DictRust, X),
    get_dict(y, DictRust, Y),
    get_dict(z, DictRust, Z),
    % Decimals become rationals
    assertion(rational(X)),
    assertion(rational(Y)),
    % 0.0 becomes integer 0
    assertion(integer(Z)),
    assertion(Z == 0).

test(large_integer) :-
    JSON = '{"big": 9007199254740991}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(big, DictRust, Big),
    assertion(integer(Big)).

test(string_values) :-
    JSON = '{"text": "hello", "empty": ""}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(text, DictRust, Text),
    assertion(string(Text)).

test(boolean_and_null) :-
    JSON = '{"active": true, "deleted": false, "data": null}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust).

test(nested_object) :-
    JSON = '{"user": {"name": "Bob", "age": 25}}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(user, DictRust, User),
    assertion(is_dict(User)).

test(array_values) :-
    JSON = '{"numbers": [1, 2, 3], "mixed": [1, "two", true, null]}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(numbers, DictRust, Numbers),
    assertion(is_list(Numbers)).

test(empty_structures) :-
    JSON = '{"empty_obj": {}, "empty_arr": []}',
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust).

test(scientific_notation) :-
    JSON = '{"sci": 1.23e10, "neg": 1.5e-5}',
    json_read_dict(JSON, DictRust, []),
    get_dict(sci, DictRust, Sci),
    get_dict(neg, DictRust, Neg),
    % Scientific notation produces rationals after expansion
    assertion((rational(Sci) ; float(Sci))),
    assertion((rational(Neg) ; float(Neg))).

test(dict_tag) :-
    JSON = '{"test": 1}',
    json_read_dict(JSON, Dict, []),
    dict_pairs(Dict, Tag, _),
    assertion(Tag == json).

test(eof_handling) :-
    % Test that reading from exhausted stream returns eof
    open_string('{"test": 1}', Stream),
    json_read_dict(Stream, Dict1, [end_of_file(eof)]),
    assertion(Dict1.test == 1),
    json_read_dict(Stream, Dict2, [end_of_file(eof)]),
    assertion(Dict2 == eof).

test(nested_subdocuments) :-
    % Test nested subdocuments like in document-get integration test
    JSON = '{"@id": "Group/0", "@type": "Group", "people": [{"@type": "Person", "name": "Alice", "age": "30"}, {"@type": "Person", "name": "Bob", "age": "25"}]}',
    json_read_dict(JSON, DictRust, []),
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    % Verify structure matches
    assertion(DictRust.'@id' == DictStd.'@id'),
    assertion(DictRust.'@type' == DictStd.'@type'),
    % Verify nested people array
    get_dict(people, DictRust, PeopleRust),
    get_dict(people, DictStd, PeopleStd),
    assertion(is_list(PeopleRust)),
    assertion(is_list(PeopleStd)),
    assertion(length(PeopleRust, 2)),
    assertion(length(PeopleStd, 2)),
    % Verify first person
    nth0(0, PeopleRust, Person1Rust),
    nth0(0, PeopleStd, Person1Std),
    assertion(Person1Rust.'@type' == Person1Std.'@type'),
    assertion(Person1Rust.name == Person1Std.name),
    assertion(Person1Rust.age == Person1Std.age).

test(array_of_documents_with_subdocuments) :-
    % Test array of documents each containing nested subdocuments
    JSON = '[{"@id": "Group/0", "@type": "Group", "people": [{"@type": "Person", "name": "Alice"}]}, {"@id": "Group/1", "@type": "Group", "people": []}]',
    json_read_dict(JSON, DictRust, []),
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    % Verify both are lists
    assertion(is_list(DictRust)),
    assertion(is_list(DictStd)),
    assertion(length(DictRust, 2)),
    assertion(length(DictStd, 2)),
    % Verify first group with subdocuments
    nth0(0, DictRust, Group0Rust),
    nth0(0, DictStd, Group0Std),
    get_dict(people, Group0Rust, People0Rust),
    get_dict(people, Group0Std, People0Std),
    assertion(length(People0Rust, 1)),
    assertion(length(People0Std, 1)),
    % Verify second group with empty array
    nth0(1, DictRust, Group1Rust),
    nth0(1, DictStd, Group1Std),
    get_dict(people, Group1Rust, People1Rust),
    get_dict(people, Group1Std, People1Std),
    assertion(length(People1Rust, 0)),
    assertion(length(People1Std, 0)).

test(deeply_nested_structures) :-
    % Test multiple levels of nesting
    JSON = '{"level1": {"level2": {"level3": {"data": "deep"}}}}',
    json_read_dict(JSON, DictRust, []),
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    % Navigate through nested structure
    get_dict(level1, DictRust, L1Rust),
    get_dict(level1, DictStd, L1Std),
    get_dict(level2, L1Rust, L2Rust),
    get_dict(level2, L1Std, L2Std),
    get_dict(level3, L2Rust, L3Rust),
    get_dict(level3, L2Std, L3Std),
    assertion(L3Rust.data == L3Std.data).

test(mixed_types_in_nested_arrays) :-
    % Test arrays containing mixed types (common in subdocuments)
    JSON = '{"items": [{"type": "A", "value": 1}, {"type": "B", "value": 2.5}, {"type": "C", "value": "text"}]}',
    json_read_dict(JSON, DictRust, []),
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    get_dict(items, DictRust, ItemsRust),
    get_dict(items, DictStd, ItemsStd),
    assertion(length(ItemsRust, 3)),
    assertion(length(ItemsStd, 3)),
    % Verify each item
    nth0(0, ItemsRust, Item0Rust),
    nth0(0, ItemsStd, Item0Std),
    assertion(Item0Rust.type == Item0Std.type),
    assertion(Item0Rust.value == Item0Std.value),
    assertion(integer(Item0Rust.value)),
    nth0(1, ItemsRust, Item1Rust),
    nth0(1, ItemsStd, Item1Std),
    % Rust converts 2.5 to rational 5r2
    assertion(rational(Item1Rust.value)),
    assertion(float(Item1Std.value)),
    % Verify they're numerically equal
    assertion(Item1Rust.value =:= Item1Std.value),
    nth0(2, ItemsRust, Item2Rust),
    nth0(2, ItemsStd, Item2Std),
    assertion(Item2Rust.value == Item2Std.value),
    assertion(string(Item2Rust.value)).

test(exact_failing_integration_test_json) :-
    % Test EXACT JSON from failing document-get integration test
    JSON = '[{"@id":"Group/0","@type":"Group","people":[{"@type":"Person","name":"Immanuel Kant","age":"79","order":"3"}]}]',
    json_read_dict(JSON, DictRust, []),
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    % These should be IDENTICAL for document insertion to work
    assertion(DictRust == DictStd).

:- end_tests(json_preserve).

% ============================================================================
% Integration Tests - Verify all input types work with Rust parser
% ============================================================================

:- begin_tests(json_integration).

% Test reading from atom
test(atom_input) :-
    json_read_dict('{"name":"Alice","age":30}', Dict, []),
    Dict.name == "Alice",
    Dict.age == 30.

% Test reading from string
test(string_input) :-
    String = "{\"city\":\"Amsterdam\",\"population\":1000000}",
    json_read_dict(String, Dict, []),
    Dict.city == "Amsterdam",
    Dict.population == 1000000.

% Test reading from string stream (single value)
test(stream_single_value) :-
    open_string('[1,2,3]', Stream),
    json_read_dict(Stream, Dict, []),
    Dict = [1,2,3],
    close(Stream).

% Test reading from string stream (multiple values, one at a time - TRUE STREAMING)
test(stream_multiple_values_one_at_a_time) :-
    open_string('{"a":1} {"b":2}', Stream),
    % First call reads first value
    json_read_dict(Stream, First, [end_of_file(eof)]),
    First.a == 1,
    % Second call reads second value (stream advanced)
    json_read_dict(Stream, Second, [end_of_file(eof)]),
    Second.b == 2,
    % Third call returns eof
    json_read_dict(Stream, Third, [end_of_file(eof)]),
    Third == eof,
    close(Stream).

% Test EOF handling on exhausted stream
test(stream_eof_after_read) :-
    open_string('{"x":10}', Stream),
    json_read_dict(Stream, Dict1, [end_of_file(eof)]),
    Dict1.x == 10,
    json_read_dict(Stream, Dict2, [end_of_file(eof)]),
    Dict2 == eof,
    close(Stream).

% Test array with nested objects
test(nested_array) :-
    JSON = '[{"@id":"A","val":1},{"@id":"B","val":2}]',
    json_read_dict(JSON, Array, []),
    is_list(Array),
    length(Array, 2),
    nth0(0, Array, First),
    First.'@id' == "A".

% Test complex nested structure
test(deeply_nested) :-
    JSON = '{"level1":{"level2":{"level3":{"data":"deep"}}}}',
    json_read_dict(JSON, Dict, []),
    Dict.level1.level2.level3.data == "deep".

% Test UTF-8 characters
test(utf8_content) :-
    JSON = '{"name":"Kurt Gödel","city":"Brünn"}',
    json_read_dict(JSON, Dict, []),
    Dict.name == "Kurt Gödel",
    Dict.city == "Brünn".

% Test numeric precision (integers)
test(large_integers) :-
    JSON = '{"big":9007199254740991,"small":42}',
    json_read_dict(JSON, Dict, []),
    integer(Dict.big),
    integer(Dict.small),
    Dict.big == 9007199254740991.

% Test numeric precision (floats)
test(float_values) :-
    JSON = '{"pi":3.14159,"e":2.71828}',
    json_read_dict(JSON, Dict, []),
    % Rust parser converts decimals to rationals for precision
    rational(Dict.pi),
    rational(Dict.e).

% Test boolean and null
test(special_values) :-
    JSON = '{"active":true,"deleted":false,"data":null}',
    json_read_dict(JSON, Dict, []),
    Dict.active == true,
    Dict.deleted == false,
    Dict.data == null.

% Test empty structures
test(empty_structures) :-
    JSON = '{"obj":{},"arr":[]}',
    json_read_dict(JSON, Dict, []),
    is_dict(Dict.obj),
    is_list(Dict.arr),
    length(Dict.arr, 0).

:- end_tests(json_integration).

% ============================================================================
% Streaming Tests - Multiple JSON values in one stream
% ============================================================================

:- begin_tests(json_stream_generic).

% Helper to get stream length
stream_len(Stream, Len) :-
    stream_property(Stream, position(Start)),
    seek(Stream, 0, eof, End),
    Len is End,
    set_stream_position(Stream, Start).

% Single JSON value
test(single_object) :-
    open_string('{"@id":"A"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    Docs = [Dict],
    get_dict('@id', Dict, "A").

% Array is ONE JSON value (comma is part of array syntax)
test(array_is_single_value) :-
    open_string('[{"@id":"A"},{"@id":"B"}]', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    Docs = [Array],
    length(Array, 2),
    nth0(0, Array, Dict1),
    nth0(1, Array, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Multiple values - Unix LF
test(multiple_lf) :-
    open_string('{"@id":"A"}\n{"@id":"B"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 2),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Multiple values - Windows CRLF
test(multiple_crlf) :-
    open_string('{"@id":"A"}\r\n{"@id":"B"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 2),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Multiple values - Old Mac CR
test(multiple_cr) :-
    open_string('{"@id":"A"}\r{"@id":"B"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 2),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Multiple values - Space
test(multiple_space) :-
    open_string('{"@id":"A"} {"@id":"B"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 2),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Multiple values - Mixed whitespace
test(multiple_mixed_whitespace) :-
    open_string('{"@id":"A"} \n\r\n {"@id":"B"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 2),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Multiple values - No whitespace (valid!)
test(multiple_no_whitespace) :-
    open_string('{"@id":"A"}{"@id":"B"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 2),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B").

% Three values with various separators
test(three_values_mixed) :-
    open_string('{"@id":"A"}\n{"@id":"B"} {"@id":"C"}', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    length(Docs, 3),
    nth0(0, Docs, Dict1),
    nth0(1, Docs, Dict2),
    nth0(2, Docs, Dict3),
    get_dict('@id', Dict1, "A"),
    get_dict('@id', Dict2, "B"),
    get_dict('@id', Dict3, "C").

% Strings (for delete operations)
test(multiple_strings) :-
    open_string('"City/Dublin" "City/Pretoria"', S),
    stream_len(S, Len),
    '$json_preserve':json_read_all_from_stream(S, Len, Docs),
    Docs = ["City/Dublin", "City/Pretoria"].

% Note: Empty streams (Content-Length: 0) are rejected at HTTP layer
% See check_content_length/1 in routes.pl

% Test Rust validation of invalid content lengths
test(rust_rejects_zero_length, [error(invalid_content_length(0), _)]) :-
    open_string('', S),
    '$json_preserve':json_read_all_from_stream(S, 0, _Docs).

test(rust_rejects_one_byte, [error(invalid_content_length(1), _)]) :-
    open_string('x', S),
    '$json_preserve':json_read_all_from_stream(S, 1, _Docs).

:- end_tests(json_stream_generic).
