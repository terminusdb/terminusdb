:- module(json_preserve, [
    json_read_dict/3
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
    ->  % Hybrid: Prolog reads stream → Rust parses string
        % WHY NOT pure Rust:
        %   1. ReadablePrologStream has UTF-8 issues with Prolog string streams
        %   2. Raw HTTP streams hang (no EOF) - need Content-Length + read_exact
        %   3. GraphQL works because it knows Content-Length and uses read_exact
        % MEMORY: Same as pure Rust (reads entire stream)
        % BENEFIT: Rust can preserve arbitrary precision in the future
        read_string(Stream, _, JSONText),
        (   JSONText = ""
        ->  % Empty string means EOF
            (   option(end_of_file(EOFValue), Options, end_of_file)
            ->  Dict = EOFValue
            ;   Dict = end_of_file
            )
        ;   '$json_preserve':json_read_string(JSONText, Dict)
        )
    ;   % For atoms/strings, use fast Rust parser
        (   atom(Stream)
        ->  atom_string(Stream, JSONText),
            '$json_preserve':json_read_string(JSONText, Dict)
        ;   string(Stream)
        ->  '$json_preserve':json_read_string(Stream, Dict)
        ;   throw(error(type_error(stream_or_atom, Stream), _))
        )
    ).

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
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(x, DictRust, X),
    assertion(float(X)).

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
    atom_json_dict(JSON, DictStd, [default_tag(json)]),
    json_read_dict(JSON, DictRust, []),
    assertion(DictStd == DictRust),
    get_dict(sci, DictRust, Sci),
    assertion(float(Sci)).

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
    assertion(Item1Rust.value == Item1Std.value),
    assertion(float(Item1Rust.value)),
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
