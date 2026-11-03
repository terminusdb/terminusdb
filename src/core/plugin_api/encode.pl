:- module(plugin_api_encode, [
    encode_query_value/2
]).

:- use_module(library(url), [www_form_encode/2]).

%% encode_query_value(+Value, -Encoded) is det.
%
%  Percent-encodes a value for use as an HTTP query parameter using
%  application/x-www-form-urlencoded rules. This encodes ALL characters
%  that have structural meaning in URLs including '/' (%2f), '&' (%26),
%  '=' (%3d), '+', '?', '#', and space.
%
%  Uses www_form_encode/2 which produces LOWERCASE hex digits. This is
%  fully RFC 3986 compliant (percent-encoding is case-insensitive per S2.1).
%
%  Accepts both atoms and strings as input; always produces an atom.
encode_query_value(Value, Encoded) :-
    (   atom(Value)
    ->  Atom = Value
    ;   atom_string(Atom, Value)
    ),
    www_form_encode(Atom, Encoded).
