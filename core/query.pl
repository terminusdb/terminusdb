:- module(query, []).

:- reexport(query/expansions).
:- reexport(query/frame).
:- reexport(query/frame_types).
:- reexport(query/inference).

% dubious - why are we working with json so far from the api?
% included in this module because various predicates in query itself depend on it.
% Those bits that depend on it should probably be in api instead.
:- reexport(query/jsonld).
:- reexport(query/json_woql).

:- reexport(query/ask).
:- reexport(query/woql_compile).
