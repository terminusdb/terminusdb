:- module(query, []).

:- reexport([query/expansions,
             query/frame,
             query/frame_types,
             query/inference,
% dubious - why are we working with json so far from the api?
% included in this module because various predicates in query itself depend on it.
% Those bits that depend on it should probably be in api instead.
             query/jsonld,
             query/json_woql,
             query/ask,
             query/woql_compile
            ]).
