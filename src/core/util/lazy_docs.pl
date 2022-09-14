:- module('util/lazy_docs', [stream_to_lazy_docs/2]).

:- use_module(library(http/json)).

% Maybe do this?
buffer_size(100).

stream_to_lazy_docs(Stream, List) :-
    put_attr(List, 'util/lazy_docs', lazy_input(Stream)).

attr_unify_hook(State, Value) :-
    State = lazy_input(Stream),
    (   json_read_dict(Stream, Term, [default_tag(json), end_of_file(eof)])
    ->  (   Term = eof
        ->  Value = []
        ;   is_list(Term)
        ->  stream_to_lazy_docs(Stream, Rest),
            append(Term,Rest,Value)
        ;   stream_to_lazy_docs(Stream, Rest),
            Value = [Term|Rest]
        )
    ;   Value = []
    ).
