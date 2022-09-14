:- module('util/lazy_docs', [stream_to_lazy_docs/2]).

:- use_module(library(http/json)).

% Maybe do this?
buffer_size(100).

stream_to_lazy_docs(Stream, List) :-
    put_attr(List, 'util/lazy_docs', lazy_input(Stream, _)).

attr_unify_hook(State, Value) :-
    State = lazy_input(Stream, Peak),
    (   var(Peak)
    ->  json_read_dict(Stream, Term, [default_tag(json), end_of_file(eof)]),
        (   Term = eof
        ->  nb_setarg(2, State, []),
            Value = []
        ;   is_list(Term)
        ->  stream_to_lazy_docs(Stream, Rest),
            append(Term,Rest,Result),
            nb_setarg(2, State, Result),
            Value = Result
        ;   stream_to_lazy_docs(Stream, Rest),
            Result = [Term|Rest],
            nb_setarg(2, State, Result),
            Value = Result
        )
    ;   Value = Peak
    ).
