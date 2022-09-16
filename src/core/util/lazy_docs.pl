:- module('util/lazy_docs', [stream_to_lazy_docs/2]).

:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(utils).

stream_to_lazy_docs(Stream, List) :-
    do_or_die(
        ground(Stream),
        error(instantiation_error, _)),
    put_attr(List, 'util/lazy_docs', lazy_input(Stream, peek(_))).

attr_unify_hook(lazy_input(Stream, Peek_State), Value) :-
    Peek_State = peek(Peek),
    (   var(Peek)
    ->  json_read_dict(Stream, Term, [default_tag(json), end_of_file(eof)]),
        (   Term = eof
        ->  nb_setarg(1, Peek_State, []),
            Value = []
        ;   is_list(Term)
        ->  stream_to_lazy_docs(Stream, Rest),
            append(Term,Rest,Result),
            nb_linkarg(1, Peek_State, Result),
            Value = Result
        ;   stream_to_lazy_docs(Stream, Rest),
            Result = [Term|Rest],
            nb_linkarg(1, Peek_State, Result),
            Value = Result
        )
    ;   Value = Peek
    ).
