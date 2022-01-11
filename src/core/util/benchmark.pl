:- module(benchmark, [
              benchmark_start/1,
              benchmark_stop/0,
              benchmark_subject_start/1,
              benchmark_subject_stop/1,
              benchmark/1,
              benchmark/0]).

:- dynamic benchmark_file/1.

:- use_module(library(yall)).

benchmark_start(Path) :-
    \+ is_stream(Path),
    !,
    open(Path, append, Stream),

    benchmark_start(Stream).

benchmark_start(Stream) :-
    retractall(benchmark_file(_)),
    asserta(benchmark_file(Stream)).

benchmark_stop() :-
    benchmark_file(S),
    flush_output(S),
    retractall(benchmark_file(_)).

:- thread_local benchmark_subject/1.
benchmark_subject_start(Subject) :-
    asserta(benchmark_subject(Subject)),
    benchmark(Subject, start).

benchmark_subject_stop(Subject) :-
    benchmark(Subject, stop),
    retract(benchmark_subject(Subject)),
    !.

deblob(Term, Deblobbed) :-
    blob(Term, Type),
    \+ Type = text,
    !,
    format(atom(Deblobbed), '~q', [Term]).
deblob(Term, Deblobbed) :-
    is_list(Term),
    !,
    maplist(deblob, Term, Deblobbed).
deblob(Term, Deblobbed) :-
    compound(Term),
    !,
    Term =.. [Head|Elts],
    maplist(deblob, Elts, Deblobbed_Elts),
    Deblobbed =.. [Head|Deblobbed_Elts].
deblob(Term, Term).

benchmark_1(Stream, Subjects, Info, Type) :-
    get_prolog_backtrace(-1, [_,_|Backtrace]),
    maplist([frame(_,_,Term), Term]>>true, Backtrace, Terms),
    get_time(Timestamp),

    Loggable = benchmark(Timestamp, Type, Subjects, Info, Terms),
    deblob(Loggable, Deblobbed_Loggable),
    with_output_to(Stream,
                   (   writeq(Deblobbed_Loggable),
                       write('.'),
                       nl
                   )).

benchmark(Info, Type) :-
    (   benchmark_file(Stream),
        findall(Subject, benchmark_subject(Subject), Subjects),
        \+ Subjects = []
    ->  benchmark_1(Stream, Subjects, Info, Type)
    ;   true).

benchmark(Info) :-
    benchmark(Info, event).

benchmark :-
    benchmark(no_info).
