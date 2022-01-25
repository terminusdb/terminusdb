:- module(plunit_patch, []).

:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(thread)).
:- use_module(library(plunit)).

:- dynamic
       passed/5,                       % Unit, Test, Line, Det, Time
       failed/4,                       % Unit, Test, Line, Reason
       failed_assertion/7,             % Unit, Test, Line, ALoc, STO, Reason, Goal
       blocked/4,                      % Unit, Test, Line, Reason
       sto/4,                          % Unit, Test, Line, Results
       fixme/5.                        % Unit, Test, Line, Reason, Status

:- redefine_system_predicate(plunit:global_test_option(_)).

plunit:global_test_option(X) :- global_test_option(X).

:- redefine_system_predicate(plunit:run_unit(_)).

plunit:run_unit(X) :- run_unit(X).

:- redefine_system_predicate(plunit:cleanup).

plunit:cleanup :- cleanup.

:- redefine_system_predicate(plunit:success(_,_,_,_,_,_)).

plunit:success(A,B,C,D,E,F) :- success(A,B,C,D,E,F).

:- redefine_system_predicate(plunit:report).

plunit:report :- report.

:- redefine_system_predicate(plunit:number_of_clauses(_,_)).

plunit:number_of_clauses(A,B) :- number_of_clauses(A,B).

:- redefine_system_predicate(plunit:failure(_,_,_,_,_)).

plunit:failure(Unit, Name, Line, E, Options) :- failure(Unit, Name, Line, E, Options).

:- redefine_system_predicate(plunit:report_result(_,_)).

plunit:report_result(A,B) :- report_result(A,B).

:- redefine_system_predicate(plunit:assert_cyclic(_)).

plunit:assert_cyclic(A) :- assert_cyclic(A).

:- redefine_system_predicate(plunit:report_blocked).

plunit:report_blocked :- report_blocked.

global_test_option(load(Load)) :-
    must_be(oneof([never,always,normal]), Load).
global_test_option(run(When)) :-
    must_be(oneof([manual,make,make(all)]), When).
global_test_option(silent(Bool)) :-
    must_be(boolean, Bool).
global_test_option(sto(Bool)) :-
    must_be(boolean, Bool).
global_test_option(cleanup(Bool)) :-
    must_be(boolean, Bool).
global_test_option(concurrent(Bool)) :-
    must_be(boolean, Bool).

run_unit([]) :- !.
run_unit([H|T]) :-
    !,
    plunit:run_unit(H),
    plunit:run_unit(T).
run_unit(Spec) :-
    plunit:unit_from_spec(Spec, Unit, Tests, Module, UnitOptions),
    (   option(blocked(Reason), UnitOptions)
    ->  plunit:info(plunit(blocked(unit(Unit, Reason))))
    ;   plunit:setup(Module, unit(Unit), UnitOptions)
    ->  plunit:info(plunit(begin(Spec))),
        plunit:current_test_flag(test_options, Global_Options),
        (   option(concurrent(true), Global_Options)
        ->  concurrent_forall((Module:'unit test'(Name, Line, Options, Body),
                               plunit:matching_test(Name, Tests)),
                              plunit:run_test(Unit, Name, Line, Options, Body))
        ;   forall((Module:'unit test'(Name, Line, Options, Body),
                    plunit:matching_test(Name, Tests)),
                   plunit:run_test(Unit, Name, Line, Options, Body))),
        plunit:info(plunit(end(Spec))),
        (   plunit:message_level(silent)
        ->  true
        ;   format(user_error, '~N', [])
        ),
        plunit:cleanup(Module, UnitOptions)
    ;   true
    ).

cleanup :-
    thread_self(Me),
    retractall(passed(_, _, _, _, _)),
    retractall(failed(_, _, _, _)),
    retractall(failed_assertion(_, _, _, _, _, _, _)),
    retractall(blocked(_, _, _, _)),
    retractall(sto(_, _, _, _)),
    retractall(fixme(_, _, _, _, _)),
    retractall(running(_,_,_,_,Me)).

success(Unit, Name, Line, Det, _Time, Options) :-
    memberchk(fixme(Reason), Options),
    !,
    (   (   Det == true
        ;   memberchk(nondet, Options)
        )
    ->  put_char(user_error, +),
        Ok = passed
    ;   put_char(user_error, !),
        Ok = nondet
    ),
    flush_output(user_error),
    assert(fixme(Unit, Name, Line, Reason, Ok)).
success(Unit, Name, Line, _, _, Options) :-
    failed_assertion(Unit, Name, Line, _,_,_,_),
    !,
    failure(Unit, Name, Line, assertion, Options).
success(Unit, Name, Line, Det, Time, Options) :-
    assert(passed(Unit, Name, Line, Det, Time)),
    (   (   Det == true
        ;   memberchk(nondet, Options)
        )
    ->  put_char(user_error, .)
    ;   plunit:unit_file(Unit, File),
        print_message(warning, plunit(nondet(File, Line, Name)))
    ),
    flush_output(user_error).

report :-
    plunit:number_of_clauses(passed/5, Passed),
    plunit:number_of_clauses(failed/4, Failed),
    plunit:number_of_clauses(failed_assertion/7, FailedAssertion),
    plunit:number_of_clauses(blocked/4, Blocked),
    plunit:number_of_clauses(sto/4, STO),
    (   Passed+Failed+FailedAssertion+Blocked+STO =:= 0
    ->  plunit:info(plunit(no_tests))
    ;   Failed+FailedAssertion+Blocked+STO =:= 0
    ->  plunit:report_fixme,
        plunit:info(plunit(all_passed(Passed)))
    ;   plunit:report_blocked,
        plunit:report_fixme,
        plunit:report_failed_assertions,
        plunit:report_failed,
        plunit:report_sto,
        plunit:info(plunit(passed(Passed)))
    ).

report_result(blocked(Unit, Name, Line, Reason), _) :-
    !,
    assert(blocked(Unit, Name, Line, Reason)).
report_result(failure(Unit, Name, Line, How), Options) :-
    !,
    failure(Unit, Name, Line, How, Options).
report_result(success(Unit, Name, Line, Determinism, Time), Options) :-
    !,
    success(Unit, Name, Line, Determinism, Time, Options).
report_result(setup_failed(_Unit, _Name, _Line), _Options).
report_result(sto(Unit, Name, Line, ResultByType), Options) :-
    assert(sto(Unit, Name, Line, ResultByType)),
    print_message(error, plunit(sto(Unit, Name, Line))),
    plunit:report_sto_results(ResultByType, Options).

number_of_clauses(F/A,N) :-
    (   current_predicate(F/A)
    ->  functor(G,F,A),
        findall(t, G, Ts),
        length(Ts, N)
    ;   N = 0
    ).

failure(Unit, Name, Line, _, Options) :-
    memberchk(fixme(Reason), Options),
    !,
    put_char(user_error, -),
    flush_output(user_error),
    assert(fixme(Unit, Name, Line, Reason, failed)).
failure(Unit, Name, Line, E, Options) :-
    plunit:report_failure(Unit, Name, Line, E, Options),
    assert_cyclic(failed(Unit, Name, Line, E)).

assert_cyclic(Term) :-
    acyclic_term(Term),
    !,
    assert(Term).
assert_cyclic(Term) :-
    Term =.. [Functor|Args],
    recorda(cyclic, Args, Id),
    functor(Term, _, Arity),
    length(NewArgs, Arity),
    Head =.. [Functor|NewArgs],
    assert((Head :- recorded(_, Var, Id), Var = NewArgs)).

report_blocked :-
    number_of_clauses(blocked/4,N),
    N > 0,
    !,
    plunit:info(plunit(blocked(N))),
    (   blocked(Unit, Name, Line, Reason),
        plunit:unit_file(Unit, File),
        print_message(informational,
                      plunit(blocked(File:Line, Name, Reason))),
        fail ; true
    ).
report_blocked.
