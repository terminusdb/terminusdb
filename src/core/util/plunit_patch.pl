:- module(plunit_patch, []).

:- use_module(library(thread)).

:- redefine_system_predicate(plunit:passed(_,_,_,_,_)).
:- redefine_system_predicate(plunit:failed(_,_,_,_)).
:- redefine_system_predicate(plunit:failed_assertion(_,_,_,_,_,_,_)).
:- redefine_system_predicate(plunit:blocked(_,_,_,_)).
:- redefine_system_predicate(plunit:sto(_,_,_,_)).
:- redefine_system_predicate(plunit:fixme(_,_,_,_,_)).

:- dynamic
    plunit:passed/5,                       % Unit, Test, Line, Det, Time
    plunit:failed/4,                       % Unit, Test, Line, Reason
    plunit:failed_assertion/7,             % Unit, Test, Line, ALoc, STO, Reason, Goal
    plunit:blocked/4,                      % Unit, Test, Line, Reason
    plunit:sto/4,                          % Unit, Test, Line, Results
    plunit:fixme/5.                        % Unit, Test, Line, Reason, Status

:- redefine_system_predicate(plunit:global_test_option(_)).

plunit:global_test_option(X) :- global_test_option(X).

:- redefine_system_predicate(plunit:run_unit(_)).

plunit:run_unit(X) :- run_unit(X).

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
