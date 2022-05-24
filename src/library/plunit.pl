/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2021, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(plunit,
          [ set_test_options/1,         % +Options
            begin_tests/1,              % +Name
            begin_tests/2,              % +Name, +Options
            end_tests/1,                % +Name
            run_tests/0,                % Run all tests
            run_tests/1,                % Run named test-set
            load_test_files/1,          % +Options
            running_tests/0,            % Prints currently running test
            current_test/5,             % ?Unit,?Test,?Line,?Body,?Options
            test_report/1               % +What
          ]).

/** <module> Unit Testing

Unit testing environment for SWI-Prolog and   SICStus Prolog. For usage,
please visit http://www.swi-prolog.org/pldoc/package/plunit.
*/

:- autoload(library(apply), [maplist/3,include/3]).
:- autoload(library(lists), [member/2,append/2]).
:- autoload(library(option), [option/3,option/2]).
:- autoload(library(ordsets), [ord_intersection/3]).
:- autoload(library(pairs), [group_pairs_by_key/2,pairs_values/2]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(thread), [concurrent_forall/2]).

:- meta_predicate valid_options(+, 1).


                 /*******************************
                 *    CONDITIONAL COMPILATION   *
                 *******************************/

:- discontiguous
    user:term_expansion/2.

:- dynamic
    include_code/1.

including :-
    include_code(X),
    !,
    X == true.
including.

if_expansion((:- if(G)), []) :-
    (   including
    ->  (   catch(G, E, (print_message(error, E), fail))
        ->  asserta(include_code(true))
        ;   asserta(include_code(false))
        )
    ;   asserta(include_code(else_false))
    ).
if_expansion((:- else), []) :-
    (   retract(include_code(X))
    ->  (   X == true
        ->  X2 = false
        ;   X == false
        ->  X2 = true
        ;   X2 = X
        ),
        asserta(include_code(X2))
    ;   throw_error(context_error(no_if),_)
    ).
if_expansion((:- endif), []) :-
    retract(include_code(_)),
    !.

if_expansion(_, []) :-
    \+ including.

user:term_expansion(In, Out) :-
    prolog_load_context(module, plunit),
    if_expansion(In, Out).

swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
swi     :- catch(current_prolog_flag(dialect, yap), _, fail).
sicstus :- catch(current_prolog_flag(system_type, _), _, fail).


:- if(swi).
throw_error(Error_term,Impldef) :-
    throw(error(Error_term,context(Impldef,_))).

:- set_prolog_flag(generate_debug_info, false).
current_test_flag(Name, Value) :-
    current_prolog_flag(Name, Value).

set_test_flag(Name, Value) :-
    create_prolog_flag(Name, Value, []).

% ensure expansion to avoid tracing
goal_expansion(forall(C,A),
               \+ (C, \+ A)).
goal_expansion(current_module(Module,File),
               module_property(Module, file(File))).

:- if(current_prolog_flag(dialect, yap)).

'$set_predicate_attribute'(_, _, _).

:- endif.
:- endif.

:- if(sicstus).
throw_error(Error_term,Impldef) :-
    throw(error(Error_term,i(Impldef))). % SICStus 3 work around

% SWI-Compatibility
:- op(700, xfx, =@=).

'$set_source_module'(_, _).

%!  current_test_flag(?Name, ?Value) is nondet.
%
%   Query  flags  that  control  the    testing   process.  Emulates
%   SWI-Prologs flags.

:- dynamic test_flag/2. % Name, Val

current_test_flag(optimise, Val) :-
    current_prolog_flag(compiling, Compiling),
    (   Compiling == debugcode ; true % TBD: Proper test
    ->  Val = false
    ;   Val = true
    ).
current_test_flag(Name, Val) :-
    test_flag(Name, Val).


%!  set_test_flag(+Name, +Value) is det.

set_test_flag(Name, Val) :-
    var(Name),
    !,
    throw_error(instantiation_error, set_test_flag(Name,Val)).
set_test_flag( Name, Val ) :-
    retractall(test_flag(Name,_)),
    asserta(test_flag(Name, Val)).

:- op(1150, fx, thread_local).

user:term_expansion((:- thread_local(PI)), (:- dynamic(PI))) :-
    prolog_load_context(module, plunit).

:- endif.

                 /*******************************
                 *            IMPORTS           *
                 *******************************/

:- initialization
   (   current_test_flag(test_options, _)
   ->  true
   ;   set_test_flag(test_options,
                 [ run(make),       % run tests on make/0
                   sto(false)
                 ])
   ).

%!  set_test_options(+Options)
%
%   Specifies how to deal with test suites.  Defined options are:
%
%           * load(+Load)
%           Whether or not the tests must be loaded.  Values are
%           =never=, =always=, =normal= (only if not optimised)
%
%           * run(+When)
%           When the tests are run.  Values are =manual=, =make=
%           or make(all).
%
%           * silent(+Bool)
%           If =true= (default =false=), report successful tests
%           using message level =silent=, only printing errors and
%           warnings.
%
%           * sto(+Bool)
%           How to test whether code is subject to occurs check
%           (STO).  If =false= (default), STO is not considered.
%           If =true= and supported by the hosting Prolog, code
%           is run in all supported unification mode and reported
%           if the results are inconsistent.
%
%           * cleanup(+Bool)
%           If =true= (default =false), cleanup report at the end
%           of run_tests/1.  Used to improve cooperation with
%           memory debuggers such as dmalloc.
%
%           * concurrent(+Bool)
%           If =true= (default =false), run all tests in a block
%           concurrently.
%

set_test_options(Options) :-
    valid_options(Options, global_test_option),
    set_test_flag(test_options, Options).

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


%!  loading_tests
%
%   True if tests must be loaded.

loading_tests :-
    current_test_flag(test_options, Options),
    option(load(Load), Options, normal),
    (   Load == always
    ->  true
    ;   Load == normal,
        \+ current_test_flag(optimise, true)
    ).

                 /*******************************
                 *            MODULE            *
                 *******************************/

:- dynamic
    loading_unit/4,                 % Unit, Module, File, OldSource
    current_unit/4,                 % Unit, Module, Context, Options
    test_file_for/2.                % ?TestFile, ?PrologFile

%!  begin_tests(+UnitName:atom) is det.
%!  begin_tests(+UnitName:atom, Options) is det.
%
%   Start a test-unit. UnitName is the  name   of  the test set. the
%   unit is ended by :- end_tests(UnitName).

begin_tests(Unit) :-
    begin_tests(Unit, []).

begin_tests(Unit, Options) :-
    must_be(atom, Unit),
    valid_options(Options, test_set_option),
    make_unit_module(Unit, Name),
    source_location(File, Line),
    begin_tests(Unit, Name, File:Line, Options).

:- if(swi).
begin_tests(Unit, Name, File:Line, Options) :-
    loading_tests,
    !,
    '$set_source_module'(Context, Context),
    (   current_unit(Unit, Name, Context, Options)
    ->  true
    ;   retractall(current_unit(Unit, Name, _, _)),
        assert(current_unit(Unit, Name, Context, Options))
    ),
    '$set_source_module'(Old, Name),
    '$declare_module'(Name, test, Context, File, Line, false),
    discontiguous(Name:'unit test'/4),
    '$set_predicate_attribute'(Name:'unit test'/4, trace, false),
    discontiguous(Name:'unit body'/2),
    asserta(loading_unit(Unit, Name, File, Old)).
begin_tests(Unit, Name, File:_Line, _Options) :-
    '$set_source_module'(Old, Old),
    asserta(loading_unit(Unit, Name, File, Old)).

:- else.

% we cannot use discontiguous as a goal in SICStus Prolog.

user:term_expansion((:- begin_tests(Set)),
                    [ (:- begin_tests(Set)),
                      (:- discontiguous(test/2)),
                      (:- discontiguous('unit body'/2)),
                      (:- discontiguous('unit test'/4))
                    ]).

begin_tests(Unit, Name, File:_Line, Options) :-
    loading_tests,
    !,
    (   current_unit(Unit, Name, _, Options)
    ->  true
    ;   retractall(current_unit(Unit, Name, _, _)),
        assert(current_unit(Unit, Name, -, Options))
    ),
    asserta(loading_unit(Unit, Name, File, -)).
begin_tests(Unit, Name, File:_Line, _Options) :-
    asserta(loading_unit(Unit, Name, File, -)).

:- endif.

%!  end_tests(+Name) is det.
%
%   Close a unit-test module.
%
%   @tbd    Run tests/clean module?
%   @tbd    End of file?

end_tests(Unit) :-
    loading_unit(StartUnit, _, _, _),
    !,
    (   Unit == StartUnit
    ->  once(retract(loading_unit(StartUnit, _, _, Old))),
        '$set_source_module'(_, Old)
    ;   throw_error(context_error(plunit_close(Unit, StartUnit)), _)
    ).
end_tests(Unit) :-
    throw_error(context_error(plunit_close(Unit, -)), _).

%!  make_unit_module(+Name, -ModuleName) is det.
%!  unit_module(+Name, -ModuleName) is det.

:- if(swi).

unit_module(Unit, Module) :-
    atom_concat('plunit_', Unit, Module).

make_unit_module(Unit, Module) :-
    unit_module(Unit, Module),
    (   current_module(Module),
        \+ current_unit(_, Module, _, _),
        predicate_property(Module:H, _P),
        \+ predicate_property(Module:H, imported_from(_M))
    ->  throw_error(permission_error(create, plunit, Unit),
                    'Existing module')
    ;  true
    ).

:- else.

:- dynamic
    unit_module_store/2.

unit_module(Unit, Module) :-
    unit_module_store(Unit, Module),
    !.

make_unit_module(Unit, Module) :-
    prolog_load_context(module, Module),
    assert(unit_module_store(Unit, Module)).

:- endif.

                 /*******************************
                 *           EXPANSION          *
                 *******************************/

%!  expand_test(+Name, +Options, +Body, -Clause) is det.
%
%   Expand test(Name, Options) :-  Body  into   a  clause  for
%   'unit test'/4 and 'unit body'/2.

expand_test(Name, Options0, Body,
            [ 'unit test'(Name, Line, Options, Module:'unit body'(Id, Vars)),
              ('unit body'(Id, Vars) :- !, Body)
            ]) :-
    source_location(_File, Line),
    prolog_load_context(module, Module),
    atomic_list_concat([Name, '@line ', Line], Id),
    term_variables(Options0, OptionVars0), sort(OptionVars0, OptionVars),
    term_variables(Body, BodyVars0), sort(BodyVars0, BodyVars),
    ord_intersection(OptionVars, BodyVars, VarList),
    Vars =.. [vars|VarList],
    (   is_list(Options0)           % allow for single option without list
    ->  Options1 = Options0
    ;   Options1 = [Options0]
    ),
    maplist(expand_option, Options1, Options2),
    valid_options(Options2, test_option),
    valid_test_mode(Options2, Options).

expand_option(Var, _) :-
    var(Var),
    !,
    throw_error(instantiation_error,_).
expand_option(A == B, true(A==B)) :- !.
expand_option(A = B, true(A=B)) :- !.
expand_option(A =@= B, true(A=@=B)) :- !.
expand_option(A =:= B, true(A=:=B)) :- !.
expand_option(error(X), throws(error(X, _))) :- !.
expand_option(exception(X), throws(X)) :- !. % SICStus 4 compatibility
expand_option(error(F,C), throws(error(F,C))) :- !. % SICStus 4 compatibility
expand_option(true, true(true)) :- !.
expand_option(O, O).

valid_test_mode(Options0, Options) :-
    include(test_mode, Options0, Tests),
    (   Tests == []
    ->  Options = [true(true)|Options0]
    ;   Tests = [_]
    ->  Options = Options0
    ;   throw_error(plunit(incompatible_options, Tests), _)
    ).

test_mode(true(_)).
test_mode(all(_)).
test_mode(set(_)).
test_mode(fail).
test_mode(throws(_)).


%!  expand(+Term, -Clauses) is semidet.

expand(end_of_file, _) :-
    loading_unit(Unit, _, _, _),
    !,
    end_tests(Unit),                % warn?
    fail.
expand((:-end_tests(_)), _) :-
    !,
    fail.
expand(_Term, []) :-
    \+ loading_tests.
expand((test(Name) :- Body), Clauses) :-
    !,
    expand_test(Name, [], Body, Clauses).
expand((test(Name, Options) :- Body), Clauses) :-
    !,
    expand_test(Name, Options, Body, Clauses).
expand(test(Name), _) :-
    !,
    throw_error(existence_error(body, test(Name)), _).
expand(test(Name, _Options), _) :-
    !,
    throw_error(existence_error(body, test(Name)), _).

:- if(swi).
:- multifile
    system:term_expansion/2.
:- endif.

system:term_expansion(Term, Expanded) :-
    (   loading_unit(_, _, File, _)
    ->  source_location(ThisFile, _),
        (   File == ThisFile
        ->  true
        ;   source_file_property(ThisFile, included_in(File, _))
        ),
        expand(Term, Expanded)
    ).


                 /*******************************
                 *             OPTIONS          *
                 *******************************/

:- if(swi).
:- else.
must_be(list, X) :-
    !,
    (   is_list(X)
    ->  true
    ;   is_not(list, X)
    ).
must_be(Type, X) :-
    (   call(Type, X)
    ->  true
    ;   is_not(Type, X)
    ).

is_not(Type, X) :-
    (   ground(X)
    ->  throw_error(type_error(Type, X), _)
    ;   throw_error(instantiation_error, _)
    ).
:- endif.

%!  valid_options(+Options, :Pred) is det.
%
%   Verify Options to be a list of valid options according to
%   Pred.
%
%   @throws =type_error= or =instantiation_error=.

valid_options(Options, Pred) :-
    must_be(list, Options),
    verify_options(Options, Pred).

verify_options([], _).
verify_options([H|T], Pred) :-
    (   call(Pred, H)
    ->  verify_options(T, Pred)
    ;   throw_error(domain_error(Pred, H), _)
    ).


%!  test_option(+Option) is semidet.
%
%   True if Option is a valid option for test(Name, Options).

test_option(Option) :-
    test_set_option(Option),
    !.
test_option(true(_)).
test_option(fail).
test_option(throws(_)).
test_option(all(_)).
test_option(set(_)).
test_option(nondet).
test_option(fixme(_)).
test_option(forall(X)) :-
    must_be(callable, X).

%!  test_option(+Option) is semidet.
%
%   True if Option is a valid option for :- begin_tests(Name,
%   Options).

test_set_option(blocked(X)) :-
    must_be(ground, X).
test_set_option(condition(X)) :-
    must_be(callable, X).
test_set_option(setup(X)) :-
    must_be(callable, X).
test_set_option(cleanup(X)) :-
    must_be(callable, X).
test_set_option(sto(V)) :-
    nonvar(V), member(V, [finite_trees, rational_trees]).
test_set_option(concurrent(V)) :-
    must_be(boolean, V).


                 /*******************************
                 *        RUNNING TOPLEVEL      *
                 *******************************/

:- dynamic
    passed/5,                       % Unit, Test, Line, Det, Time
    failed/4,                       % Unit, Test, Line, Reason
    failed_assertion/7,             % Unit, Test, Line, ALoc, STO, Reason, Goal
    blocked/4,                      % Unit, Test, Line, Reason
    sto/4,                          % Unit, Test, Line, Results
    fixme/5.                        % Unit, Test, Line, Reason, Status

:- dynamic
    running/5.                      % Unit, Test, Line, STO, Thread

%!  run_tests is semidet.
%!  run_tests(+TestSet) is semidet.
%
%   Run  tests  and  report  about    the   results.  The  predicate
%   run_tests/0 runs all known  tests  that   are  not  blocked. The
%   predicate run_tests/1 takes a  specification   of  tests to run.
%   This  is  either  a  single   specification    or   a   list  of
%   specifications. Each single specification is  either the name of
%   a test-unit or a term <test-unit>:<test>, denoting a single test
%   within a unit.

run_tests :-
    cleanup,
    setup_call_cleanup(
        setup_trap_assertions(Ref),
        run_current_units,
        report_and_cleanup(Ref)).

run_current_units :-
    forall(current_test_set(Set),
           run_unit(Set)),
    check_for_test_errors.

report_and_cleanup(Ref) :-
    cleanup_trap_assertions(Ref),
    report,
    cleanup_after_test.

run_tests(Set) :-
    cleanup,
    setup_call_cleanup(
        setup_trap_assertions(Ref),
        run_unit_and_check_errors(Set),
        report_and_cleanup(Ref)).

run_unit_and_check_errors(Set) :-
    run_unit(Set),
    check_for_test_errors.

run_unit([]) :- !.
run_unit([H|T]) :-
    !,
    run_unit(H),
    run_unit(T).
run_unit(Spec) :-
    unit_from_spec(Spec, Unit, Tests, Module, UnitOptions),
    (   option(blocked(Reason), UnitOptions)
    ->  info(plunit(blocked(unit(Unit, Reason))))
    ;   setup(Module, unit(Unit), UnitOptions)
    ->  info(plunit(begin(Spec))),
        current_test_flag(test_options, GlobalOptions),
        (   option(concurrent(true), GlobalOptions),
            option(concurrent(true), UnitOptions, false)
        ->  concurrent_forall((Module:'unit test'(Name, Line, Options, Body),
                               matching_test(Name, Tests)),
                              run_test(Unit, Name, Line, Options, Body))
        ;   forall((Module:'unit test'(Name, Line, Options, Body),
                    matching_test(Name, Tests)),
                   run_test(Unit, Name, Line, Options, Body))),
        info(plunit(end(Spec))),
        (   message_level(silent)
        ->  true
        ;   format(user_error, '~N', [])
        ),
        cleanup(Module, UnitOptions)
    ;   true
    ).

unit_from_spec(Unit, Unit, _, Module, Options) :-
    atom(Unit),
    !,
    (   current_unit(Unit, Module, _Supers, Options)
    ->  true
    ;   throw_error(existence_error(unit_test, Unit), _)
    ).
unit_from_spec(Unit:Tests, Unit, Tests, Module, Options) :-
    atom(Unit),
    !,
    (   current_unit(Unit, Module, _Supers, Options)
    ->  true
    ;   throw_error(existence_error(unit_test, Unit), _)
    ).


matching_test(X, X) :- !.
matching_test(Name, Set) :-
    is_list(Set),
    memberchk(Name, Set).

cleanup :-
    thread_self(Me),
    retractall(passed(_, _, _, _, _)),
    retractall(failed(_, _, _, _)),
    retractall(failed_assertion(_, _, _, _, _, _, _)),
    retractall(blocked(_, _, _, _)),
    retractall(sto(_, _, _, _)),
    retractall(fixme(_, _, _, _, _)),
    retractall(running(_,_,_,_,Me)).

cleanup_after_test :-
    current_test_flag(test_options, Options),
    option(cleanup(Cleanup), Options, false),
    (   Cleanup == true
    ->  cleanup
    ;   true
    ).


%!  run_tests_in_files(+Files:list) is det.
%
%   Run all test-units that appear in the given Files.

run_tests_in_files(Files) :-
    findall(Unit, unit_in_files(Files, Unit), Units),
    (   Units == []
    ->  true
    ;   run_tests(Units)
    ).

unit_in_files(Files, Unit) :-
    is_list(Files),
    !,
    member(F, Files),
    absolute_file_name(F, Source,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]),
    unit_file(Unit, Source).


                 /*******************************
                 *         HOOKING MAKE/0       *
                 *******************************/

%!  make_run_tests(+Files)
%
%   Called indirectly from make/0 after Files have been reloaded.

make_run_tests(Files) :-
    current_test_flag(test_options, Options),
    option(run(When), Options, manual),
    (   When == make
    ->  run_tests_in_files(Files)
    ;   When == make(all)
    ->  run_tests
    ;   true
    ).

:- if(swi).

unification_capability(sto_error_incomplete).
% can detect some (almost all) STO runs
unification_capability(rational_trees).
unification_capability(finite_trees).

set_unification_capability(Cap) :-
    cap_to_flag(Cap, Flag),
    set_prolog_flag(occurs_check, Flag).

current_unification_capability(Cap) :-
    current_prolog_flag(occurs_check, Flag),
    cap_to_flag(Cap, Flag),
    !.

cap_to_flag(sto_error_incomplete, error).
cap_to_flag(rational_trees, false).
cap_to_flag(finite_trees, true).

:- else.
:- if(sicstus).

unification_capability(rational_trees).
set_unification_capability(rational_trees).
current_unification_capability(rational_trees).

:- else.

unification_capability(_) :-
    fail.

:- endif.
:- endif.

                 /*******************************
                 *      ASSERTION HANDLING      *
                 *******************************/

:- if(swi).

:- dynamic prolog:assertion_failed/2.

setup_trap_assertions(Ref) :-
    asserta((prolog:assertion_failed(Reason, Goal) :-
                    test_assertion_failed(Reason, Goal)),
            Ref).

cleanup_trap_assertions(Ref) :-
    erase(Ref).

test_assertion_failed(Reason, Goal) :-
    thread_self(Me),
    running(Unit, Test, Line, STO, Me),
    (   catch(get_prolog_backtrace(10, Stack), _, fail),
        assertion_location(Stack, AssertLoc)
    ->  true
    ;   AssertLoc = unknown
    ),
    current_test_flag(test_options, Options),
    report_failed_assertion(Unit, Test, Line, AssertLoc,
                            STO, Reason, Goal, Options),
    assert_cyclic(failed_assertion(Unit, Test, Line, AssertLoc,
                                   STO, Reason, Goal)).

assertion_location(Stack, File:Line) :-
    append(_, [AssertFrame,CallerFrame|_], Stack),
    prolog_stack_frame_property(AssertFrame,
                                predicate(prolog_debug:assertion/1)),
    !,
    prolog_stack_frame_property(CallerFrame, location(File:Line)).

report_failed_assertion(Unit, Test, Line, AssertLoc,
                        STO, Reason, Goal, _Options) :-
    print_message(
        error,
        plunit(failed_assertion(Unit, Test, Line, AssertLoc,
                                STO, Reason, Goal))).

:- else.

setup_trap_assertions(_).
cleanup_trap_assertions(_).

:- endif.


                 /*******************************
                 *         RUNNING A TEST       *
                 *******************************/

%!  run_test(+Unit, +Name, +Line, +Options, +Body) is det.
%
%   Run a single test.

run_test(Unit, Name, Line, Options, Body) :-
    option(forall(Generator), Options),
    !,
    unit_module(Unit, Module),
    term_variables(Generator, Vars),
    forall(Module:Generator,
           run_test_once(Unit, @(Name,Vars), Line, Options, Body)).
run_test(Unit, Name, Line, Options, Body) :-
    run_test_once(Unit, Name, Line, Options, Body).

run_test_once(Unit, Name, Line, Options, Body) :-
    current_test_flag(test_options, GlobalOptions),
    option(sto(false), GlobalOptions, false),
    !,
    current_unification_capability(Type),
    begin_test(Unit, Name, Line, Type),
    run_test_6(Unit, Name, Line, Options, Body, Result),
    end_test(Unit, Name, Line, Type),
    report_result(Result, Options).
run_test_once(Unit, Name, Line, Options, Body) :-
    current_unit(Unit, _Module, _Supers, UnitOptions),
    option(sto(Type), UnitOptions),
    \+ option(sto(_), Options),
    !,
    current_unification_capability(Cap0),
    call_cleanup(run_test_cap(Unit, Name, Line, [sto(Type)|Options], Body),
                 set_unification_capability(Cap0)).
run_test_once(Unit, Name, Line, Options, Body) :-
    current_unification_capability(Cap0),
    call_cleanup(run_test_cap(Unit, Name, Line, Options, Body),
                 set_unification_capability(Cap0)).

run_test_cap(Unit, Name, Line, Options, Body) :-
    (   option(sto(Type), Options)
    ->  unification_capability(Type),
        set_unification_capability(Type),
        begin_test(Unit, Name, Line, Type),
        run_test_6(Unit, Name, Line, Options, Body, Result),
        end_test(Unit, Name, Line, Type),
        report_result(Result, Options)
    ;   findall(Key-(Type+Result),
                test_caps(Type, Unit, Name, Line, Options, Body, Result, Key),
                Pairs),
        group_pairs_by_key(Pairs, Keyed),
        (   Keyed == []
        ->  true
        ;   Keyed = [_-Results]
        ->  Results = [_Type+Result|_],
            report_result(Result, Options)          % consistent results
        ;   pairs_values(Pairs, ResultByType),
            report_result(sto(Unit, Name, Line, ResultByType), Options)
        )
    ).

%!  test_caps(-Type, +Unit, +Name, +Line, +Options, +Body, -Result, -Key) is nondet.

test_caps(Type, Unit, Name, Line, Options, Body, Result, Key) :-
    unification_capability(Type),
    set_unification_capability(Type),
    begin_test(Unit, Name, Line, Type),
    run_test_6(Unit, Name, Line, Options, Body, Result),
    end_test(Unit, Name, Line, Type),
    result_to_key(Result, Key),
    Key \== setup_failed.

result_to_key(blocked(_, _, _, _), blocked).
result_to_key(failure(_, _, _, How0), failure(How1)) :-
    ( How0 = succeeded(_T) -> How1 = succeeded ; How0 = How1 ).
result_to_key(success(_, _, _, Determinism, _), success(Determinism)).
result_to_key(setup_failed(_,_,_), setup_failed).

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
    report_sto_results(ResultByType, Options).

report_sto_results([], _).
report_sto_results([Type+Result|T], Options) :-
    print_message(error, plunit(sto(Type, Result))),
    report_sto_results(T, Options).


%!  run_test_6(+Unit, +Name, +Line, +Options, :Body, -Result) is det.
%
%   Result is one of:
%
%           * blocked(Unit, Name, Line, Reason)
%           * failure(Unit, Name, Line, How)
%           * success(Unit, Name, Line, Determinism, Time)
%           * setup_failed(Unit, Name, Line)

run_test_6(Unit, Name, Line, Options, _Body,
           blocked(Unit, Name, Line, Reason)) :-
    option(blocked(Reason), Options),
    !.
run_test_6(Unit, Name, Line, Options, Body, Result) :-
    option(all(Answer), Options),                  % all(Bindings)
    !,
    nondet_test(all(Answer), Unit, Name, Line, Options, Body, Result).
run_test_6(Unit, Name, Line, Options, Body, Result) :-
    option(set(Answer), Options),                  % set(Bindings)
    !,
    nondet_test(set(Answer), Unit, Name, Line, Options, Body, Result).
run_test_6(Unit, Name, Line, Options, Body, Result) :-
    option(fail, Options),                         % fail
    !,
    unit_module(Unit, Module),
    (   setup(Module, test(Unit,Name,Line), Options)
    ->  statistics(runtime, [T0,_]),
        (   catch(Module:Body, E, true)
        ->  (   var(E)
            ->  statistics(runtime, [T1,_]),
                Time is (T1 - T0)/1000.0,
                Result = failure(Unit, Name, Line, succeeded(Time)),
                cleanup(Module, Options)
            ;   Result = failure(Unit, Name, Line, E),
                cleanup(Module, Options)
            )
        ;   statistics(runtime, [T1,_]),
            Time is (T1 - T0)/1000.0,
            Result = success(Unit, Name, Line, true, Time),
            cleanup(Module, Options)
        )
    ;   Result = setup_failed(Unit, Name, Line)
    ).
run_test_6(Unit, Name, Line, Options, Body, Result) :-
    option(true(Cmp), Options),
    !,
    unit_module(Unit, Module),
    (   setup(Module, test(Unit,Name,Line), Options) % true(Binding)
    ->  statistics(runtime, [T0,_]),
        (   catch(call_det(Module:Body, Det), E, true)
        ->  (   var(E)
            ->  statistics(runtime, [T1,_]),
                Time is (T1 - T0)/1000.0,
                (   catch(Module:Cmp, E, true)
                ->  (   var(E)
                    ->  Result = success(Unit, Name, Line, Det, Time)
                    ;   Result = failure(Unit, Name, Line, cmp_error(Cmp, E))
                    )
                ;   Result = failure(Unit, Name, Line, wrong_answer(Cmp))
                ),
                cleanup(Module, Options)
            ;   Result = failure(Unit, Name, Line, E),
                cleanup(Module, Options)
            )
        ;   Result = failure(Unit, Name, Line, failed),
            cleanup(Module, Options)
        )
    ;   Result = setup_failed(Unit, Name, Line)
    ).
run_test_6(Unit, Name, Line, Options, Body, Result) :-
    option(throws(Expect), Options),
    !,
    unit_module(Unit, Module),
    (   setup(Module, test(Unit,Name,Line), Options)
    ->  statistics(runtime, [T0,_]),
        (   catch(Module:Body, E, true)
        ->  (   var(E)
            ->  Result = failure(Unit, Name, Line, no_exception),
                cleanup(Module, Options)
            ;   statistics(runtime, [T1,_]),
                Time is (T1 - T0)/1000.0,
                (   match_error(Expect, E)
                ->  Result = success(Unit, Name, Line, true, Time)
                ;   Result = failure(Unit, Name, Line, wrong_error(Expect, E))
                ),
                cleanup(Module, Options)
            )
        ;   Result = failure(Unit, Name, Line, failed),
            cleanup(Module, Options)
        )
    ;   Result = setup_failed(Unit, Name, Line)
    ).


%!  non_det_test(+Expected, +Unit, +Name, +Line, +Options, +Body, -Result)
%
%   Run tests on non-deterministic predicates.

nondet_test(Expected, Unit, Name, Line, Options, Body, Result) :-
    unit_module(Unit, Module),
    result_vars(Expected, Vars),
    statistics(runtime, [T0,_]),
    (   setup(Module, test(Unit,Name,Line), Options)
    ->  (   catch(findall(Vars, Module:Body, Bindings), E, true)
        ->  (   var(E)
            ->  statistics(runtime, [T1,_]),
                Time is (T1 - T0)/1000.0,
                (   nondet_compare(Expected, Bindings, Unit, Name, Line)
                ->  Result = success(Unit, Name, Line, true, Time)
                ;   Result = failure(Unit, Name, Line, wrong_answer(Expected, Bindings))
                ),
                cleanup(Module, Options)
            ;   Result = failure(Unit, Name, Line, E),
                cleanup(Module, Options)
            )
        )
    ;   Result = setup_failed(Unit, Name, Line)
    ).


%!  result_vars(+Expected, -Vars) is det.
%
%   Create a term v(V1, ...) containing all variables at the left
%   side of the comparison operator on Expected.

result_vars(Expected, Vars) :-
    arg(1, Expected, CmpOp),
    arg(1, CmpOp, Vars).

%!  nondet_compare(+Expected, +Bindings, +Unit, +Name, +Line) is semidet.
%
%   Compare list/set results for non-deterministic predicates.
%
%   @tbd    Properly report errors
%   @bug    Sort should deal with equivalence on the comparison
%           operator.

nondet_compare(all(Cmp), Bindings, _Unit, _Name, _Line) :-
    cmp(Cmp, _Vars, Op, Values),
    cmp_list(Values, Bindings, Op).
nondet_compare(set(Cmp), Bindings0, _Unit, _Name, _Line) :-
    cmp(Cmp, _Vars, Op, Values0),
    sort(Bindings0, Bindings),
    sort(Values0, Values),
    cmp_list(Values, Bindings, Op).

cmp_list([], [], _Op).
cmp_list([E0|ET], [V0|VT], Op) :-
    call(Op, E0, V0),
    cmp_list(ET, VT, Op).

%!  cmp(+CmpTerm, -Left, -Op, -Right) is det.

cmp(Var  == Value, Var,  ==, Value).
cmp(Var =:= Value, Var, =:=, Value).
cmp(Var  =  Value, Var,  =,  Value).
:- if(swi).
cmp(Var =@= Value, Var, =@=, Value).
:- else.
:- if(sicstus).
cmp(Var =@= Value, Var, variant, Value). % variant/2 is the same =@=
:- endif.
:- endif.


%!  call_det(:Goal, -Det) is nondet.
%
%   True if Goal succeeded.  Det is unified to =true= if Goal left
%   no choicepoints and =false= otherwise.

:- if((swi|sicstus)).
call_det(Goal, Det) :-
    call_cleanup(Goal,Det0=true),
    ( var(Det0) -> Det = false ; Det = true ).
:- else.
call_det(Goal, true) :-
    call(Goal).
:- endif.

%!  match_error(+Expected, +Received) is semidet.
%
%   True if the Received errors matches the expected error. Matching
%   is based on subsumes_term/2.

match_error(Expect, Rec) :-
    subsumes_term(Expect, Rec).

%!  setup(+Module, +Context, +Options) is semidet.
%
%   Call the setup handler and  fail  if   it  cannot  run  for some
%   reason. The condition handler is  similar,   but  failing is not
%   considered an error.  Context is one of
%
%       * unit(Unit)
%       If it is the setup handler for a unit
%       * test(Unit,Name,Line)
%       If it is the setup handler for a test

setup(Module, Context, Options) :-
    option(condition(Condition), Options),
    option(setup(Setup), Options),
    !,
    setup(Module, Context, [condition(Condition)]),
    setup(Module, Context, [setup(Setup)]).
setup(Module, Context, Options) :-
    option(setup(Setup), Options),
    !,
    (   catch(call_ex(Module, Setup), E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(error, plunit(error(setup, Context, E))),
            fail
        )
    ;   print_message(error, error(goal_failed(Setup), _)),
        fail
    ).
setup(Module, Context, Options) :-
    option(condition(Setup), Options),
    !,
    (   catch(call_ex(Module, Setup), E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(error, plunit(error(condition, Context, E))),
            fail
        )
    ;   fail
    ).
setup(_,_,_).

%!  call_ex(+Module, +Goal)
%
%   Call Goal in Module after applying goal expansion.

call_ex(Module, Goal) :-
    Module:(expand_goal(Goal, GoalEx),
                GoalEx).

%!  cleanup(+Module, +Options) is det.
%
%   Call the cleanup handler and succeed.   Failure  or error of the
%   cleanup handler is reported, but tests continue normally.

cleanup(Module, Options) :-
    option(cleanup(Cleanup), Options, true),
    (   catch(call_ex(Module, Cleanup), E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(warning, E)
        )
    ;   print_message(warning, goal_failed(Cleanup, '(cleanup handler)'))
    ).

success(Unit, Name, Line, Det, _Time, Options) :-
    memberchk(fixme(Reason), Options),
    !,
    (   (   Det == true
        ;   memberchk(nondet, Options)
        )
    ->  progress(Unit, Name, nondet),
        Ok = passed
    ;   progress(Unit, Name, fixme),
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
    ->  progress(Unit, Name, passed)
    ;   unit_file(Unit, File),
        print_message(warning, plunit(nondet(File, Line, Name)))
    ).

failure(Unit, Name, Line, _, Options) :-
    memberchk(fixme(Reason), Options),
    !,
    progress(Unit, Name, failed),
    assert(fixme(Unit, Name, Line, Reason, failed)).
failure(Unit, Name, Line, E, Options) :-
    report_failure(Unit, Name, Line, E, Options),
    assert_cyclic(failed(Unit, Name, Line, E)).

%!  assert_cyclic(+Term) is det.
%
%   Assert  a  possibly  cyclic  unit   clause.  Current  SWI-Prolog
%   assert/1 does not handle cyclic terms,  so we emulate this using
%   the recorded database.
%
%   @tbd    Implement cycle-safe assert and remove this.

:- if(swi).
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
:- else.
:- if(sicstus).
:- endif.
assert_cyclic(Term) :-
    assert(Term).
:- endif.


                 /*******************************
                 *            REPORTING         *
                 *******************************/

%!  begin_test(Unit, Test, Line, STO) is det.
%!  end_test(Unit, Test, Line, STO) is det.
%
%   Maintain running/5 and report a test has started/is ended using
%   a =silent= message:
%
%       * plunit(begin(Unit:Test, File:Line, STO))
%       * plunit(end(Unit:Test, File:Line, STO))
%
%   @see message_hook/3 for intercepting these messages

begin_test(Unit, Test, Line, STO) :-
    thread_self(Me),
    assert(running(Unit, Test, Line, STO, Me)),
    unit_file(Unit, File),
    print_message(silent, plunit(begin(Unit:Test, File:Line, STO))).

end_test(Unit, Test, Line, STO) :-
    thread_self(Me),
    retractall(running(_,_,_,_,Me)),
    unit_file(Unit, File),
    print_message(silent, plunit(end(Unit:Test, File:Line, STO))).

%!  running_tests is det.
%
%   Print the currently running test.

running_tests :-
    running_tests(Running),
    print_message(informational, plunit(running(Running))).

running_tests(Running) :-
    findall(running(Unit:Test, File:Line, STO, Thread),
            (   running(Unit, Test, Line, STO, Thread),
                unit_file(Unit, File)
            ), Running).


%!  current_test(?Unit, ?Test, ?Line, ?Body, ?Options)
%
%   True when a test with the specified properties is loaded.

current_test(Unit, Test, Line, Body, Options) :-
    current_unit(Unit, Module, _Supers, _UnitOptions),
    Module:'unit test'(Test, Line, Options, Body).

%!  check_for_test_errors is semidet.
%
%   True if there are no errors, otherwise false.

check_for_test_errors :-
    number_of_clauses(failed/4, Failed),
    number_of_clauses(failed_assertion/7, FailedAssertion),
    number_of_clauses(sto/4, STO),
    Failed+FailedAssertion+STO =:= 0.     % fail on errors


%!  report is det.
%
%   Print a summary of the tests that ran.

report :-
    number_of_clauses(passed/5, Passed),
    number_of_clauses(failed/4, Failed),
    number_of_clauses(failed_assertion/7, FailedAssertion),
    number_of_clauses(blocked/4, Blocked),
    number_of_clauses(sto/4, STO),
    print_message(silent,
                  plunit(summary(plunit{passed:Passed,
                                        failed:Failed,
                                        failed_assertions:FailedAssertion,
                                        blocked:Blocked,
                                        sto:STO}))),
    (   Passed+Failed+FailedAssertion+Blocked+STO =:= 0
    ->  info(plunit(no_tests))
    ;   Failed+FailedAssertion+Blocked+STO =:= 0
    ->  report_fixme,
        info(plunit(all_passed(Passed)))
    ;   report_blocked,
        report_fixme,
        report_failed_assertions,
        report_failed,
        report_sto,
        info(plunit(passed(Passed)))
    ).

number_of_clauses(F/A,N) :-
    (   current_predicate(F/A)
    ->  functor(G,F,A),
        findall(t, G, Ts),
        length(Ts, N)
    ;   N = 0
    ).

report_blocked :-
    number_of_clauses(blocked/4,N),
    N > 0,
    !,
    info(plunit(blocked(N))),
    (   blocked(Unit, Name, Line, Reason),
        unit_file(Unit, File),
        print_message(informational,
                      plunit(blocked(File:Line, Name, Reason))),
        fail ; true
    ).
report_blocked.

report_failed :-
    number_of_clauses(failed/4, N),
    info(plunit(failed(N))).

report_failed_assertions :-
    number_of_clauses(failed_assertion/7, N),
    info(plunit(failed_assertions(N))).

report_sto :-
    number_of_clauses(sto/4, N),
    info(plunit(sto(N))).

report_fixme :-
    report_fixme(_,_,_).

report_fixme(TuplesF, TuplesP, TuplesN) :-
    fixme(failed, TuplesF, Failed),
    fixme(passed, TuplesP, Passed),
    fixme(nondet, TuplesN, Nondet),
    print_message(informational, plunit(fixme(Failed, Passed, Nondet))).


fixme(How, Tuples, Count) :-
    findall(fixme(Unit, Name, Line, Reason, How),
            fixme(Unit, Name, Line, Reason, How), Tuples),
    length(Tuples, Count).


report_failure(Unit, Name, _, assertion, _) :-
    !,
    progress(Unit, Name, assertion).
report_failure(Unit, Name, Line, Error, _Options) :-
    print_message(error, plunit(failed(Unit, Name, Line, Error))).


%!  test_report(What) is det.
%
%   Produce reports on test results after the run.

test_report(fixme) :-
    !,
    report_fixme(TuplesF, TuplesP, TuplesN),
    append([TuplesF, TuplesP, TuplesN], Tuples),
    print_message(informational, plunit(fixme(Tuples))).
test_report(What) :-
    throw_error(domain_error(report_class, What), _).


                 /*******************************
                 *             INFO             *
                 *******************************/

%!  current_test_set(?Unit) is nondet.
%
%   True if Unit is a currently loaded test-set.

current_test_set(Unit) :-
    current_unit(Unit, _Module, _Context, _Options).

%!  unit_file(+Unit, -File) is det.
%!  unit_file(-Unit, +File) is nondet.

unit_file(Unit, File) :-
    current_unit(Unit, Module, _Context, _Options),
    current_module(Module, File).
unit_file(Unit, PlFile) :-
    nonvar(PlFile),
    test_file_for(TestFile, PlFile),
    current_module(Module, TestFile),
    current_unit(Unit, Module, _Context, _Options).


                 /*******************************
                 *             FILES            *
                 *******************************/

%!  load_test_files(+Options) is det.
%
%   Load .plt test-files related to loaded source-files.

load_test_files(_Options) :-
    (   source_file(File),
        file_name_extension(Base, Old, File),
        Old \== plt,
        file_name_extension(Base, plt, TestFile),
        exists_file(TestFile),
        (   test_file_for(TestFile, File)
        ->  true
        ;   load_files(TestFile,
                       [ if(changed),
                         imports([])
                       ]),
            asserta(test_file_for(TestFile, File))
        ),
        fail ; true
    ).



                 /*******************************
                 *           MESSAGES           *
                 *******************************/

%!  info(+Term)
%
%   Runs print_message(Level, Term), where Level  is one of =silent=
%   or =informational= (default).

info(Term) :-
    message_level(Level),
    print_message(Level, Term).

progress(Unit, Name, Result) :-
    print_message(information, plunit(progress(Unit, Name, Result))).

message_level(Level) :-
    current_test_flag(test_options, Options),
    option(silent(Silent), Options, false),
    (   Silent == false
    ->  Level = informational
    ;   Level = silent
    ).

locationprefix(File:Line) -->
    !,
    [ url(File:Line), ':\n\t' ].
locationprefix(test(Unit,_Test,Line)) -->
    !,
    { unit_file(Unit, File) },
    locationprefix(File:Line).
locationprefix(unit(Unit)) -->
    !,
    [ 'PL-Unit: unit ~w: '-[Unit] ].
locationprefix(FileLine) -->
    { throw_error(type_error(locationprefix,FileLine), _) }.

:- discontiguous
    message//1.
:- '$hide'(message//1).

message(error(context_error(plunit_close(Name, -)), _)) -->
    [ 'PL-Unit: cannot close unit ~w: no open unit'-[Name] ].
message(error(context_error(plunit_close(Name, Start)), _)) -->
    [ 'PL-Unit: cannot close unit ~w: current unit is ~w'-[Name, Start] ].
message(plunit(nondet(File, Line, Name))) -->
    locationprefix(File:Line),
    [ 'PL-Unit: Test ~w: Test succeeded with choicepoint'- [Name] ].
message(error(plunit(incompatible_options, Tests), _)) -->
    [ 'PL-Unit: incompatible test-options: ~p'-[Tests] ].

                                        % Unit start/end
:- if(swi).
message(plunit(progress(_Unit, _Name, Result))) -->
    [ at_same_line ], result(Result), [flush].
message(plunit(begin(Unit))) -->
    [ 'PL-Unit: ~w '-[Unit], flush ].
message(plunit(end(_Unit))) -->
    [ at_same_line, ' done' ].
:- else.
message(plunit(begin(Unit))) -->
    [ 'PL-Unit: ~w '-[Unit]/*, flush-[]*/ ].
message(plunit(end(_Unit))) -->
    [ ' done'-[] ].
:- endif.
message(plunit(blocked(unit(Unit, Reason)))) -->
    [ 'PL-Unit: ~w blocked: ~w'-[Unit, Reason] ].
message(plunit(running([]))) -->
    !,
    [ 'PL-Unit: no tests running' ].
message(plunit(running([One]))) -->
    !,
    [ 'PL-Unit: running ' ],
    running(One).
message(plunit(running(More))) -->
    !,
    [ 'PL-Unit: running tests:', nl ],
    running(More).
message(plunit(fixme([]))) --> !.
message(plunit(fixme(Tuples))) -->
    !,
    fixme_message(Tuples).

                                        % Blocked tests
message(plunit(blocked(1))) -->
    !,
    [ 'one test is blocked:'-[] ].
message(plunit(blocked(N))) -->
    [ '~D tests are blocked:'-[N] ].
message(plunit(blocked(Pos, Name, Reason))) -->
    locationprefix(Pos),
    test_name(Name),
    [ ': ~w'-[Reason] ].

                                        % fail/success
message(plunit(no_tests)) -->
    !,
    [ 'No tests to run' ].
message(plunit(all_passed(1))) -->
    !,
    [ 'test passed' ].
message(plunit(all_passed(Count))) -->
    !,
    [ 'All ~D tests passed'-[Count] ].
message(plunit(passed(Count))) -->
    !,
    [ '~D tests passed'-[Count] ].
message(plunit(failed(0))) -->
    !,
    [].
message(plunit(failed(1))) -->
    !,
    [ '1 test failed'-[] ].
message(plunit(failed(N))) -->
    [ '~D tests failed'-[N] ].
message(plunit(failed_assertions(0))) -->
    !,
    [].
message(plunit(failed_assertions(1))) -->
    !,
    [ '1 assertion failed'-[] ].
message(plunit(failed_assertions(N))) -->
    [ '~D assertions failed'-[N] ].
message(plunit(sto(0))) -->
    !,
    [].
message(plunit(sto(N))) -->
    [ '~D test results depend on unification mode'-[N] ].
message(plunit(fixme(0,0,0))) -->
    [].
message(plunit(fixme(Failed,0,0))) -->
    !,
    [ 'all ~D tests flagged FIXME failed'-[Failed] ].
message(plunit(fixme(Failed,Passed,0))) -->
    [ 'FIXME: ~D failed; ~D passed'-[Failed, Passed] ].
message(plunit(fixme(Failed,Passed,Nondet))) -->
    { TotalPassed is Passed+Nondet },
    [ 'FIXME: ~D failed; ~D passed; (~D nondet)'-
      [Failed, TotalPassed, Nondet] ].
message(plunit(failed(Unit, Name, Line, Failure))) -->
    { unit_file(Unit, File) },
    locationprefix(File:Line),
    test_name(Name),
    [': '-[] ],
    failure(Failure).
:- if(swi).
message(plunit(failed_assertion(Unit, Name, Line, AssertLoc,
                                _STO, Reason, Goal))) -->
    { unit_file(Unit, File) },
    locationprefix(File:Line),
    test_name(Name),
    [ ': assertion'-[] ],
    assertion_location(AssertLoc, File),
    assertion_reason(Reason), ['\n\t'],
    assertion_goal(Unit, Goal).

assertion_location(File:Line, File) -->
    [ ' at line ~w'-[Line] ].
assertion_location(File:Line, _) -->
    [ ' at ', url(File:Line) ].
assertion_location(unknown, _) -->
    [].

assertion_reason(fail) -->
    !,
    [ ' failed'-[] ].
assertion_reason(Error) -->
    { message_to_string(Error, String) },
    [ ' raised "~w"'-[String] ].

assertion_goal(Unit, Goal) -->
    { unit_module(Unit, Module),
      unqualify(Goal, Module, Plain)
    },
    [ 'Assertion: ~p'-[Plain] ].

unqualify(Var, _, Var) :-
    var(Var),
    !.
unqualify(M:Goal, Unit, Goal) :-
    nonvar(M),
    unit_module(Unit, M),
    !.
unqualify(M:Goal, _, Goal) :-
    callable(Goal),
    predicate_property(M:Goal, imported_from(system)),
    !.
unqualify(Goal, _, Goal).

result(passed)    --> ['.'-[]].
result(nondet)    --> ['+'-[]].
result(fixme)     --> ['!'-[]].
result(failed)    --> ['-'-[]].
result(assertion) --> ['A'-[]].

:- endif.
                                        % Setup/condition errors
message(plunit(error(Where, Context, Exception))) -->
    locationprefix(Context),
    { message_to_string(Exception, String) },
    [ 'error in ~w: ~w'-[Where, String] ].

                                        % STO messages
message(plunit(sto(Unit, Name, Line))) -->
    { unit_file(Unit, File) },
       locationprefix(File:Line),
       test_name(Name),
       [' is subject to occurs check (STO): '-[] ].
message(plunit(sto(Type, Result))) -->
    sto_type(Type),
    sto_result(Result).

                                        % Interrupts (SWI)
:- if(swi).
message(interrupt(begin)) -->
    { thread_self(Me),
      running(Unit, Test, Line, STO, Me),
      !,
      unit_file(Unit, File)
    },
    [ 'Interrupted test '-[] ],
    running(running(Unit:Test, File:Line, STO, Me)),
    [nl],
    '$messages':prolog_message(interrupt(begin)).
message(interrupt(begin)) -->
    '$messages':prolog_message(interrupt(begin)).
:- endif.

test_name(@(Name,Bindings)) -->
    !,
    [ 'test ~w (forall bindings = ~p)'-[Name, Bindings] ].
test_name(Name) -->
    !,
    [ 'test ~w'-[Name] ].

sto_type(sto_error_incomplete) -->
    [ 'Finite trees (error checking): ' ].
sto_type(rational_trees) -->
    [ 'Rational trees: ' ].
sto_type(finite_trees) -->
    [ 'Finite trees: ' ].

sto_result(success(_Unit, _Name, _Line, Det, Time)) -->
    det(Det),
    [ ' success in ~2f seconds'-[Time] ].
sto_result(failure(_Unit, _Name, _Line, How)) -->
    failure(How).

det(true) -->
    [ 'deterministic' ].
det(false) -->
    [ 'non-deterministic' ].

running(running(Unit:Test, File:Line, STO, Thread)) -->
    thread(Thread),
    [ '~q:~q at '-[Unit, Test], url(File:Line) ],
    current_sto(STO).
running([H|T]) -->
    ['\t'], running(H),
    (   {T == []}
    ->  []
    ;   [nl], running(T)
    ).

thread(main) --> !.
thread(Other) -->
    [' [~w] '-[Other] ].

current_sto(sto_error_incomplete) -->
    [ ' (STO: error checking)' ].
current_sto(rational_trees) -->
    [].
current_sto(finite_trees) -->
    [ ' (STO: occurs check enabled)' ].

:- if(swi).
write_term(T, OPS) -->
    ['~@'-[write_term(T,OPS)]].
:- else.
write_term(T, _OPS) -->
    ['~q'-[T]].
:- endif.

expected_got_ops_(Ex, E, OPS, Goals) -->
    ['    Expected: '-[]], write_term(Ex, OPS), [nl],
    ['    Got:      '-[]], write_term(E,  OPS), [nl],
    ( { Goals = [] } -> []
    ; ['       with: '-[]], write_term(Goals, OPS), [nl]
    ).


failure(Var) -->
    { var(Var) },
    !,
    [ 'Unknown failure?' ].
failure(succeeded(Time)) -->
    !,
    [ 'must fail but succeeded in ~2f seconds~n'-[Time] ].
failure(wrong_error(Expected, Error)) -->
    !,
    { copy_term(Expected-Error, Ex-E, Goals),
      numbervars(Ex-E-Goals, 0, _),
      write_options(OPS)
    },
    [ 'wrong error'-[], nl ],
    expected_got_ops_(Ex, E, OPS, Goals).
failure(wrong_answer(Cmp)) -->
    { Cmp =.. [Op,Answer,Expected],
      !,
      copy_term(Expected-Answer, Ex-A, Goals),
      numbervars(Ex-A-Goals, 0, _),
      write_options(OPS)
    },
    [ 'wrong answer (compared using ~w)'-[Op], nl ],
    expected_got_ops_(Ex, A, OPS, Goals).
failure(wrong_answer(CmpExpected, Bindings)) -->
    { (   CmpExpected = all(Cmp)
      ->  Cmp =.. [_Op1,_,Expected],
          Got = Bindings,
          Type = all
      ;   CmpExpected = set(Cmp),
          Cmp =.. [_Op2,_,Expected0],
          sort(Expected0, Expected),
          sort(Bindings, Got),
          Type = set
      )
    },
    [ 'wrong "~w" answer:'-[Type] ],
    [ nl, '    Expected: ~q'-[Expected] ],
    [ nl, '       Found: ~q'-[Got] ].
:- if(swi).
failure(cmp_error(_Cmp, Error)) -->
    { message_to_string(Error, Message) },
    [ 'Comparison error: ~w'-[Message] ].
failure(Error) -->
    { Error = error(_,_),
      !,
      message_to_string(Error, Message)
    },
    [ 'received error: ~w'-[Message] ].
:- endif.
failure(Why) -->
    [ '~p~n'-[Why] ].

fixme_message([]) --> [].
fixme_message([fixme(Unit, _Name, Line, Reason, How)|T]) -->
    { unit_file(Unit, File) },
    fixme_message(File:Line, Reason, How),
    (   {T == []}
    ->  []
    ;   [nl],
        fixme_message(T)
    ).

fixme_message(Location, Reason, failed) -->
    [ 'FIXME: ~w: ~w'-[Location, Reason] ].
fixme_message(Location, Reason, passed) -->
    [ 'FIXME: ~w: passed ~w'-[Location, Reason] ].
fixme_message(Location, Reason, nondet) -->
    [ 'FIXME: ~w: passed (nondet) ~w'-[Location, Reason] ].


write_options([ numbervars(true),
                quoted(true),
                portray(true),
                max_depth(100),
                attributes(portray)
              ]).

:- if(swi).

:- multifile
    prolog:message/3,
    user:message_hook/3.

prolog:message(Term) -->
    message(Term).

%       user:message_hook(+Term, +Kind, +Lines)

user:message_hook(make(done(Files)), _, _) :-
    make_run_tests(Files),
    fail.                           % give other hooks a chance

:- endif.

:- if(sicstus).

user:generate_message_hook(Message) -->
    message(Message),
    [nl].                           % SICStus requires nl at the end

%!  user:message_hook(+Severity, +Message, +Lines) is semidet.
%
%   Redefine printing some messages. It appears   SICStus has no way
%   to get multiple messages at the same   line, so we roll our own.
%   As there is a lot pre-wired and   checked in the SICStus message
%   handling we cannot reuse the lines. Unless I miss something ...

user:message_hook(informational, plunit(begin(Unit)), _Lines) :-
    format(user_error, '% PL-Unit: ~w ', [Unit]),
    flush_output(user_error).
user:message_hook(informational, plunit(end(_Unit)), _Lines) :-
    format(user, ' done~n', []).

:- endif.
