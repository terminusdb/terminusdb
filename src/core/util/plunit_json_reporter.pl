:- module(plunit_json_reporter, [run_tests_json/0, run_tests_json/1]).

/** <module> PL-Unit JSON Reporter
 *
 * Runs PL-Unit tests and outputs Mocha-compatible JSON.
 */

:- use_module(library(plunit)).
:- use_module(library(http/json)).

%! run_tests_json is det.
%
% Run all tests, capture output, and produce JSON.
run_tests_json :-
    run_tests_json(_).

%! run_tests_json(+Spec) is det.
%
% Run specific tests and output JSON.
% Spec can be a test unit name, module:test, or _ for all tests.
run_tests_json(Spec) :-
    get_time(StartTime),
    format_time(string(StartISO), '%FT%T%:z', StartTime),
    
    % Capture test output using a temporary file
    % PL-Unit writes to user_error, so we redirect that stream
    tmp_file_stream(text, TmpFile, TmpStream),
    stream_property(OldErr, alias(user_error)),
    catch(
        setup_call_cleanup(
            set_stream(TmpStream, alias(user_error)),
            run_tests(Spec),
            (   flush_output(TmpStream),
                close(TmpStream),
                set_stream(OldErr, alias(user_error))
            )
        ),
        error(existence_error(unit_test, _), _),
        true  % Catch non-existent test suite but continue
    ),
    read_file_to_string(TmpFile, Output, []),
    delete_file(TmpFile),
    
    get_time(EndTime),
    format_time(string(EndISO), '%FT%T%:z', EndTime),
    Duration is round((EndTime - StartTime) * 1000),
    
    % Parse output
    parse_test_output(Output, Tests, Suites, Passes, Failures, TestObjects, PassTests, FailTests),
    
    % Build JSON
    Stats = _{
        suites: Suites,
        tests: Tests,
        passes: Passes,
        pending: 0,
        failures: Failures,
        start: StartISO,
        end: EndISO,
        duration: Duration
    },
    
    Result = _{
        stats: Stats,
        tests: TestObjects,
        pending: [],
        failures: FailTests,
        passes: PassTests
    },
    
    json_write(current_output, Result, [width(0)]),
    nl.

parse_test_output(Output, Tests, Suites, Passes, Failures, TestObjects, PassObjects, FailObjects) :-
    split_string(Output, "\n", "\n", Lines),
    
    % Modern PL-Unit output format: "% All 98 tests passed" or "% 2 tests passed, 1 failed"
    (   member(Line, Lines),
        sub_string(Line, _, _, _, "tests passed"),
        parse_summary_line(Line, Tests, Passes, Failures)
    ->  % Found summary line
        Suites = 1,
        % Create placeholder test objects (we don't have individual test details)
        length(PassList, Passes),
        maplist(create_pass_object, PassList, PassObjects),
        length(FailList, Failures),
        maplist(create_fail_object, FailList, FailObjects),
        append(PassObjects, FailObjects, TestObjects)
    ;   % No summary found - no tests ran
        Tests = 0,
        Passes = 0,
        Failures = 0,
        Suites = 0,
        TestObjects = [],
        PassObjects = [],
        FailObjects = []
    ).

parse_summary_line(Line, Tests, Passes, Failures) :-
    % Parse "% All 98 tests passed" or "% 2 tests passed, 1 failed"
    (   sub_string(Line, _, _, _, "All "),
        split_string(Line, " ", " ", Parts),
        nth0(2, Parts, TestStr),
        atom_number(TestStr, Tests),
        Passes = Tests,
        Failures = 0
    ;   split_string(Line, " ,", " ,", Parts),
        nth0(1, Parts, PassStr),
        nth0(5, Parts, FailStr),
        atom_number(PassStr, Passes),
        atom_number(FailStr, Failures),
        Tests is Passes + Failures
    ).

create_pass_object(_, _{title: "test", fullTitle: "test", duration: 0, currentRetry: 0, speed: fast, err: _{}, state: passed}).
create_fail_object(_, _{title: "test", fullTitle: "test", duration: 0, currentRetry: 0, err: _{message: "Test failed", stack: ""}, state: failed}).

parse_test_line(Line, TestObj) :-
    % Extract: suite:test_name ... passed/failed (X.XXX sec)
    sub_string(Line, AfterBracket, _, _, "] "),
    !,  % Commit to first match
    BracketEnd is AfterBracket + 2,
    sub_string(Line, BracketEnd, _, 0, Rest),
    
    % Split on " ... "
    sub_string(Rest, Before, _, After, " ..."),
    sub_string(Rest, 0, Before, _, FullTitle),
    sub_string(Rest, _, After, 0, ResultPart),
    
    % Extract result and duration
    (   sub_string(ResultPart, _, _, _, " passed (")
    ->  sub_string(ResultPart, DurStart, _, _, " sec)"),
        sub_string(ResultPart, 0, DurStart, _, BeforeSec),
        split_string(BeforeSec, "(", " ", [_, DurStr]),
        atom_string(DurAtom, DurStr),
        atom_number(DurAtom, DurSec),
        Duration is round(DurSec * 1000),
        determine_speed(Duration, Speed),
        TestObj = _{
            title: FullTitle,
            fullTitle: FullTitle,
            duration: Duration,
            currentRetry: 0,
            speed: Speed,
            err: _{},
            state: passed
        }
    ;   % Failed test
        TestObj = _{
            title: FullTitle,
            fullTitle: FullTitle,
            duration: 0,
            currentRetry: 0,
            err: _{message: "Test failed", stack: ""},
            state: failed
        }
    ).

determine_speed(Duration, fast) :- Duration < 50, !.
determine_speed(Duration, medium) :- Duration < 200, !.
determine_speed(_, slow).

is_passed_test(TestObj) :-
    get_dict(state, TestObj, passed).

is_failed_test(TestObj) :-
    get_dict(state, TestObj, failed).
