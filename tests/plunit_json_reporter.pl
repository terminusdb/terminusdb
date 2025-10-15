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
    setup_call_cleanup(
        (   stream_property(OldErr, alias(user_error)),
            set_stream(TmpStream, alias(user_error))
        ),
        run_tests(Spec),
        (   flush_output(TmpStream),
            close(TmpStream),
            set_stream(OldErr, alias(user_error))
        )
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
    
    % Parse individual test lines
    % Format: % [1/3] sample_tests:simple_pass ... passed (0.002 sec)
    findall(
        TestObj,
        (   member(Line, Lines),
            sub_string(Line, _, _, _, "] "),
            sub_string(Line, _, _, _, ":"),
            parse_test_line(Line, TestObj)
        ),
        TestObjects
    ),
    
    % Separate passed and failed tests
    include(is_passed_test, TestObjects, PassObjects),
    include(is_failed_test, TestObjects, FailObjects),
    
    % Get counts
    length(TestObjects, Tests),
    length(PassObjects, Passes),
    length(FailObjects, Failures),
    
    % Count test suites by counting "End unit" lines
    findall(
        1,
        (member(L, Lines), sub_string(L, _, _, _, "End unit")),
        SuiteList
    ),
    length(SuiteList, Suites).

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
