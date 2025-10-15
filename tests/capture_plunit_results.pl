#!/usr/bin/env swipl

/** Capture PL-Unit test results with timing information
 * 
 * Outputs JSON with detailed test results for benchmarking.
 * Usage: swipl -g run_and_report -t halt capture_plunit_results.pl
 */

:- use_module(library(plunit)).
:- use_module(library(http/json)).

% Load the main test entry point
:- consult('../src/interactive.pl').

% Run tests and output JSON
run_and_report :-
    % Redirect test output to stderr so JSON goes to stdout
    set_test_options([silent(true)]),
    
    get_time(StartTime),
    
    % Run all tests and capture results
    run_tests(_Tests, [silent(true)]),
    
    get_time(EndTime),
    TotalTime is (EndTime - StartTime) * 1000, % Convert to ms
    
    % Get test statistics
    test_report(json),
    
    % Simple JSON output with timing
    format('~n{"total_time_ms": ~w}~n', [TotalTime]),
    
    halt(0).
