:- module(test_utils,[
              try/1,
              status_200/1,
              curl_json/2,
              report_curl_command/1,
              admin_pass/1,
              with_temp_store/1
          ]).

/** <module> Test Utilities
 *
 * Utils to assist in testing.
 *
 * Printing during tests goes through two pipelines.
 *
 * Actual test output is sent to print_message with a **Kind** of
 * `testing`. Use test_format/3 for most things.
 *
 * progress of testing is reported to `debug/3` with a topic of
 * `terminus(testing_progress(Msg))`, where `Msg` in
 * `[run, error, fail]`
 *
 * Debug output should go through `debug/3`
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

:- use_module(library(utils)).
:- use_module(library(file_utils)).
:- use_module(library(triplestore)).
:- use_module(library(terminus_store)).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- meta_predicate test_format(:, +, +).

%!  test_format(+Goal:callable, +Format:text, +Args:list) is det
%
%   print the message formed as in [[format/2]] for message from
%   test Goal.
%
%   @arg Goal a callable term, the argument to [[try/1]], name of our
%   test
%   @arg Format string (an atom) as in [[format/2]]
%   @arg Args list of arguments for the format string
%
test_format(Goal, Format, Args) :-
    print_message(testing, test_format(Goal, Format, Args)).

:- multifile prolog:message//1.

prolog:message(test_format(Goal, Format, Args)) -->
    [
           '~Ntest ~q:'-[Goal],
           Format-Args
       ].

:- meta_predicate try(0).

%!  try(+Goal:callable) is semidet
%
%   calls `Goal` as once, writing debug information,
%
try(Goal) :-
    test_format(Goal, '~N* Running test ~q', [Goal]),
    debug(terminus(testing_progress(run)), 'running ~q', [Goal]),
    (   catch(Goal, Error, true)
    ->  (   var(Error)
        ->  true
        ;   test_format(Goal, '~N+ ERROR! Could not successfully run ~q: ~q',[Goal,Error]),
            debug(terminus(testing_progress(error)), 'ERROR! Could not successfully run ~q: ~q',[Goal,Error]),
            fail
        )
    ;
        test_format(Goal, '~N+ FAIL! Could not successfully run ~q',[Goal]),
        debug(terminus(testing_progress(fail)), 'FAIL! Could not successfully run ~q',[Goal]),
        fail
    ).

write_arg(Arg) :-
    string(Arg),
    !,
    writeq(Arg).
write_arg(Arg) :-
    re_match('.*:.*', Arg),
    !,
    writeq(Arg).
write_arg(Arg) :-
    re_match('[^ ]+ ', Arg),
    !,
    format('"~s"',Arg).
write_arg(Arg) :-
    atom(Arg),
    !,
    write(Arg).

write_args(Args) :-
    intersperse(' ', Args,Spaced),
    !,
    maplist(write_arg,Spaced),
    format('~n',[]).

report_curl_command(Args) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, predicate_indicator, PredIndicator),
    with_output_to(string(ArgStr), write_args(Args)),
    test_format(PredIndicator, '~NRunning command: curl ~w',[ArgStr]).

status_200(URL) :-
    http_open(URL, _, [status_code(200)]).


/*
 * curl_json(+Args,-JSON) is semidet.
 */
curl_json(Args,JSON) :-
    terminus_path(Path),
    process_create(path(curl), Args,
                   [ stdout(pipe(Out)),
                     stderr(std),
                     process(PID),
                     cwd(Path)
                   ]),

    process_wait(PID,Status),

    (   Status=killed(Signal)
    ->  interpolate(["curl killed with signal ",Signal], M),
        format('~n~s~n', M),
        fail
    ;   true),

    catch(json_read_dict(Out, JSON),
          _,
          JSON = _{'terminus:status' : 'terminus:failure'}),

    close(Out).

/*
 * admin_pass(+Pass) is det.
 *
 * Get the administrator password for testing from the environment,
 * or try the default ('root')
 */
admin_pass(Pass) :-
    (   getenv('TERMINUS_ADMIN_PASSWD', Pass)
    ->  true
    ;   Pass='root').

:- meta_predicate with_temp_store(:).
with_temp_store(Goal) :-
    tmp_file(temporary_terminus_store, Dir),
    make_directory(Dir),
    open_directory_store(Dir, Store),

    (   catch_with_backtrace(with_triple_store(Store, Goal),
                             E,
                             true)
    ->  Success = true
    ;   Success = false),
    delete_directory_and_contents(Dir),
    (   var(E)
    ->  Success = true
    ;   throw(E)).
