:- module(test_utils,[
              try/1,
              status_200/1,
              curl_json/2,
              report_curl_command/1,
              admin_pass/1
          ]).
                 
/** <module> Test Utilities
 * 
 * Utils to assist in testing.
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
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- meta_predicate try(0).
try(Goal) :- 
    format('~n*****************************************************',[]),
    format('~n* Running test ~q', [Goal]),
    format('~n*****************************************************~n',[]),
    (   catch(Goal, Error, true)
    ->  (   var(Error)
        ->  true
        ;   format('~n+++++++++++++++++++++++++++++++++++++++++++++++++++++', []),
            format('~n+ ERROR! Could not successfully run ~q: ~q',[Goal,Error]),
            format('~n+++++++++++++++++++++++++++++++++++++++++++++++++++++~n', []),
            fail
        )
    ;   format('~n+++++++++++++++++++++++++++++++++++++++++++++++++++++', []),
        format('~n+ FAIL! Could not successfully run ~q',[Goal]),
        format('~n+++++++++++++++++++++++++++++++++++++++++++++++++++++~n', []),
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
    format('~nRunning command: curl ',[]),
    write_args(Args).

status_200(URL) :-
    http_open(URL, _, [status_code(200)]).


/*
 * curl_json(+Args,-JSON) is semidet. 
 */
curl_json(Args,JSON) :-
    process_create(path(curl), Args,
                   [ stdout(pipe(Out)),
                     stderr(null),
                     process(PID)
                   ]),
    
    process_wait(PID,Status),
    
    (   Status=killed(Signal)
    ->  interpolate(["curl killed with signal ",Signal], M),
        format('~n~s~n', M),
        fail
    ;   true),

    json_read_dict(Out, JSON),
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
