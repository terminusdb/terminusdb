#!/usr/bin/env swipl

:- use_module(library(http/http_server)).
:- use_module(library(http/http_log)).
:- use_module(library(process)).

:- initialization(main).

term_expansion((:- initialization(_)), []).

:- load_files("db_util").

:- http_handler(root(.), welcome_screen, [methods([get])]).
:- http_handler(root(submit), server_catch(welcome_screen_submit), [methods([post])]).

:- meta_predicate cors_catch(1,?).
server_catch(Goal, Request) :-
    call(Goal, Request),
    current_prolog_flag(pid, PID),
    process_kill(PID).

welcome_screen(Request):-
    http_reply_file('./welcome.html', [], Request).

welcome_screen_submit(Request) :-
    http_parameters(Request, [password(Password, []),
                              port(Port_Arg, []),
                              public_url(Public_URL, [])]),
    Args = ['db_util', '--public_url', Public_URL, '--port ', Port_Arg, '-k ', Password],
    process_create(path(swipl), Args, [process(PID)]),
    process_release(PID),
    reply_html_page(title('Initialized'),
                    [ h1('Succesfully initialized. Close the server now')
                    ]).

main([]) :-
   http_server([port(6363)]).
main([Port_Arg]) :-
   atom_number(Port_Arg, Port),
   assertz(port(Port)),
   http_server([port(Port)]).
