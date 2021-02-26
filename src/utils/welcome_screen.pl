#!/usr/bin/env swipl

:- use_module(library(http/http_server)).
:- use_module(library(http/http_log)).
:- use_module(library(http/js_write)).
:- use_module(library(process)).

:- initialization(main).

term_expansion((:- initialization(_)), []).

:- load_files("db_init").

:- http_handler(root(.), welcome_screen, [methods([get])]).
:- http_handler(root(submit), server_catch(welcome_screen_submit), [methods([post])]).

:- meta_predicate server_catch(1,?).
server_catch(Goal, Request) :-
    call(Goal, Request),
    current_prolog_flag(pid, PID),
    alarm(2, process_kill(PID), _).

welcome_screen(Request):-
    http_reply_file('./welcome.html', [], Request).

welcome_screen_submit(Request) :-
    http_parameters(Request, [password(Password, []),
                              port(Port_Arg, []),
                              public_url(Public_URL, [])]),
    Args = ['db_init', '--port ', Port_Arg, '-k ', Password],
    process_create(path(swipl), Args, [process(PID)]),
    process_release(PID),
    atom_concat(Public_URL, '/console', Redirect_URL),
    reply_html_page(title('Initialized'),
                    [ h1('Succesfully initialized. Redirecting to console, wait a bit.'),
                      script({|javascript(Redirect_URL)||
                               setTimeout(function(){
                                              window.location.replace(Redirect_URL);
                                          }, 4000);
                        |})
                    ]).

main([]) :-
   http_server([port(6363)]).
main([Port_Arg]) :-
   atom_number(Port_Arg, Port),
   assertz(port(Port)),
   http_server([port(Port)]).
