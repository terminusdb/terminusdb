:- module(config,[
              default_envs/0,
              server/1,
              server_worker_options/1,
              http_options/1,
              max_journal_queue_length/1
          ]).

:- use_module(core(util/utils)).


default_envs :-
    setenv_conditional("TERMINUS_SERVER_NAME", 'http://localhost'),
    setenv_conditional("TERMINUS_SERVER_PORT", 6363),
    setenv_conditional("TERMINUS_SERVER_PUBLIC_URL", 'http://localhost:6363'),
    setenv_conditional("TERMINUS_SERVER_MAX_TRANSACTION_RETRIES", 3),
    setenv_conditional("TERMINUS_SERVER_WORKERS", 8).


server(Server) :-
    getenv("TERMINUS_SERVER_NAME", Name),
    getenv("TERMINUS_SERVER_PORT", Port),
    atomic_list_concat([Name,':',Port],Server).

server_worker_options([]).

http_options([]).

% this number can never be less than 4 or bad things will happen.
max_journal_queue_length(30).

:- set_prolog_flag(stack_limit, 2_147_483_648).

% Turn off mavis
:- set_prolog_flag(optimise, true).
