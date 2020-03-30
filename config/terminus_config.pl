:- module(config,[
              server/1,
              server_name/1,
              server_port/1,
              public_url/1,
              worker_amount/1,
              max_transaction_retries/1,
              server_worker_options/1,
              http_options/1,
              max_journal_queue_length/1
          ]).

:- use_module(core(util/utils)).


server_name(Value) :-
    (   getenv('TERMINUS_SERVER_NAME', Value)
    ->  true
    ;   Value = 'http://localhost').

server_port(Value) :-
    (   getenv_number('TERMINUS_SERVER_PORT', Value)
    ->  true
    ;   Value = 6363).

public_url(Value) :-
    (   getenv('TERMINUS_SERVER_PUBLIC_URL', Value)
    ->  true
    ;   Value = 'http://localhost:6363').

worker_amount(Value) :-
    (   getenv_number('TERMINUS_SERVER_WORKERS', Value)
    ->  true
    ;   Value = 8).

max_transaction_retries(Value) :-
    (   getenv_number('TERMINUS_SERVER_MAX_TRANSACTION_RETRIES', Value)
    ->  true
    ;   Value = 3).


server(Server) :-
    server_name(Name),
    server_port(Port),
    atomic_list_concat([Name,':',Port],Server).

server_worker_options([]).

http_options([]).

% this number can never be less than 4 or bad things will happen.
max_journal_queue_length(30).

:- set_prolog_flag(stack_limit, 2_147_483_648).

% Turn off mavis
:- set_prolog_flag(optimise, true).
