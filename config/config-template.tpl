:- module(config,[
              server/1,
              server_name/1,
              server_port/1,
              server_workers/1,
              server_worker_options/1,
              http_options/1,
              max_journal_queue_length/1
          ]).

server_name('http://~s').
server_port(~d).

server(Server) :-
    server_name(Name),
	server_port(Port),
	atomic_list_concat([Name,':',Port],Server).

server_workers(~d).
server_worker_options([]).

http_options([]).

max_journal_queue_length(30).

:- set_prolog_flag(stack_limit, 2_147_483_648).

% Turn off mavis 
:- set_prolog_flag(optimise, true).
