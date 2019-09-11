:- module(config,[
              http_blob_user_auth/1,
              http_blob_pass_auth/1,
              server_name/1,
              server_port/1,
              server_workers/1,
              server_worker_options/1,
              http_options/1
          ]).

server_name('http://~s').
server_port(~d).

server_workers(~d).
server_worker_options([]).

http_options([]).

:- set_prolog_flag(stack_limit, 2_147_483_648).

% Turn off mavis 
:- set_prolog_flag(optimise, true).
