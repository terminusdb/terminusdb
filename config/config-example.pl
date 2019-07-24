:- module(config,[
              http_blob_user_auth/1,
              http_blob_pass_auth/1,
              server_name/1,
              server_port/1,
              server_workers/1,
              server_worker_options/1,
              http_options/1
          ]).

http_blob_user_auth('a username').
http_blob_pass_auth('A password').

root_passwd_hash('63a9f0ea7bb98050796b649e85481845').

server_name('http://localhost/dcog/').

server_port(6363).

server_workers(8).

server_worker_options([]).

http_options([]).

:- set_prolog_flag(stack_limit, 2_147_483_648).

% Turn off mavis 
:- set_prolog_flag(optimise, true).
