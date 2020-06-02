:- module(db_unpack, [
              unpack/2
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).


unpack(Branch, Payload) :-
    % 1. Deconstruct Payload, as head and tgz
    % 2. look at the branch, make sure it exists
    % 3. linear future for current repository head
    % 4. unpack, advance repository head

    true.
