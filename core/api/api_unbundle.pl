:- module(api_unbundle, [unbundle/5]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

unbundle(_System_DB, _Auth, _Path, _Branch_Target, Payload) :-
    throw(error(unimplemented)).
