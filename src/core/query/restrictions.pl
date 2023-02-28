:- module('query/restrictions', [
              run_restriction_named/3
          ]).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(query/algebra)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).

run_restriction_named(_Db, _Name, _Results) :-
    true.

run_restriction(Db, Dictionary, Results),
get_dict('@having', Dictionary, Restriction) =>
    run_restriction_rule(Db, Restriction, Results).


run_restriction_rule(_Db, _Restriction, _Results) :-
    true.
