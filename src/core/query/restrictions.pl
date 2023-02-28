:- module('query/restrictions', [
              run_restriction_named/4
          ]).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(query/algebra)).
:- use_module(core(query/constraints)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).


interpret_restriction(and(A,B),DB) :-
    interpret_restriction(A,DB),
    interpret_restriction(B,DB).
interpret_restriction(or(A,_),DB) :-
    interpret_restriction(A, DB).
interpret_restriction(or(_,B), DB) :-
    interpret_restriction(B, DB).
interpret_restriction(impl(A,B), DB) :-
    (   interpret_restriction(A, DB)
    *-> interpret_restriction(B, DB)
    ;   true
    ).
interpret_restriction(op(Op,A,B),_) :-
    effective_operator(Op, Operator),
    unannotated(A, ARaw),
    unannotated(B, BRaw),
    call(Operator, ARaw, BRaw).
interpret_restriction(t(A,P,C), DB) :-
    DB = database(Schema,Instance),
    database_schema_prefixes(Schema,Prefixes),
    prefix_expand_schema(P, Prefixes, PEx),
    xrdf(Instance, A, Pex, C).
interpret_restriction(isa(X,C), DB) :-
    DB = database(Schema,Instance),
    database_schema_prefixes(Schema,Prefixes),
    prefix_expand_schema(C, Prefixes, CEx),
    global_prefix_expand(rdf:type, RDF_Type),
    xrdf(Instance, X, RDF_Type, CEx).

compile_restriction(Dictionary, Subject, Expression),
is_dict(Dictionary),
get_dict('@having', Dictionary, Having) =>
    compile_constraint_rule(Having, Subject, Expression, _{}, _).
compile_restriction(Dictionary, Subject, Expression),
is_dict(Dictionary),
get_dict('@anyOf', Dictionary, Restrictions) =>
    maplist({Subject}/[Restriction, Expression]
            >>compile_restriction(Restriction, Expression _{}, _),
            Restrictions,
            Expressions),
    termlist_disjunction(Expresssions, Expression).

run_restriction_named(Schema, Instance, Name, Results) :-
    database_schema_context_object(Schema, Context),
    get_dict('@metadata', Context, Metadata),
    get_dict('restrictions', Metadata, Restrictions),
    mapconv({Name}/[Restriction,Restriction]>>get_dict('_id', Restriction, Name),
            Restrictions,
            [Named]),
    compile_restriction(Named, Subject, Expression),
    findall(Subject,
            interpret_restriction(Expression, database(Schema, Instance)),
            Results).
