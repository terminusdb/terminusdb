:- module(schema_util, [calculate_subsumptionOf/3,
                        entity/2]).
:- use_module(graph_management).
:- use_module(schema_definitions).


calculate_subsumptionOf(CC, CP, Graph) :-
    is_graph(Graph),
    !,
    graph_schema(Graph, Schema),
    calculate_subsumptionOf(CC, CP, Schema).

calculate_subsumptionOf(CC, CC, Module) :-
    Module:class(CC).
calculate_subsumptionOf(CC, CP, Module) :-
    Module:subClassOf(CC, CZ),
    calculate_subsumptionOf(CZ, CP, Module).
calculate_subsumptionOf(CC, CP, Module) :-
    Module:class(CC),
    Module:unionOf(CZ, CC),
    calculate_subsumptionOf(CZ,CP,Module).
calculate_subsumptionOf(CC, CP, Module) :-
    Module:class(CC),
    Module:disjointUnionOf(CZ, CC),
    calculate_subsumptionOf(CZ, CP, Module).
calculate_subsumptionOf(CC, CP, Module) :-
    Module:class(CC),
    Module:intersectionOf(CC, CZ),
    calculate_subsumptionOf(CZ, CP, Module).
calculate_subsumptionOf(CC, CP, Module) :-
    % the tbox version does extra work in anonymousEquivalentClass, but why?
    Module:equivalentClass(CC, CZ),
    calculate_subsumptionOf(CZ, CP, Module).
calculate_subsumptionOf(_,'http://www.w3.org/2002/07/owl#Thing',_).
calculate_subsumptionOf('http://www.w3.org/2002/07/owl#Nothing',_,_).

term_expansion(generate_owl_predicates, Terms) :-
    findall(Predicate,
            schema_definitions:schema_predicate(Predicate),
            Predicates),

    schema_definitions:util_predicates(Utils),

    findall(Pair,
            (   member(Predicate/Arity, Predicates),
                \+ member(Predicate/Arity, Utils),
                functor(Inner_Fn, Predicate, Arity),
                Inner_Fn =.. [_|Inner_Arguments],
                append(Inner_Arguments, [Module], Arguments),
                append(Inner_Arguments, [Schema], GraphVersion_Arguments),
                Fn =.. [Predicate|Arguments],
                Graph_Fn =.. [Predicate|GraphVersion_Arguments],
                Definition1 = (Fn :- is_graph(Module),
                                     !,
                                     graph_schema(Module, Schema),
                                     Graph_Fn),
                Definition2 = (Fn :- Module:Inner_Fn),
                Pair = [Definition1, Definition2],
                Arity_Plus1 is Arity + 1,
                export(Predicate/Arity_Plus1)
            ),
            Pairs),
    flatten(Pairs, Terms).

generate_owl_predicates.

entity(Class, Graph) :-
    is_graph(Graph),
    !,
    graph_schema(Graph, Schema),
    Schema:subsumptionOf(Class, 'https://datachemist.net/ontology/dcog#Entity').

entity(Class, Module) :-
    Module:subsumptionOf(Class, 'https://datachemist.net/ontology/dcog#Entity').
