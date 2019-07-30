:- module(schema, [
              cleanup_schema_module/1,
              compile_schema_to_module/2,
              ensure_schema_in_module/2
          ]).

/** <module> Schema 
 * 
 * The schema module deals with compilation and clearing of ontology 
 * compilation to predicates. 
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(prefixes, [global_prefix_expand/2]). 
:- use_module(collection).
:- use_module(triplestore).
:- use_module(schema_definitions).
:- use_module(schema_util).
:- use_module(utils).

:- reexport(schema_util).

cleanup_schema_module(Module) :-
    forall(schema_predicate(Predicate/Arity),
           (   functor(Functor, Predicate, Arity),
               Module:retractall(Functor))).

capitalize(Atom, Capitalized) :-
    atom_chars(Atom, [First|Rest]),
    upcase_atom(First, First_Upper),
    atom_chars(Capitalized, [First_Upper|Rest]).

compile_schema_type_to_module(Collection, Schema, Namespace, Type, Module) :-
    Module:dynamic(Type/1),
    capitalize(Type, Capitalized_Type),
    global_prefix_expand(Namespace:Capitalized_Type, Prefixed_Type),
    global_prefix_expand(rdf:type, RDF_TYPE),
    forall(xrdf(Collection, Schema, Thing, RDF_TYPE, Prefixed_Type),
           (   X =.. [Type, Thing],
               asserta(Module:X))).

compile_schema_types_to_module(Collection, Schema, Namespace, Types, Module) :-
    forall(member(Type, Types),
           compile_schema_type_to_module(Collection, Schema, Namespace, Type, Module)).
    

compile_schema_types_to_module(Collection, Schema, Module) :-
    rdfs_types(Rdfs_Types),
    compile_schema_types_to_module(Collection, Schema, rdfs, Rdfs_Types, Module),

    owl_types(Owl_Types),
    compile_schema_types_to_module(Collection, Schema, owl, Owl_Types, Module).

compile_schema_basic_property_to_module(Collection, Schema, Property_Namespace, Property_Name, Module) :-
    Module:dynamic(Property_Name/2),
    forall((   global_prefix_expand(Property_Namespace:Property_Name,Prop),
               xrdf(Collection, Schema, Subject, Prop, Object)),
           (   X =.. [Property_Name, Subject, Object],
               asserta(Module:X))).

basic_property(Namespace, Property) :-
    (   rdfs_properties(Properties),
        Namespace = rdfs,
        member(Property, Properties)
    ;   owl_basic_properties(Properties),
        Namespace = owl,
        member(Property, Properties)).

compile_schema_basic_properties_to_module(Collection, Schema, Module) :-
    forall(basic_property(Namespace, Property),
           compile_schema_basic_property_to_module(Collection, Schema, Namespace, Property, Module)).

rdf_prolog_list(Collection, Graph_Id, Object, [Head|Tail]) :-
    global_prefix_expand(rdf:first,RDF_First),
    global_prefix_expand(rdf:rest,RDF_Rest),
    global_prefix_expand(rdf:nil,RDF_Nil),
    once(xrdf(Collection, Graph_Id, Object, RDF_First, Head)),
    (   once(xrdf(Collection, Graph_Id, Object, RDF_Rest, RDF_Nil))
    ->  Tail=[]
    ;   once(xrdf(Collection, Graph_Id, Object, RDF_Rest, Rest)),
        rdf_prolog_list(Collection, Graph_Id, Rest, Tail)).

compile_schema_list_property_to_module(Collection, Schema, Namespace, Property, Module) :-
    Module:dynamic(Property/2),
    global_prefix_expand(Namespace:Property,Prop),              
    forall(xrdf(Collection, Schema, Subject, Prop, Object),
           (   rdf_prolog_list(Collection, Schema, Object, Options),
               X =.. [Property, Subject, Options],
               asserta(Module:X))).

compile_schema_list_properties_to_module(Collection, Schema, Module) :-
    % as far as I can tell, only four owl properties have a list as range
    owl_list_properties(Properties),
    forall(member(Property, Properties),
           compile_schema_list_property_to_module(Collection, Schema, owl, Property, Module)).

add_metadata_to_module(Graph, Module) :-
    graph_collection(Graph, Collection),
    graph_schema(Graph, Schema),
    Module:asserta(schema(Collection, Schema)).

precalculate_subsumptions(Graph, Module) :-
    Module:dynamic(subsumptionOf/2),
    unique_solutions(CC-CP,
                     (   schema_util:calculate_subsumptionOf(CC,CP,Graph),
                         ground(CC),
                         ground(CP)),
                     Pairs),

    forall(member(CC-CP, Pairs),
           asserta(Module:subsumptionOf(CC, CP))).

compile_util_predicates(Module) :-
    util_predicates(Predicates),
    forall(member(Predicate/Arity, Predicates),
           (   functor(Fn, Predicate, Arity),
               Fn =.. [_|Arguments],
               append(Arguments, [Module], Module_Arguments),
               Inner_Fn =.. [Predicate|Module_Arguments],
               Definition = (Fn :- schema_util:Inner_Fn),
               debug(schema,'~q~n',[Definition]),
               Module:asserta(Definition))).

compile_schema_to_module(Graph, Module) :-
    % use the transaction mutex to ensure that no transaction is running while a schema module is updated.
    with_mutex(transaction,
               (   graph_collection(Graph, Collection),
                   graph_schema(Graph, Schema),                   
                   cleanup_schema_module(Module),
                   compile_schema_types_to_module(Collection, Schema, Module),
                   compile_schema_basic_properties_to_module(Collection, Schema, Module),
                   compile_schema_list_properties_to_module(Collection, Schema, Module),
                   precalculate_subsumptions(Graph, Module),
                   compile_util_predicates(Module),
                   add_metadata_to_module(Graph, Module))).


schema_defined(Module) :-
    % a schema is defined when all the expected metadata predicates are there
    metadata_predicates(Predicates),
    forall(member(P, Predicates), current_predicate(Module:P)).

schema_metadata(Module, Collection, Schema) :-
    Module:schema(Collection, Schema).

/* 
 * ensure_schema_in_module(+Graph,-Module) is det.
 */ 
ensure_schema_in_module(Graph, Module) :-
    graph_module(Graph, Module),            
    graph_collection(Graph, Collection),
    graph_schema(Graph, Schema),
    (   (   schema_defined(Module),
            schema_metadata(Module, Collection, Schema))
    ->  true
    ;   compile_schema_to_module(Graph, Module)).
