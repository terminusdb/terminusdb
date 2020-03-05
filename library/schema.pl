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
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(global_prefixes, [global_prefix_expand/2]).
:- use_module(database).
:- use_module(triplestore).
:- use_module(schema_definitions).
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

compile_schema_type_to_module(Database, Schema, Namespace, Type, Module) :-
    Module:dynamic(Type/1),
    capitalize(Type, Capitalized_Type),
    global_prefix_expand(Namespace:Capitalized_Type, Prefixed_Type),
    global_prefix_expand(rdf:type, RDF_TYPE),
    forall(xrdf(Schema, Thing, RDF_TYPE, Prefixed_Type),
           (   X =.. [Type, Thing],
               asserta(Module:X))).

compile_schema_types_to_module(Database, Schema, Namespace, Types, Module) :-
    maybe_open_read_transaction(Database, DB),
    forall(member(Type, Types),
           compile_schema_type_to_module(DB, Schema, Namespace, Type, Module)).


compile_schema_types_to_module(Database, Schema, Module) :-
    rdfs_types(Rdfs_Types),
    compile_schema_types_to_module(Database, Schema, rdfs, Rdfs_Types, Module),

    owl_types(Owl_Types),
    compile_schema_types_to_module(Database, Schema, owl, Owl_Types, Module).

compile_schema_basic_property_to_module(Database, Schema, Property_Namespace, Property_Name, Module) :-
    Module:dynamic(Property_Name/2),
    forall((   global_prefix_expand(Property_Namespace:Property_Name,Prop),
               xrdf(Schema, Subject, Prop, Object)),
           (   X =.. [Property_Name, Subject, Object],
               asserta(Module:X))).

basic_property(Namespace, Property) :-
    (   rdfs_properties(Properties),
        Namespace = rdfs,
        member(Property, Properties)
    ;   owl_basic_properties(Properties),
        Namespace = owl,
        member(Property, Properties)).

compile_schema_basic_properties_to_module(Database, Schema, Module) :-
    maybe_open_read_transaction(Database, DB),
    forall(basic_property(Namespace, Property),
           compile_schema_basic_property_to_module(DB, Schema, Namespace, Property, Module)).

rdf_prolog_list(Database, Database_Id, Object, [Head|Tail]) :-
    global_prefix_expand(rdf:first,RDF_First),
    global_prefix_expand(rdf:rest,RDF_Rest),
    global_prefix_expand(rdf:nil,RDF_Nil),
    once(xrdf(Database_Id, Object, RDF_First, Head)),
    (   once(xrdf(Database_Id, Object, RDF_Rest, RDF_Nil))
    ->  Tail=[]
    ;   once(xrdf(Database_Id, Object, RDF_Rest, Rest)),
        rdf_prolog_list(Database, Database_Id, Rest, Tail)).

compile_schema_list_property_to_module(Database, Schema, Namespace, Property, Module) :-
    Module:dynamic(Property/2),
    global_prefix_expand(Namespace:Property,Prop),
    forall(xrdf(Schema, Subject, Prop, Object),
           (   rdf_prolog_list(Database, Schema, Object, Options),
               X =.. [Property, Subject, Options],
               asserta(Module:X))).

compile_schema_list_properties_to_module(Database, Schema, Module) :-
    % as far as I can tell, only four owl properties have a list as range
    maybe_open_read_transaction(Database, DB),
    owl_list_properties(Properties),
    forall(member(Property, Properties),
           compile_schema_list_property_to_module(DB, Schema, owl, Property, Module)).

add_metadata_to_module(Database, Module) :-
    database_schema(Database, Schema),
    Module:asserta(schema(Database, Schema)).

precalculate_subsumptions(Database, Module) :-
    Module:dynamic(subsumption_of/2),
    unique_solutions(CC-CP,
                     (   schema_util:calculate_subsumption_of(CC,CP,Database),
                         ground(CC),
                         ground(CP)),
                     Pairs),

    forall(member(CC-CP, Pairs),
           asserta(Module:subsumption_of(CC, CP))).

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

compile_schema_to_module(Database, Module) :-
    % use the transaction mutex to ensure that no transaction is running while a schema module is updated.
    with_mutex(transaction,
               (   database_schema(Database, Schema),
                   cleanup_schema_module(Module),
                   compile_schema_types_to_module(Database, Schema, Module),
                   compile_schema_basic_properties_to_module(Database, Schema, Module),
                   compile_schema_list_properties_to_module(Database, Schema, Module),
                   precalculate_subsumptions(Database, Module),
                   compile_util_predicates(Module),
                   add_metadata_to_module(Database, Module))).


schema_defined(Module) :-
    % a schema is defined when all the expected metadata predicates are there
    metadata_predicates(Predicates),
    forall(member(P, Predicates), current_predicate(Module:P)).

schema_metadata(Module, Database, Schema) :-
    Module:schema(Database, Schema).

/*
 * ensure_schema_in_module(+Database,-Module) is det.
 */
ensure_schema_in_module(Database, Module) :-
    database_module(Database, Module),
    database_schema(Database, Schema),
    (   (   schema_defined(Module),
            schema_metadata(Module, Database, Schema))
    ->  true
    ;   compile_schema_to_module(Database, Module)).
