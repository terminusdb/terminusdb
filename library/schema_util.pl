:- module(schema_util, [
              % module construction
              database_module/2,
              collection_schema_module/3,
              % utils
              calculate_subsumptionOf/3,
              document/2
          ]).

/** <module> Schema Util
 * 
 * Utilities to assist in the compilation of schemata.
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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


:- use_module(library(utils), [intersperse/3]).
:- use_module(library(database)).
:- use_module(library(types)).
:- use_module(library(schema_definitions)).

/* 
 * database_module(+Database:database, -Module:atom) is det. 
 */
database_module(Database,Module) :-
    database_name(Database,C),
    database_schema(Database,S),
    collection_schema_module(C,S,Module).

/* 
 * collection_schema_module(+Collection:atom,+Schema,-Module:atom) is det.
 * 
 * Construct a canonical module name for the current collection. 
 * 
 */ 
collection_schema_module(Collection,_,Collection).
    % Actually we only want one per collection, so we will not do any
    % fine grained graph based cache coherence for now.


calculate_subsumptionOf(CC, CP, Database) :-
    is_database(Database),
    !,
    database_module(Database, Module),
    calculate_subsumptionOf(CC, CP, Module).

calculate_subsumptionOf(_,'http://www.w3.org/2002/07/owl#Thing',_).
calculate_subsumptionOf('http://www.w3.org/2002/07/owl#Nothing',_,_).
% Always available...
calculate_subsumptionOf('https://datachemist.net/ontology/dcog#Document',
                        'https://datachemist.net/ontology/dcog#Document',_).
calculate_subsumptionOf('https://datachemist.net/ontology/dcog#Entity',
                        'https://datachemist.net/ontology/dcog#Entity',_).
calculate_subsumptionOf('https://datachemist.net/ontology/dcog#Relationship',
                        'https://datachemist.net/ontology/dcog#Relationship',_).
% Subsumed by Document
calculate_subsumptionOf('https://datachemist.net/ontology/dcog#Relationship',
                        'https://datachemist.net/ontology/dcog#Document',_).
calculate_subsumptionOf('https://datachemist.net/ontology/dcog#Entity',
                        'https://datachemist.net/ontology/dcog#Document',_).
% Structural rules
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
                append(Inner_Arguments, [Database_Module], DatabaseVersion_Arguments),
                Fn =.. [Predicate|Arguments],
                Database_Fn =.. [Predicate|DatabaseVersion_Arguments],
                Definition1 = (Fn :- is_database(Module),
                                     !,
                                     database_module(Module, Database_Module),
                                     Database_Fn),
                Definition2 = (Fn :- Module:Inner_Fn),
                Pair = [Definition1, Definition2],
                Arity_Plus1 is Arity + 1,
                export(Predicate/Arity_Plus1)
            ),
            Pairs),
    flatten(Pairs, Terms).

generate_owl_predicates.

document(Class, Database) :-
    is_database(Database),
    !,
    database_module(Database, Module),
    Module:subsumptionOf(Class, 'https://datachemist.net/ontology/dcog#Document').

document(Class, Module) :-
    Module:subsumptionOf(Class, 'https://datachemist.net/ontology/dcog#Document').
