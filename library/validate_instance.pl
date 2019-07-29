:- module(validate_instance,[
              instanceClass/3,
              most_specific_type/3
          ]).

/** <module> Instance Validation
 *
 * This module deals with instance checking as against a given schema.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of RegulumDB.                                      *
 *                                                                       *
 *  RegulumDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  RegulumDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(validate_schema).
:- use_module(collection).
:- use_module(triplestore).
:- use_module(utils). 
:- use_module(types).
:- use_module(base_type).

/**
 * most_specific_type(+Entity, -Sorted, +Graph)
 *
 * Gets the most specific type for a class 
 **/
most_specific_type(Entity, Entity_Type, Graph):-
    get_ordered_instance_classes(Entity, [Entity_Type|_], Graph).

/**
 * get_ordered_instance_classes(+Entity, -Sorted, +Graph)
 *
 * Gets all classes for a specific entity and returns
 * them ordered by subsumption.  
 **/
get_ordered_instance_classes(Entity, Sorted, Graph) :-
    findall(
        Class,
        instanceClass(Entity, Class, Graph),
        Classes
    ),

    predsort(
        {Graph}/[Delta,C1,C2]>>
        (   strictSubsumptionOf(C1, C2, Graph)
        ->  Delta = (<)
        ;   strictSubsumptionOf(C2, C1, Graph)
        ->  Delta = (>)
        ;   C1 = C2
        ->  Delta = (=)
        ), Classes, Sorted
    ).

/** 
 * instanceClass(?X:uri, ?C:uri, +Graph:graph) is nondet.
 * 
 * Determines the class C identified with the instance X.
 */
instanceClass(X, Y, Graph) :-
    graph_collection(Graph,Collection),
    graph_instance(Graph,Instance),
    xrdf(Collection,Instance, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y).
instanceClass(X, Y, Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema), % instances can also exist in the schema
    xrdf(Collection,Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y).
