:- module(inference,[
              inferredEdge/4
          ]).

/** <module> Inference
 * 
 * Predicates for inferring additional triples from the Inference ontology.
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

:- use_module(library(database)).
:- use_module(library(validate_schema)).
:- use_module(library(triplestore)).

/** 
 * runChain(?X,?P:list(uri),?Y,+Instance:atom,+Database:database is nondet.
 * 
 * Run a property axiom chain PropList from X to Y.
 */
%:- table runChain/4.
runChain(X,[P],Y,Database) :-
    inferredEdge(X,P,Y,Database).
runChain(X,[P|PropList],Z,Database) :-
    database_name(Database,C),
    database_instance(Database,I),
    xrdf(C,I,X,P,Y),
    runChain(Y,PropList,Z,Database).

/** 
 * inferredTransitiveEdge(?X,?OP:property_uri,Z,+Instance:atom,+Database:database is nondet.
 * 
 * We impose an ordering to avoid non-termination (subproperty ordering)
 * Concrete links are already in InferredEdge
 */ 
inferredTransitiveEdge(X,OP,Z,Database) :-
    database_name(Database,Collection),
    database_inference(Database,Inference),
    xrdf(Collection, Inference,
         SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Database),
    inferredEdge(Y,OP,Z,Database).

/** 
 * inferredEdge(?X,?OP,?Y,+Database:database is nondet.
 * 
 * Calculates all available triples under inference
 * 
 * [ owl:EquivalentProperty owl:ReflexiveProperty and others not yet implemented ]
 */
inferredEdge(X,OP,Y,Database) :-
    database_name(Database,Collection),
    database_instance(Database,Instance),
    xrdf(Collection,Instance,X,OP,Y).
inferredEdge(X,OP,Y,Database) :-
    database_name(Database,Collection),
    database_inference(Database,Inference),
    xrdf(Collection,Inference,OP,rdf:type,owl:'TransitiveProperty'),
    inferredTransitiveEdge(X,OP,Y,Database).
inferredEdge(X,OP,Y,Database) :-
    database_name(Database,Collection),
    database_inference(Database,Inference),
    xrdf(Collection,Inference,OP,owl:'inverseOf',P),
    inferredEdge(Y,P,X,Database).
inferredEdge(X,OP,Y,Database) :-
    database_name(Database,Collection),
    database_inference(Database,Inference),
    xrdf(Collection,Inference,OP,owl:propertyChainAxiom,ListObj),
    collect(Collection,Inference,ListObj,PropList),
    runChain(X,PropList,Y,Database).
inferredEdge(X,OP,Y,Database) :-
    database_name(Database,Collection),
    database_inference(Database,Inference),
    xrdf(Collection,Inference,SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Database).
