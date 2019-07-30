:- module(inferrence,[
              inferredEdge/4
          ]).

/** <module> Inferrence
 * 
 * Inference information
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

:- use_module(library(collection)).
:- use_module(library(validate_schema)).
:- use_module(library(triplestore)).

/** 
 * runChain(?X,?P:list(uri),?Y,+Instance:atom,+Graph:graph) is nondet.
 * 
 * Run a property axiom chain PropList from X to Y.
 */
runChain(X,[P],Y,Graph) :-
    inferredEdge(X,P,Y,Graph).
runChain(X,[P|PropList],Z,Graph) :-
    inferredEdge(X,P,Y,Graph),
    runChain(Y,PropList,Z,Graph).

/** 
 * inferredTransitiveEdge(?X,?OP:property_uri,Z,+Instance:atom,+Graph:graph) is nondet.
 * 
 * We impose an ordering to avoid non-termination (subproperty ordering)
 * Concrete links are already in InferredEdge
 */ 
inferredTransitiveEdge(X,OP,Z,Graph) :-
    graph_collection(Graph,Collection),
    graph_inference(Graph,Inference),
    xrdf(Collection, Inference,
         SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Graph),
    inferredEdge(Y,OP,Z,Graph).

/** 
 * inferredEdge(?X,?OP,?Y,+Graph:graph) is nondet.
 * 
 * Calculates all available triples under inference
 * 
 * [ owl:EquivalentProperty owl:ReflexiveProperty and others not yet implemented ]
 */
inferredEdge(X,OP,Y,Graph) :-
    graph_collection(Graph,Collection),
    graph_instance(Graph,Instance),
    xrdf(Collection,Instance,X,OP,Y).
inferredEdge(X,OP,Y,Graph) :-
    graph_collection(Graph,Collection),
    graph_inference(Graph,Inference),
    xrdf(Collection,Inference,OP,rdf:type,owl:'TransitiveProperty'),
    inferredTransitiveEdge(X,OP,Y,Graph).
inferredEdge(X,OP,Y,Graph) :-
    graph_collection(Graph,Collection),
    graph_inference(Graph,Inference),
    xrdf(Collection,Inference,OP,owl:'inverseOf',P),
    inferredEdge(Y,P,X,Graph).
inferredEdge(X,OP,Y,Graph) :-
    graph_collection(Graph,Collection),
    graph_inference(Graph,Inference),
    xrdf(Collection,Inference,OP,owl:propertyChain,ListObj),
    collect(Collection,Inference,ListObj,PropList),
    runChain(X,PropList,Y,Graph).
inferredEdge(X,OP,Y,Graph) :-
    graph_collection(Graph,Collection),
    graph_inference(Graph,Inference),
    xrdf(Collection,Inference,SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Graph).
