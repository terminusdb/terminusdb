:- module(relationships,[
              relationship_source_property/3,
              relationship_target_property/3,
              relationship_to_pseudoedges_and_classes/4
          ]).

/** <module> Management of relationships 
 * 
 * This module helps other modules with the representation of graphs and collections 
 * by bundling them as objects with some convenience operators.
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

:- use_module(dqs_utils).
:- use_module(tbox).
:- use_module(graph_management).
:- use_module(hdt_transaction_graph).
:- use_module(library(semweb/rdf_db)).

relationship_source_property(Relationship,Property,Graph) :-
    graph_schema(Graph,Schema),
    xrdf(Relationship,dcog:source_property,Property,Schema).

relationship_target_property(Relationship,Property,Graph) :-
    graph_schema(Graph,Schema),
    xrdf(Relationship,dcog:target_property,Property,Schema).

pseudo_domain(Relationship,Domain,Graph) :-
    relationship_source_property(Relationship,Property,Graph), 
    range(Property,Domain,Graph).

pseudo_range(Relationship,Range,Graph) :-
    relationship_target_property(Relationship,Property,Graph), 
    range(Property,Range,Graph).

/** 
 * relationship_to_pseudoedges_and_classes(+Relationship:uri,+Graph:graph,-PseudoEdge:pseudoEdge,Classes:list(uri)) is det.
 * 
 * Calculate the pseudoedge and classes associated with a given relationship class
 */
relationship_to_pseudoedges_and_classes(Relationship,Graph,
                                        pseudoEdge(SourceP,Relationship,TargetP),
                                        [Domain,Range]) :-
    % We really only want one solution.
    once(
        (   relationship_source_property(Relationship,SourceP,Graph),
            relationship_target_property(Relationship,TargetP,Graph),
            pseudo_domain(Relationship,Domain,Graph),
            pseudo_range(Relationship,Range,Graph)
        )
    ).

    
