:- module(turtle_utils,[
              graph_to_turtle/3
          ]).

/** <module> Turtle utilities
 * 
 * Utilities for manipulating, inputing and outputing turtle 
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

:- use_module(library(prefixes)). 
:- use_module(library(triplestore)).
:- use_module(library(semweb/turtle)).

/** 
 * graph_to_turtle(+N,+G,+Output_Stream) is det.
 * 
 * Create a ttl representation of the graph G in the 
 * database named N.
 */
graph_to_turtle(N,G,Out_Stream) :-
    get_collection_prefix_pairs(N,Prefixes),
    storage(Store),
    safe_open_named_graph(Store,G,Obj),
    head(Obj, L),
    layer_to_turtle(L,Prefixes,Out_Stream).

/** 
 * turtle_triples(Layer,Graph,X,P,Y) is nondet.
 */
turtle_triples(Layer,X,P,Y,_) :-    
    xrdf_db(Layer,X,P,Y).

/** 
 * layer_to_turtle(Layer,Prefixes,Out_Stream) is det.
 * 
 * Write out triples from Layer to Out_Stream, using turtle_triples/5 
 * as the generator, with prefixes, Prefixes. 
 */
layer_to_turtle(Layer,Prefixes,Out_Stream) :- 
   rdf_save_canonical_turtle(
        Out_Stream,
        [prefixes(Prefixes),expand(turtle_triples(Layer))]
    ). 
    
