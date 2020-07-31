:- module(turtle_utils,[
              graph_to_turtle/3,
              update_turtle_graph/2,
              dump_turtle_graph/2,
              dump_md/2
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

:- use_module(core(util)).
:- use_module(triplestore).
:- use_module(literals).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(semweb/turtle)).

/*
 * update_turtle_graph(+Context,+TTL) is det.
 *
 */
update_turtle_graph(Context,TTL) :-
    (   graph_descriptor_transaction_objects_read_write_object(Context.write_graph, Context.transaction_objects, Graph)
    ->  true
    ;   throw(error(unknown_graph(Context.write_graph), _))),

    coerce_literal_string(TTL, TTLS),
    setup_call_cleanup(
        open_string(TTLS, TTLStream),
        turtle_transaction(Context, Graph, TTLStream,_),
        close(TTLStream)
    ).

/*
 * dump_turtle_graph(+Context,-String) is semidet.
 *
 */
dump_turtle_graph(Context,String) :-
    Graph_Filter = (Context.filter),
    [Transaction_Object] = (Context.transaction_objects),
    filter_transaction_object_read_write_objects(Graph_Filter, Transaction_Object, [Graph]),

    with_output_to(
        string(String),
        (   current_output(Stream),
            dict_pairs(Context.prefixes, _, Pairs),
            graph_to_turtle(Pairs, Graph, Stream)
        )
    ).

dump_md(Context,String) :-
    Graph_Filter = (Context.filter),
    do_or_die((Graph_Filter.type = schema),
              error(unimplemented('Only schema graph supported for MD dump'))),
    [Transaction_Object] = (Context.transaction_objects),
    Prefixes = (Context.prefixes),
    with_output_to(
        string(String),
        (   current_output(Stream),
            dump_md_stream(Transaction_Object,Prefixes,Stream))
    ).

/**
 * graph_to_turtle(+Prefixes,+G,+Output_Stream) is det.
 *
 * Create a ttl representation of the graph G in the
 * database named N.
 */
graph_to_turtle(Prefixes,G,Out_Stream) :-
    (   var(G.read)
    ->  true
    ;   layer_to_turtle(G.read,Prefixes,Out_Stream)).

:- use_module(core(validation)).
:- use_module(core(triple/database_utils)).
dump_md_stream(Collection,Prefixes,Out_Stream) :-
    format_ontology(Collection,Prefixes, Out_Stream),
    forall(
        topologically_sorted_class(Class,Collection),
        format_class(Collection,Prefixes,Class,Out_Stream)).

topologically_sorted_class(Class, Collection) :-
    distinct(Class,
             (   strict_subsumption_of(Class, 'http://www.w3.org/2002/07/owl#Thing', Collection),
                 Class \= 'http://www.w3.org/2002/07/owl#Nothing'
             )).

format_label(Collection,Prefixes,URI,Out_Stream, Header_Depth) :-
    pad('', '#', Header_Depth, Header),
    compress_dict_uri(URI, Prefixes, URI_Short),
    (   label(URI, Label@_, Collection)
    ->  format(Out_Stream, "~s ~s (`~s`)", [Header,Label,URI_Short])
    ;   format(Out_Stream, "~s (`~s`)", [Header,URI_Short])).

format_comment(Collection, _Prefixes, URI, Out_Stream) :-
    (   comment(URI, Comment@_, Collection)
    ->  format(Out_Stream, "~s~n~n", [Comment])
    ;   format(Out_Stream, "~n~n", [])).

format_authors(Collection, _Prefixes, Uri, Out_Stream) :-
    forall((   format(Out_Stream, "Authors: ", []),
               database_schema(Collection, Schema),
               xrdf(Schema, Uri, system:author, Author@en)
           ),
           format(Out_Stream, "~s ", [Author])),
    format(Out_Stream, "~n~n", []).

format_ontology(Collection,Prefixes, Out_Stream) :-
    (   database_schema(Collection, Schema),
        xrdf(Schema, O, rdf:type, owl:'Ontology')
    ->  format_label(Collection,Prefixes, O, Out_Stream, 1),
        format(Out_Stream, "~n~n",[]),
        format_authors(Collection,Prefixes, O, Out_Stream),
        format_comment(Collection,Prefixes, O, Out_Stream)
    ;   format(Out_Stream, "# Unknown Ontology",[])).

format_label_list(Collection, Prefixes, List, Out_Stream) :-
    maplist({Collection, Prefixes}/[Class,Label]>>(
                format_label(Collection, Prefixes, Class, string(Label), 0)
            ),
            List, Labels),
    maplist({Out_Stream}/[Arg]>>format(Out_Stream, " * ~s~n", [Arg]),Labels).

format_supers(Collection, Prefixes, Class, Out_Stream) :-
    findall(
        Super,
        (   strict_subsumption_of(Class, Super, Collection),
            Super \= 'http://www.w3.org/2002/07/owl#Thing'
        ),
        Supers),
    sort(Supers, All_Supers),
    (   All_Supers \= []
    ->  format(Out_Stream, "### Super classes ~n", []),
        format_label_list(Collection, Prefixes, All_Supers, Out_Stream),
        format(Out_Stream, "~n",[])
    ;   true).

format_oneof(Collection, Prefixes, Class, Out_Stream) :-
    (   one_of_list(Class, OneOf, Collection)
    ->  format(Out_Stream, "### One of ~n", []),
        format_label_list(Collection, Prefixes, OneOf, Out_Stream)
    ;   true).

format_class(Collection,Prefixes, Class, Out_Stream) :-
    % Write label
    format(Out_Stream, "## Class: ", []),
    format_label(Collection,Prefixes,Class,Out_Stream, 0),
    format(Out_Stream, "~n~n",[]),

    % Write comment
    format_comment(Collection,Prefixes, Class, Out_Stream),
    format_supers(Collection, Prefixes, Class, Out_Stream),
    format_oneof(Collection, Prefixes, Class, Out_Stream),
    format(Out_Stream, "~n~n",[]),

    forall(
        (   any_domain(Property, Class, Collection),
            % This should be a \+ something
            Property \= 'http://www.w3.org/2002/07/owl#topObjectProperty',
            Property \= 'http://www.w3.org/2002/07/owl#topDataProperty',
            Property \= 'http://www.w3.org/2000/01/rdf-schema#label',
            Property \= 'http://www.w3.org/2000/01/rdf-schema#comment'
        ),
        (   format(Out_Stream, "### Property: ", []),
            format_property(Collection,Prefixes, Property, Out_Stream)
        )
    ),
    format(Out_Stream, "~n", []).

format_property(Collection,Prefixes, Property, Out_Stream) :-
    format_label(Collection,Prefixes, Property, Out_Stream, 0),
    range(Property, Range, Collection),
    format(Out_Stream, "~n#### Range: ", []),
    format_label(Collection,Prefixes, Range, Out_Stream, 0),
    format(Out_Stream, "~n~n",[]),
    format_comment(Collection, Prefixes, Property, Out_Stream),
    format(Out_Stream, "~n~n",[]),
    format(Out_Stream, "~n", []).

/**
 * turtle_triples(Layer,Graph,X,P,Y) is nondet.
 */
turtle_triples(Layer,X,P,Y,_) :-
    ground(Y),
    !,
    (   \+ (   atom(Y)
           ;   string(Y))
    ->  fixup_schema_literal(Y,YO)
    ;   Y = YO),
    xrdf_db(Layer,X,P,YO).
turtle_triples(Layer,X,P,Y,_) :-
    xrdf_db(Layer,X,P,YO),
    (   is_literal(YO)
    ->  fixup_schema_literal(Y,YO)
    ;   Y = YO).

/**
 * layer_to_turtle(Layer,Prefixes,Out_Stream) is det.
 *
 * Write out triples from Layer to Out_Stream, using turtle_triples/5
 * as the generator, with prefixes, Prefixes.
 */
layer_to_turtle(Layer,Prefixes,Out_Stream) :-
   rdf_save_canonical_turtle(
        Out_Stream,
        [prefixes(Prefixes),
         encoding(utf8),
         expand(turtle_triples(Layer)),
         inline_bnodes(true),
         group(true),
         indent(2),
         silent(true)]
    ).
