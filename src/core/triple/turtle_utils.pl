:- module(turtle_utils,[
              graph_to_turtle/3,
              update_turtle_graph/2,
              insert_turtle_graph/2,
              dump_turtle_graph/2,
              dump_md/2
          ]).

/** <module> Turtle utilities
 *
 * Utilities for manipulating, inputing and outputing turtle
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(core(util)).
:- use_module(triplestore).
:- use_module(literals).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple/casting)).
:- use_module(library(semweb/turtle)).

:- multifile user:portray/1.
user:portray(turtle_utils:open_string(_, Stream)) :-
    format("~q", [turtle_utils:open_string("... string elided...", Stream)]).

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
        update_turtle_graph_(Context, Graph, TTLStream,_),
        close(TTLStream)
    ).

update_turtle_graph_(Database, Graph, New_Graph_Stream, Meta_Data) :-
    with_transaction(
        Database,
        (   % make a fresh empty graph against which to diff
            open_memory_store(Store),
            open_write(Store, Builder),

            % write to a temporary builder.
            rdf_process_turtle(
                New_Graph_Stream,
                {Builder}/
                [Triples,_Resource]>>(
                    forall(member(T, Triples),
                           (   normalise_triple(T, rdf(X,P,Y)),
                               ground_object_storage(Y,S),
                               nb_add_triple(Builder, X, P, S)
                           ))),
                [encoding(utf8),
                 on_error(error)]),

            % commit this builder to a temporary layer to perform a diff.
            nb_commit(Builder,Layer),

            % first write everything into the layer-builder that is in the new
            % file, but not in the db.
            forall(
                (   xrdf([Graph], A_Old, B_Old, C_Old),
                    \+ xrdf_db(Layer,A_Old,B_Old,C_Old)),
                delete(Graph,A_Old,B_Old,C_Old,_)
            ),
            forall(
                (   xrdf_db(Layer,A_New,B_New,C_New),
                    \+ xrdf([Graph], A_New, B_New, C_New)),
                insert(Graph,A_New,B_New,C_New,_)

            )
        ),
        Meta_Data
    ).

insert_turtle_graph(Context,TTL) :-
    (   graph_descriptor_transaction_objects_read_write_object(Context.write_graph, Context.transaction_objects, Graph)
    ->  true
    ;   throw(error(unknown_graph(Context.write_graph), _))),

    % Setup blank nodes
    get_dict(prefixes,Context, Prefixes),
    (   get_dict(doc,Prefixes,Doc)
    ->  random_idgen(Doc,['Blank_Node'],Blank_Node_Prefix)
    ;   random_idgen('terminusdb:///data/Blank_Node',[],Blank_Node_Prefix)),

    coerce_literal_string(TTL, TTLS),
    setup_call_cleanup(
        open_string(TTLS, TTLStream),
        insert_turtle_graph_(Context, Graph, TTLStream, Blank_Node_Prefix, _),
        close(TTLStream)
    ).

add_all_turtle_triples(Graph,Triples,_Resource) :-
    forall(member(T, Triples),
           (   normalise_triple(T, rdf(X,P,Y)),
               insert(Graph,X,P,Y,_)
           )).

insert_turtle_graph_(Database, Graph, New_Graph_Stream, Blank_Node_Prefix, Meta_Data) :-
    with_transaction(
        Database,
        (   rdf_process_turtle(
                New_Graph_Stream,
                add_all_turtle_triples(Graph),
                [encoding(utf8),anon_prefix(Blank_Node_Prefix),on_error(error)])
        ),
        Meta_Data
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
    xrdf_db(Layer,X,P,YO),
    (   is_literal(YO)
    ->  literal_to_turtle(YO,Y)
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

