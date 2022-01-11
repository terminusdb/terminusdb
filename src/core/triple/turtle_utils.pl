:- module(turtle_utils,[
              graph_to_turtle/4,
              update_turtle_graph/2,
              insert_turtle_graph/2,
              dump_turtle_graph/2
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
:- use_module(core(document)).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).

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
    (   get_dict('@base',Prefixes,Prefix)
    ->  atomic_list_concat([Prefix,'Blank_Node/'], Blank_Node),
        idgen_random(Blank_Node,Blank_Node_Prefix_String),
        atom_string(Blank_Node_Prefix, Blank_Node_Prefix_String)
    ;   idgen_random('terminusdb:///data/Blank_Node/',Blank_Node_Prefix_String),
        atom_string(Blank_Node_Prefix, Blank_Node_Prefix_String)
    ),

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

    Prefixes = (Context.prefixes),

    (   (Graph.descriptor.type) = instance,
        get_dict('@base',Prefixes,Instance_Base_String)
    ->  atom_string(Instance_Base,Instance_Base_String),
        Base = [base(Instance_Base)],
        ignore(
            (   get_dict('@schema',Prefixes,Schema_Base_String),
                Prefixes2 = (Prefixes.put('scm',Schema_Base_String))
            )
        )
    ;   (Graph.descriptor.type) = schema,
        get_dict('@schema',Prefixes,Schema_Base_String)
    ->  atom_string(Schema_Base,Schema_Base_String),
        Base = [base(Schema_Base)],
        ignore(
            (   get_dict('@base',Prefixes,Instance_Base_String),
                Prefixes2 = (Prefixes.put('doc',Instance_Base_String))
            )
        )
    ;   Base = [],
        Prefixes = Prefixes2
    ),

    with_output_to(
        string(String),
        (   current_output(Stream),
            dict_pairs(Prefixes2, _, Pairs),
            exclude([X-_]>>member(X,['@schema','@base','@type']), Pairs, Prefix_String_List),
            maplist([P-V,P-A]>>atom_string(A,V), Prefix_String_List, Prefix_List),
            graph_to_turtle(Base, Prefix_List, Graph, Stream)
        )
    ).

/**
 * graph_to_turtle(+Base,+Prefixes,+G,+Output_Stream) is det.
 *
 * Create a ttl representation of the graph G in the
 * database named N.
 */
graph_to_turtle(Base,Prefixes,G,Out_Stream) :-
    (   var(G.read)
    ->  true
    ;   layer_to_turtle(G.read,Base,Prefixes,Out_Stream)).

/**
 * turtle_triples(Layer,Graph,X,P,Y) is nondet.
 */
turtle_triples(Layer,X,P,Y,_) :-
    xrdf_db(Layer,X,P,YO),
    (   is_literal(YO)
    ->  literal_to_turtle(YO,Y)
    ;   Y = YO).

/**
 * layer_to_turtle(Layer,Base,Prefixes,Out_Stream) is det.
 *
 * Write out triples from Layer to Out_Stream, using turtle_triples/5
 * as the generator, with prefixes, Prefixes.
 */
layer_to_turtle(Layer,Base,Prefixes,Out_Stream) :-
   rdf_save_canonical_turtle(
        Out_Stream,
        [prefixes(Prefixes),
         encoding(utf8),
         expand(turtle_triples(Layer)),
         inline_bnodes(true),
         group(true),
         indent(2),
         silent(true)
         |Base]
    ).

