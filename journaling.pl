:- module(journaling,[write_triple/4,
                      initialise_graph/5,
                      finalise_graph/5,
                      set_graph_stream/5,
                      close_graph_stream/5]).
 
/** <module> Journaling
 * 
 * Responsible for journaling writes so we can reconstruct the in memory database.
 * 
 */ 

:- use_module(types).
:- use_module(utils).
:- use_module(collection).

/** 
 * graph_stream(+C:collection_identifier,+G:graph_identifier,-Stream,-Type,-Ext) is det.  
 *
 * Stores the currently associated stream and its type with graph G. 
 */ 
:- dynamic graph_stream/5.

/** 
 * set_graph_stream(+C:collection_identifier,+G:graph_identifier,+Stream,+Type,+Ext) is det.  
 *
 * Stores the currently associated stream with graph G and its type. 
 * Raises an error if there is already an associated stream for graph G.
 */
set_graph_stream(Collection_Id,Graph_Id,S,Type,Ext) :-
    (   graph_stream(Collection_Id,Graph_Id,S0,Type,Ext)
    ->  throw(graph_stream_already_set(Collection_Id,Graph_Id,S0,Type,Ext))
    ;   assertz(graph_stream(Collection_Id,Graph_Id,S,Type,Ext))
    ).


/** 
 * close_graph_stream(+C,+G,+Stream,+Type,+Ext) is det.  
 *
 * Close stream associated with a given graph.
 */ 
close_graph_stream(Collection_Id,Graph_Id,Stream,Type,Ext) :-
    forall(graph_stream(Collection_Id,Graph_Id,Stream,Type,Ext),
           (   retractall(graph_stream(Collection_Id,Graph_Id,Stream,Type,Ext)),
               flush_output(Stream),
               close(Stream)
           )).

/** 
 * close_graph_streams is det.  
 *
 * Close all streams associated with all graphs.
 */ 
closeStreams :-
    forall(graph_stream(Collection_Id,Graph_Id,Stream,Type,Ext),
           close_graph_stream(Collection_Id,Graph_Id,Stream,Type,Ext)).

/* 
 * initialise_graph(Collection_ID,Graph_ID,Stream,Type,Ext) is semidet.
 */
initialise_graph(Collection_Id,Graph_Id,Stream,Type,Ext) :-
    graph_stream(Collection_Id,Graph_Id,Stream,Type,Ext),
    (   Ext=ttl
    ->  write_turtle_prelude(Stream)
    ;   Ext=ntr
    ->  throw(unimplemented_storage_type(ntr))
    ;   Ext=hdt
    ->  throw(unimplemented_storage_type(hdt))
    ).

/* 
 * initialise_graph(Collection_ID,Graph_ID,Stream,Type,Ext) is semidet.
 */
finalise_graph(Colection_Id,Graph,Stream,Type,Ext) :-
    graph_stream(Colection_Id,Graph,Stream,Type,Ext),
    (   Ext=ttl
    ->  close_graph_stream(Colection_Id,Graph,Stream,Type,Ext)
    ;   Ext=ntr
    ->  throw(unimplemented_storage_type(ntr))
    ;   Ext=hdt
    ->  throw(unimplemented_storage_type(hdt))
    ).

/** 
 * write_triple(+PX,+PY,+PZ,+Graph_Id:graph_identifier) is det.
 * 
 * Write a triple to graph G where Ext is one of {ttl,ntl,hdt}
 */ 
write_triple(PX,PY,PZ,Graph_Id) :-
    debug(write_triple, 'PX ~q      PY ~q      PZ ~q', [PX, PY,PZ]),
    graph_stream(Graph_Id,Stream,_Type,Ext),
    (   Ext=ttl
    ->  write_triple_turtle(PX,PY,PZ,Stream)
    ;   Ext=ntr
    ->  throw(unimplemented_storage_type(ntr))
    ;   Ext=hdt
    ->  throw(unimplemented_storage_type(hdt))
    ;   throw(unimplemented_storage_type(Ext))
    ).


