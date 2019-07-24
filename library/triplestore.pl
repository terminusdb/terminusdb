:- module(triplestore, [
              destroy_graph/2,
              make_empty_graph/2,
              destroy_indexes/0,
              sync_from_journals/0,
              sync_from_journals/2,
              xrdf/5,
              commit/2,
              rollback/2,
              collections/0,
              collections/1,
              check_graph_exists/2,
              last_plane_number/2,
              graph_checkpoint/3,
              current_checkpoint_directory/3,
              last_checkpoint_number/2
          ]).

:- use_module(file_utils).
:- use_module(journaling).
:- use_module(utils).

/** <module> Triplestore

This module contains the database management predicates responsible for creating 
collections, graphs and syncing from journals.

**/


/** 
 * retract_graph(+G) is det. 
 * 
 * Retract all dynamic elements of graph. 
 */
retract_graph(Graph_Name) :-
    schema:cleanup_schema_module(Graph_Name),
    retractall(xrdf_pos(_,_,_,Graph_Name)),
    retractall(xrdf_neg(_,_,_,Graph_Name)),
    retractall(xrdf_pos_trans(_,_,_,Graph_Name)),
    retractall(xrdf_neg_trans(_,_,_,Graph_Name)).

/** 
 * destroy_graph(+Collection,+Graph_Id:graph_identifier) is det. 
 * 
 * Completely remove a graph from disk.
 */ 
destroy_graph(Collection,Graph_Name) :-
    retract_graph(Graph_Name),
    graph_directory(Collection, Graph_Name, GraphPath),
    delete_directory_and_contents(GraphPath).

/** 
 * destroy_indexes(+C,+G) is det.
 * 
 * Destroy indexes for graph G in collection C. 
 */
destroy_indexes(C,G) :-
    current_checkpoint_directory(C,G,CPD),
    files(CPD,Entries),
    include(hdt_file_type,Entries,HDTEntries),
    maplist({CPD}/[H,F]>>(interpolate([CPD,'/',H],Path),
                          atom_concat(Path,'.index.v1-1',F)), HDTEntries, FileCandidates),
    include(exists_file,FileCandidates,Files),
    maplist(delete_file,Files).

destroy_indexes :-
    forall(
        (
            graphs(C,Gs),
            member(G,Gs)
        ),
        ignore(destroy_indexes(C,G))
    ).

/** 
 * sync_from_journals(+C,+G) is det.
 * 
 * Sync journals for a graph and collection
 */
sync_from_journals(Collection,Graph_Name) :-
    % First remove everything in the dynamic predicates.
    (   retract_graph(Graph_Name),
        hdt_transform_journals(Collection,Graph_Name),
        get_truncated_queue(Collection,Graph_Name,Queue),
        sync_queue(Graph_Name, Queue)
    ->  true
    % in case anything failed, retract the graph
    ;   retract_graph(Graph_Name),
        throw(graph_sync_error(Graph_Name))
    ).

/** 
 * cleanup_edinburgh_escapes(+File) is det.
 * 
 * Removes character escapes of the form '\ddd\' and replaces them 
 * with the re-readable '\ddd'.
 * 
 * Currently does an "in-place" replace.
 */ 
cleanup_edinburgh_escapes(File) :-
    process_create(path(sed), ['-i','s/\\\\\\([0-9][0-9][0-9]\\)\\\\/ /g',File],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["sed killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(Out).
    

/** 
 * ttl_to_hdt(+File) is det.
 * 
 * Create a hdt file from ttl using the rdf2hdt tool.
 */
ttl_to_hdt(FileIn,FileOut) :-
    cleanup_edinburgh_escapes(FileIn),
    process_create(path(rdf2hdt), ['-f','turtle',FileIn,FileOut],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["rdf2hdt killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(Out).

/** 
 * ntriples_to_hdt(+File) is det.
 * 
 * Create a hdt file from ttl using the rdf2hdt tool.
 */
ntriples_to_hdt(FileIn,FileOut) :-
    process_create(path(rdf2hdt), ['-f','ntriples',FileIn,FileOut],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["rdf2hdt killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(Out).

/** 
 * cut_queue_at_checkpoint(+Sorted,-RelevantQueue) is det.
 * 
 * Takes the relevant queue, and clips it at the first checkpoint. 
 */
cut_queue_at_checkpoint([H|_T],[H]) :-
    graph_file_type(H,ckp).
cut_queue_at_checkpoint([H|T],[H|R]) :-
    graph_file_type(H,Type), (Type=pos; Type=neg),
    cut_queue_at_checkpoint(T,R).

/** 
 * type_compare(+Ty1,+Ty2,-Ord).
 * 
 * Ty1,Ty2 is one of {ckp,neg,pos}. This gives a total ordering.
 */ 
type_compare(X,X,(=)).
type_compare(ckp,_,(>)).
type_compare(pos,_,(<)).
type_compare(neg,ckp,(<)).
type_compare(neg,pos,(>)).

/** 
 * get_truncated_queue(+Collection,+Graph,-Queue) is semidet. 
 * 
 * Get the queue associated with a graph up to the first checkpoint if it exists. 
 */
get_truncated_queue(C,G,Queue) :-
    current_checkpoint_directory(C,G,DirPath), 
    files(DirPath,Entries),
    include(hdt_file_type,Entries,HDTEntries),
    predsort([Delta,X,Y]>>(   graph_file_timestamp_compare(X,Y,Ord),
                              (   Ord=(>)
                              ->  Delta=(<)
                              ;   Ord=(<)
                              ->  Delta=(>)
                              ;   graph_file_type(X,Tx),
                                  graph_file_type(Y,Ty),
                                  type_compare(Tx,Ty,Delta))),
             HDTEntries,Sorted),

    %format('Sorted queue: ~q~n',[Sorted]),
    
    cut_queue_at_checkpoint(Sorted,RelevantQueue),

    %format('Relevant queue: ~q~n',[Sorted]),

    findall(Elt,
            (
                member(File,RelevantQueue),
                interpolate([DirPath,'/',File],FilePath),
                hdt_open(HDT0, FilePath),
		        graph_file_type(File,Type),                
                ( Type=pos, Elt=pos(HDT0)
                ; Type=neg, Elt=neg(HDT0)
                ; Type=ckp, Elt=ckp(HDT0))
            ),
            Queue).
    
/** 
 * hdt_tranform_journals(+Collection_ID,+Graph_ID) is det.
 * 
 * Transform all oustanding journals to hdt files for graph G. 
 * 
 * TODO: This has never been tested.
 */
hdt_transform_journals(Collection_ID,Graph_Name) :-
    graph_directory(Collection_ID,Graph_Name,DirPath),
    subdirectories(DirPath,Entries),
    forall(member(Entry,Entries),
           (   interpolate([DirPath,'/',Entry],Directory),
               files(Directory,Files),
             
               include({Directory}/[F]>>(interpolate([Directory,'/',F],Full),
                                         turtle_file_type(Full)),
                       Files,TurtleEntries), 
             
               forall(member(TTLFile,TurtleEntries),
                      (   graph_file_base(TTLFile,Base),
                          interpolate([Directory,'/',TTLFile],TTLFilePath),
                          interpolate([Directory,'/',Base,'.hdt'],HDTFilePath),
                          (   exists_file(HDTFilePath)
                          ->  true
                          ;   ttl_to_hdt(TTLFilePath,HDTFilePath),
                              % get rid of any possible old indexes
                              interpolate([Directory,'/',Base,'.hdt.index.v1-1'],I),
                              (   exists_file(I)
                              ->  delete_file(I)
                              ;   true)
                          )
                      )
                     )
           )
          ).

/** 
 * xrdf_pos_trans(+C,+G,?X,?Y,?Z) is nondet.
 * 
 * The dynamic predicate which stores positive updates for transactions.
 * This is thread local - it only functions in a transaction
 */
:- thread_local xrdf_pos_trans/5.

/** 
 * xrdf_neg_trans(+C,+G,?X,?Y,?Z) is nondet.
 * 
 * The dynamic predicate which stores negative updates for transactions.
 */
:- thread_local xrdf_neg_trans/5.

/** 
 * xrdf_pos(?X,?Y,?Z,+G) is nondet.
 * 
 * The dynamic predicate which stores positive updates from the journal. 
 */
% Deprecated
%:- dynamic xrdf_pos/4.

/** 
 * xrdf_neg(?X,?Y,?Z,+G) is nondet.
 * 
 * The dynamic predicate which stores negative updates from the journal. 
 */
% Deprecated
%:- dynamic xrdf_neg/4.

/**
 * collections is det.
 *
 * Writes a list of the current collections. 
 **/
collections :-
    collections(Cs),
    format('Current Collections: ~q~n', [Cs]).
    
/** 
 * collections(-Collections:list(uri)) is det.
 * 
 * Return a list of all current graphs. 
 * FIX: This is probably a bit dangerous as newly constructed 
 * directories in the hdt dir will *look* like new graphs. 
 * Probably need some sort of metadata. 
 */ 
collections(Collections) :-
    db_path(Collection_Dir), 
    subdirectories(Collection_Dir,Collection_Files),
    include({Collection_Dir}/[Collection_File]>>(
                interpolate([Collection_Dir,Collection_File,'/COLLECTION'], Collection_Marker_Path),
                exists_file(Collection_Marker_Path)
            ), Collection_Files, Valid_Collection_Files), 
    maplist(sanitise_file_name,Collections,Valid_Collection_Files).

/*
 * graphs(?Collection_ID,-Graphs:list(uri)) is nondet.
 * 
 * Return a list of all current graphs. 
 * FIX: This is probably a bit dangerous as newly constructed 
 * directories in the hdt dir will *look* like new graphs. 
 * Probably need some sort of metadata. 
 */
graphs(Collection_ID,Graphs) :-
    collections(Collections),
    (   member(Collection_ID,Collections)
    ->  sanitise_file_name(Collection_ID,Collection_Name),
        db_path(Path),
        interpolate([Path,Collection_Name], Collection_Path),
        subdirectories(Collection_Path,Graph_Names),
        include({Collection_Path}/[Name]>>(
                    interpolate([Collection_Path,'/',Name],X),
                    exists_directory(X)
                ),Graph_Names,Valid_Graph_Names),
        maplist([N,S]>>sanitise_file_name(N,S),Graphs,Valid_Graph_Names)
    ;   Graphs = []).


/**
 * check_graph_exists(+Collection_ID,+G:graph_identifier) is semidet.
 * 
 * checks to see is the graph id in the current graph list
 **/
check_graph_exists(C,G):-
    graphs(C, G_List),
    member(G, G_List),
    !.  

/** 
 * graph_checkpoint(+C,+G,-HDT) is semidet.
 * 
 * Returns the last checkpoint HDT associated with a given graph. 
 */
graph_checkpoint(C, G, HDT) :-
    graph_hdt_queue(C, G, Queue),
    graph_checkpoint_search(Queue, HDT).

graph_checkpoint_search([], _) :- fail.

graph_checkpoint_search([ckp(HDT)|_], HDT).
graph_checkpoint_search([pos(_)|Rest], Result) :-
    graph_checkpoint_search(Rest, Result).
graph_checkpoint_search([neg(_)|Rest], Result) :-
    graph_checkpoint_search(Rest, Result).

/**
 * graph_hdt_queue(+C, +G, -HDTs) is semidet.
 *
 * Returns a queue of hdt files up to and including the last checkpoint.
 * This is recalculated by sync_queue/3.
 */
:- dynamic graph_hdt_queue/3.

/** 
 * sync_queue(+Collection_ID,+G,+Queue) is det. % + exception
 * 
 * Update the xrdf_pos/5 and xrdf_neg/5 predicates and the 
 * graph_checkpoint/2 predicate from the current graph. 
 */
sync_queue(C, G, Queue) :-
    (   check_queue(Queue)
    ->  true
    ;   throw(malformed_graph_journal_queue(G))),
    (   graph_hdt_queue(C, G, Old_Queue)
    ->  close_all_handles(Old_Queue)
    ;   true),
    retractall(graph_hdt_queue(C, G,_)),
    asserta(graph_hdt_queue(C, G, Queue)).

/**
 * close_all_handles(+Queue:list) is det.
 *
 * Close all hdt handles of the queue.
 */
close_all_handles([]).
close_all_handles([Head|Rest]) :-
    Head =.. [_, HDT],
    hdt_close(HDT),
    close_all_handles(Rest).

/** 
 * with_output_graph(+Template,:Goal) is det. 
 * 
 * Template is graph(Collection_ID,Graph_Id,Type,Ext) where Type is one of {ckp,neg,pos}
 * and Ext is one of {ttl,hdt,ntr}
 *
 * ckp is for new graph (initial checkpoint) or any thereafter. 
 */
:- meta_predicate with_output_graph(?, 0).
with_output_graph(graph(C,G,Type,Ext),Goal) :-
    with_mutex(
        G,
        (   current_checkpoint_directory(C,G,CPD),
            last_plane_number(CPD,M),
            N is M+1,
            %get_time(T),floor(T,N), %switch to sequence numbers
            interpolate([CPD,'/',N,'-',Type,'.',Ext],NewFile),

            catch(
                (   
                    open(NewFile,write,Stream),
                    set_graph_stream(C,G,Stream,Type,Ext),
                    initialise_graph(C,G,Stream,Type,Ext),
                    !, % Don't ever retreat!
                    (   once(call(Goal))
                    ->  true
                    ;   format(atom(Msg),'Goal (~q) in with_output_graph failed~n',[Goal]),
                        throw(transaction_error(Msg))
                    ),
                    finalise_graph(C,G,Stream,Type,Ext)
                ),
                E,
                (   finalise_graph(C,G,Stream,Type,Ext),
                    throw(E)
                )
            )
        )
    ).

/** 
 * collapse_planes(+Collection_Id,+Graph_Id:graph_identifier) is det.
 * 
 * Create a new graph checkpoint from our current dynamic triple state
 */
collapse_planes(Collection_Id,Graph_Id) :-
    make_checkpoint_directory(Collection_Id,Graph_Id, _),
    with_output_graph(
        graph(Collection_Id,Graph_Id,ckp,ttl),
        (
            forall(
                xrdf(Collection_Id,Graph_Id,X,Y,Z),
                write_triple(Collection_Id,Graph_Id,X,Y,Z)
            )
        )
    ).


/** 
 * check_queue(+Queue) is det.
 * 
 * Sanity check the queue. 
 */
check_queue([pos(_HDT)|Res]) :-
        check_queue(Res). 
check_queue([neg(_HDT)|Res]) :-
        check_queue(Res). 
check_queue([ckp(_HDT)]).

/** 
 * sync_from_journals is det.  
 * 
 * This predicate updates the xrdf_pos/5 and xrdf_neg/5 predicate so that 
 * it reflects the current state of the database on file. 
 */ 
sync_from_journals :-
    collections(Collections),

    forall(
        (   member(Collection,Collections),
            graphs(Collection,Graphs),
            member(Graph_Name, Graphs),
            * length(Graphs, G),
            * Count = count(0)
        ),
        (
            format("~n ** Syncing ~q in collection ~q ~n", [Graph_Name,Collection]),
            * Count = count(N),
            * C is N + 1,
            * nb_setarg(1, Count, C),
            catch(sync_from_journals(Collection,Graph_Name),
                  graph_sync_error(_Name),
                  format("~n ** ERROR: Graph ~s in Collection ~s failed to sync.", [Graph_Name,Collection])),
            * format("~n ** Synced ~q of ~q ** ~n", [C, G])
            
        )
    ).

/** 
 * make_checkpoint_directory(+Collection_ID,+Graph_ID:graph_identifer,-CPD) is det. 
 * 
 * Create the current checkpoint directory
 */
make_checkpoint_directory(Collection_ID, Graph_Id, CPD) :-
    graph_directory(Collection_ID,Graph_Id, Graph_Path),
    ensure_directory(Graph_Path),
    last_checkpoint_number(Graph_Path,M),
    N is M+1,
    %get_time(T),floor(T,N),
    interpolate([Graph_Path,'/',N],CPD),
    make_directory(CPD).

/** 
 * make_empty_graph(+Collection_ID,+Graph) is det.
 * 
 * Create a new empty graph
 */
make_empty_graph(Collection_ID,Graph_Id) :-
    % create the collection if it doesn't exist
    collection_directory(Collection_ID,Collection_Path),
    ensure_directory(Collection_Path),
    interpolate([Collection_Path,'/COLLECTION'],Collection_File),
    touch(Collection_File),
    % create the graph if it doesn't exist
    graph_directory(Collection_ID,Graph_Id,Graph_Path),
    ensure_directory(Graph_Path),
    make_checkpoint_directory(Collection_ID,Graph_Id, CPD),
    %get_time(T),floor(T,N),
    N=1,
    interpolate([CPD,'/',N,'-ckp.ttl'],TTLFile),
    touch(TTLFile),
    interpolate([CPD,'/',N,'-ckp.hdt'],CKPFile),
    ttl_to_hdt(TTLFile,CKPFile).

/** 
 * import_graph(+File,+Collection_ID,+Graph) is det.
 * 
 * This predicate imports a given File as the latest checkpoint of Graph_Name
 * 
 * File will be in either ntriples, turtle or hdt format. 
 */
import_graph(File, Collection_ID, Graph_Id) :-    
    graph_file_extension(File,Ext),
    (   Ext = hdt,
        hdt_open(_, File, [access(map), indexed(false)]), % make sure this is a proper HDT file
        make_checkpoint_directory(Collection_ID,Graph_Id,CPD),
        N=1,
        %get_time(T),floor(T,N),
        interpolate([CPD,'/',N,'-ckp.hdt'],NewFile),
        copy_file(File,NewFile)
    ;   Ext = ttl,
        make_checkpoint_directory(Collection_ID,Graph_Id,CPD),
        N=1,                
        %get_time(T),floor(T,N),
        interpolate([CPD,'/',N,'-ckp.hdt'],NewFile),
        ttl_to_hdt(File,NewFile)
    ;   Ext = ntr,
        make_checkpoint_directory(Collection_ID,Graph_Id,CPD),
        N=1,                
        %get_time(T),floor(T,N),
        interpolate([CPD,'/',N,'-ckp.hdt'],NewFile),
        ntriples_to_hdt(File,NewFile)
    ),
    sync_from_journals(Collection_ID,Graph_Id).

/** 
 * commit(+C:collection_id,+G:graph_id) is det.
 * 
 * Commits the current transaction state to backing store and dynamic predicate
 * for a given collection and graph.
 */
commit(CName,GName) :-
    % Time order here is critical as we actually have a time stamp for ordering.
    % negative before positive
    % graph_id_name(G,GName),
    with_output_graph(
            graph(CName,GName,neg,ttl),
            (   forall(
                     xrdf_neg_trans(CName,GName,X,Y,Z),
                     (   % journal
                         write_triple(CName,GName,X,Y,Z),
                         % dynamic 
                         retractall(xrdf_pos(CName,GName,X,Y,Z)),
                         retractall(xrdf_neg(CName,GName,X,Y,Z)),
                         asserta(xrdf_neg(CName,GName,X,Y,Z))
                     ))
            )
    ),
    retractall(xrdf_neg_trans(X,Y,Z,GName)),
    
    with_output_graph(
            graph(CName,GName,pos,ttl),
            (   forall(
                    xrdf_pos_trans(CName,GName,X,Y,Z),
                     
                    (% journal
                        write_triple(CName,GName,X,Y,Z),
                        % dynamic
                        retractall(xrdf_pos(CName,GName,X,Y,Z)),
                        retractall(xrdf_neg(CName,GName,X,Y,Z)),
                        asserta(xrdf_pos(CName,GName,X,Y,Z))
                    ))
            )
    ),
    retractall(xrdf_pos_trans(CName,GName,X,Y,Z)).

/** 
 * rollback(+Collection_Id,+Graph_Id:graph_identifier) is det.
 * 
 * Rollback the current transaction state.
 */
rollback(Collection_Id,GName) :-
    % graph_id_name(Graph_Id, GName),
    retractall(xrdf_pos_trans(Collection_Id,GName,X,Y,Z)),
    retractall(xrdf_neg_trans(Collection_Id,GName,X,Y,Z)). 

/** 
 * xrdf(+Collection_Id,+Graph_Id,?Subject,?Predicate,?Object) is nondet.
 * 
 * The basic predicate implementing the the RDF database.
 * This layer has the transaction updates included.
 *
 * Graph is either an atom or a list of atoms, referring to the name(s) of the graph(s).
 */
% temporarily remove id behaviour.
xrdf(C,G,X,Y,Z) :-
    xrdfid(C,G,X,Y,Z).

xrdfid(C,G,X0,Y0,Z0) :-
    !,
    (   xrdf_pos_trans(C,G,X0,Y0,Z0)     % If in positive graph, return results
    ;   (   xrdf_neg_trans(C,G,X0,Y0,Z0) % If it's not negative
        *-> false                      % If it *is* negative, fail		
        ;   xrdfdb(C,G,X0,Y0,Z0))).      % Read in the permanent store

/** 
 * xrdfdb(+Collection_Id,+Graph_Id,?X,?Y,?Z) is nondet.
 * 
 * This layer has only those predicates with backing store.
 */ 
xrdfdb(C,G,X,Y,Z) :-
    graph_hdt_queue(C,G, Queue),
    xrdf_search_queue(Queue,X,Y,Z).

/* 
 * xrdf_search_queue(Queue,X,Y,Z) is nondet.
 * 
 * Underlying planar access to hdts
 */
xrdf_search_queue([ckp(HDT)|_],X,Y,Z) :-
    hdt_search_safe(HDT,X,Y,Z).
xrdf_search_queue([pos(HDT)|Rest],X,Y,Z) :-
    (   hdt_search_safe(HDT,X,Y,Z)
    ;   xrdf_search_queue(X,Y,Z,Rest)).
xrdf_search_queue([neg(HDT)|Rest],X,Y,Z) :-
    (   hdt_search_safe(HDT,X,Y,Z)
    *-> false
    ;   xrdf_search_queue(X,Y,Z,Rest)).
