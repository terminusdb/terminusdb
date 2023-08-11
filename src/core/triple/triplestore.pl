:- module(triplestore, [
              safe_graph_name_length_ok/1,
              safe_create_named_graph/3,
              safe_named_graph_exists/2,
              safe_open_named_graph/3,
              safe_open_graph_head/3,
              safe_delete_named_graph/2,
              xrdf/4,
              crdf/4,
              xquad/5,
              xrdf_db/4,
              xrdf_deleted/4,
              crdf_deleted/4,
              xrdf_added/4,
              crdf_added/4,
              insert/5,
              delete/5,
              delete_all/1,
              unlink_object/2,
              storage/1,
              triple_store/1,
              global_triple_store/1,
              local_triple_store/1,
              retract_local_triple_store/1,
              set_local_triple_store/1,
              default_triple_store/1,
              memory_triple_store/1,
              with_triple_store/2
          ]).

:- use_module(literals).
:- use_module(constants).

:- use_module(core(util)).
:- use_module(core(query)).

:- use_module(core(transaction)).

:- use_module(config(terminus_config), [db_path/1, grpc_label_endpoint/1, lru_cache_size/1]).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(lists)).
:- use_module(library(url)).

:- use_module(check_db).

:- reexport(library(terminus_store)).


/** <module> Triplestore
 *
 * This module contains the database management predicates responsible
 * for creating collections, graphs and syncing from journals.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/**
 * checkpoint(+Collection_Id,+Database_Id:graph_identifier) is det.
 *
 * Create a new graph checkpoint from our current dynamic triple state
 * and sync.
 *
 * UUU Should this ever be called? - should it be automatic?
 */
checkpoint(_DB_ID,_Graph_ID) :-
    throw(error("Unimplemented")).

/**
 * default_triple_store(-Triple_Store) is det.
 *
 * Opens the default triple store, a directory store with the path retrieved from file_utils:db_path/1.
 */
default_triple_store(Triple_Store) :-
    grpc_label_endpoint(Endpoint),
    !,
    db_path(Path),
    assert_database_version_is_current(Path),
    lru_cache_size(Cache_Size),
    open_grpc_store(Path, Endpoint, 1, Cache_Size, Triple_Store).
default_triple_store(Triple_Store) :-
    db_path(Path),
    assert_database_version_is_current(Path),
    lru_cache_size(Cache_Size),
    open_archive_store(Path,Cache_Size,Triple_Store).

/**
 * memory_triple_store(-Triple_Store) is det.
 *
 * Opens an in-memory triple store.
 */
memory_triple_store(Triple_Store) :-
    open_memory_store(Triple_Store).

:- dynamic global_triple_store_var/1.

/**
 * global_triple_store(?Triple_Store) is semidet.
 *
 * set or retrieve the global triple store.
 * The global triple store is stored in a dynamic predicate, which
 * global_triple_store/1 maintains in a thread-safe
 * way. triple_store/1 will return the global triple store, unless a
 * local triple store has been set (see local_triple_store/1).
 */
global_triple_store(Triple_Store) :-
    var(Triple_Store),
    !,
    global_triple_store_var(Triple_Store).
global_triple_store(Triple_Store) :-
    % Triple_Store is non-var, so this is a set
    with_mutex(
        sync_storage,
        (
            retractall(global_triple_store_var(_)),
            assertz(global_triple_store_var(Triple_Store))
        )
    ).

:- thread_local local_triple_store_var/1.
/**
 * local_triple_store(?Triple_Store) is semidet.
 *
 * set or retrieve the local triple store.
 * The local triple store is stored in a thread-local predicate. On
 * set, local_triple_store/1 will add a new first clause to this
 * predicate, without removing existing clauses. Make sure to use
 * retract_local_triple_store/1 to undo the set when done.
 *
 * See also with_triple_store/2, which runs a goal with the local
 * triple store temporarily set to the given store.
 */
local_triple_store(Triple_Store) :-
    once(local_triple_store_var(Triple_Store)).

set_local_triple_store(Triple_Store) :-
    asserta(local_triple_store_var(Triple_Store)).

/**
 * retract_local_triple_store(+Triple_Store) is det.
 *
 * ensures a particular triple store won't be returned (anymore) by local_triple_store/1.
 */
retract_local_triple_store(Triple_Store) :-
    retract(local_triple_store_var(Triple_Store)).

/**
 * with_triple_store(+Triple_Store, :Goal) is nondet.
 *
 * Call Goal with the given triple store set as local triple store.
 * The local triple store will be retracted when Goal finishes.
 */
:- meta_predicate with_triple_store(+, :).
with_triple_store(Triple_Store, _Goal) :-
    var(Triple_Store),
    !,
    instantiation_error(Triple_Store).
with_triple_store(Triple_Store, Goal) :-
    setup_call_cleanup(set_local_triple_store(Triple_Store),
                       Goal,
                       retract_local_triple_store(Triple_Store)).

/**
 * triple_store(?Triple_Store) is det.
 *
 * Returns the current triple store associated with the server.
 *
 * This could be:
 * - the local triple store, as set with local_triple_store/1 or with_triple_store/2
 * - the global triple store, as set with global_triple_store/1.
 *
 * If neither a local nor a global triple store is set, it is
 * retrieved using default_triple_store/1 and subsequently set as the
 * global triple store.
 */
triple_store(Triple_Store) :-
    local_triple_store(Triple_Store),
    !.
triple_store(Triple_Store) :-
    global_triple_store(Triple_Store),
    !.
triple_store(Triple_Store) :-
    default_triple_store(Triple_Store),
    global_triple_store(Triple_Store).

/**
 * storage(Triple_Store) is det.
 *
 * backwards_compatible alias for triple_store/1.
 */
storage(Triple_Store) :-
    triple_store(Triple_Store).


/*
 * safe_create_named_graph(+Store,+Graph_ID,-Graph_Obj) is det.
 *
 * create the named graph, encoded for file-name safety
 */
safe_create_named_graph(Store,Graph_ID,Graph_Obj) :-
    www_form_encode(Graph_ID,Safe_Graph_ID),
    create_named_graph(Store,Safe_Graph_ID,Graph_Obj).

/*
 * graph_name_valid(+Graph_ID) is semidet.
 *
 * Succeeds if the given name for a graph can be used, and fails otherwise.
 * A graph name is bad if it is too long, cause we cannot create a
 * label file in that case.
 */
safe_graph_name_length_ok(Graph_ID) :-
    www_form_encode(Graph_ID,Safe_Graph_ID),
    atom_length(Safe_Graph_ID, Len),
    Len < 250.

/*
 * safe_named_graph_exists(+Store,+Graph_ID) is semidet.
 *
 * succeeds if the graph exists, and fails otherwise
 */
safe_named_graph_exists(Store, Graph_ID) :-
    safe_open_named_graph(Store, Graph_ID, _).

/*
 * safe_open_named_graph(+Store,+Graph_ID,-Graph_Obj) is semidet.
 *
 * open the named graph, encoded for file-name safety
 */
safe_open_named_graph(Store, Graph_ID, Graph_Obj) :-
    www_form_encode(Graph_ID,Safe_Graph_ID),
    open_named_graph(Store,Safe_Graph_ID,Graph_Obj).

pinned_graph_label(X) :-
    system_schema_name(X).
pinned_graph_label(X) :-
    repository_ontology(X).
pinned_graph_label(X) :-
    ref_ontology(X).
pinned_graph_label(X) :-
    woql_ontology(X).

safe_open_graph_head(Store, Graph_ID, Layer_Obj) :-
    pinned_graph_label(Graph_ID),
    safe_open_graph_head_pinned(Store, Graph_ID, Layer_Obj),
    !.
safe_open_graph_head(Store, Graph_ID, Layer_Obj) :-
    safe_open_graph_head_(Store, Graph_ID, Layer_Obj).

safe_open_graph_head_(Store, Graph_ID, Layer_Obj) :-
    safe_open_named_graph(Store, Graph_ID, Graph_Obj),
    head(Graph_Obj, Layer_Obj).

:- table safe_open_graph_head_pinned/3.
safe_open_graph_head_pinned(Store, Graph_ID, Layer_Obj) :-
    safe_open_graph_head_(Store, Graph_ID, Layer_Obj).

/*
 * safe_delete_named_graph(+Store, +Graph_ID) is semidet.
 *
 * delete the named graph, encoded for file-name safety
 */
safe_delete_named_graph(Store, Graph_ID) :-
    www_form_encode(Graph_ID, Safe_Graph_ID),
    delete_named_graph(Store, Safe_Graph_ID).

/**
 * import_graph(+File,+DB_ID,+Graph_ID) is det.
 *
 * This predicate imports a given File as the latest checkpoint of Database_Name
 *
 * File will be in either ntriples, turtle or hdt format.
 *
 * UUU - do some transactional stuff here as with schema.
 */
import_graph(_File, _DB_ID, _Graph_ID) :-
    throw(error(_{'system:status' : 'system:error',
                  'system:message' : "import_graph/3 is unimplemented"})).

/**
 * insert(+G:read_write_obj,+X,+Y,+Z,-Changed) is det.
 *
 * Insert triple into transaction layer, record changed as 1 or 0
 */
insert(G,X,Y,Z,Changed) :-
    realise_value(X,XV),
    realise_value(Y,YV),
    realise_value(Z,ZV),
    do_or_die(ground(ZV),
              error(instantiation_error, _)),
    object_storage(ZV, ZS),
    read_write_obj_builder(G, Builder),
    (   nb_add_object_triple(Builder, XV, YV, ZS)
    ->  Changed = 1
    ;   Changed = 0).

/**
 * delete(+G,+Builder,+X,+Y,+Z,-Changed) is det.
 *
 * Delete quad from transaction predicates, record changed as 1 or 0
 */
delete(G,X,Y,Z,Changed) :-
    realise_value(X,XV),
    realise_value(Y,YV),
    realise_value(Z,ZV),
    do_or_die(ground(ZV),
              error(instantiation_error, _)),
    object_storage(ZV, ZS),
    read_write_obj_builder(G, Builder),
    (   nb_remove_object_triple(Builder, XV, YV, ZS)
    ->  Changed = 1
    ;   Changed = 0).

delete_all(G) :-
    forall(xrdf([G], S, P, O),
           delete(G, S, P, O, _)).

/*
 * xrdf_added(+Gs:list(read_write_obj),+X,+Y,+Z) is nondet.
 *
 * Query exactly the current layer (and no deeper) for added triples.
 */
xrdf_added(Gs,X,Y,Z) :-
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    pre_convert_object(Z,ZC),
    xrdf_added_db(Layer, X, Y, ZC),
    Z = ZC,
    fix(X), fix(Y), fix(Z).

crdf_added(Gs,X,Y,Z) :-
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    xrdf_added_db(Layer, X, Y, Z).

xrdf_added_db(Layer, X, Y, Z) :-
    realise_id(X, Layer, o(XI)),
    realise_id(Y, Layer, p(YI)),
    realise_id(Z, Layer, o(ZI)),
    id_triple_addition(Layer, XI, YI, ZI),
    ids(X2, Layer, o(XI)),
    X = X2,
    ids(Y2, Layer, p(YI)),
    Y = Y2,
    ids(Z2, Layer, o(ZI)),
    Z = Z2.

/*
 * xrdf_deleted(+Gs:list(read_write_obj),+X,+Y,+Z) is nondet.
 *
 * Query exactly the current layer (and no deeper) for deleted triples.
 */
xrdf_deleted(Gs,X,Y,Z) :-
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    pre_convert_object(Z,ZC),
    xrdf_deleted_db(Layer, X, Y, ZC),
    Z = ZC,
    fix(X), fix(Y), fix(Z).

crdf_deleted(Gs,X,Y,Z) :-
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    xrdf_delete_db(Layer, X, Y, Z).

xrdf_deleted_db(Layer, X, Y, Z) :-
    realise_id(X, Layer, o(XI)),
    realise_id(Y, Layer, p(YI)),
    realise_id(Z, Layer, o(ZI)),
    id_triple_removal(Layer, XI, YI, ZI),
    ids(X2, Layer, o(XI)),
    X = X2,
    ids(Y2, Layer, p(YI)),
    Y = Y2,
    ids(Z2, Layer, o(ZI)),
    Z = Z2.

/**
 * xrdf(+Gs,?Subject,?Predicate,?Object) is nondet.
 *
 * The basic predicate implementing the the RDF database.
 * This layer has the transaction updates included.
 *
 */
xrdf(Graphs, X, Y, Z) :-
    member(Graph, Graphs),
    read_write_obj_reader(Graph, Layer),
    pre_convert_object(Z,ZC),
    xrdf_db(Layer, X, Y, ZC),
    Z = ZC,
    fix(X), fix(Y), fix(Z).

pre_convert_object(Z, Z) :-
    ground(Z),
    !.
pre_convert_object(Z, ZC) :-
    nonvar(Z),
    !,
    (   Z = _^^ZT
    ->  lit_type(ZC,ZT)
    ;   Z = _@ZL
    ->  lit_type(ZC,ZL)
    ).
pre_convert_object(Z, Z).

% maintain constraints instead of bindings.
crdf(Graphs, X, Y, Z) :-
    member(Graph, Graphs),
    read_write_obj_reader(Graph, Layer),
    xrdf_db(Layer, X, Y, Z).

xrdf_db(Layer, X, Y, Z) :-
    realise_id(X, Layer, o(XI)),
    realise_id(Y, Layer, p(YI)),
    realise_id(Z, Layer, o(ZI)),
    id_triple(Layer, XI, YI, ZI),
    ids(X2, Layer, o(XI)),
    X = X2,
    ids(Y2, Layer, p(YI)),
    Y = Y2,
    ids(Z2, Layer, o(ZI)),
    Z = Z2.

/**
 * xquad(Gs:list(graph),?G:graph,?Subject,?Predicate,?Object) is nondet.
 *
 * The transactional quad predicate allows us to remember which graph
 * we found a result in.
 */
xquad(Gs,G,X,Y,Z) :-
    memberchk(G,Gs),
    read_write_obj_reader(G, Layer),
    xrdf_db(Layer,X,Y,Z),
    fix(X), fix(Y), fix(Z).

unlink_object(Gs, ID) :-
    global_prefix_expand(rdf:first, First),
    forall(
        (   xquad(Gs, G, Subject, Predicate, ID),
            Predicate \= First),
        delete(G, Subject, Predicate, ID, _)
    ).

pre_convert_node(X,A) :-
    (   nonvar(X)
    ->  (   X = id(_)
        ->  A = X
        ;   atom_string(X,A)
        )
    ;   true).

post_convert_node(A,X) :-
    (   nonvar(X)
    ->  (   atom(X)
        ->  atom_string(X,A)
        ;   X = A)
    ;   atom_string(X,A)).

:- begin_tests(xrdf).

test(search_half_bound_literal) :-
    ask(system_descriptor{},
        t(_, sys:prefix, Prefix^^xsd:string, schema)),
    Prefix = "xsd".

:- end_tests(xrdf).
