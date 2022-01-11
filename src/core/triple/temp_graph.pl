:- module(temp_graph,[
              extend_database_with_temp_graph/6
          ]).

/** <module> Temp Graph
 *
 * Create a temporary graph from one of the recognised file formats, with an
 * implicit ontology. This can then be queried repeatedly.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(triplestore).
:- use_module(literals).

:- use_module(core(transaction)).

:- use_module(library(semweb/turtle)).
:- use_module(library(lists)).
:- use_module(library(yall)).

/*
 * extend_database_with_temp_graph(+Graph_Name, +Path, +Options, -Program,
 *                                 ?Old_Database, ?Database) is det.
 *
 * Currently only respects rdf!
 */
extend_database_with_temp_graph(Graph_Name, Path, Options, Program, Old_Database, Database) :-

    Program = (
        (   triplestore:open_memory_store(Store),
            triplestore:open_write(Store, Builder),
            open(Path, read, Stream, []),
            turtle:rdf_process_turtle(
                Stream,
                {Builder}/
                [Triples,_Resource]>>(
                    forall(member(T, Triples),
                           (   literals:normalise_triple(T, rdf(X,P,Y)),
                               literals:object_storage(Y,S),
                               triplestore:nb_add_triple(Builder, X, P, S)
                           ))),
                [encoding(utf8)]),
            % commit this builder to a temporary layer to perform a diff.
            triplestore:nb_commit(Builder,Layer)
        ->  true
        ;   format(atom(M), 'Unable to load graph ~q from path ~q with options ~q',
                   [Graph_Name, Path, Options]),
            throw(error(M))
        )
    ),

    Old_Database.name = N,
    Old_Database.read_transaction = RTs,
    Old_Database.write_transaction = WTs,
    New_RTs = [read_obj{dbid : N,
                        graphid : Graph_Name,
                        layer : Layer}
               | RTs],
    Database = Old_Database.put( database{read_transaction: New_RTs,
                                          write_transaction: WTs }).
