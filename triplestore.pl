:- module(triplestore, [
              destroy_graph/2,
              destroy_graphs/2,
              create_graphs/2,
              make_empty_graph/2,
              destroy_indexes/0,
              with_transaction/2,
              sync_from_journals/0,
              sync_from_journals/2,
              xrdf/5,
              delete/5,
              insert/5,
              update/5,
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

:- use_module(journaling).
:- use_module(utils).

/** <module> Triplestore

This module contains the database management predicates responsible for creating 
collections, graphs and syncing from journals.

**/

