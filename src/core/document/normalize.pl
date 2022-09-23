:- module('document/normalize', [
              normalize_document/3
          ]).

:- use_module(json).

:- use_module(library(assoc)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(crypto)).
:- use_module(library(when)).

% performance
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
:- use_module(library(plunit)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(util/tables)).

:- use_module(core(api/api_document)).

normalize_document(Transaction, Document, Normalized) :-
    open_memory_store(Memory_Store),
    [Schema] = (Transaction.schema_objects),
    Descriptor = layer_descriptor{ variety: branch,
                                   schema: Schema,
                                   instance: Instance },
    open_write(Memory_Store, Write),
    Instance = read_write_obj{
                   descriptor: labelled_graph{ label: instance,
                                               type: instance },
                   triple_update: false,
                   backlinks: [],
                   read: _Layer,
                   write: Write
               },
    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : [Instance],
                             schema_objects : [Schema],
                             inference_objects : []
                         },
    call_catch_document_mutation(Document,
                                 insert_document(Transaction_Object, Document, ID)),
    nb_commit(Write,New_Instance_Layer),
    New_Instance = read_write_obj{
                       descriptor: labelled_graph{ label: instance,
                                                   type: instance },
                       triple_update: false,
                       backlinks: [],
                       read: New_Instance_Layer,
                       write: _Write
                   },
    Transaction_Object2 = transaction_object{
                              descriptor : Descriptor,
                              instance_objects : [New_Instance],
                              schema_objects : [Schema],
                              inference_objects : []
                          },
    do_or_die(
        get_document(Transaction_Object2, ID, Normalized),
        error(normalized_document_retrieval_failed(Document), _)
    ).

