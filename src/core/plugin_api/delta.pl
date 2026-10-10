:- module(plugin_api_delta, [
    document_changes/5,
    schema_types_with_metadata/2
]).

:- use_module(core(document/history), [commits_changed_id/5]).
:- use_module(core(document), [schema_metadata_descriptor/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple), [database_schema/2, xrdf/4]).
:- use_module(core(util)).

%% schema_types_with_metadata(+Commit_Descriptor, -TypeQueries) is det.
%
%  Discovers schema types that carry embedding metadata.
%  Returns a list of Type-Query-Template triples.
schema_types_with_metadata(Commit_Descriptor, TypeQueries) :-
    open_descriptor(Commit_Descriptor, Transaction),
    database_schema(Transaction, Schema),
    findall(
        Type-Query-Template,
        (   xrdf(Schema, Type, sys:metadata, _),
            schema_metadata_descriptor(Schema, Type, metadata(Metadata)),
            get_dict(embedding, Metadata, Embedding),
            get_dict(query, Embedding, Query),
            (   get_dict(template, Embedding, Template)
            ->  true
            ;   Template = none)),
        TypeQueries
    ).

%% document_changes(+Maybe_Previous_Commit_Id, +Descriptor, +Commit_Id, +Type, -Operation) is nondet.
%
%  Enumerates document insertions, updates, and deletions between
%  two commits for a given type. Operation is a dict with keys
%  op ('Inserted', 'Changed', 'Deleted') and id.
document_changes(some(Previous_Commit_Id), Descriptor, Commit_Id, Type, Operation) :-
    commits_changed_id(Descriptor, Previous_Commit_Id, Commit_Id, Id,
                       options{ type: Type }),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                After_Commit_Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Previous_Commit_Id],
                                Before_Commit_Descriptor),
    (   ask(After_Commit_Descriptor,
            t(Id, rdf:type, _))
    ->  (   ask(Before_Commit_Descriptor,
                t(Id, rdf:type, _))
        ->  Operation = json{ op: 'Changed',
                              id: Id }
        ;   Operation = json{ op: 'Inserted',
                              id: Id }
        )
    ;   Operation = json{ op: 'Deleted',
                          id: Id }
    ).
document_changes(none, Descriptor, Commit_Id, Type, Operation) :-
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    ask(Commit_Descriptor, t(Id, rdf:type, Type), [compress_prefixes(false)]),
    Operation = json{ op: 'Inserted',
                      id: Id }.
