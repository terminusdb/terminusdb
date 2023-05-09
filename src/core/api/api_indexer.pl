:- module(api_indexer, [
              api_start_job/4,
              api_check_job/2,
              api_index/5,
              api_query/5,
              api_index_jobs/5
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).

% api_start_job(+System,+Domain:string,+Commit:string,-Task_id, +Options) is det.
api_start_job(System, Domain, Commit, Task_Id) :-
    config:semantic_indexer_endpoint(Endpoint),
    http_get(
        [ host(Endpoint),
          path('/start'),
          search([ domain=Domain,
                   commit=Commit])],
        Task_Id,
        []).

api_check_job(Task_Id, Status) :-
    config:semantic_indexer_endpoint(Endpoint),
    http_get(
        [ host(Endpoint),
          path('/check'),
          search([ domain=Domain,
                   task_id=Task_Id])],
        Status,
        []).

embedding_type_queries(Commit_Descriptor, TypeQueries) :-
    findall(
        Type-Query,
        ask(Commit_Descriptor,
            (   t(Embedding, json:query,  Query),
                t(Meta, json:embedding,  Embedding),
                t(Type, sys:metadata, Meta))),
        TypeQueries
    ).

api_indexable(some(Previous_Commit_Id), Descriptor, Commit_Id, Type, Operation) :-
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
api_indexable(none, Descriptor, Commit_Id, Type, Operation) :-
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    ask(Commit_Descriptor, t(Id, rdf:type, Type)),
    Operation = json{ op: 'Inserted',
                      id: Id }.

/* predicate which returns the various jobs as op/string:
{ "op" : "Inserted", "id" : "Doc/1", "string" : "this is in doc 1" }
{ "op" : "Changed", "id" : "Doc/2", "string" : "this is new in doc 2" }
{ "op" : "Deleted", "id" : "Doc/3"}
*/
api_index_jobs(Stream, Path, Commit_Id, Maybe_Previous_Commit_Id, Options) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    forall(
        (   member(Type-_Query, TypeQueries),
            api_indexable(Maybe_Previous_Commit_Id, Descriptor, Commit_Id,
                          Type, Operation)),
        (   get_dict(op, Operation, Op),
            (   member(Op, ['Inserted', 'Changed'])
            ->  get_dict(id, Operation, Id),
                % This is wrong, but we need a predicate to go from
                % graphql query string to result, and we can fake it for now.
                get_document(Commit_Descriptor, Id, Document),
                atom_json_dict(Document_Atom, Document, [width(0)]),
                put_dict(_{string : Document_Atom }, Operation, Final_Operation),
                atom_json_dict(Operation_Atom, Final_Operation, [width(0)]),
                write(Stream, Operation_Atom),
                nl(Stream)
            ;   atom_json_dict(Operation_Atom, Operation, [width(0)]),
                write(Stream, Operation_Atom),
                nl(Steram)
            )
        )
    ).

/*
Options:
* last_commit(Commit_Id)
  1. If no last_commit id is specified then we will reindex from the head commit.
  2. If a last_commit_id is specified, we will re-use the index for this commit
     if it has an index.

api_index(System, Path, Options) :-


{ "@type" : "Class",
  "@id" : "MyClass",
  "@metadata" : { "embedding" : { "type" : "openAI",
                                  "GraphQLQuery" : "query{ }" } } }

{ "@type" : "Commit",
  "metadata" : "sys:JSON"
}

Between commit A, B

Return all Ids with Insert/Delete/Update for type Type



*/
