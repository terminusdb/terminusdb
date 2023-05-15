:- module(api_indexer, [
              api_start_job/3,
              api_check_job/2,
              %api_index/5,
              %api_query/5,
              api_index_jobs/8
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).
:- use_module(core(document),[get_document/3]).
:- use_module(core(query)).
:- use_module(core(util)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(core(api/api_graphql)).
:- use_module(core(triple), [super_user_authority/1]).

% api_start_job(+Domain:string,+Commit:string,-Task_id, +Options) is det.
api_start_job(Domain, Commit, Task_Id) :-
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
          search([ task_id=Task_Id ])],
        Status,
        []).

embedding_type_queries(Commit_Descriptor, TypeQueries) :-
    findall(
        Type-Query,
        (   ask(Commit_Descriptor,
                (   t(Embedding, json:query,  Query_Point, schema),
                    t(Meta, json:embedding,  Embedding, schema),
                    t(Type, sys:metadata, Meta, schema))),
            Query_Point = Query^^xsd:string),
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
:- meta_predicate api_index_jobs(+, +, +, 1, +, +, +, +).
api_index_jobs(System_DB, _Auth, Stream, Prelude, Path, Commit_Id, Maybe_Previous_Commit_Id, _Options) :-
    super_user_authority(Auth),
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    call(Prelude,Stream),
    forall(
        (   member(Type-Query, TypeQueries),
            api_indexable(Maybe_Previous_Commit_Id, Descriptor, Commit_Id,
                          Type, Operation)),
        (   get_dict(op, Operation, Op),
            (   member(Op, ['Inserted', 'Changed'])
            ->  get_dict(id, Operation, Id),
                GraphQL_Query = json{ query: Query, variables: [json{id: Id}]},
                atom_json_dict(Id_Query, GraphQL_Query, [width(0)]),
                string_length(Id_Query, Content_Length),
                open_string(Id_Query, Query_Stream),
                handle_graphql_request(System_DB, Auth, open_source_swearword, Path, Query_Stream, Response, string, Content_Length),
                put_dict(_{string : Response }, Operation, Final_Operation),
                atom_json_dict(Operation_Atom, Final_Operation, [width(0)]),
                write(Stream, Operation_Atom),
                nl(Stream)
            ;   atom_json_dict(Operation_Atom, Operation, [width(0)]),
                write(Stream, Operation_Atom),
                nl(Stream)
            )
        )
    ).

stringify_document(Document, Atom) :-
    do_or_die(
        is_dict(Document),
        error(not_a_document, _)),
    dict_keys(Document, Keys),
    findall(
        Key-Value_Atom,
        (   member(Key, Keys),
            \+ atom_concat('@', _, Key),
            get_dict(Key, Document, Value),
            stringify_value(Value, Value_Atom)
        ),
        KeyValues
    ),
    maplist([Key-Value,String]>>format(atom(String), "~q: ~q", [Key-Value]), KeyValues, List),
    atomic_list_concat(List, Atom).

stringify_value(Value, Atom) :-
    (   is_dict(Value)
    ->  stringify_document(Value, Atom)
    ;   is_list(Value)
    ->  maplist(stringify_value, Value, Atom_List),
        atomic_list_concat(Atom_List, Atom)
    ;   number(Value)
    ->  format(atom(Atom), "~q", [Value])
    ;   text(Value)
    ->  atom_string(Atom, Value)
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
