:- module(api_indexer, [
              api_start_job/3,
              api_check_job/2,
              %api_index/5,
              %api_query/5,
              api_index_jobs/8
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).
:- use_module(core(document),[get_document/3, all_class_frames/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(core(api/api_graphql)).
:- use_module(core(triple), [super_user_authority/1]).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(dicts)).

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
        Type-Query-Template,
        (   ask(Commit_Descriptor,
                (   t(Embedding, json:query,  Query_Point, schema),
                    opt(t(Embedding, json:template, Template_Point, schema)),
                    t(Meta, json:embedding,  Embedding, schema),
                    t(Type, sys:metadata, Meta, schema))),
            Query_Point = Query^^xsd:string,
            (   ground(Template_Point)
            ->  Template_Point = Template^^_
            ;   true)),
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
    ask(Commit_Descriptor, t(Id, rdf:type, Type),[compress_prefixes(false)]),
    Operation = json{ op: 'Inserted',
                      id: Id }.

/* predicate which returns the various jobs as op/string:
{ "op" : "Inserted", "id" : "Doc/1", "string" : "this is in doc 1" }
{ "op" : "Changed", "id" : "Doc/2", "string" : "this is new in doc 2" }
{ "op" : "Deleted", "id" : "Doc/3"}
*/
:- meta_predicate api_index_jobs(+, +, +, 1, +, +, +, +).
api_index_jobs(System_DB, _Auth, Stream, Prelude, Path, Commit_Id, Maybe_Previous_Commit_Id, _Options) :-
    %super_user_authority(Auth),
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    convlist([Type-Query-Template, Type-Template, Type-Query]>>ground(Template),
            TypeQueries,
            Templates,
            Queries),
    % '$handlebars':handlebars_context(Templates, Handlebars),
    open_descriptor(Commit_Descriptor, Transaction),
    all_class_frames(Transaction, Frames, [compress_ids(true),expand_abstract(true),simple(true)]),
    '$index':embedding_context(System_DB, Transaction, Templates, Queries, Frames, Embedding_Context),
    call(Prelude,Stream),
    forall(
        (   member(Type-_Query-_Template, TypeQueries),
            api_indexable(Maybe_Previous_Commit_Id, Descriptor, Commit_Id,
                          Type, Operation)),
        (   get_dict(op, Operation, Op),
            (   member(Op, ['Inserted', 'Changed'])
            ->  (   get_dict(id, Operation, Id),
                    '$index':embedding_string_for(System_DB, Transaction, Embedding_Context, Type, Id, Embedding_String),
                    put_dict(_{string : Embedding_String }, Operation, Final_Operation),
                    atom_json_dict(Operation_Atom, Final_Operation, [width(0)]),
                    write(Stream, Operation_Atom),
                    nl(Stream)
                ;   throw(error(some_terrible_error, _)),
                    get_dict(id, Operation, Id),
                    format(Stream, '{ "op" : "Error", "message" : "Failed to process embedding operation for id ~s"}~n',
                           [Id])
                )
            ;   atom_json_dict(Operation_Atom, Operation, [width(0)]),
                write(Stream, Operation_Atom),
                nl(Stream)
            )
        )
    ).
