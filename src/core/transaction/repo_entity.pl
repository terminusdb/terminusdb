:- module(repo_entity, [
              has_repository/2,
              has_local_repository/2,
              has_remote_repository/2,
              repository_name_uri/3,
              repository_type/3,
              repository_head/3,
              repository_remote_url/3,

              insert_local_repository/3,
              insert_local_repository/4,
              insert_remote_repository/4,
              insert_remote_repository/5,

              remove_local_repository/2,
              remove_remote_repository/2,

              update_repository_head/3,
              update_repository_remote_url/3
          ]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(document)).

:- use_module(layer_entity).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

has_repository(Askable, Repo_Name) :-
    ask(Askable,
        t(_, name, Repo_Name^^xsd:string)).

has_local_repository(Askable, Repo_Name) :-
    ask(Askable,
        (   t(Repo_Uri, name, Repo_Name^^xsd:string),
            t(Repo_Uri, rdf:type, '@schema':'Local'))).

has_remote_repository(Askable, Repo_Name) :-
    ask(Askable,
        (   t(Repo_Uri, name, Repo_Name^^xsd:string),
            t(Repo_Uri, rdf:type, '@schema':'Remote'))).

repository_name_uri(Askable, Repo_Name, Repo_Uri) :-
    once(ask(Askable,
             t(Repo_Uri, name, Repo_Name^^xsd:string))).

repository_type(Askable, Repo_Name, Type) :-
    repository_name_uri(Askable, Repo_Name, Repo_Uri),
    (   ask(Askable,
            t(Repo_Uri, rdf:type, '@schema':'Local'))
    ->  Type = local
    ;   ask(Askable,
            t(Repo_Uri, rdf:type, '@schema':'Remote'))
    ->  Type = remote
    ).

repository_head(Askable, Repo_Name, Layer_Id) :-
    repository_name_uri(Askable, Repo_Name, Repo_Uri),
    once(ask(Askable,
             t(Repo_Uri, head, Layer_Uri))),
    layer_id_uri(Askable, Layer_Id, Layer_Uri).

repository_remote_url(Askable, Repo_Name, Remote_Url) :-
    repository_name_uri(Askable, Repo_Name, Repo_Uri),
    once(ask(Askable,
             t(Repo_Uri, remote_url, Remote_Url^^xsd:string))).

insert_local_repository(Context, Repo_Name, Repo_Uri) :-
    insert_local_repository(Context, Repo_Name, _, Repo_Uri).

insert_local_repository(Context, Repo_Name, Head_Layer_Id, Repo_Uri) :-
    insert_document(
        Context,
        json{
            '@type' : "Local",
            'name' : Repo_Name
        },
        Repo_Uri),

    (   ground(Head_Layer_Id)
    ->  insert_layer_object(Context, Head_Layer_Id, Layer_Uri),
        once(ask(Context,
                 insert(Repo_Uri, head, Layer_Uri)))
    ;   true).

insert_remote_repository(Context, Repo_Name, Remote_Url, Repo_Uri) :-
    insert_remote_repository(Context, Repo_Name, Remote_Url, _, Repo_Uri).

insert_remote_repository(Context, Repo_Name, Remote_Url, Head_Layer_Id, Repo_Uri) :-
    insert_document(
        Context,
        json{
            '@type' : "Remote",
            'remote_url' : Remote_Url,
            'name' : Repo_Name
        },
        Repo_Uri),

    (   ground(Head_Layer_Id)
    ->  insert_layer_object(Context, Head_Layer_Id, Layer_Uri),
        once(ask(Context,
                 insert(Repo_Uri, head, Layer_Uri)))
    ;   true).

update_repository_head(Context, Repo_Name, Layer_Id) :-
    repository_name_uri(Context, Repo_Name, Repo_Uri),
    once(ask(Context,
             opt((t(Repo_Uri, head, Old_Layer_Uri),
                  delete(Repo_Uri, head, Old_Layer_Uri))))),

    insert_layer_object(Context, Layer_Id, New_Layer_Uri),

    once(ask(Context,
             insert(Repo_Uri, head, New_Layer_Uri))).

update_repository_remote_url(Context, Repo_Name, Remote_Url) :-
    repository_name_uri(Context, Repo_Name, Repo_Uri),
    once(ask(Context,
             (   t(Repo_Uri, remote_url, Old_Remote_Url^^xsd:string),
                 delete(Repo_Uri, remote_url, Old_Remote_Url^^xsd:string),
                 insert(Repo_Uri, remote_url, Remote_Url^^xsd:string)))).


remove_repository_head(Context, Repo_Uri):-
    ignore(ask(Context,
             (   t(Repo_Uri, head,Layer_Uri),
                 delete(Repo_Uri, head,Layer_Uri)))).


remove_local_repository(Context, Repo_Name) :-
    repository_name_uri(Context, Repo_Name, Repo_Uri),
    remove_repository_head(Context, Repo_Uri),
    once(ask(Context,
             (   delete(Repo_Uri, rdf:type, '@schema':'Local'),
                 delete(Repo_Uri, name, Repo_Name^^xsd:string) 
                 ))).

remove_remote_repository(Context, Repo_Name) :-
    repository_name_uri(Context, Repo_Name, Repo_Uri),
    remove_repository_head(Context, Repo_Uri),
    once(ask(Context,
             (   t(Repo_Uri, remote_url, Old_Remote_Url^^xsd:string),
                 delete(Repo_Uri, rdf:type, '@schema':'Remote'),
                 delete(Repo_Uri, name, Repo_Name^^xsd:string),
                 delete(Repo_Uri, remote_url, Old_Remote_Url^^xsd:string)))).


:- begin_tests(local_repo_objects).
:- use_module(core(util/test_utils)).
:- use_module(database).
test(local_repo_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-
    repo_schema_context_from_label_descriptor("testlabel", _, Context),

    with_transaction(Context,
                     (   insert_local_repository(Context, "foo", _),
                         insert_local_repository(Context, "bar", _),
                         insert_local_repository(Context, "baz", _)),
                     _),

    % We need to get a valid schema+context and so can not use the bare descriptor!
    repo_schema_context_from_label_descriptor("testlabel", _, Context2),

    findall(Name, has_repository(Context2, Name), Repositories),
    findall(Name, has_local_repository(Context2, Name), Repositories),
    Expected = ["foo","bar","baz"],

    union(Repositories, Expected, Union),
    intersection(Repositories, Expected, Intersection),
    subtract(Union, Intersection, []).

test(local_repo_insert_with_head,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-
    repo_schema_context_from_label_descriptor("testlabel", _, Context),

    with_transaction(Context,
                     insert_local_repository(Context, "foo", "245cf1f533d691a031205b7bd21025076b31ee35", _),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context2),
    repository_head(Context2, "foo", "245cf1f533d691a031205b7bd21025076b31ee35").

:- end_tests(local_repo_objects).

:- begin_tests(remote_repo_objects).
:- use_module(core(util/test_utils)).
:- use_module(database).
test(remote_repo_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-

    repo_schema_context_from_label_descriptor("testlabel", _, Context),

    with_transaction(Context,
                     (   insert_remote_repository(Context, "foo", "http://remote1/", _),
                         insert_remote_repository(Context, "bar", "http://remote2/", _),
                         insert_remote_repository(Context, "baz", "http://remote3/", _)),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context2),
    findall(Name, has_repository(Context2, Name), Repositories),
    findall(Name, has_remote_repository(Context2, Name), Repositories),

    maplist({Context2}/[Name,Name-Url]>>(repository_remote_url(Context2, Name, Url)),
            Repositories,
            RepositoriesWithUrls),

    Expected = ["foo"-"http://remote1/",
                "bar"-"http://remote2/",
                "baz"-"http://remote3/"],

    union(RepositoriesWithUrls, Expected, Union),
    intersection(RepositoriesWithUrls, Expected, Intersection),
    subtract(Union, Intersection, []).

test(remote_repo_insert_with_head,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-

    repo_schema_context_from_label_descriptor("testlabel", _, Context),

    with_transaction(Context,
                     insert_remote_repository(Context, "foo", "http://remote/", "245cf1f533d691a031205b7bd21025076b31ee35", _),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context2),
    repository_head(Context2, "foo", "245cf1f533d691a031205b7bd21025076b31ee35").
:- end_tests(remote_repo_objects).

:- begin_tests(repo_update).
:- use_module(core(util/test_utils)).
:- use_module(database).
test(repo_update_unset_head,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-

    repo_schema_context_from_label_descriptor("testlabel", _, Context1),

    with_transaction(Context1,
                     insert_local_repository(Context1, "foo", _),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context2),

    \+ repository_head(Context2, "foo", _),

    with_transaction(Context2,
                     update_repository_head(Context2, "foo", "245cf1f533d691a031205b7bd21025076b31ee35"),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context3),
    repository_head(Context3, "foo", "245cf1f533d691a031205b7bd21025076b31ee35").

test(repo_update_set_head,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-
    repo_schema_context_from_label_descriptor("testlabel", _, Context1),

    with_transaction(Context1,
                     insert_local_repository(Context1, "foo", "df96c0a9f2262116ed3d106e4b8c1f125d4f54ef", _),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context2),
    with_transaction(Context2,
                     update_repository_head(Context2, "foo", "245cf1f533d691a031205b7bd21025076b31ee35"),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context3),
    repository_head(Context3, "foo", "245cf1f533d691a031205b7bd21025076b31ee35").

test(repo_update_remote_url,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-

    repo_schema_context_from_label_descriptor("testlabel", _, Context1),

    with_transaction(Context1,
                     insert_remote_repository(Context1, "foo", "http://remote1/", _),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context2),
    with_transaction(Context2,
                     update_repository_remote_url(Context2, "foo", "http://remote2/"),
                     _),

    repo_schema_context_from_label_descriptor("testlabel", _, Context3),
    repository_remote_url(Context3, "foo", "http://remote2/").

:- end_tests(repo_update).
