:- module(db_delete,[
              delete_db/5,
              force_delete_db/2
          ]).

/** <module> Database Deletion Logic
 *
 * Predicates for deleting databases
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(document/meta_commit_queue)).
:- use_module(core(account)).
:- use_module(core(plugins)).

:- use_module(library(terminus_store)).
:- use_module(library(lists)).

begin_deleting_db_from_system(System, Organization,DB_Name) :-
    organization_database_name_uri(System,Organization,DB_Name,Db_Uri),
    ask(System,
        (   delete(Db_Uri, state, '@schema':'DatabaseState/finalized'),
            insert(Db_Uri, state, '@schema':'DatabaseState/deleting'))).

/**
* Deletes the database from the system. Fails if the database does not exist.
*/
delete_db_from_system(Organization,DB) :-
    create_context(system_descriptor{}, System),
    with_transaction(
        System,
        once((  organization_database_name_uri(System,Organization,DB,Db_Uri),
                ignore(ask(System,
                           (   t(Cap_Uri,scope,Db_Uri),
                               delete_document(Cap_Uri)))),
                ask(System,
                    delete_document(Db_Uri))
            )),
        _Meta_Data).

/**
 * delete_db(+Organization,+DB_Name) is semidet.
 *
 * Deletes a database if it exists, fails if it doesn't.
 */
delete_db(System, Auth, Organization,DB_Name, Force) :-
    error_on_excluded_organization(Organization),
    error_on_excluded_database(DB_Name),
    create_context(System, System_Context),
    with_transaction(
        System_Context,
        (
            do_or_die(organization_name_uri(System_Context, Organization, Organization_Uri),
                      error(unknown_organization(Organization), _)),

            assert_auth_action_scope(System_Context, Auth, '@schema':'Action/create_database', Organization_Uri),
            assert_auth_action_scope(System_Context, Auth, '@schema':'Action/delete_database', Organization_Uri),

            do_or_die(database_exists(System_Context,Organization,DB_Name),
                      error(unknown_database(Organization,DB_Name), _)),
            % Do something here? User may need to know what went wrong

            (   Force \= true
            ->  do_or_die(
                    database_finalized(System_Context,Organization,DB_Name),
                    error(database_not_finalized(Organization,DB_Name),
                          _)),

                begin_deleting_db_from_system(System_Context, Organization,DB_Name)
            ;   true)
        ),
        _),

    (   Force = true
    ->  force_delete_db(Organization, DB_Name)
    ;   do_or_die(delete_database_label(Organization, DB_Name),
                  error(database_files_do_not_exist(Organization, DB_Name), _)),
        delete_db_from_system(Organization, DB_Name),
        ignore(forall(plugins:post_delete_db_hook(Organization, DB_Name), true))
    ).

/**
* Deletes the database label for the global store. Fails if the label does not
* exist.
*/
delete_database_label(Organization, DB_Name) :-
    triple_store(Store),
    organization_database_name(Organization, DB_Name, Named_Graph_Name),
    Database_Descriptor = database_descriptor{organization_name: Organization,
                                              database_name: DB_Name},
    % Invalidate all cached layers associated with this database.
    organization_database_name(Organization, DB_Name, Composite),
    terminus_store:invalidate_database_layers(Store, Composite, _Invalidated_Count),
    % Retract retained_descriptor_layers to drop strong Arc<InternalLayer> refs.
    (   descriptor:retained_descriptor_layers(Desc, _),
        (   Database_Descriptor :< Desc
        ;   repository_descriptor{database_descriptor: Database_Descriptor} :< Desc
        ;   branch_descriptor{repository_descriptor:
                              repository_descriptor{database_descriptor: Database_Descriptor}} :< Desc
        ;   commit_descriptor{repository_descriptor:
                              repository_descriptor{database_descriptor: Database_Descriptor}} :< Desc
        ),
        retractall(descriptor:retained_descriptor_layers(Desc, _)),
        fail
    ;   true
    ),
    % Now delete the named graph from disk.
    with_meta_commit_lock(
        Named_Graph_Name,
        safe_delete_named_graph(Store, Named_Graph_Name)
    ),
    % Purge any remaining dead layer cache entries.
    terminus_store:cleanup_layer_cache(Store, _Removed).

/**
 * force_delete_db(+Organization, +DB_Name) is semidet.
 *
 * Deletes a database label and deletes a database from the system. Does
 * not first check the database state. Does not fail.
 */
force_delete_db(Organization, DB_Name) :-
    ignore(delete_database_label(Organization, DB_Name)),
    ignore(delete_db_from_system(Organization, DB_Name)),
    ignore(forall(plugins:post_delete_db_hook(Organization, DB_Name), true)).

:- begin_tests(db_delete_cache_invalidation).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api/db_create)).

test(delete_db_invalidates_all_cached_layers,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-
    Path = 'admin/testdb',
    resolve_absolute_string_descriptor(Path, Descriptor),
    super_user_authority(Auth),

    % Create several commits to cache multiple layers
    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{author: "me", message: "commit 1"},
                    Context1),
    with_transaction(Context1, ask(Context1, insert(a,b,c)), _),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{author: "me", message: "commit 2"},
                    Context2),
    with_transaction(Context2, ask(Context2, insert(d,e,f)), _),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{author: "me", message: "commit 3"},
                    Context3),
    with_transaction(Context3, ask(Context3, insert(g,h,i)), _),

    % Verify some layers are cached
    storage(Store),
    terminus_store:cached_layer_ids(Store, Cached_Before),
    Cached_Before \= [],

    % Delete the database
    force_delete_db("admin", "testdb"),

    % All layers associated with admin|testdb should be invalidated
    terminus_store:cached_layer_ids(Store, Cached_After),
    forall(
        (   member(Id, Cached_Before),
            \+ memberchk(Id, Cached_After)
        ),
        true
    ).

test(delete_db_does_not_invalidate_global_layers,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-
    Path = 'admin/testdb',
    resolve_absolute_string_descriptor(Path, Descriptor),
    super_user_authority(Auth),

    % Create a commit to cache some layers
    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{author: "me", message: "commit 1"},
                    Context1),
    with_transaction(Context1, ask(Context1, insert(a,b,c)), _),

    % Open the system graph to ensure its layers are cached
    system_instance_name(System_Label),
    storage(Store),
    safe_open_named_graph(Store, System_Label, System_Graph),
    head(System_Graph, System_Layer),
    layer_to_id(System_Layer, System_Layer_Id),

    % Delete the database
    force_delete_db("admin", "testdb"),

    % The system graph layer should still be cached (it's a global layer)
    terminus_store:cached_layer_ids(Store, Cached_After),
    (   memberchk(System_Layer_Id, Cached_After)
    ->  true
    ;   % System layer may not have been cached if no one queried it,
        % which is also acceptable
        true).

:- end_tests(db_delete_cache_invalidation).
