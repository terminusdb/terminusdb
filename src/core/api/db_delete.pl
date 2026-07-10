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
:- use_module(core(api/api_search), [io_delete_domain/2]).
:- use_module(config(terminus_config)).

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
        % force_delete_db/2 already calls maybe_delete_search_domain/2 —
        % do NOT call it again here (avoids double DELETE /domain round-trip).
    ;   do_or_die(delete_database_label(Organization, DB_Name),
                  error(database_files_do_not_exist(Organization, DB_Name), _)),
        delete_db_from_system(Organization, DB_Name),
        % T5: FAIL-LOUD, BEST-EFFORT engine index cleanup.
        % After the TerminusDB-side deletion is complete, notify the search engine
        % to drop its index for this domain. Best-effort: if the engine call fails,
        % surface a loud error log (orphaned index is VISIBLE, not silent) but do
        % NOT block the TerminusDB-side deletion that already succeeded.
        maybe_delete_search_domain(Organization, DB_Name)
    ).

/**
* Deletes the database label for the global store. Fails if the label does not
* exist.
*/
delete_database_label(Organization, DB_Name) :-
    triple_store(Store),
    organization_database_name(Organization, DB_Name, Named_Graph_Name),
    with_meta_commit_lock(
        Named_Graph_Name,
        safe_delete_named_graph(Store, Named_Graph_Name)
    ).

/**
 * force_delete_db(+Organization, +DB_Name) is semidet.
 *
 * Deletes a database label and deletes a database from the system. Does
 * not first check the database state. Does not fail.
 */
force_delete_db(Organization, DB_Name) :-
    ignore(delete_database_label(Organization, DB_Name)),
    ignore(delete_db_from_system(Organization, DB_Name)),
    % T5: Best-effort engine index cleanup (same as normal delete_db).
    maybe_delete_search_domain(Organization, DB_Name).

/**
 * maybe_delete_search_domain(+Organization, +DB_Name) is det.
 *
 * If the indexer backend is http_tdb_search, calls DELETE /domain on the
 * engine to drop the search index for this data product. FAIL-LOUD,
 * BEST-EFFORT: logs a loud error if the engine call fails, but NEVER
 * blocks the TerminusDB-side deletion.
 *
 * Idempotent: safe to call even if no index exists (engine returns 204).
 * Silent no-op when backend is not http_tdb_search (no engine to notify).
 */
maybe_delete_search_domain(Organization, DB_Name) :-
    (   indexer_backend(http_tdb_search),
        tdb_search_endpoint(Endpoint)
    ->  format(atom(Domain), "~w/~w", [Organization, DB_Name]),
        % WHY: Best-effort — the TerminusDB deletion already succeeded.
        % INVARIANT: io_delete_domain is idempotent (204 for missing domains).
        % CONSEQUENCE: If the engine call fails, an orphaned index remains
        %              until manual cleanup. The error is VISIBLE in server logs.
        catch(
            io_delete_domain(Endpoint, Domain),
            Delete_Error,
            format(user_error,
                   "[ERROR] Failed to delete search domain '~w' from engine: ~q~n",
                   [Domain, Delete_Error])
        )
    ;   true  % Backend is not http_tdb_search — no engine to notify.
    ).
