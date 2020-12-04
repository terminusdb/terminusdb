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
:- use_module(core(account)).

:- use_module(library(terminus_store)).

begin_deleting_db_from_system(System, Organization,DB_Name) :-
    organization_database_name_uri(System,Organization,DB_Name,Db_Uri),
    ask(System,
        (   delete(Db_Uri, system:database_state, system:finalized, "instance/main"),
            insert(Db_Uri, system:database_state, system:deleting, "instance/main"))).

delete_db_from_system(Organization,DB) :-
    create_context(system_descriptor{}, System),
    with_transaction(
        System,
        once((  organization_database_name_uri(System,Organization,DB,Db_Uri),
                ignore(ask(System,
                           (   t(Cap_Uri,system:direct_capability_scope,Db_Uri),
                               delete_object(Cap_Uri)))),
                ask(System,
                    delete_object(Db_Uri))
            )),
        _Meta_Data).

/**
 * delete_db(+Organization,+DB_Name) is semidet.
 *
 * Deletes a database if it exists, fails if it doesn't.
 */
delete_db(System, Auth, Organization,DB_Name, Force) :-
    create_context(System, System_Context),
    with_transaction(
        System_Context,
        (
            do_or_die(organization_name_uri(System_Context, Organization, Organization_Uri),
                      error(unknown_organization(Organization), _)),

            assert_auth_action_scope(System_Context, Auth, system:create_database, Organization_Uri),
            assert_auth_action_scope(System_Context, Auth, system:delete_database, Organization_Uri),

            do_or_die(database_exists(System_Context,Organization,DB_Name),
                      error(database_does_not_exist(Organization,DB_Name), _)),
            % Do something here? User may need to know what went wrong

            (   Force \= true
            ->  do_or_die(
                    database_finalized(System_Context,Organization,DB_Name),
                    error(database_not_finalized(Organization,DB_Name),
                          context(delete_db/2))),

                begin_deleting_db_from_system(System_Context, Organization,DB_Name)
            ;   true)
        ),
        _),

    (   Force = true
    ->  force_delete_db(Organization, DB_Name)
    ;   delete_database_label(Organization,DB_Name),

        delete_db_from_system(Organization,DB_Name)).

delete_database_label(Organization,Db) :-
    db_path(Path),
    organization_database_name(Organization,Db,Composite_Name),
    www_form_encode(Composite_Name,Composite_Name_Safe),
    atomic_list_concat([Path,Composite_Name_Safe,'.label'], File_Path),

    (   exists_file(File_Path)
    ->  delete_file(File_Path)
    ;   throw(error(database_files_do_not_exist(Organization,Db),
                    context(delete_database_label/2)))
    ).

/* Force deletion of databases in an inconsistent state */
force_delete_db(Organization,DB) :-
    ignore(delete_db_from_system(Organization,DB)),
    catch(
        delete_database_label(Organization,DB),
        error(database_files_do_not_exist(_,_), _),
        true).
