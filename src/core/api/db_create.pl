:- module(db_create, [
              create_db_unfinalized/10,
              create_db/9,
              create_db/10,
              create_schema/3,
              create_schema/4,
              create_ref_layer/1,
              finalize_db/1,
              make_db_private/2,
              make_db_public/2,
              validate_prefixes/1
          ]).

/** <module> Implementation of database graph management
 *
 * This module helps other modules with the representation of databases and
 * their associated graphs by bundling them as objects with some convenience
 * operators and accessors.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).

:- use_module(library(lists)).
:- use_module(library(plunit)).

:- multifile prolog:message//1.
prolog:message(error(database_exists(Name), _)) -->
                [ 'The database ~w already exists'-[Name]].

local_repo_uri(Name, Uri) :-
    atomic_list_concat(['terminusdb:///repository/', Name, '/data/Local'], Uri).

/**
 * create_repo_graph(+Organization,+Name)
 */
create_repo_graph(Organization,Database) :-
    triple_store(Store),
    organization_database_name(Organization,Database,Name),
    safe_create_named_graph(Store,Name,_Graph),
    Descriptor = database_descriptor{ organization_name : Organization,
                                      database_name: Database },
    create_context(Descriptor, Context),
    with_transaction(Context,
                     insert_local_repository(Context, "local", _),
                     _).

create_ref_layer(Descriptor) :-
    create_context(Descriptor, Context),
    with_transaction(
        Context,
        insert_branch_object(Context, "main", _),
        _).

finalize_db(DB_Uri) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        (   ask(Context, (
                    t(DB_Uri, rdf:type, '@schema':'UserDatabase'),
                    t(DB_Uri, state, '@schema':'DatabaseState/creating')
                )
               )
        ->  ask(Context,
                (   delete(DB_Uri, state, '@schema':'DatabaseState/creating'),
                    insert(DB_Uri, state, '@schema':'DatabaseState/finalized')
                )
               )
        ;   throw(error(database_in_inconsistent_state))),
        _).

make_db_private(System_Context,Db_Uri) :-
    ask(System_Context,
        (   t(Cap_Id, scope, Db_Uri),
            t(Cap_Id, role, 'Role/consumer'),
            delete_document(Cap_Id))
       ).

make_db_public(System_Context,DB_Uri) :-
    (   ask(System_Context,
            (   t(Cap_Id, scope, DB_Uri),
                t(Cap_Id, role, 'Role/consumer'),
                t('User/anonymous', capability, Cap_Id)
            ))
    ->  true
    ;   insert_document(
            System_Context,
            _{
                '@type' : 'Capability',
                'scope' : DB_Uri,
                'role' : [ 'Role/consumer' ]
            },
            Capability_Uri),
        ask(System_Context,
            (   insert('User/anonymous', capability, Capability_Uri)))
    ).

error_on_invalid_graph_name(Organization, Database) :-
    organization_database_name(Organization, Database, Name),
    do_or_die(safe_graph_name_length_ok(Name),
              error(database_name_too_long(Organization, Database), _)).

create_db_unfinalized(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes, Db_Uri) :-
    validate_prefixes(Prefixes),
    error_on_excluded_organization(Organization_Name),
    error_on_excluded_database(Database_Name),
    error_on_invalid_graph_name(Organization_Name, Database_Name),

    % Run the initial checks and insertion of db object in system graph inside of a transaction.
    % If anything fails, everything is retried, including the auth checks.
    create_context(System_DB, System_Context),
    with_transaction(
        System_Context,
        (
            % don't create if already exists
            do_or_die(organization_name_uri(System_Context, Organization_Name, Organization_Uri),
                      error(unknown_organization(Organization_Name),_)),
            assert_auth_action_scope(System_Context, Auth, '@schema':'Action/create_database', Organization_Uri),

            (   database_exists(Organization_Name, Database_Name)
            ->  throw(error(database_already_exists(Organization_Name, Database_Name),_))
            ;   true),

            text_to_string(Organization_Name, Organization_Name_String),
            text_to_string(Database_Name, Database_Name_String),

            % insert new db object into the terminus db
            insert_db_object(System_Context, Organization_Name_String, Database_Name_String, Label, Comment, Db_Uri),
            (   Public = true
            ->  make_db_public(System_Context, Db_Uri)
            ;   true)
        ),
        _),

    % Steps 2-4 are outside the system graph transaction because they have
    % side-effects that cannot be rolled back. We assert a thread-local bypass
    % so that open_descriptor can open this database while it is still in
    % 'creating' state. The bypass is retracted on completion or exception.
    Repository_Descriptor = repository_descriptor{
                                database_descriptor:
                                database_descriptor{
                                    organization_name: Organization_Name_String,
                                    database_name: Database_Name_String
                                },
                                repository_name: "local"
                            },
    Branch_Descriptor = branch_descriptor{
                            repository_descriptor:Repository_Descriptor,
                            branch_name: "main" },
    setup_call_cleanup(
        assert(db_creation_bypass(Organization_Name_String, Database_Name_String)),
        (   create_repo_graph(Organization_Name_String, Database_Name_String),
            create_ref_layer(Repository_Descriptor),
            create_schema(Branch_Descriptor, Auth, Schema, Prefixes)
        ),
        retract(db_creation_bypass(Organization_Name_String, Database_Name_String))
    ).

create_schema(Branch_Desc, Schema, Prefixes) :-
    create_schema(Branch_Desc, _Auth, Schema, Prefixes).

create_schema(Branch_Desc, Auth, Schema, Prefixes) :-
    Commit_Info0 = commit_info{author: "system", message: "create initial schema", commit_type: 'InitialCommit'},
    maybe_inject_auth_user(Auth, Commit_Info0, Commit_Info),
    create_context(Branch_Desc, Commit_Info, Query_Context),
    Prefix_Obj = (Prefixes.put('@type', "@context")),

    with_transaction(
        Query_Context,
        (   insert_context_document(Query_Context, Prefix_Obj),

            (   Schema = true
            ->  true
            ;   ask(Query_Context,
                    insert('terminusdb://data/Schema', rdf:type, rdf:nil, schema)))
        ),
        _).

default_schema_path(Organization_Name, Database_Name, Graph_Path) :-
    atomic_list_concat([Organization_Name, '/', Database_Name, '/',
                        "local/branch/main/schema/main"], Graph_Path).

validate_prefixes(Prefixes) :-
    forall(member(Prefix_Name, ['@base', '@schema']),
           do_or_die(get_dict(Prefix_Name, Prefixes, _),
                     error(missing_required_prefix(Prefix_Name), _))),
    forall(get_dict(Prefix_Name, Prefixes, Prefix_Value),
           do_or_die(uri_has_protocol(Prefix_Value),
                     error(invalid_uri_prefix(Prefix_Name, Prefix_Value), _))).

create_db(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes) :-
    create_db(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes, _).

create_db(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes, Db_Uri) :-
    text_to_string(Organization_Name, Org_S),
    text_to_string(Database_Name, DB_S),
    setup_call_cleanup(
        true,
        (   create_db_unfinalized(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes, Db_Uri),
            % Mark as creating so other threads are blocked by the gate in
            % open_descriptor. The flag is retracted in cleanup.
            assert(db_currently_creating(Org_S, DB_S)),
            % update system with finalized
            % This reopens system graph internally, as it was advanced
            finalize_db(Db_Uri)
        ),
        ignore(retract(db_currently_creating(Org_S, DB_S)))
    ).

:- begin_tests(database_creation).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(create_db_and_check_master_branch, [
         setup(setup_temp_store(State)),
         cleanup((ignore(retract(descriptor:db_currently_creating(_,_))),
                  teardown_temp_store(State))),

         true((once(ask(Repo_Descriptor, t(_,name,"main"^^xsd:string))),
               \+ ask(Branch_Descriptor, t(_,_,_)),
               database_prefixes(Branch_Descriptor,
                                _{'@base':"http://somewhere/document",
                                  '@schema':"http://somewhere/schema",
                                  '@type':_})))
     ])
:-
    Prefixes = _{ '@base' : 'http://somewhere/document', '@schema' : 'http://somewhere/schema' },
    open_descriptor(system_descriptor{}, System),
    create_db(System, 'User/admin', admin, testdb, 'testdb', 'a test db', false, false, Prefixes),
    Database_Descriptor = database_descriptor{
                              organization_name: "admin",
                              database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{
                          database_descriptor: Database_Descriptor,
                          repository_name: "local" },
    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: Repo_Descriptor,
                            branch_name: "main" }.

test(open_descriptor_rejects_creating_db, [
         setup(setup_temp_store(State)),
         cleanup((ignore(retract(descriptor:db_currently_creating("admin","testdb"))),
                  teardown_temp_store(State))),
         error(database_not_finalized("admin","testdb"))
     ])
:-
    Prefixes = _{ '@base' : 'http://somewhere/document', '@schema' : 'http://somewhere/schema' },
    open_descriptor(system_descriptor{}, System),
    create_db_unfinalized(System, 'User/admin', admin, testdb, 'testdb', 'a test db', false, false, Prefixes, _Db_Uri),
    % Simulate the creating-state gate: callers (create_db, clone_) assert this
    % after create_db_unfinalized. Here we assert it directly to test the gate.
    assert(descriptor:db_currently_creating("admin", "testdb")),
    % DB is now in 'creating' state — opening a descriptor should fail
    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: repository_descriptor{
                                database_descriptor: database_descriptor{
                                    organization_name: "admin",
                                    database_name: "testdb"
                                },
                                repository_name: "local"
                            },
                            branch_name: "main" },
    open_descriptor(Branch_Descriptor, _Transaction).

test(duplicate_create_db_rejected, [
         setup(setup_temp_store(State)),
         cleanup((ignore(retract(descriptor:db_currently_creating(_,_))),
                  teardown_temp_store(State))),
         error(database_already_exists(admin, testdb))
     ])
:-
    Prefixes = _{ '@base' : 'http://somewhere/document', '@schema' : 'http://somewhere/schema' },
    open_descriptor(system_descriptor{}, System),
    create_db_unfinalized(System, 'User/admin', admin, testdb, 'testdb', 'a test db', false, false, Prefixes, _Db_Uri1),
    % Second creation attempt for same DB should fail with database_already_exists
    create_db(System, 'User/admin', admin, testdb, 'testdb2', 'another test db', false, false, Prefixes, _Db_Uri2).

test(finalized_db_opens_normally, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    Prefixes = _{ '@base' : 'http://somewhere/document', '@schema' : 'http://somewhere/schema' },
    open_descriptor(system_descriptor{}, System),
    create_db(System, 'User/admin', admin, testdb, 'testdb', 'a test db', false, false, Prefixes),
    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: repository_descriptor{
                                database_descriptor: database_descriptor{
                                    organization_name: "admin",
                                    database_name: "testdb"
                                },
                                repository_name: "local"
                            },
                            branch_name: "main" },
    open_descriptor(Branch_Descriptor, _Transaction).

:- end_tests(database_creation).
