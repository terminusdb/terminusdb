:- module(db_create, [
              create_db_unfinalized/10,
              create_db/9,
              create_schema/3,
              create_ref_layer/1,
              finalize_db/1
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

make_db_public(System_Context,DB_Uri) :-
    insert_document(
        System_Context,
        _{
            '@type' : 'Capability',
            'scope' : DB_Uri,
            'role' : [ 'Role/consumer' ]
        },
        Capability_Uri),

    ask(System_Context,
        (   insert('User/anonymous', capability, Capability_Uri))).

create_db_unfinalized(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes, Db_Uri) :-
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

    % create repo graph - it has name as label
    % This is outside of the transaction because it has side-effects that cannot be rolled back.
    create_repo_graph(Organization_Name_String, Database_Name_String),

    % create ref layer with master branch
    Repository_Descriptor = repository_descriptor{
                                database_descriptor:
                                database_descriptor{
                                    organization_name: Organization_Name_String,
                                    database_name: Database_Name_String
                                },
                                repository_name: "local"
                            },

    create_ref_layer(Repository_Descriptor),

    Branch_Descriptor = branch_descriptor{
                            repository_descriptor:Repository_Descriptor,
                            branch_name: "main" },
    create_schema(Branch_Descriptor, Schema, Prefixes).

create_schema(Branch_Desc, Schema, Prefixes) :-
    create_context(Branch_Desc, commit_info{author: "system", message: "create initial schema", commit_type: 'InitialCommit'}, Query_Context),
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
    validate_prefixes(Prefixes),
    error_on_excluded_organization(Organization_Name),
    error_on_excluded_database(Database_Name),

    create_db_unfinalized(System_DB, Auth, Organization_Name, Database_Name, Label, Comment, Schema, Public, Prefixes, Db_Uri),

    % update system with finalized
    % This reopens system graph internally, as it was advanced
    finalize_db(Db_Uri).

:- begin_tests(database_creation).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(create_db_and_check_master_branch, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State)),

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

:- end_tests(database_creation).
