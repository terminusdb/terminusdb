:- module(api_db, [
              list_databases/4,
              list_database/6,
              list_existing_databases/3,
              pretty_print_databases/1,
              db_exists_api/4
          ]).

:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).

:- use_module(library(lists)).
:- use_module(library(plunit)).
:- use_module(library(option)).

db_exists_api(System_DB, Auth, Organization, Database) :-
    error_on_excluded_organization(Organization),
    error_on_excluded_database(Database),
    resolve_absolute_descriptor([Organization,Database], Descriptor),
    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_read_access', Auth).

get_all_databases(System_DB, Databases) :-
    create_context(System_DB, Context),
    findall(
        Path,
        (
            ask(Context,
                (
                    isa(Organization_Uri, 'Organization'),
                    t(Organization_Uri, name, Organization^^xsd:string),
                    t(Organization_Uri, database, Db_Uri),
                    isa(Db_Uri, 'UserDatabase'),
                    t(Db_Uri, name, Name^^xsd:string)
            )),
            format(string(Path),"~s/~s",[Organization,Name])),
        Databases).

get_user_databases(System_DB, Auth, User_Databases) :-
    findall(
        Path,
        get_user_database(System_DB, Auth, _Org, _Name, Path),
        User_Databases).

get_user_database(System_DB, Auth, Org, Name, User_Database) :-
    create_context(System_DB, Context),
    user_accessible_database(Context, Auth, Org, Name, _),
    format(string(User_Database),"~s/~s",[Org,Name]).

list_databases(System_DB, Auth, Database_Objects, Options) :-
    (   is_super_user(Auth)
    ->  get_all_databases(System_DB, User_Databases)
    ;   get_user_databases(System_DB, Auth, User_Databases)),
    list_existing_databases(User_Databases, Database_Objects, Options).

list_database(System_DB, Auth, Org, Name, Database_Object, Options) :-
    get_user_database(System_DB, Auth, Org, Name, User_Database),
    list_existing_database(User_Database, Database_Object, Options).

list_existing_databases(Databases, Database_Objects, Options) :-
    findall(Database_Object,
            (   member(DB, Databases),
                list_existing_database(DB, Database_Object, Options)),
            Database_Objects).

list_existing_database(Database, Database_Object, Options) :-
    (   option(branches(true), Options)
    ->  do_or_die(
            (   resolve_absolute_string_descriptor(Database,Desc),
                resolve_relative_string_descriptor(Desc,'_commits', Repo)),
            error(invalid_absolute_path(Database),_)),

        setof(Branch,has_branch(Repo, Branch),Branches),
        Database_Object = _{ database_name: Database,
                             branch_name: Branches }
    ;   Database_Object = _{ database_name: Database }
    ).

joint(true,"└──").
joint(false,"├──").

arm(true," ").
arm(false,"│").

pretty_print_databases(Databases) :-
    format("TerminusDB~n│~n", []),
    forall(
        member_last(Database_Object, Databases, Last_DB),
        (
            Database_Name = (Database_Object.database_name),
            joint(Last_DB,Joint),
            arm(Last_DB,Arm),
            format("~s ~s~n", [Joint, Database_Name]),
            (   get_dict(branch_name, Database_Object, Branches)
            ->  forall(
                    member_last(Branch, Branches, Last_Branch),
                    (   joint(Last_Branch, Branch_Joint),
                        format("~s   ~s ~s~n", [Arm, Branch_Joint, Branch])
                    )
                )
            ;   true),
            format("~s~n", [Arm])
        )
    ).

:- begin_tests(db).
:- use_module(core(util/test_utils)).

test(list_all,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"),
             create_db_without_schema("admin","bar"))),
      cleanup(teardown_temp_store(State))]) :-

    super_user_authority(Auth),
    list_databases(system_descriptor{}, Auth, Database_Objects, _{ branches : true }),
    Expected_Objects = [_{branch_name:["main"],database_name:"admin/bar"},
                        _{branch_name:["main"],database_name:"admin/foo"}],

    forall(member(Object, Database_Objects),
           member(Object, Expected_Objects)).

test(list_existing,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo2"),
             create_db_without_schema("admin","bar2"))),
      cleanup(teardown_temp_store(State))]) :-

    list_existing_databases(["admin/foo2", "admin/bar2"], Database_Objects, _{branches : true}),
    Expected_Objects = [_{branch_name:["main"],database_name:"admin/bar2"},
                        _{branch_name:["main"],database_name:"admin/foo2"}],

    forall(member(Object, Database_Objects),
           member(Object, Expected_Objects)).

test(list_existing_no_branches,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo2"),
             create_db_without_schema("admin","bar2"))),
      cleanup(teardown_temp_store(State))]) :-

    list_existing_databases(["admin/foo2", "admin/bar2"], Database_Objects, _{branches : false}),
    Expected_Objects = [_{database_name:"admin/bar2"},
                        _{database_name:"admin/foo2"}],

    forall(member(Object, Database_Objects),
           member(Object, Expected_Objects)).

:- end_tests(db).
