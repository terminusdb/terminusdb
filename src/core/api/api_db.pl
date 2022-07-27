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
:- use_module(core(document)).

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
    do_or_die(
        (   resolve_absolute_string_descriptor(Database,Desc),
            resolve_relative_string_descriptor(Desc,'_commits', Repo)),
        error(invalid_absolute_path(Database),_)),

    (   option(verbose(true), Options)
    ->  Db = (Desc.repository_descriptor.database_descriptor),
        organization_database_name_uri(system_descriptor{}, Db.organization_name, Db.database_name, Db_Uri),
        get_document(system_descriptor{}, Db_Uri, Database_Record)
    ;   Database_Record = _{}
    ),
    (   (   option(branches(true), Options)
        ;   option(verbose(true), Options))
    ->  setof(Branch,has_branch(Repo, Branch),Branches),
        put_dict(_{ branches: Branches}, Database_Record, Database_Pre)
    ;   Database_Pre = _{}
    ),
    put_dict(_{ path: Database }, Database_Pre, Database_Object).

joint(true,"└──").
joint(false,"├──").

arm(true," ").
arm(false,"│").

pretty_print_databases(Databases) :-
    format("TerminusDB~n│~n", []),
    forall(
        member_last(Database_Object, Databases, Last_DB),
        (   del_dict(path, Database_Object, Database_Name, Database_Meta),
            joint(Last_DB,Joint),
            arm(Last_DB,Arm),
            format("~s ~s~n", [Joint, Database_Name]),
            (   del_dict(branches, Database_Meta, _, Record)
            ->  true
            ;   Database_Meta = Record),
            pretty_print_record(4,Record,Last_DB),
            (   get_dict(branches, Database_Meta, Branches)
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

max_key_and_value_length(Record, Max) :-
    dict_pairs(Record, _, Pairs),
    max_key_and_value_length(Pairs, 0, Max).

max_key_and_value_length([], Max, Max).
max_key_and_value_length([Key-Value|Rest], Running, Max) :-
    format(string(S), '~s: ~w', [Key,Value]),
    string_length(S, Length),
    (   Length > Running
    ->  New = Length
    ;   New = Running),
    max_key_and_value_length(Rest, New, Max).

pretty_print_record(_,_{},_) :-
    !.
pretty_print_record(Offset,Record,Last_DB) :-
    max_key_and_value_length(Record, Max),
    Length is min(120, Max + Offset + 3),
    (   Last_DB = true
    ->  Edge = ' '
    ;   Edge = '│'),
    format('~s~` t~*|├~`─t~*|┐~n', [Edge,Offset,Length]),
    dict_pairs(Record, _, Pairs),
    sort(Pairs, Sorted_Pairs),
    forall(
        member(Key-Value,Sorted_Pairs),
        format('~s~` t~*|│ ~s: ~w~` t~*|│~n', [Edge,Offset,Key,Value,Length])
    ),
    format('~s~` t~*|├~`─t~*|┘~n', [Edge,Offset,Length]).

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
