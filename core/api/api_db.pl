:- module(api_db, [
              list_databases/4,
              pretty_print_databases/1
          ]).

:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

get_user_databases(System_DB, Auth, User_Databases) :-
    create_context(System_DB, Context),
    user_object(Context, Auth, User_Obj),
    Role = (User_Obj.'system:role'),
    Capability = (Role.'system:capability'),
    Scope = (Capability.'system:capability_scope'),
    askable_prefixes(Context,Prefixes),

    findall(
        Path,
        (   member(DB,Scope),
            get_dict('@type',DB,'system:Database'),
            get_dict('@id',DB,ID),
            prefix_expand(ID, Prefixes, Ex_ID),
            organization_database_name_uri(Context, Organization, Name, Ex_ID),

            format(string(Path),"~s/~s",[Organization,Name])),
        User_Databases).

list_databases(System_DB ,Auth, Databases, Database_Objects) :-

    (   Databases = []
    ->  get_user_databases(System_DB, Auth, User_Databases)
    ;   User_Databases = Databases),

    findall(Database_Object,
            (   member(DB, User_Databases),
                list_database(DB, Database_Object)),
            Database_Objects).

list_database(Database, Database_Object) :-
    do_or_die(
        resolve_absolute_string_descriptor(Database, Desc),
        error(invalid_absolute_path(Database),_)),

    Repo = (Desc.repository_descriptor),

    setof(Branch,has_branch(Repo, Branch),Branches),

    Database_Object = _{ database_name: Database,
                         branch_name: Branches }.

member_last(X,List,Last) :-
    member_last_(List,X,Last).

member_last_([A],A,true).
member_last_([A,_|_],A,false).
member_last_([_|Rest],A,Last) :-
    member_last_(Rest,A,Last).

joint(true,"└──").
joint(false,"├──").

arm(true," ").
arm(false,"│").

pretty_print_databases(Databases) :-
    format("TerminusDB~n", []),
    forall(
        member_last(Database_Object, Databases, Last_DB),
        (
            Database_Name = (Database_Object.database_name),
            joint(Last_DB,Joint),
            arm(Last_DB,Arm),
            format("~s ~s~n", [Joint, Database_Name]),
            Branches = (Database_Object.branch_name),
            forall(
                member_last(Branch, Branches, Last_Branch),
                (   joint(Last_Branch, Branch_Joint),
                    format("~s   ~s ~s~n", [Arm, Branch_Joint, Branch])
                )
            ),
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
    list_databases(system_descriptor{}, Auth, [], Database_Objects),
    Database_Objects = [_{branch_name:["main"],database_name:"admin/bar"},
                        _{branch_name:["main"],database_name:"admin/foo"}].

:- end_tests(db).
