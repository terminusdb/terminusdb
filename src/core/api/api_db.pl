:- module(api_db, [
              list_databases/4,
              list_database/6,
              list_existing_databases/4,
              pretty_print_databases/1,
              db_exists_api/4,
              user_databases_serve_ready/0
          ]).

:- meta_predicate retry_startup(+, 0).

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

get_all_databases(System_DB, Organization, Databases) :-
    findall(
        Path-Db_Uri,
        (
            ask(System_DB,
                (
                    isa(Organization_Uri, 'Organization'),
                    t(Organization_Uri, name, Organization^^xsd:string),
                    t(Organization_Uri, database, Db_Uri),
                    isa(Db_Uri, 'UserDatabase'),
                    t(Db_Uri, name, Name^^xsd:string)
                )
            ),
            format(string(Path),"~s/~s",[Organization,Name])),
        Databases).

get_user_databases(System_DB, Auth, Organization, User_Databases) :-
    findall(
        Path,
        get_user_database(System_DB, Auth, Organization, _Name, Path),
        User_Databases).

get_user_database(System_DB, Auth, Organization, Name, User_Database-Db_Uri) :-
    user_accessible_database_id(System_DB, Auth, Organization, Name, Db_Uri),
    format(string(User_Database),"~s/~s",[Organization,Name]).

%% user_databases_serve_ready is det.
%% user_databases_serve_ready(+Retries) is det.
%
%  Resolve the serving descriptor of every finalized user database
%  recorded in the system database. Startup gates the `dbs` readiness
%  component on this so HTTP requests can only dispatch once each
%  committed database demonstrably opens — a database still coming up
%  must never look like absent data.
%
%  Databases still in 'creating' state are skipped: they have no
%  committed data to serve and a killed creation must not block startup.
%  Each resolution retries to absorb startup transients (a store or
%  commit graph still loading); exhaustion throws so the caller can mark
%  the `dbs` component failed.
user_databases_serve_ready :-
    user_databases_serve_ready(120).

user_databases_serve_ready(Retries) :-
    do_or_die(
        retry_startup(Retries,
            (   create_context(system_descriptor{}, System_DB),
                get_all_databases(System_DB, _Organization, Databases))),
        error(system_database_not_serve_ready, _)),
    forall(
        member(Path-Db_Uri, Databases),
        (   database_state_finalized(System_DB, Db_Uri)
        ->  do_or_die(
                retry_startup(Retries,
                    (   resolve_absolute_string_descriptor(Path, Descriptor),
                        open_descriptor(Descriptor, _Transaction))),
                error(user_database_not_serve_ready(Path), _))
        ;   true)).

%% database_state_finalized(+System_DB, +Db_Uri) is semidet.
%
%  The system database records `DatabaseState/finalized` once database
%  creation has committed every backing graph.
database_state_finalized(System_DB, Db_Uri) :-
    ask(System_DB, t(Db_Uri, state, '@schema':'DatabaseState/finalized')).

%% retry_startup(+Retries, :Goal) is semidet.
%
%  Run Goal, retrying failure or exceptions every half-second while a
%  startup transient may still resolve. Once Retries is exhausted the
%  goal runs unprotected: its exception propagates and its failure
%  fails, so the caller sees the real outcome.
retry_startup(Retries, Goal) :-
    (   Retries > 0
    ->  (   catch(Goal, _Error, fail)
        ->  true
        ;   sleep(0.5),
            Retries1 is Retries - 1,
            retry_startup(Retries1, Goal))
    ;   Goal).

list_databases(System_DB, Auth, Database_Objects, Options) :-
    ignore(option(organization(Organization), Options)),
    (   is_super_user(Auth)
    ->  get_all_databases(System_DB, Organization, User_Databases)
    ;   get_user_databases(System_DB, Auth, Organization, User_Databases)),
    list_existing_databases(System_DB, User_Databases, Database_Objects, Options).

list_database(System_DB, Auth, Org, Name, Database_Object, Options) :-
    get_user_database(System_DB, Auth, Org, Name, User_Database),
    list_existing_database(System_DB, User_Database, Database_Object, Options).

list_existing_databases(System_DB, Databases, Database_Objects, Options) :-
    findall(Database_Object,
            (   member(DB, Databases),
                list_existing_database(System_DB, DB, Database_Object, Options)),
            Database_Objects).

list_existing_database(System_DB, Database-Db_Uri, Database_Object, Options) :-
    do_or_die(
        (   resolve_absolute_string_descriptor(Database,Desc),
            resolve_relative_string_descriptor(Desc,'_commits', Repo)),
        error(invalid_absolute_path(Database),_)),

    (   option(verbose(true), Options)
    ->  get_document(System_DB, Db_Uri, Database_Record)
    ;   Database_Record = _{}
    ),
    (   option(branches(true), Options)
    ->  setof(Branch,has_branch(Repo, Branch),Branches),
        put_dict(_{ branches: Branches}, Database_Record, Database_Pre)
    ;   Database_Pre = Database_Record
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
    Expected_Objects = [_{branches:["main"],path:"admin/bar"},
                        _{branches:["main"],path:"admin/foo"}],

    forall(member(Object, Database_Objects),
           member(Object, Expected_Objects)).

test(list_existing,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo2"),
             create_db_without_schema("admin","bar2"))),
      cleanup(teardown_temp_store(State))]) :-

    open_descriptor(system_descriptor{}, System_DB),
    list_existing_databases(System_DB, ["admin/foo2", "admin/bar2"], Database_Objects, _{branches : true}),
    Expected_Objects = [_{branches:["main"],path:"admin/bar2"},
                        _{branches:["main"],path:"admin/foo2"}],

    forall(member(Object, Database_Objects),
           member(Object, Expected_Objects)).

test(list_existing_no_branches,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo2"),
             create_db_without_schema("admin","bar2"))),
      cleanup(teardown_temp_store(State))]) :-

    open_descriptor(system_descriptor{}, System_DB),
    list_existing_databases(System_DB, ["admin/foo2", "admin/bar2"], Database_Objects, _{branches : false}),
    Expected_Objects = [_{path:"admin/bar2"},
                        _{path:"admin/foo2"}],

    forall(member(Object, Database_Objects),
           member(Object, Expected_Objects)).

% user_databases_serve_ready/1 — the `dbs` readiness check. Every finalized
% user database must open its serving descriptor; databases still in
% 'creating' state are skipped (they have no committed data to serve),
% and a finalized database that cannot resolve must fail the check so
% startup can mark `dbs` failed instead of serving phantom 404/empty
% responses. Tests pass zero retries so a transient check resolves
% immediately.

test(serve_ready_empty_store,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))]) :-
    user_databases_serve_ready(0).

test(serve_ready_finalized_databases,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"),
             create_db_without_schema("admin","bar"))),
      cleanup(teardown_temp_store(State))]) :-
    user_databases_serve_ready(0).

test(serve_ready_skips_unfinalized_databases,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))]) :-
    % A database record still in 'creating' state (e.g. a killed create)
    % has no backing store label yet and must not block serve-readiness.
    create_context(system_descriptor{}, System_Context),
    with_transaction(System_Context,
        system_entity:insert_db_object(System_Context, "admin", "ghostdb",
                                       "ghost label", "ghost comment", _Db_Uri),
        _),
    user_databases_serve_ready(0).

test(serve_ready_fails_for_unresolvable_database,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State)),
      error(user_database_not_serve_ready("admin/ghostdb"))]) :-
    % A finalized record whose backing store cannot resolve must fail the
    % check — passing here is how committed data later looks absent.
    create_context(system_descriptor{}, System_Context),
    with_transaction(System_Context,
        system_entity:insert_db_object(System_Context, "admin", "ghostdb",
                                       "ghost label", "ghost comment", Db_Uri),
        _),
    db_create:finalize_db(Db_Uri),
    user_databases_serve_ready(0).

:- end_tests(db).
