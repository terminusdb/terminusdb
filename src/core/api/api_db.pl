:- module(api_db, [
              list_databases/4,
              list_database/6,
              list_existing_databases/4,
              pretty_print_databases/1,
              db_exists_api/4,
              system_database_serve_ready/0
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

%% system_database_serve_ready is det.
%% system_database_serve_ready(+Retries) is det.
%
%  Verify that the system database resolves and enumerates through the
%  request path — the machinery every request needs to locate its target
%  database. Startup gates the `dbs` readiness component on this so HTTP
%  requests can only dispatch once databases are findable at all.
%
%  Individual user databases are intentionally not probed here: boot must
%  not load every database, and a single damaged database must never
%  abort startup. Each database is verified lazily at first touch inside
%  open_descriptor — a database that exists but cannot open is marked
%  unservable in memory (descriptor:db_failed/3) and answers a real
%  error rather than a phantom not-found.
system_database_serve_ready :-
    system_database_serve_ready(120).

system_database_serve_ready(Retries) :-
    do_or_die(
        retry_startup(Retries,
            (   create_context(system_descriptor{}, System_DB),
                get_all_databases(System_DB, _Organization, _Databases))),
        error(system_database_not_serve_ready, _)).

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

% system_database_serve_ready/1 — the `dbs` readiness check. It proves the
% system database resolves and enumerates through the request path; user
% databases are verified lazily at first touch instead, so a damaged or
% still-loading database never blocks boot and never looks like absent
% data — it is marked unservable in memory and answers a real error.
% Tests pass zero retries so a transient check resolves immediately.

test(serve_ready_empty_store,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))]) :-
    system_database_serve_ready(0).

test(serve_ready_with_databases,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"),
             create_db_without_schema("admin","bar"))),
      cleanup(teardown_temp_store(State))]) :-
    system_database_serve_ready(0).

test(serve_ready_ignores_unresolvable_database,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))]) :-
    % A finalized record whose backing store cannot resolve must not block
    % or fail serve-readiness — one damaged database never takes the
    % server down.
    create_context(system_descriptor{}, System_Context),
    with_transaction(System_Context,
        system_entity:insert_db_object(System_Context, "admin", "ghostdb",
                                       "ghost label", "ghost comment", Db_Uri),
        _),
    db_create:finalize_db(Db_Uri),
    system_database_serve_ready(0).

test(unresolvable_database_marks_failed,
     [setup(setup_temp_store(State)),
      cleanup((ignore(retract(descriptor:db_failed(_,_,_))),
               ignore(retract(descriptor:db_unservable_loading(_,_,_))),
               teardown_temp_store(State)))]) :-
    % The finalized-but-unresolvable database is marked loading at first
    % touch and the single background attempt fails, so the request waits
    % for that attempt and then answers a 500-class error — never a
    % phantom not-found. Later requests fail fast on the fresh mark.
    create_context(system_descriptor{}, System_Context),
    with_transaction(System_Context,
        system_entity:insert_db_object(System_Context, "admin", "ghostdb",
                                       "ghost label", "ghost comment", Db_Uri),
        _),
    db_create:finalize_db(Db_Uri),
    resolve_absolute_string_descriptor("admin/ghostdb", Descriptor),
    assertion(catch(open_descriptor(Descriptor, _Transaction),
                    error(database_unservable("admin", "ghostdb"), _),
                    true)),
    assertion(descriptor:db_failed("admin", "ghostdb", _Since)),
    assertion(catch(open_descriptor(Descriptor, _Transaction2),
                    error(database_unservable("admin", "ghostdb"), _),
                    true)).

test(loading_database_answers_retryable_error,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","realdb"),
             setenv('TERMINUSDB_DB_LOAD_WAIT','0'))),
      cleanup((unsetenv('TERMINUSDB_DB_LOAD_WAIT'),
               ignore(retract(descriptor:db_unservable_loading(_,_,_))),
               teardown_temp_store(State)))]) :-
    % While a load is in flight the request waits out its grace window,
    % then answers a retryable 503-class error — never a phantom not-found.
    get_time(Now),
    assertz(descriptor:db_unservable_loading("admin", "realdb", Now)),
    resolve_absolute_string_descriptor("admin/realdb", Descriptor),
    assertion(catch(open_descriptor(Descriptor, _Transaction),
                    error(database_loading("admin", "realdb"), _),
                    true)).

test(expired_failed_mark_retries_open,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","realdb"))),
      cleanup((ignore(retract(descriptor:db_failed(_,_,_))),
               teardown_temp_store(State)))]) :-
    % A failure mark older than 30 minutes is retracted and the database
    % gets a fresh attempt — the 500 is sticky, not permanent.
    get_time(Now), Stale is Now - 1801,
    assertz(descriptor:db_failed("admin", "realdb", Stale)),
    resolve_absolute_string_descriptor("admin/realdb", Descriptor),
    open_descriptor(Descriptor, _Transaction),
    assertion(\+ descriptor:db_failed("admin", "realdb", _)).

:- end_tests(db).
