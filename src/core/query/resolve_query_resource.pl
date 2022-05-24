:- module(resolve_query_resource,[
              resolve_string_descriptor/3,
              resolve_absolute_descriptor/2,
              resolve_relative_descriptor//2,
              resolve_relative_descriptor/3,
              resolve_absolute_string_descriptor/2,
              resolve_relative_string_descriptor/3,
              resolve_absolute_graph_descriptor/2,
              resolve_absolute_string_graph_descriptor/2,
              resolve_absolute_string_descriptor_and_graph/3,
              resolve_absolute_string_descriptor_and_default_graph/3,
              resolve_absolute_or_relative_string_descriptor/3,
              resolve_filter/2
          ]).

/** <module> Resolve Query Resource
 *
 * Resolves resource URIs to the appropiate associated graph_descriptor.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(core(util)).
:- use_module(library(pcre)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

resolve_string_descriptor(Default_Descriptor, String, Descriptor) :-
    (   resolve_relative_string_descriptor(Default_Descriptor,
                                           String,
                                           Descriptor)
    ->  true
    ;   resolve_absolute_string_descriptor(String, Descriptor)).

resolve_absolute_descriptor(["_system"], system_descriptor{}) :- !.
resolve_absolute_descriptor(['_system'], system_descriptor{}) :- !.
resolve_absolute_descriptor([X, Label], Descriptor) :-
    ground(X),
    X = label,
    !,
    resolve_absolute_descriptor(["label", Label], Descriptor).
resolve_absolute_descriptor(["label", Label], label_descriptor{label: Label_String}) :-
    !,
    (   atom(Label)
    ->  atom_string(Label, Label_String)
    ;   Label = Label_String).
resolve_absolute_descriptor([Organization, Database, X], Descriptor) :-
    ground(X),
    X = '_meta',
    !,
    resolve_absolute_descriptor([Organization, Database, "_meta"],Descriptor).
resolve_absolute_descriptor([Organization, Database, "_meta"], database_descriptor{organization_name: Organization_String, database_name: Database_String}) :-
    !,
    (   atom(Organization)
    ->  atom_string(Organization, Organization_String)
    ;   Organization = Organization_String),

    (   atom(Database)
    ->  atom_string(Database, Database_String)
    ;   Database = Database_String).
resolve_absolute_descriptor([Organization, Database, Repository, X], Descriptor) :-
    ground(X),
    X = '_commits',
    !,
    resolve_absolute_descriptor([Organization, Database, Repository, "_commits"],Descriptor).
resolve_absolute_descriptor([Organization, Database, Repository, "_commits"],
                            repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: Repository_String
                            })
:-
    !,
    (   atom(Repository)
    ->  atom_string(Repository, Repository_String)
    ;   freeze(Repository, string(Repository)),
        Repository = Repository_String),
    resolve_absolute_descriptor([Organization, Database, "_meta"], Database_Descriptor).
resolve_absolute_descriptor([Organization, Database, Repository, X, Branch], Descriptor) :-
    ground(X),
    X = branch,
    !,
    resolve_absolute_descriptor([Organization, Database, Repository, "branch", Branch], Descriptor).
resolve_absolute_descriptor([Organization, Database, Repository, "branch", Branch],
                            branch_descriptor{
                                repository_descriptor: Repository_Descriptor,
                                branch_name: Branch_String
                            })
:-
    !,
    (   var(Branch)
    ->  Branch = Branch_String
    ;   text_to_string(Branch, Branch_String)),
    resolve_absolute_descriptor([Organization, Database, Repository, "_commits"], Repository_Descriptor).
resolve_absolute_descriptor([Organization, Database, Repository, X, Commit], Descriptor) :-
    ground(X),
    X = commit,
    !,
    resolve_absolute_descriptor([Organization, Database, Repository, "commit", Commit], Descriptor).
resolve_absolute_descriptor([Organization, Database, Repository, "commit", Commit],
                            commit_descriptor{
                                repository_descriptor: Repository_Descriptor,
                                commit_id: Commit_String
                            })
:-
    !,
    (   var(Commit)
    ->  Commit = Commit_String
    ;   text_to_string(Commit, Commit_String)),
    resolve_absolute_descriptor([Organization, Database, Repository, "_commits"], Repository_Descriptor).
resolve_absolute_descriptor([_, "_meta"], _) :-
    % This happens when we append '/_meta' to the incoming path before splitting
    % it into segments. If the incoming path doesn't have a /, it doesn't get
    % split, and we end up with two things in the list, one of which is _meta.
    !,
    fail.
resolve_absolute_descriptor([Organization, Database],
                            Descriptor) :-
    !,
    resolve_absolute_descriptor([Organization, Database, "local", "branch", "main"], Descriptor).
resolve_absolute_descriptor([Organization, Database, Repository], Descriptor) :-
    !,
    resolve_absolute_descriptor([Organization, Database, Repository, branch, main], Descriptor).

:- begin_tests(resolve_absolute_string).
test(organization_db) :-
    Address = ["an_organization", "a_database"],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "main"
                 },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "local"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name:"an_organization",
                              database_name:"a_database"
                          }.

test(user_db_repo) :-
    Address = ["an_organization", "a_database", "a_remote"],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "main"
                 },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "a_remote"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.
test(user_db_repo_branch) :-
    Address = ["an_organization", "a_database", "a_remote", "branch", "a_branch"],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "a_branch"
                 },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "a_remote"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.
test(user_db_repo_commits) :-
    Address = ["an_organization", "a_database", "a_remote", "_commits"],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "a_remote"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.
test(user_db_meta) :-
    Address = ["an_organization", "a_database", "_meta"],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = database_descriptor{
                     organization_name: "an_organization",
                     database_name:"a_database"
                 }.

:- end_tests(resolve_absolute_string).
:- begin_tests(resolve_absolute_atom).
test(user_db) :-
    Address = [an_organization, a_database],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "main"
                 },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "local"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.

test(user_db_repo) :-
    Address = [an_organization, a_database, a_remote],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "main"
                 },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "a_remote"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.
test(user_db_repo_branch) :-
    Address = [an_organization, a_database, a_remote, branch, a_branch],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "a_branch"
                 },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "a_remote"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.
test(user_db_repo_commits) :-
    Address = [an_organization, a_database, a_remote, '_commits'],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = repository_descriptor{
                                database_descriptor: Database_Descriptor,
                                repository_name: "a_remote"
                            },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          }.
test(user_db_meta) :-
    Address = [an_organization, a_database, '_meta'],

    resolve_absolute_descriptor(Address, Descriptor),

    Descriptor = database_descriptor{
                     organization_name: "an_organization",
                     database_name:"a_database"
                 }.

:- end_tests(resolve_absolute_atom).

:- begin_tests(address_from_descriptor).
test(database_descriptor) :-
    Descriptor = database_descriptor{
                     organization_name: "an_organization",
                     database_name:"a_database"
                 },

    resolve_absolute_descriptor(Address, Descriptor),

    Address = ["an_organization", "a_database", "_meta"].

test(repository_descriptor) :-
    Descriptor = repository_descriptor{
                     database_descriptor: Database_Descriptor,
                     repository_name: "a_repo"
                 },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          },

    resolve_absolute_descriptor(Address, Descriptor),

    Address = ["an_organization", "a_database", "a_repo", "_commits"].

test(branch_descriptor) :-
    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name: "a_branch"
                 },
    Repository_Descriptor = repository_descriptor{
                     database_descriptor: Database_Descriptor,
                     repository_name: "a_repo"
                 },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          },

    resolve_absolute_descriptor(Address, Descriptor),

    Address = ["an_organization", "a_database", "a_repo", "branch", "a_branch"].

test(commit_descriptor) :-
    Descriptor = commit_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     commit_id: "a_commit_id"
                 },
    Repository_Descriptor = repository_descriptor{
                     database_descriptor: Database_Descriptor,
                     repository_name: "a_repo"
                 },
    Database_Descriptor = database_descriptor{
                              organization_name: "an_organization",
                              database_name:"a_database"
                          },

    resolve_absolute_descriptor(Address, Descriptor),

    Address = ["an_organization", "a_database", "a_repo", "commit", "a_commit_id"].

:- end_tests(address_from_descriptor).

descriptor_parent(system_descriptor{}, _) :-
    throw(error(descriptor_has_no_parent(system_descriptor{}))).
descriptor_parent(root, _) :-
    throw(error(descriptor_has_no_parent(system_descriptor{}))).
descriptor_parent(system_descriptor{}, root) :- !.
descriptor_parent(organization(_), root) :- !.
descriptor_parent(some_label, root) :- !.
descriptor_parent(branch_of(Parent), Parent) :- !.
descriptor_parent(commit_of(Parent), Parent) :- !.
descriptor_parent(Descriptor, some_label) :-
    label_descriptor{} :< Descriptor,
    !.
descriptor_parent(database_descriptor{
                      organization_name: Organization_Name,
                      database_name: _Database_Name
                  },
                  organization(Organization_Name)) :-
    !.
descriptor_parent(Descriptor, Parent) :-
    repository_descriptor{ database_descriptor: Parent } :< Descriptor,
    !.
descriptor_parent(Descriptor, branch_of(Parent)) :-
    branch_descriptor{ repository_descriptor: Parent } :< Descriptor,
    !.
descriptor_parent(Descriptor, commit_of(Parent)) :-
    commit_descriptor{ repository_descriptor: Parent } :< Descriptor,
    !.

descriptor_organization(root, _Organization) :-
    !,
    throw(error(descriptor_has_no_organization)).
descriptor_organization(Database_Descriptor, Organization_Name) :-
    is_dict(Database_Descriptor),
    database_descriptor{
        organization_name: Organization_Name
    } :< Database_Descriptor,
    !.
descriptor_organization(organization(Organization), Organization) :- !.
descriptor_organization(Descriptor, Organization) :-
    descriptor_parent(Descriptor, Parent),
    descriptor_organization(Parent, Organization).

descriptor_database(root, _Database_Descriptor) :-
    !,
    throw(error(descriptor_has_no_database)).
descriptor_database(Database_Descriptor, Database_Descriptor) :-
    is_dict(Database_Descriptor),
    database_descriptor{} :< Database_Descriptor,
    !.
descriptor_database(Descriptor, Database_Descriptor) :-
    descriptor_parent(Descriptor, Parent),
    descriptor_database(Parent, Database_Descriptor).

descriptor_repository(root, _Repository_Descriptor) :-
    !,
    throw(error(descriptor_has_no_repository)).
descriptor_repository(Repository_Descriptor, Repository_Descriptor) :-
    is_dict(Repository_Descriptor),
    repository_descriptor{} :< Repository_Descriptor,
    !.
descriptor_repository(Descriptor, Repository_Descriptor) :-
    descriptor_parent(Descriptor, Parent),
    descriptor_repository(Parent, Repository_Descriptor).

context_completion(system_descriptor{},
                   system_descriptor{}) :- !.
context_completion(Label_Context, Label_Context) :-
    label_descriptor{} :< Label_Context,
    !.
context_completion(Database_Context, Completion) :-
    database_descriptor{} :< Database_Context,
    !,
    Repo_Descriptor = repository_descriptor{
                          database_descriptor: Database_Context,
                          repository_name: "local"
                      },
    Completion = branch_descriptor{
                     repository_descriptor: Repo_Descriptor,
                     branch_name: "main"
                 }.
context_completion(Repository_Context, Completion) :-
    repository_descriptor{} :< Repository_Context,
    !,
    Completion = branch_descriptor{
                     repository_descriptor: Repository_Context,
                     branch_name: "main"
                 }.
context_completion(Branch_Context, Branch_Context) :-
    branch_descriptor{} :< Branch_Context,
    !.
context_completion(Commit_Context, Commit_Context) :-
    commit_descriptor{} :< Commit_Context.

resolve_relative_descriptor(root, _Descriptor, [], []) :-
    !,
    throw(error(address_resolve('tried to resolve root which is not a valid descriptor'))).
resolve_relative_descriptor(some_label, _Descriptor, [], []) :-
    !,
    throw(error(address_resolve('tried to resolve the label root which is not a valid descriptor'))).
resolve_relative_descriptor(organization(_Organization), _Descriptor, [], []) :-
    !,
    throw(error(address_resolve('tried to resolve an organization which is not a valid descriptor'))).
resolve_relative_descriptor(branch_of(_Repo), _Descriptor, [], []) :-
    !,
    throw(error(address_resolve('tried to resolve a branch root which is not a valid descriptor'))).
resolve_relative_descriptor(commit_of(_Repo), _Descriptor, [], []) :-
    !,
    throw(error(address_resolve('tried to resolve a commit root which is not a valid descriptor'))).
resolve_relative_descriptor(Context, Descriptor) -->
    [".."],
    !,
    { descriptor_parent(Context, Parent) },
    resolve_relative_descriptor(Parent, Descriptor).
resolve_relative_descriptor(_Context, Descriptor) -->
    ["_root"],
    !,
    resolve_relative_descriptor(root, Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    ["_organization"],
    !,
    { descriptor_organization(Context, Organization) },
    resolve_relative_descriptor(organization(Organization), Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    ["_database"],
    !,
    { descriptor_database(Context, Database) },
    resolve_relative_descriptor(Database, Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    ["_repository"],
    !,
    { descriptor_repository(Context, Repository) },
    resolve_relative_descriptor(Repository, Descriptor).
resolve_relative_descriptor(root, Descriptor) -->
    !,
    resolve_root_relative_descriptor(Descriptor).
resolve_relative_descriptor(some_label, Descriptor) -->
    % todo: perhaps ensure that label cannot be an encoded organization/database pair
    % on the other hand, being able to do so may be a feature, as long as it's only allowed for admin users
    [ Label ],
    !,
    resolve_relative_descriptor(label_descriptor{label: Label},
                                Descriptor).
resolve_relative_descriptor(branch_of(Repo_Descriptor),
                            Descriptor) -->
    [ Branch_Name ],
    !,
    resolve_relative_descriptor(branch_descriptor{
                                    repository_descriptor: Repo_Descriptor,
                                    branch_name: Branch_Name
                                },
                                Descriptor).
resolve_relative_descriptor(commit_of(Repo_Descriptor),
                            Descriptor) -->
    [ Commit_Name ],
    !,
    resolve_relative_descriptor(commit_descriptor{
                                    repository_descriptor: Repo_Descriptor,
                                    commit_id: Commit_Name
                                },
                                Descriptor).
resolve_relative_descriptor(organization(_), _Descriptor, [], []) :-
    !,
    throw(error(address_resolve('tried to resolve organization which is not a valid descriptor'))).
resolve_relative_descriptor(Context, Descriptor, [], []) :-
    !,
    context_completion(Context, Descriptor).
resolve_relative_descriptor(organization(Organization), Descriptor) -->
    !,
    resolve_organization_relative_descriptor(Organization, Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    { database_descriptor{} :< Context },
    !,
    resolve_database_relative_descriptor(Context, Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    { repository_descriptor{} :< Context },
    !,
    resolve_repository_relative_descriptor(Context, Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    { branch_descriptor{} :< Context },
    !,
    resolve_repository_relative_descriptor(Context.repository_descriptor, Descriptor).
resolve_relative_descriptor(Context, Descriptor) -->
    { commit_descriptor{} :< Context },
    !,
    resolve_repository_relative_descriptor(Context.repository_descriptor, Descriptor).

resolve_root_relative_descriptor(root) -->
    [ ".." ],
    !.
resolve_root_relative_descriptor(system_descriptor{}) -->
    [ "_system" ],
    !.
resolve_root_relative_descriptor(Descriptor) -->
    [ "label" ],
    !,
    resolve_relative_descriptor(some_label, Descriptor).
resolve_root_relative_descriptor(Descriptor) -->
    [ Organization ],
    !,
    resolve_relative_descriptor(organization(Organization), Descriptor).

resolve_organization_relative_descriptor(Organization, Descriptor) -->
    [ Db ],
    !,
    resolve_relative_descriptor(database_descriptor{
                                    organization_name: Organization,
                                    database_name: Db
                                }, Descriptor).
resolve_database_relative_descriptor(Context, Context) -->
    [ "_meta" ],
    !.
resolve_database_relative_descriptor(Context, Descriptor) -->
    [ Repository ],
    !,
    resolve_relative_descriptor(repository_descriptor{
                                    database_descriptor: Context,
                                    repository_name: Repository
                                },
                                Descriptor).

resolve_repository_relative_descriptor(Context, Context) -->
    [ "_commits" ],
    !.
resolve_repository_relative_descriptor(Context, Descriptor) -->
    [ "branch" ],
    !,
    resolve_relative_descriptor(branch_of(Context),
                                Descriptor).
resolve_repository_relative_descriptor(Context, Descriptor) -->
    [ "commit" ],
    !,
    resolve_relative_descriptor(commit_of(Context),
                                Descriptor).

:- begin_tests(absolute_and_relative_paths_equivalent).
test(system_descriptor) :-
    Address = ["_system"],
    resolve_absolute_descriptor(Address, Descriptor1),
    resolve_relative_descriptor(root, Descriptor2, Address, []),
    Descriptor1 = Descriptor2.

test(label_descriptor) :-
    Address = ["label", "a_label"],
    resolve_absolute_descriptor(Address, Descriptor1),
    resolve_relative_descriptor(root, Descriptor2, Address, []),
    Descriptor1 = Descriptor2.

test(database_descriptor) :-
    Address = ["an_organization", "a_database", "_meta"],
    resolve_absolute_descriptor(Address, Descriptor1),
    resolve_relative_descriptor(root, Descriptor2, Address, []),
    Descriptor1 = Descriptor2.

test(repository_descriptor) :-
    Address = ["an_organization", "a_database", "a_repository", "_commits"],
    resolve_absolute_descriptor(Address, Descriptor1),
    resolve_relative_descriptor(root, Descriptor2, Address, []),
    Descriptor1 = Descriptor2.

test(branch_descriptor) :-
    Address = ["an_organization", "a_database", "a_repository", "branch", "a_branch"],
    resolve_absolute_descriptor(Address, Descriptor1),
    resolve_relative_descriptor(root, Descriptor2, Address, []),
    Descriptor1 = Descriptor2.

test(commit_descriptor) :-
    Address = ["an_organization", "a_database", "a_repository", "commit", "a_commit"],
    resolve_absolute_descriptor(Address, Descriptor1),
    resolve_relative_descriptor(root, Descriptor2, Address, []),
    Descriptor1 = Descriptor2.

:- end_tests(absolute_and_relative_paths_equivalent).

resolve_relative_descriptor(Context, Path, Descriptor) :-
    resolve_relative_descriptor(Context, Descriptor, Path, []).

resolve_absolute_string_descriptor(String, Descriptor) :-
    var(String),
    !,
    resolve_absolute_descriptor(Path_List, Descriptor),
    merge_separator_split(String, '/', Path_List).
resolve_absolute_string_descriptor(String, Descriptor) :-
    pattern_string_split('/', String, Path_Unfiltered),
    exclude('='(""), Path_Unfiltered, Path),
    resolve_absolute_descriptor(Path, Descriptor).

resolve_relative_string_descriptor(Context, String, Descriptor) :-
    pattern_string_split('/', String, Path_Unfiltered),
    exclude('='(""), Path_Unfiltered, Path),

    resolve_relative_descriptor(Context, Path, Descriptor).

resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph) :-
    pattern_string_split('/', String, Path_Unfiltered),
    exclude('='(""), Path_Unfiltered, Path),
    (   resolve_absolute_graph_descriptor(Path, Graph)
    ->  once(append(Descriptor_Path,[_Type],Path)),
        resolve_absolute_descriptor(Descriptor_Path, Descriptor)
    ;   append(Path, [], Path_and_Name),
        resolve_absolute_graph_descriptor(Path_and_Name, Graph)
    ->  once(append(Descriptor_Path,[_Type],Path)),
        resolve_absolute_descriptor(Descriptor_Path, Descriptor)
    ;   append(Path, [instance], Path_Type_and_Name),
        resolve_absolute_graph_descriptor(Path_Type_and_Name, Graph)
    ->  resolve_absolute_descriptor(Path, Descriptor)
    % This is the case where no graph information is supplied and we're
    % finding the original descriptor in order to find the graph
    ;   resolve_absolute_string_descriptor(String, Descriptor),
        resolve_absolute_descriptor(Absolute, Descriptor),
        append(Absolute,[instance], Graph_Absolute),
        resolve_absolute_graph_descriptor(Graph_Absolute, Graph)
    ),
    resolve_absolute_descriptor(Descriptor_Path, Descriptor).

resolve_absolute_string_descriptor_and_graph(String, Descriptor,Graph) :-
    pattern_string_split('/', String, Path_Unfiltered),
    exclude('='(""), Path_Unfiltered, Path),
    once(append(Descriptor_Path,[_Type],Path)),
    resolve_absolute_descriptor(Descriptor_Path, Descriptor),
    resolve_absolute_graph_descriptor(Path, Graph).

resolve_absolute_string_graph_descriptor(String, Graph_Descriptor) :-
    var(String),
    !,
    resolve_absolute_graph_descriptor(Graph_List, Graph_Descriptor),
    pattern_string_split('/', String, Graph_List).
resolve_absolute_string_graph_descriptor(String, Graph_Descriptor) :-
    pattern_string_split('/', String, Graph_List),
    resolve_absolute_graph_descriptor(Graph_List, Graph_Descriptor).

% Note: Currently we only have instance/schema/inference updates for normal and terminus graphs.
% so this resolution is limited to these types.
resolve_absolute_graph_descriptor([Organization, DB, Repo, "branch", Branch, Type], Graph) :-
    !,
    Graph = branch_graph{ organization_name: Organization_Str,
                          database_name : DB_Str,
                          repository_name : Repo,
                          branch_name : Branch,
                          type : Type_Atom},
    coerce_string(Organization, Organization_Str),
    coerce_string(DB, DB_Str),
    atom_string(Type_Atom, Type).
resolve_absolute_graph_descriptor([Organization, DB, Repo, "commit", RefID, Type], Graph) :-
    !,
    Graph = single_commit_graph{ organization_name: Organization_Str,
                                 database_name : DB_Str,
                                 repository_name : Repo,
                                 commit_id : RefID,
                                 type : Type_Atom},
    coerce_string(Organization, Organization_Str),
    coerce_string(DB, DB_Str),
    atom_string(Type_Atom, Type).
resolve_absolute_graph_descriptor([Organization, DB, Repo, "_commits", Type], Graph) :-
    !,
    Graph = commit_graph{ organization_name: Organization_Str,
                          database_name : DB_Str,
                          repository_name : Repo_Str,
                          type : Type_Atom },
    coerce_string(Organization, Organization_Str),
    coerce_string(DB, DB_Str),
    coerce_string(Repo, Repo_Str),
    atom_string(Type_Atom, Type).
resolve_absolute_graph_descriptor([Organization, DB, "_meta", Type], Graph) :-
    !,
    Graph = repo_graph{ organization_name: Organization_Str,
                        database_name : DB_Str,
                        type : Type_Atom},
    coerce_string(Organization, Organization_Str),
    coerce_string(DB, DB_Str),
    atom_string(Type_Atom, Type).
resolve_absolute_graph_descriptor(["_system", Type], Graph) :-
    Graph = system_graph{ type : Type_Atom },
    atom_string(Type_Atom, Type).

%%
% resolve_filter(Filter_String,Filter) is det.
%
%  Turn a filter string into a a filter - used with 'from'
%
%  Syntax:
%
%  '*' => search instance and schema graphs
%  'instance' => search in instance graph
%  'schema' => search in schema graph
%
resolve_filter(Filter_String,Filter) :-
    atom_string(Type,Filter_String),
    (   Type = '*'
    ->  Filter = type_filter{ types : [instance,schema] }
    ;   Filter = type_name_filter{ type : Type }).

resolve_absolute_or_relative_string_descriptor(Desc_In,String,Desc_Out) :-
    once((   resolve_relative_string_descriptor(Desc_In,String,Desc_Out)
         ;   resolve_absolute_string_descriptor(String,Desc_Out))).

:- begin_tests(resolution).

test(test_full_resolution, []) :-
    String = "admin/db/local/branch/main/instance",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = branch_descriptor{
                     branch_name:"main",
                     repository_descriptor:
                     repository_descriptor{
                         database_descriptor:
                         database_descriptor{
                             database_name:"db",
                             organization_name:"admin"
                         },
                         repository_name:"local"}},
    Graph = branch_graph{
                branch_name:"main",
                database_name:"db",
                organization_name:"admin",
                repository_name:"local",
                type:instance}.

test(test_missing_name_resolution, []) :-
    String = "admin/db/local/branch/main/instance",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),

    Descriptor = branch_descriptor{
                     branch_name:"main",
                     repository_descriptor:
                     repository_descriptor{
                         database_descriptor:
                         database_descriptor{
                             database_name:"db",
                             organization_name:"admin"
                         },
                         repository_name:"local"}},

    Graph = branch_graph{
                branch_name:"main",
                database_name:"db",
                organization_name:"admin",
                repository_name:"local",
                type:instance}.

test(test_missing_type_and_name_resolution, []) :-
    String = "admin/db/local/branch/main",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),

    Descriptor = branch_descriptor{
                     branch_name:"main",
                     repository_descriptor:
                     repository_descriptor{
                         database_descriptor:
                         database_descriptor{
                             database_name:"db",
                             organization_name:"admin"
                         },
                         repository_name:"local"}},

    Graph = branch_graph{
                branch_name:"main",
                database_name:"db",
                organization_name:"admin",
                repository_name:"local",
                type:instance}.

test(test_missing_type_and_name_and_branch_resolution, []) :-
    String = "admin/db/local",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),

    Descriptor = branch_descriptor{
                     branch_name:"main",
                     repository_descriptor:
                     repository_descriptor{
                         database_descriptor:
                         database_descriptor{
                             database_name:"db",
                             organization_name:"admin"
                         },
                         repository_name:"local"}},

    Graph = branch_graph{
                branch_name:"main",
                database_name:"db",
                organization_name:"admin",
                repository_name:"local",
                type:instance}.

test(test_system_bare, []) :-
    String = "_system",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = system_descriptor{},
    Graph = system_graph{
                type : instance
            }.

% NOTE: There is no default given for system schema graphs here...
test(test_system_schema, []) :-
    String = "_system/instance",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = system_descriptor{},

    Graph = system_graph{
                type : instance
            }.

test(test_repo_bare, []) :-
    String = "admin/db/_meta",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = database_descriptor{database_name:"db",organization_name:"admin"},
    Graph = repo_graph{database_name:"db", organization_name:"admin", type:instance}.

test(test_repo_instance, []) :-
    String = "admin/db/_meta/instance",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = database_descriptor{database_name:"db",organization_name:"admin"},
    Graph = repo_graph{database_name:"db", organization_name:"admin", type:instance}.

test(test_commits_instance, []) :-
    String = "admin/db/local/_commits/instance",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = repository_descriptor{
                     database_descriptor:
                     database_descriptor{
                         database_name:"db",
                         organization_name:"admin"},
                     repository_name:"local"},
    Graph = commit_graph{
                database_name:"db",
                organization_name:"admin",
                repository_name:"local",
                type:instance
            }.

test(test_commits_bare, []) :-
    String = "admin/db/local/_commits",
    resolve_absolute_string_descriptor_and_default_graph(String, Descriptor, Graph),
    Descriptor = repository_descriptor{
                     database_descriptor:
                     database_descriptor{
                         database_name:"db",
                         organization_name:"admin"},
                     repository_name:"local"},
    Graph = commit_graph{
                database_name:"db",
                organization_name:"admin",
                repository_name:"local",
                type:instance
            }.

test(resolve_absolute_or_relative_string_descriptor, []) :-
    resolve_absolute_string_descriptor("admin/db", Desc1),
    String1 = "admin/db/local/branch/dev",
    resolve_absolute_or_relative_string_descriptor(Desc1,String1, Desc1_Out),
    Desc1_Out =
    branch_descriptor{branch_name:"dev",
                      repository_descriptor:
                      repository_descriptor{
                          database_descriptor:
                          database_descriptor{
                              database_name:"db",
                              organization_name:"admin"},
                          repository_name:"local"}},
    String2 = "_commits",
    resolve_absolute_or_relative_string_descriptor(Desc1,String2, Desc2),
    Desc2 = repository_descriptor{
                database_descriptor:
                database_descriptor{
                    database_name:"db",
                    organization_name:"admin"},
                repository_name:"local"}.

:- end_tests(resolution).
