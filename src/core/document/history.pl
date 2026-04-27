:- module('document/history', [
              document_created_at/3,
              document_updated_at/3,
              document_history/5,
              document_history/6,
              document_history_entries/5,
              enrich_entry/7,
              changed_document_id/2,
              commits_changed_id/5
          ]).

:- use_module(library(yall)).
:- use_module(library(lists), [reverse/2, union/3, member/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(lazy_lists)).
:- use_module(library(solution_sequences)).
:- use_module(core(util)).
:- use_module(core(document/json), [
                  database_prefixes/2,
                  get_document/3,
                  get_document/4
              ]).
:- use_module(core(document/diff), [
                  simple_diff/4
              ]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(plugins)).
:- use_module(library(option)).
:- use_module(library(http/json), [json_write_dict/3]).

commit_info_dict(Repo, Commit_Id, Info) :-
    commit_id_uri(Repo, Commit_Id, Commit_Uri),
    commit_uri_to_metadata(Repo, Commit_Uri, Author, Message, Timestamp),
    (   commit_uri_to_user(Repo, Commit_Uri, User)
    ->  true
    ;   User = null
    ),
    Info = json{
               identifier: Commit_Id,
               author: Author,
               message: Message,
               timestamp: Timestamp,
               user: User
           }.

/*

History (paged) of an object returns an historical list of changes

document_history(Askable, Id, Start, Count, Result).

Returns a list `Result` of the form:

```json
[{ identifier : "SDFSDLKJE",
   modified : "2012-12-03T23:22:01Z",
   author : "Author",
   message : "Message"},
   ...
]

*/

history_to_created_at([Commit_Id|_], Repo, Id, Info) :-
    resolve_relative_descriptor(Repo,["commit", Commit_Id],Commit_Descriptor),
    document_created(Commit_Descriptor, Id, []),
    !,
    commit_info_dict(Repo, Commit_Id, Info).
history_to_created_at([_|T], Repo, Id, Info) :-
    history_to_created_at(T, Repo, Id, Info).

document_created_at(Descriptor, Id, Info) :-
    Branch_Name = (Descriptor.branch_name),
    Repo = (Descriptor.repository_descriptor),
    commits(Repo,Branch_Name,LL),
    database_prefixes(Descriptor, Prefixes),
    prefix_expand(Id,Prefixes,Id_Ex),
    history_to_created_at(LL,Repo,Id_Ex,Info).

history_to_updated_at([Commit_Id|_], Repo, Id, Info) :-
    resolve_relative_descriptor(Repo,["commit", Commit_Id],Commit_Descriptor),
    document_modified(Commit_Descriptor, Id, []),
    !,
    commit_info_dict(Repo, Commit_Id, Info).
history_to_updated_at([_|T], Repo, Id, Info) :-
    history_to_updated_at(T, Repo, Id, Info).

document_updated_at(Descriptor, Id, Info) :-
    Branch_Name = (Descriptor.branch_name),
    Repo = (Descriptor.repository_descriptor),
    commits(Repo,Branch_Name,LL),
    database_prefixes(Descriptor, Prefixes),
    prefix_expand(Id,Prefixes,Id_Ex),
    history_to_updated_at(LL,Repo,Id_Ex,Info).

document_created(Askable, Id, Options) :-
    ignore(option(type(Type), Options)),
    ask(Askable,
        (   addition(Id, rdf:type, Type),
            once(((   t(Type,rdf:type,sys:'Class',schema)
                  ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                  ;   Type = sys:'JSONDocument'),
                  not(t(Type,sys:subdocument, _,schema)))))).

document_deleted(Askable, Id, Options) :-
    ignore(option(type(Type), Options)),
    ask(Askable,
        (   removal(Id, rdf:type, Type),
            once(((   t(Type,rdf:type,sys:'Class',schema)
                  ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                  ;   Type = sys:'JSONDocument'),
                  not(t(Type,sys:subdocument, _,schema)))))).

document_modified(Askable, Containing, Options) :-
    ignore(option(type(Type), Options)),
    ask(Askable,
        (   distinct(Id, (   addition(Id, _, _)
                         ;   removal(Id, _, _),
                             once(t(Id, _, _))
                         )),
            once((path(Containing, star(p), Id),
                  t(Containing,rdf:type,Type),
                  (   t(Type,rdf:type,sys:'Class',schema)
                  ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                  ;   Type = sys:'JSONDocument'),
                  not(t(Type,sys:subdocument, _,schema)))))).

changed_document_id(Askable,Containing) :-
    changed_document_id(Askable,Containing,options{}).

changed_document_id(Askable,Containing,Options) :-
    distinct(Containing,
             (   document_modified(Askable, Containing, Options)
             ;   document_created(Askable, Containing, Options)
             ;   document_deleted(Askable, Containing, Options))).

commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Changed, Options) :-
    create_context(Branch_Descriptor.repository_descriptor, Context),
    most_recent_common_ancestor(Context, Context,
                                Before_Commit_Id, After_Commit_Id,
                                _Shared_Commit_Id, Commit1_Path, Commit2_Path),

    distinct(Changed,
             (   union(Commit1_Path, Commit2_Path, All_Commits),
                 member(Commit_Id, All_Commits),
                 resolve_relative_descriptor(Branch_Descriptor,
                                             ["commit", Commit_Id],
                                             Commit_Descriptor),
                 do_or_die(
                     open_descriptor(Commit_Descriptor, Transaction),
                     error(unresolvable_collection(Commit_Descriptor), _)),
                 changed_document_id(Transaction, Changed, Options)
             )
            ).

commit_generator(Repo, state(name(Branch_Name)), state(commit(Head_Commit)), Commit_Id) :-
    ask(Repo,
        (   t(Branch, name, Branch_Name^^xsd:string),
            t(Branch, head, Head_Commit),
            t(Head_Commit, identifier, Commit_Id^^xsd:string)
        )).
commit_generator(Repo, state(commit(Commit_Uri)), state(commit(Commit_Parent)), Commit_Id) :-
    ask(Repo,
        (   t(Commit_Uri, parent, Commit_Parent),
            t(Commit_Parent, identifier, Commit_Id^^xsd:string)
        )).

commits(Repo, Branch_Name, Commits) :-
    lazy_list(commit_generator(Repo), state(name(Branch_Name)), Commits).

has_change(Repo,Commit_Id,Id,Info) :-
    resolve_relative_descriptor(Repo,["commit", Commit_Id],Commit_Descriptor),
    changed_document_id(Commit_Descriptor, Id),
    commit_info_dict(Repo, Commit_Id, Info).

enrich_entry(Repo, _Descriptor, Commit_Id, Id, Info0, Info, Options) :-
    (   option(complete(true), Options)
    ->  (   catch(
                (   commit_id_uri(Repo, Commit_Id, Commit_Uri),
                    get_document(Repo, Commit_Uri, Commit_Doc),
                    (   del_dict('@id', Commit_Doc, _, CD1)
                    ->  true
                    ;   CD1 = Commit_Doc
                    ),
                    (   del_dict('@type', CD1, _, Info1)
                    ->  true
                    ;   Info1 = CD1
                    )
                ),
                _,
                fail)
        ->  true
        ;   Info1 = Info0
        )
    ;   Info1 = Info0
    ),
    (   option(diff(true), Options)
    ->  (   catch(
                (   commit_id_uri(Repo, Commit_Id, CUri),
                    resolve_relative_descriptor(Repo, ["commit", Commit_Id], Commit_Desc),
                    open_descriptor(Commit_Desc, commit_info{}, Commit_Transaction, [], Map1),
                    Doc_Options = options{
                                      compress_ids : true,
                                      unfold: true,
                                      keep_json_type: true
                                  },
                    get_document(Commit_Transaction, Id, After_Doc, Doc_Options),
                    (   commit_uri_to_parent_uri(Repo, CUri, Parent_Uri),
                        commit_id_uri(Repo, Parent_Commit_Id, Parent_Uri),
                        resolve_relative_descriptor(Repo, ["commit", Parent_Commit_Id], Parent_Desc),
                        open_descriptor(Parent_Desc, commit_info{}, Parent_Transaction, Map1, _),
                        get_document(Parent_Transaction, Id, Before_Doc, Doc_Options)
                    ->  simple_diff(Before_Doc, After_Doc, Diff,
                                    options{keep: json{'@id': true, '_id': true}})
                    ;   Diff = json{ '@op' : "Insert",
                                     '@insert' : After_Doc }
                    ),
                    put_dict(diff, Info1, Diff, Info)
                ),
                _,
                fail)
        ->  true
        ;   Info = Info1
        )
    ;   Info = Info1
    ).

walk_history(Commits, Repo, Descriptor, Id, Start, Count, I, Goal, Before, After, Options) :-
    walk_history_(Commits, Repo, Descriptor, Id, Start, Count, I, Goal, Before, After, Options).

walk_history_([_|_], _Repo, _Descriptor, _Id, _Start, Count, I, _Goal, _Before, _After, _Options) :-
    I >= Count,
    !.
walk_history_([Commit_Id|Rest], Repo, Descriptor, Id, Start, Count, I, Goal, Before, After, Options) :-
    commit_id_uri(Repo, Commit_Id, Commit_Uri),
    commit_uri_to_metadata(Repo, Commit_Uri, _, _, TS),
    !,
    (   Before \= none, TS >= Before
    ->  walk_history_(Rest, Repo, Descriptor, Id, Start, Count, I, Goal, Before, After, Options)
    ;   After \= none, TS < After
    ->  true
    ;   I >= Start
    ->  (   has_change(Repo, Commit_Id, Id, Info0)
        ->  call(Goal, Info0),
            Iprime is I + 1,
            walk_history_(Rest, Repo, Descriptor, Id, Start, Count, Iprime, Goal, Before, After, Options)
        ;   walk_history_(Rest, Repo, Descriptor, Id, Start, Count, I, Goal, Before, After, Options)
        )
    ;   StartPrime is Start - 1,
        walk_history_(Rest, Repo, Descriptor, Id, StartPrime, Count, I, Goal, Before, After, Options)
    ).
walk_history_([], _Repo, _Descriptor, _Id, _Start, _Count, _I, _Goal, _Before, _After, _Options).

document_history_entries(Descriptor, Id, Goal, Options, History_Options) :-
    database_prefixes(Descriptor, Prefixes),
    prefix_expand(Id, Prefixes, Id_Ex),
    Branch_Name = (Descriptor.branch_name),
    Repo = (Descriptor.repository_descriptor),
    Enriched_Goal = 'document/history':enrich_and_call(Repo, Descriptor, Id_Ex, Options, Goal),
    (   option(fast(true), Options)
    ->  (   plugins:fast_document_history_entries(Descriptor, Id_Ex,
                                                  Enriched_Goal, Options,
                                                  History_Options)
        ->  true
        ;   throw(error(fast_history_not_available, _))
        )
    ;   commits(Repo, Branch_Name, LL),
        option(start(Start), History_Options, 0),
        option(count(Count), History_Options, inf),
        (   option(before(Before), History_Options) -> true ; Before = none ),
        (   option(after(After), History_Options) -> true ; After = none ),
        walk_history(LL, Repo, Descriptor, Id_Ex, Start, Count, 0,
                     Enriched_Goal, Before, After, Options)
    ).

enrich_and_call(Repo, Descriptor, Id, Options, Goal, Info0) :-
    enrich_history_entry(Repo, Descriptor, Id, Options, Info0, Info),
    call(Goal, Info).

enrich_history_entry(Repo, Descriptor, Id, Options, Info0, Info) :-
    get_dict(identifier, Info0, Commit_Id),
    enrich_entry(Repo, Descriptor, Commit_Id, Id, Info0, Info, Options).

collect_goal(Acc, Entry) :-
    nb_getval(Acc, Entries),
    nb_setval(Acc, [Entry|Entries]).

document_history(Descriptor, Id, Start, Count, History) :-
    document_history(Descriptor, Id, Start, Count, History, []).

document_history(Descriptor, Id, Start, Count, History, Options) :-
    database_prefixes(Descriptor, Prefixes),
    prefix_expand(Id,Prefixes,Id_Ex),
    (   fast_document_history(Descriptor, Id_Ex, Start, Count, History0, Options)
    ->  true
    ;   option(fast(true), Options)
    ->  throw(error(fast_history_not_available, _))
    ;   Branch_Name = (Descriptor.branch_name),
        Repo0 = (Descriptor.repository_descriptor),
        commits(Repo0, Branch_Name, LL),
        (   option(before(Before), Options) -> true ; Before = none ),
        (   option(after(After), Options) -> true ; After = none ),
        Acc = '$history_acc',
        nb_setval(Acc, []),
        walk_history(LL, Repo0, Descriptor, Id_Ex, Start, Count, 0,
                     collect_goal(Acc), Before, After, Options),
        nb_getval(Acc, Rev_History0),
        reverse(Rev_History0, History0),
        nb_setval(Acc, [])
    ),
    Repo = (Descriptor.repository_descriptor),
    maplist(enrich_history_entry(Repo, Descriptor, Id_Ex, Options), History0, History).


:- begin_tests(history).

:- use_module(core(util/test_utils)).
:- use_module(core(util)).
:- use_module(core(document/json), [
                  insert_document/3,
                  replace_document/3
              ]).

test(show_document_history,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    resolve_absolute_string_descriptor(Path, Descriptor),
    with_test_transaction(
        Descriptor,
        C1,
        insert_document(C1,_{'@type' : "City", name : "Warsaw"}, Warsaw)
    ),

    with_test_transaction(
        Descriptor,
        C2,
        replace_document(C2,_{'@id' : Warsaw, '@type' : "City", name : "Warszawa"}, _)
    ),

    document_history(Descriptor, Warsaw, 0, inf, History),
    History = [ json{ author:"test",
	                  identifier:_,
	                  message:"test",
	                  timestamp:_,
	                  user:null
                    },
                json{ author:"test",
	                  identifier:_,
	                  message:"test",
	                  timestamp:_,
	                  user:null
                    }
              ].

test(show_document_updated_created,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    resolve_absolute_string_descriptor(Path, Descriptor),
    with_test_transaction(
        Descriptor,
        C1,
        insert_document(C1,_{'@type' : "City", name : "Warsaw"}, Warsaw)
    ),

    with_test_transaction(
        Descriptor,
        C2,
        replace_document(C2,_{'@id' : Warsaw, '@type' : "City", name : "Warszawa"}, _)
    ),

    document_history(Descriptor, Warsaw, 0, inf, History),
    History = [ json{ author:"test",
	                  identifier:Commit_1,
	                  message:"test",
	                  timestamp:TS_1,
	                  user:null
                    },
                json{ author:"test",
	                  identifier:Commit_2,
	                  message:"test",
	                  timestamp:TS_2,
	                  user:null
                    }
              ],

    document_updated_at(Descriptor, Warsaw, Updated),
    json{ author:"test",
	      identifier:Commit_1,
	      message:"test",
	      timestamp:TS_1,
	      user:null
        } :< Updated,

    document_created_at(Descriptor, Warsaw, Created),
    json{ author:"test",
	      identifier:Commit_2,
	      message:"test",
	      timestamp:TS_2,
	      user:null
        } :< Created.

test(complete_history_includes_commit,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    resolve_absolute_string_descriptor(Path, Descriptor),
    with_test_transaction(
        Descriptor,
        C1,
        insert_document(C1,_{'@type' : "City", name : "Warsaw"}, Warsaw)
    ),

    document_history(Descriptor, Warsaw, 0, inf, History, [complete(true)]),
    History = [Entry],
    get_dict(identifier, Entry, _),
    get_dict(author, Entry, _),
    get_dict(message, Entry, _),
    get_dict(timestamp, Entry, _),
    get_dict(instance, Entry, _),
    get_dict(schema, Entry, _),
    get_dict(parent, Entry, _).

test(diff_history_includes_diff,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    resolve_absolute_string_descriptor(Path, Descriptor),
    with_test_transaction(
        Descriptor,
        C1,
        insert_document(C1,_{'@type' : "City", name : "Warsaw"}, Warsaw)
    ),

    with_test_transaction(
        Descriptor,
        C2,
        replace_document(C2,_{'@id' : Warsaw, '@type' : "City", name : "Warszawa"}, _)
    ),

    document_history(Descriptor, Warsaw, 0, inf, History, [diff(true)]),
    History = [Update_Entry, _Create_Entry],
    get_dict(diff, Update_Entry, Diff),
    get_dict(name, Diff, _).

test(complete_and_diff_combined,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    resolve_absolute_string_descriptor(Path, Descriptor),
    with_test_transaction(
        Descriptor,
        C1,
        insert_document(C1,_{'@type' : "City", name : "Warsaw"}, Warsaw)
    ),

    with_test_transaction(
        Descriptor,
        C2,
        replace_document(C2,_{'@id' : Warsaw, '@type' : "City", name : "Warszawa"}, _)
    ),

    document_history(Descriptor, Warsaw, 0, inf, History, [complete(true), diff(true)]),
    History = [Entry1, Entry2],
    get_dict(instance, Entry1, _),
    get_dict(diff, Entry1, _),
    get_dict(instance, Entry2, _),
    get_dict(diff, Entry2, _).

original_changed_document_id(Askable,Containing) :-
    ask(Askable,
        distinct(Containing,
                 (   distinct(Id, (   addition(Id, _, _)
                                  ;   removal(Id, _, _),
                                      once(t(Id, _, _))
                                  )),
                     once((path(Containing, star(p), Id),
                           t(Containing,rdf:type,Type),
                           (   t(Type,rdf:type,sys:'Class',schema)
                           ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                           ;   Type = sys:'JSONDocument'),
                           not(t(Type,sys:subdocument, _,schema))))
                 ;   removal(Id, rdf:type, Type),
                     once(((   t(Type,rdf:type,sys:'Class',schema)
                           ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                           ;   Type = sys:'JSONDocument'),
                           not(t(Type,sys:subdocument, _,schema)))),
                     Containing = Id
                 )
                )
       ).

subdocument_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type" : "Class",
  "@id" : "Thing",
  "sub" : "Sub" }

{ "@type" : "Class",
  "@id" : "Sub",
  "@subdocument" : [],
  "@key" : { "@type" : "Random" },
  "string" : "xsd:string" }

').

:- use_module(core(document/json)).

test(subdocument_apply,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(subdocument_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Desc,
        C2,
        insert_document(C2,_{sub : _{ string : "foo" }}, _),
        _
    ),

    findall(Id,
            changed_document_id(Desc, Id),
            Ids),

    findall(Id,
            original_changed_document_id(Desc, Id),
            Original_Ids),

    open_descriptor(Desc, Transaction),
    findall(Id,
            (   '$changes':changed_document_id(Transaction, Id_String, _Change_Type),
                atom_string(Id, Id_String)),
            Rust_Ids),

    sort(Ids, Sorted_Ids),
    sort(Original_Ids, Original_Sorted_Ids),
    sort(Rust_Ids, Rust_Sorted_Ids),

    length(Original_Ids, 1),
    Sorted_Ids = Original_Sorted_Ids,
    Sorted_Ids = Rust_Sorted_Ids.

:- end_tests(history).

:- begin_tests(rust_changes).

:- use_module(core(util/test_utils)).
:- use_module(core(util)).
:- use_module(core(document/json)).
schema('
{
  "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"
}
{
  "@id": "Top",
  "@type": "Class",
  "subs": {
    "@class": "Sub",
    "@type": "Set"
  },
  "x": {
    "@class": "X",
    "@type": "Set"
  }
}
{
  "@id": "Sub",
  "@key": {
    "@type": "Random"
  },
  "@subdocument": [],
  "@type": "Class",
  "foo": "xsd:string"
}
{
  "@id": "X",
  "@type": "Class",
  "bar": "xsd:string"
}

{
  "@id": "Top2",
  "@type": "Class",
  "a": "A"
}
{
  "@id": "A",
  "@key": {
    "@type": "Random"
  },
  "@subdocument": [],
  "@type": "Class",
  "b": "B"
}
{
  "@id": "B",
  "@key": {
    "@type": "Random"
  },
  "@subdocument": [],
  "@type": "Class",
  "c": "C"
}
{
  "@id": "C",
  "@key": {
    "@type": "Random"
  },
  "@subdocument": [],
  "@type": "Class",
  "d": {
    "@class": "D",
    "@type": "Set"
  }
}
{
  "@id": "D",
  "@key": {
    "@type": "Random"
  },
  "@subdocument": [],
  "@type": "Class",
  "e": "xsd:string"
}
').
test(insert_toplevel,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    with_test_transaction(Desc,T,
                          (   insert_document(T, _{
                                                     a: _{b: _{c: _{d: _{e: "hello"}}}}
                                                 }, Inserted_Id1),
                              insert_document(T, _{
                                                     a: _{b: _{c: _{d: _{e: "how"}}}}
                                                 }, Inserted_Id2),
                              insert_document(T, _{
                                                     a: _{b: _{c: _{d: _{e: "are"}}}}
                                                 }, Inserted_Id3),
                              insert_document(T, _{
                                                     a: _{b: _{c: _{d: _{e: "you"}}}}
                                                 }, Inserted_Id4)
                          ),
                          _),
    open_descriptor(Desc, T2),
    findall(Id-Change,
            (   '$changes':changed_document_id(T2, Id_String, Change),
                atom_string(Id, Id_String)),
            Changes),
    sort(Changes, Sorted),
    Expected = [Inserted_Id1-added,
                Inserted_Id2-added,
                Inserted_Id3-added,
                Inserted_Id4-added],
    sort(Expected, Expected_Sorted),

    Expected_Sorted = Sorted,

    with_test_transaction(Desc,T3,
                          delete_document(T3, Inserted_Id4),
                          _),

    open_descriptor(Desc, T4),

    findall(Id-Change,
            (   '$changes':changed_document_id(T4, Id_String, Change),
                atom_string(Id, Id_String)),
            Changes2),
    Expected2 = [Inserted_Id4-deleted],
    Expected2 = Changes2,

    with_test_transaction(Desc, T5,
                          replace_document(T5,
                                           _{'@id': Inserted_Id2,
                                             a: _{b: _{c: _{d: _{e: "why"}}}}},
                                           _),
                          _),
    open_descriptor(Desc, T6),

    findall(Id-Change,
            (   '$changes':changed_document_id(T6, Id_String, Change),
                atom_string(Id, Id_String)),
            Changes3),
    Expected3 = [Inserted_Id2-changed],
    Expected3 = Changes3,

    get_document(Desc, Inserted_Id1, Doc1),
    Inner_Dict=(Doc1.a.b.c),
    b_set_dict(d, Inner_Dict, [_{e:"hi"}|(Inner_Dict.d)]),
    with_test_transaction(Desc, T7,
                          replace_document(T7,
                                           Doc1,
                                           _),
                          _),

    open_descriptor(Desc, T8),

    findall(Id-Change,
            (   '$changes':changed_document_id(T8, Id_String, Change),
                atom_string(Id, Id_String)),
            Changes4),
    Expected4 = [Inserted_Id1-changed],
    Expected4 = Changes4,

    get_document(Desc, Inserted_Id1, Doc2),
    Inner_Dict2=(Doc2.a.b.c),
    [Single_E|_] = (Inner_Dict2.d),
    b_set_dict(d, Inner_Dict2, [Single_E]),
    with_test_transaction(Desc, T9,
                          replace_document(T9,
                                           Doc2,
                                           _),
                          _),

    open_descriptor(Desc, T10),

    findall(Id-Change,
            (   '$changes':changed_document_id(T10, Id_String, Change),
                atom_string(Id, Id_String)),
            Changes5),
    Expected5 = [Inserted_Id1-changed],
    Expected5 = Changes5,

    true.


:- end_tests(rust_changes).

