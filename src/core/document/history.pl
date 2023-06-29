:- module('document/history', [
              document_created_at/3,
              document_updated_at/3,
              document_history/5,
              changed_document_id/2,
              commits_changed_id/5
          ]).

:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(lazy_lists)).
:- use_module(library(solution_sequences)).
:- use_module(core(util)).
:- use_module(core(document/json), [
                  database_prefixes/2
              ]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(library(option)).

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
    commit_id_to_metadata(Repo, Commit_Id, Author, Message, Timestamp),
    Info = json{
               identifier: Commit_Id,
               author: Author,
               message: Message,
               timestamp: Timestamp
           }.
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
    commit_id_to_metadata(Repo, Commit_Id, Author, Message, Timestamp),
    Info = json{
               identifier: Commit_Id,
               author: Author,
               message: Message,
               timestamp: Timestamp
           }.
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
    commit_id_to_metadata(Repo, Commit_Id, Author, Message, Timestamp),
    Info = json{
               identifier: Commit_Id,
               author: Author,
               message: Message,
               timestamp: Timestamp
           }.

collect_history([_|_], _Repo, _Id, _Start, Count, I, History-History) :-
    I >= Count,
    !.
collect_history([Commit_URI|Rest], Repo, Id, Start, Count, I, History-History_Tail) :-
    I >= Start,
    !,
    (   has_change(Repo,Commit_URI,Id,Info)
    ->  History=[Info|Middle],
        Iprime is I + 1,
        collect_history(Rest, Repo, Id, Start, Count, Iprime, Middle-History_Tail)
    ;   collect_history(Rest, Repo, Id, Start, Count, I, History-History_Tail)
    ).
collect_history([_|Rest], Repo, Id, Start, Count, I, History-History_Tail) :-
    I < Start,
    !,
    StartPrime is Start - 1,
    collect_history(Rest, Repo, Id, StartPrime, Count, I, History-History_Tail).
collect_history([], _Repo, _Id, _Start, _Count, _I, History-History).

document_history(Descriptor, Id, Start, Count, History) :-
    Branch_Name = (Descriptor.branch_name),
    Repo = (Descriptor.repository_descriptor),
    commits(Repo,Branch_Name,LL),
    database_prefixes(Descriptor, Prefixes),
    prefix_expand(Id,Prefixes,Id_Ex),
    collect_history(LL,Repo,Id_Ex,Start,Count,0,History-[]).


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
	                  timestamp:_
                    },
                json{ author:"test",
	                  identifier:_,
	                  message:"test",
	                  timestamp:_
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
	                  timestamp:TS_1
                    },
                json{ author:"test",
	                  identifier:Commit_2,
	                  message:"test",
	                  timestamp:TS_2
                    }
              ],

    document_updated_at(Descriptor, Warsaw, Updated),
    json{ author:"test",
	      identifier:Commit_1,
	      message:"test",
	      timestamp:TS_1
        } :< Updated,

    document_created_at(Descriptor, Warsaw, Created),
    json{ author:"test",
	      identifier:Commit_2,
	      message:"test",
	      timestamp:TS_2
        } :< Created.

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

