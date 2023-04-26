:- module('document/history', [
              document_created_at/3,
              document_updated_at/3,
              document_history/5,
              changed_document_id/2
          ]).

:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(lazy_lists)).
:- use_module(library(solution_sequences)).

:- use_module(core(document/json), [
                  database_prefixes/2
              ]).
:- use_module(core(query)).
:- use_module(core(transaction/ref_entity)).

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
    document_created(Commit_Descriptor, Id),
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
    document_modified(Commit_Descriptor, Id),
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

document_created(Askable, Id) :-
    ask(Askable,
        (   addition(Id, rdf:type, Type),
            once(((   t(Type,rdf:type,sys:'Class',schema)
                  ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                  ;   Type = sys:'JSONDocument'),
                  not(t(Type,sys:subdocument, _,schema)))))).

document_deleted(Askable, Id) :-
    ask(Askable,
        (   removal(Id, rdf:type, Type),
            once(((   t(Type,rdf:type,sys:'Class',schema)
                  ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
                  ;   Type = sys:'JSONDocument'),
                  not(t(Type,sys:subdocument, _,schema)))))).

document_modified(Askable, Containing) :-
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
    distinct(Containing,
             (   document_modified(Askable, Containing)
             ;   document_created(Askable, Containing)
             ;   document_deleted(Askable, Containing))).

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

    sort(Ids, Sorted_Ids),
    sort(Original_Ids, Original_Sorted_Ids),

    length(Original_Ids, 1),
    Sorted_Ids = Original_Sorted_Ids.

:- end_tests(history).
