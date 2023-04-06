:- module('document/history', [
              document_created_at/3,
              document_updated_at/3,
              document_history/5,
              changed_document_id/2
          ]).

:- use_module(library(yall)).

:- use_module(core(query)).
:- use_module(core(transaction/ref_entity)).

/*

History (paged) of an object returns an historical list of changes

history(Askable, Id, Start, End, Result).


Returns a list `Result` of the form:

```json
[{ commit_id : "SDFSDLKJE",
   modified : "2012-12-03T23:22:01Z",
   author : "Author",
   message : "Message"},
   ...
]

*/

document_created_at(_Askable, _Document, _At) :-
    fail.

document_updated_at(_Askable, _Document, _At) :-
    fail.

changed_document_id(Askable,Containing) :-
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

commit_generator(Repo, Branch_Name, Commit_Id) :-
    ask(Repo,
        (   t(Branch, name, Branch_Name^^xsd:string),
            t(Branch, head, Head_Commit),
            path(Head_Commit, star(p(parent)), Commit),
            t(Commit, identifier, Commit_Id^^xsd:string))).

has_change(Repo,Commit_Id,Id,Info) :-
    resolve_relative_descriptor(Repo,["commit", Commit_Id],Commit_Descriptor),
    changed_document_id(Commit_Descriptor, Id),
    commit_id_to_metadata(Commit_Descriptor, Commit_Id, Author, Message, Timestamp),
    Info = json{
               commit_id: Commit_Id,
               author: Author,
               message: Message,
               timestamp: Timestamp
           }.

collect_history([_|_], _Repo, _Id, _Start, End, I, History-History) :-
    I > End,
    !.
collect_history([Commit_URI|Rest], Repo, Id, Start, End, I, History-History_Tail) :-
    I >= Start,
    !,
    (   has_change(Repo,Commit_URI,Id,Info)
    ->  History=[Info|Middle],
        Iprime is I + 1,
        collect_history(Rest, Repo, Id, Start, End, Iprime, Middle-History_Tail)
    ;   collect_history(Rest, Repo, Id, Start, End, I, History-History_Tail)
    ).
collect_history([_|Rest], Repo, Id, Start, End, I, History-History_Tail) :-
    I < Start,
    !,
    collect_history(Rest, Repo, Id, Start, End, I, History-History_Tail).
collect_history([], _Repo, _Id, _Start, _End, _I, History-History) :-
    !.

document_history(Descriptor, Id, Start, End, History) :-
    Branch_Name = (Descriptor.branch_name),
    Repo = (Descriptor.repository_descriptor),
    lazy_list(commit_generator(Repo,Branch_Name),LL),
    collect_history(LL,Repo,Id,Start,End,0,History-[]).


:- begin_tests(history).

:- use_module(core(util/test_utils)).

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
        C1,
        replace_document(C1,_{'@id' : Warsaw, '@type' : "City", name : "Warszawa"}, _)
    ),

    document_history(Descriptor, Warsaw, 0, inf, History),
    print_term(History, []).


:- end_tests(history).
