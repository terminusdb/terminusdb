:- module('document/history', [
              document_created_at/3,
              document_updated_at/3,
              document_history/5,
              changed_document_id/2
          ]).

:- use_module(library(yall)).

:- use_module(core(document/json), [
                  database_prefixes/2
              ]).
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

pred(fail, H-T, H-T).
pred(next(Next), H-T1, H-T2) :-
    freeze(T1, T2 = [Next|T2]).

next([X|Tail], Tail) :-
    member(X, [1,2,3]).
next([], []).

document_created_at(_Askable, _Document, _At) :-
    fail.

document_updated_at(_Askable, _Document, _At) :-
    fail.


document_created(Askable, Id) :-
    ask(Askable,
        (   addition(Id, rdf:type, Type),
            (   t(Type,rdf:type,sys:'Class',schema)
            ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
            ;   Type = sys:'JSONDocument'))).

document_deleted(Askable, Id) :-
    ask(Askable,
        (   removal(Id, rdf:type, Type),
            (   t(Type,rdf:type,sys:'Class',schema)
            ;   t(Type,rdf:type,sys:'TaggedUnion',schema)
            ;   Type = sys:'JSONDocument'))).

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
             (   document_created(Askable, Containing)
             ;   document_deleted(Askable, Containing)
             ;   document_modified(Askable, Containing))).

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
collect_history([], _Repo, _Id, _Start, _End, _I, History-History).

document_history(Descriptor, Id, Start, End, History) :-
    Branch_Name = (Descriptor.branch_name),
    Repo = (Descriptor.repository_descriptor),
    commits(Repo,Branch_Name,LL),
    database_prefixes(Descriptor, Prefixes),
    prefix_expand(Id,Prefixes,Id_Ex),
    collect_history(LL,Repo,Id_Ex,Start,End,0,History-[]).


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
	                  commit_id:_,
	                  message:"test",
	                  timestamp:_
                    },
                json{ author:"test",
	                  commit_id:_,
	                  message:"test",
	                  timestamp:_
                    }
              ].


:- end_tests(history).
