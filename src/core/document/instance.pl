:- module(json_instance, [
              refute_instance/1
          ]).

/*
 * Introconversion between JSON-LD and a schema language.
 *
 */

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

:- use_module(schema).
%:- use_module(json_conv).

:- use_module(library(aggregate)).


is_rdf_list_(Instance, rdf:nil) :-
    !.
is_rdf_list_(Instance, Type) :-
    xrdf(Instance, Type, rdf:first, _Car),
    xrdf(Instance, Type, rdf:rest, Cdr),
    is_rdf_list_(Instance, Cdr).

is_rdf_list(Validation_Object, Type) :-
    database_instance(Validation_Object, Instance),
    is_rdf_list_(Instance, Type).

% This looks dubious
is_instance(_Validation_Object, _^^T,T) :-
    !.
is_instance(_Validation_Object, _@_,rdf:literal) :-
    !.
is_instance(Validation_Object, X,C) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, rdf:type,Class),
    is_simple_class(Validation_Object, Class),
    class_subsumed(Validation_Object, Class,C),
    !.

instance_of(Validation_Object, X,C) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, X,rdf:type,C).

/* Not the best checker... */
is_date(A) :-
    atom(A).
is_date(A) :-
    string(A).

is_dateTime(A) :-
    atom(A).
is_dateTime(A) :-
    string(A).

refute_base_type_(xsd:string, A, Witness) :-
    atom(A),
    !,
    Witness = witness{ '@type': object_not_of_type,
                       value: A,
                       value_type: xsd:string
                     }.

refute_base_type_(xsd:string, S^^_, Witness) :-
    \+ string(S),
    Witness = witness{ '@type': object_not_of_type,
                       value: S,
                       value_type: xsd:string
                     }.
refute_base_type_(xsd:decimal, N^^_, Witness) :-
    \+ number(N),
    Witness = witness{ '@type': object_not_of_type,
                       value: N,
                       value_type: xsd:decimal
                     }.
refute_base_type_(xsd:date, D^^_, Witness) :-
    \+ is_date(D),
    Witness = witness{ '@type': object_not_of_type,
                       value: D,
                       value_type: xsd:date
                     }.
refute_base_type_(xsd:dateTime, D^^_, Witness) :-
    \+ is_dateTime(D),
    Witness = witness{ '@type': object_not_of_type,
                       value: D,
                       value_type: xsd:dateTime
                     }.

refute_base_type(N,Type,Witness) :-
    refute_base_type_(Type,N,Witness).

array_object(Validation_Object, S,I,O) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, S,rdf:type,array_object),
    % cardinality one
    findall(t(S,index,I),
            xrdf(Instance, S,index,I),
            [t(S,index,I)]),
    % cardinality one
    findall(t(S,object,O),
            xrdf(Instance, S,object,O),
            [t(S,object,O)]).

member_list_(Instance, O,L) :-
    xrdf(Instance, L, rdf:first, O).
member_list_(Instance, O,L) :-
    xrdf(Instance, L, rdf:rest, Cdr),
    member_list_(Instance,O,Cdr).

member_list(Validation_Object, O, L) :-
    database_instance(Validation_Object, Instance),
    member_list_(Instance, O, L).

card_count(Validation_Object, S,P,O,N) :-
    % choose as existential anything free
    database_instance(Validation_Object, Instance),
    aggregate(count,[S,P,O]^xrdf(Instance,S,P,O),N).

refute_cardinality_(class(C),Validation_Object,S,P,Witness) :-
    \+ card_count(Validation_Object, S,P,_,1),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       class: C,
                       predicate: P
                     }.
refute_cardinality_(base_class(C),Validation_Object,S,P,Witness) :-
    \+ card_count(Validation_Object, S,P,_,1),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       class: C,
                       predicate: P
                     }.
refute_cardinality_(enum(C,_),Validation_Object, S,P,Witness) :-
    \+ card_count(Validation_Object, S,P,_,1),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       class: C,
                       predicate: P
                     }.
refute_cardinality_(tagged_union(C,_),Validation_Object,S,P,Witness) :-
    \+ card_count(Validation_Object,S,P,_,1),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       class: C,
                       predicate: P
                     }.
refute_cardinality_(set(_C),_Validation_Object,_S,_P,_Witness) :-
    % no bad cardinality possible
    fail.
refute_cardinality_(seq(_C),_Validation_Object,_S,_P,_Witness) :-
    % no bad cardinality possible
    fail.
refute_cardinality_(list(C),Validation_Object,S,P,Witness) :-
    \+ card_count(Validation_Object,S,P,_,1),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       class: C,
                       predicate: P
                     }.
refute_cardinality_(optional(C),Validation_Object,S,P,Witness) :-
    card_count(Validation_Object,S,P,_,N),
    (   \+ memberchk(N, [0,1])
    ->  Witness = witness{ '@type': instance_has_wrong_cardinality,
                           class: C,
                           instance: S,
                           predicate: P,
                           cardinality: N
                         }
    ).
refute_cardinality_(card(C,N),Validation_Object,S,P,Witness) :-
    \+ card_count(Validation_Object,S,P,_,N),
    Witness = witness{ '@type': instance_has_wrong_cardinality,
                       class: C,
                       instance: S,
                       predicate: P,
                       cardinality: N
                     }.

refute_cardinality(Validation_Object,S,P,C,Witness) :-
    class_predicate_type(Validation_Object, C,P,Desc),
    refute_cardinality_(Desc,Validation_Object,S,P,Witness).

refute_built_in_value(Validation_Object, rdf:type,O,Witness) :-
    refute_class(Validation_Object, O,Witness).
refute_built_in_value(_Validation_Object, rdfs:comment,O@L,Witness) :-
    \+ (string(O),
        atom(L)),
    format(atom(Atom), '~q@~q', [O,L]),
    Witness = witness{
                  '@type': comment_not_valid,
                  comment: Atom
              }.
refute_built_in_value(_Validation_Object, rdfs:label,O@L,Witness) :-
    \+ (string(O),
        atom(L)),
    format(atom(Atom), '~q@~q', [O,L]),
    Witness = witness{
                  '@type': comment_not_valid,
                  comment: Atom
              }.

subject_changed(Validation_Object, Subject) :-
    database_instance(Validation_Object, Instance),
    distinct(Subject,(   xrdf_deleted(Instance, Subject,_,_)
                     ;   xrdf_added(Instance, Subject,_,_))).

subject_inserted(Validation_Object, Subject) :-
    database_instance(Validation_Object, Instance),
    xrdf_added(Instance, Subject,rdf:type,_),
    \+ xrdf_removed(Subject,_,_),
    !.

subject_updated(Validation_Object, Subject) :-
    database_instance(Validation_Object, Instance),
    distinct(Subject,(xrdf_deleted(Instance, Subject,_,_),
                      xrdf_added(Instance, Subject,_,_))).

subject_deleted(Validation_Object, Subject) :-
    database_instance(Validation_Object, Instance),
    xrdf_deleted(Instance, Subject,rdf:type,_).

subject_predicate_changed(Validation_Object, Subject,Predicate) :-
    database_instance(Validation_Object, Instance),
    distinct(Subject-Predicate,(   xrdf_deleted(Instance, Subject,Predicate,_)
                               ;   xrdf_added(Instance, Subject,Predicate,_))).

subject_predicate_updated(Validation_Object, Subject,Predicate) :-
    database_instance(Validation_Object, Instance),
    distinct(Subject-Predicate,(xrdf_deleted(Instance, Subject,Predicate,_),
                                xrdf_added(Instance, Subject,Predicate,_))).

refute_key(Validation_Object, Subject,Predicate,Class,Witness) :-
    key_descriptor(Validation_Object, Class,Desc),
    refute_key_(Desc,Validation_Object,Subject,Predicate,Witness).

refute_key_(lexical(_,Fields),Validation_Object,Subject,Predicate,Witness) :-
    subject_predicate_updated(Validation_Object,Subject,Predicate),
    member(Predicate,Fields),
    Witness = json{ '@type' : lexical_key_changed,
                    subject: Subject }.
refute_key_(value_hash(_),Validation_Object,Subject,Predicate,Witness) :-
    subject_predicate_updated(Validation_Object,Subject,Predicate),
    Witness = json{ '@type' : value_key_changed,
                    subject: Subject }.
refute_key_(hash(_,Fields),Validation_Object,Subject,Predicate,Witness) :-
    subject_predicate_updated(Validation_Object,Subject,Predicate),
    member(Predicate,Fields),
    Witness = json{ '@type' : hash_key_changed,
                    subject: Subject }.

refute_subject_deletion(Validation_Object, Subject,Witness) :-
    subject_deleted(Validation_Object, Subject),
    database_instance(Validation_Object, Instance),
    xrdf(Instance,Subject,Predicate,Object),
    Witness = json{ '@type' : entire_object_not_deleted,
                    subject : Subject,
                    predicate : Predicate,
                    object : Object }.

refute_subject_type_change(Validation_Object,Subject,Witness) :-
    database_instance(Validation_Object, Instance),
    xrdf_added(Instance, Subject,rdf:type,Old_Type),
    xrdf_deleted(Instance, Subject,rdf:type,New_Type),
    Witness = json{ '@type' : subject_type_has_changed,
                    old_type : Old_Type,
                    new_type : New_Type}.

refute_object_type(Validation_Object, Class,Subject,Predicate,Witness) :-
    database_instance(Validation_Object, Instance),
    (   class_predicate_type(Validation_Object, Class,Predicate,Type)
    ->  xrdf_added(Instance, Subject,Predicate,Object),
        refute_object_type_(Type,Validation_Object,Object,Witness)
    ;   Witness = json{ '@type' : invalid_predicate,
                        class: Class,
                        predicate: Predicate,
                        subject: Subject }
    ).

refute_object_type_(base_type(C),_Validation_Object,Object,Witness) :-
    refute_base_type(Object,C,Witness).
refute_object_type_(class(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(set(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(cardinality(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(optional(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(array(C),Validation_Object,Object,Witness) :-
    database_instance(Validation_Object, Instance),
    xrdf_added(Instance,Object,sys:value,O),
    \+ is_instance(Validation_Object,O,C),
    Witness = witness{
                  '@type': array_instance_not_of_class,
                  class: C,
                  instance: O,
                  array: Object
              }.
refute_object_type_(list(C),Validation_Object,Object,Witness) :-
    (   \+ is_rdf_list(Validation_Object, Object)
    ->  Witness = witness{'@type':not_a_valid_list,
                          class:C,
                          list:Object}
    ;   member_list(Elt,Object),
        \+ is_instance(Validation_Object,Elt,C),
        Witness = witness{
                      '@type': list_element_of_wrong_type,
                      class: C,
                      object: Elt,
                      list: Object
                  }
    ).

refute_built_in(Validation_Object,Subject,Predicate,Witness) :-
    database_instance(Validation_Object, Instance),
    xrdf_added(Instance,Subject,Predicate,Value,i),
    refute_built_in_value(Validation_Object,Predicate,Value,Witness).

refute_subject(Validation_Object,Subject,Witness) :-
    instance_of(Validation_Object,Subject,Class),
    subject_predicate_changed(Validation_Object,Subject,Predicate),
    % We also need to check arrays / lists for coherence here?
    (   is_built_in(Predicate)
    ->  refute_built_in(Validation_Object,Subject,Predicate,Witness)
    ;   refute_subject_deletion(Validation_Object,Subject,Witness)
    ;   refute_subject_type_change(Validation_Object,Subject,Witness)
    ;   refute_key(Validation_Object,Subject,Predicate,Class,Witness)
    ;   refute_cardinality(Validation_Object,Subject,Predicate,Class,Witness)
    ;   refute_object_type(Validation_Object,Class,Subject,Predicate,Witness)
    ).

refute_instance(Validation_Object, Witness) :-
    subject_changed(Validation_Object, Subject),
    refute_subject(Validation_Object,Subject,Witness).

/*
insert_document(JSON,JSON_ID) :-
    json_elaborate(JSON,Elaborated),
    json_jsonid(Elaborated,JSON_ID),
    (   json_instance:t(JSON_ID, rdf:type, _)
    ->  throw(error(document_alread_exists(JSON_ID)))
    ;   true),
    forall(
        json_triple_(Elaborated,Triple),
        insert_triple(Triple)
    ),

    forall(
        json_triple_(Elaborated,t(X,Y,Z)),
        (   refute_insert(X,Y,Z,Witness)
        ->  throw(error(insert_failure(Witness)))
        ;   true)
    ),
    commit.

delete_document(JSON) :-
    json_elaborate(JSON,Elaborated),
    json_jsonid(Elaborated,JSON_ID),
    (   json_instance:t(JSON_ID, rdf:type, _)
    ->  throw(error(document_alread_exists(JSON_ID)))
    ;   true),
    forall(
        json_triple_(Elaborated,Triple),
        delete_triple(Triple)
    ),

    forall(
        json_triple_(Elaborated,t(X,Y,Z)),
        (   refute_insert(X,Y,Z,Witness)
        ->  throw(error(insert_failure(Witness)))
        ;   true)
    ),
    commit.
*/

:- begin_tests(json_instance).

schema1(person, rdf:type, 'Class').
schema1(person, name, xsd:string).
schema1(person, birthdate, xsd:date).
schema1(person, friends, set_person).
schema1(set_person, rdf:type, 'Set').
schema1(set_person, sys:class, person).
schema1(employee, rdf:type, 'Class').
schema1(employee, rdfs:subClassOf, person).
schema1(employee, staff_number, xsd:string).
schema1(employee, boss, optional_employee).
schema1(optional_employee, rdf:type, 'Optional').
schema1(optional_employee, sys:class, employee).
schema1(employee, tasks, list_task).
schema1(list_task, rdf:type, 'List').
schema1(list_task, sys:class, task).
schema1(task, rdf:type, 'Class').
schema1(task, name, xsd:string).
schema1(criminal, rdf:type, 'Class').
schema1(criminal, rdfs:subClassOf, person).
schema1(criminal, aliases, list_string).
schema1(list_string, rdf:type, 'List').
schema1(list_string, sys:class, xsd:string).

test(simple_class_with_set,
     [
         setup(
             (   delete_database,
                 create_database,

              % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, person)),
                 insert_triple(t(jim, name, "jim"^^xsd:string)),
                 insert_triple(t(jim, birthdate, "1978-10-09"^^xsd:date)),
                 insert_triple(t(person, friends, jane)),

             % Jane
                 insert_triple(t(jane, rdf:type, person)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),
                 insert_triple(t(jane, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(person, friends, jim)),
                 insert_triple(t(person, friends, jane)),

                 stage

             )),

         cleanup(
             delete_database
         )
     ]) :-

    check_and_commit.

test(simple_class_with_bad_required,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 insert_triple(s(person, rdf:type, 'Class')),
                 insert_triple(s(person, name, xsd:string)),

             % Jim
                 insert_triple(t(jim, rdf:type, person)),
                 insert_triple(t(jim, name, "jim"^^xsd:string)),

             % Jane
                 insert_triple(t(jane, rdf:type, person)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),

                 check_and_commit
             )
         ),

         cleanup(
             delete_database
         ),
         error(subject_refutation_failure(
                   witness{'@type':instance_not_cardinality_one,
                           class:xsd:string,
                           predicate:name}),
               _)
     ]) :-
    %class_predicate_type(name, person, _5712)

    delete_triple(t(jane, name, "jane"^^xsd:string)),

    stage,

    check_and_commit.


test(simple_class_with_set_list_optional,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, employee)),
                 insert_triple(t(jim, staff_number, "12"^^xsd:string)),
                 insert_triple(t(jim, tasks, b:tasks1)),
                 insert_triple(t(b:tasks1, rdf:first, copy_stuff)),
                 insert_triple(t(b:tasks1, rdf:rest, rdf:nil)),

             % Copy stuff
                 insert_triple(t(copy_stuff, rdf:type, task)),
                 insert_triple(t(copy_stuff, name, "copy stuff"^^xsd:string)),

             % Jane
                 insert_triple(t(jane, rdf:type, employee)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),
                 insert_triple(t(jane, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(jane, boss, jim)),
                 insert_triple(t(jane, friends, jim))

             )),

         cleanup(
             delete_database
         )
     ]) :-

    check_and_commit.


test(simple_class_with_bad_set,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, employee)),
                 insert_triple(t(jim, staff_number, "12"^^xsd:string)),
                 insert_triple(t(jim, tasks, b:tasks1)),
                 insert_triple(t(b:tasks1, rdf:first, copy_stuff)),
                 insert_triple(t(b:tasks1, rdf:rest, rdf:nil)),

             % Copy stuff
                 insert_triple(t(copy_stuff, rdf:type, task)),
                 insert_triple(t(copy_stuff, name, "copy stuff"^^xsd:string)),

             % Jane
                 insert_triple(t(jane, rdf:type, employee)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),
                 insert_triple(t(jane, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(jane, boss, jim)),
                 insert_triple(t(jane, friends, copy_stuff))

             )),

         cleanup(
             delete_database
         ),
         error(
             subject_refutation_failure(witness{'@type':instance_not_of_class,
                                                class:person,
                                                instance:copy_stuff}),
             _)
     ]) :-

    check_and_commit.

test(simple_class_with_bad_list,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, employee)),
                 insert_triple(t(jim, staff_number, "12"^^xsd:string)),
                 insert_triple(t(jim, tasks, jane)),

             % Copy stuff
                 insert_triple(t(copy_stuff, rdf:type, task)),
                 insert_triple(t(copy_stuff, name, "copy stuff"^^xsd:string)),

             % Jane
                 insert_triple(t(jane, rdf:type, employee)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),
                 insert_triple(t(jane, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(jane, boss, jim)),

                 stage

             )),

         cleanup(
             delete_database
         ),
         error(subject_refutation_failure(
                   witness{'@type':not_a_valid_list,
                           class:task,
                           list:jane}),_)
     ]) :-

    check_and_commit.

test(simple_class_with_bad_optional,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, employee)),
                 insert_triple(t(jim, staff_number, "12"^^xsd:string)),
                 insert_triple(t(jim, tasks, b:task1)),
                 insert_triple(t(b:task1, rdf:first, copy_stuff)),
                 insert_triple(t(b:task1, rdf:rest, rdf:nil)),

             % Copy stuff
                 insert_triple(t(copy_stuff, rdf:type, task)),
                 insert_triple(t(copy_stuff, name, "copy stuff"^^xsd:string)),

             % Jane
                 insert_triple(t(jane, rdf:type, employee)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),
                 insert_triple(t(jane, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(jane, boss, jim)),
                 insert_triple(t(jane, boss, jane))

             )),

         cleanup(
             delete_database
         ),
         error(subject_refutation_failure(
                   witness{'@type':instance_has_wrong_cardinality,
                           cardinality:2,
                           class:employee,
                           instance:jane,
                           predicate:boss}),
               _)
     ]) :-

    check_and_commit.

test(simple_class_bad_optional,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, employee)),
                 insert_triple(t(jim, staff_number, "12")),
                 insert_triple(t(jim, tasks, rdf:nil)),

             % Copy stuff
                 insert_triple(t(copy_stuff, rdf:type, task)),
                 insert_triple(t(copy_stuff, name, "copy stuff"^^xsd:string)),

             % Jane
                 insert_triple(t(jane, rdf:type, employee)),
                 insert_triple(t(jane, name, "jane"^^xsd:string)),
                 insert_triple(t(jane, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(jane, boss, jim)),
                 insert_triple(t(jane, boss, cleanup))


             )),

         setup(
             delete_database
         ),
         error(subject_refutation_failure(
                   witness{'@type':instance_has_wrong_cardinality,
                           cardinality:2,
                           class:employee,
                           instance:jane,
                           predicate:boss}),_)
     ]) :-

    check_and_commit.

test(base_type_list,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

             % Jim
                 insert_triple(t(jim, rdf:type, criminal)),
                 insert_triple(t(jim, name, "jim"^^xsd:string)),
                 insert_triple(t(jim, birthdate, "1979-11-02"^^xsd:date)),
                 insert_triple(t(jim, aliases, b:aliases1)),
                 insert_triple(t(b:aliases1, rdf:first, "jimbo"^xsd:string)),
                 insert_triple(t(b:aliases1, rdf:rest, b:aliases2)),
                 insert_triple(t(b:aliases2, rdf:first, "jamesy"^xsd:string)),
                 insert_triple(t(b:aliases2, rdf:rest, rdf:nil))

             )),

         cleanup(
             delete_database
         ),
         error(subject_refutation_failure(
                   witness{'@type':list_element_of_wrong_type,
                           class:xsd:string,
                           list:b:aliases1,
                           object:"jimbo"^xsd:string}),
               _)
     ]) :-

    check_and_commit.



:- end_tests(json_instance).
