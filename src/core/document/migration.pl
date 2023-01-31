:- module('document/migration', []).

:- use_module(instance).
:- use_module(schema).

:- use_module(library(assoc)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(crypto)).
:- use_module(library(when)).
:- use_module(library(option)).

% performance
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
:- use_module(library(plunit)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(util/tables)).


:- begin_tests(migration).

:- use_module(core(util/test_utils)).

before1('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}


{ "@type" : "Class",
  "@id" : "A",
  "a" : "xsd:string" }

').

after1('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}


{ "@type" : "Class",
  "@id" : "B",
  "b" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "C",
  "c" : "xsd:string" }

').

class_difference(Before_Frame, After_Frame, Plan) :-
    true.

generate_pre_plan(Before, After, Pre_Plan) :-
    findall(Plan,
            (
                % both
                (   get_dict(Class,Before,Before_Frame),
                    get_dict(Class,After,After_Frame)
                *-> class_difference(Before_Frame, After_Frame, Plan)
                % first
                ;   get_dict(Class,Before,Before_Frame)
                *-> class_deleted(Before_Frame, Plan)
                % second
                ;   get_dict(Class,After,After_frame)
                *-> class_added(After_Frame, Plan))
            ),
            Plans),
    append(Plans,Pre_Plan).

test(add_remove_classes,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Before),
             write_schema(before1,Before),
             test_document_label_descriptor(After),
             write_schema(after1,After)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    all_class_frames(Before, Before_Frames),
    all_class_frames(After, After_Frames),
    generate_pre_plan(Before,After,Pre_Plan),

    print_term(Pre_Plan).

:- end_tests(migration).
