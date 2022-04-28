:- module('document/inference', [
              infer_type/4
          ]).

:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core('document/schema')).
:- use_module(core('document/json')).
:- use_module(core(util)).

/*

           Top{}
             |
         NamedThing{name}
          /           \
Person{name}         Company{name,Optional[address]}
        |                   /
CompanyPerson{name}        /
          \               /
        Something{name,address}

insert_document({name:"Gavin"})

Error: Could not find a principal type.
*/


% DB, Prefixes |- Dictionary <= Type
check_type(Database,Prefixes,Dictionary,Type,Annotated) :-
    \+ is_abstract(Database, Type),
    class_frame(Database, Type, Frame),
    dict_pairs(Frame,json,Pairs),
    expand_dictionary_pairs(Pairs,Prefixes,Pairs_Expanded),
    check_type_pairs(Pairs_Expanded,Database,Prefixes,Dictionary,Annotated).

expand_dictionary_pairs([],_Prefixes,[]).
expand_dictionary_pairs([Key-Value|Pairs],Prefixes,[Key_Ex-Value|Expanded_Pairs]) :-
    prefix_expand_schema(Key, Prefixes, Key_Ex),
    expand_dictionary_pairs(Pairs,Prefixes,Expanded_Pairs).

expand_dictionary_keys(Dictionary, Prefixes, Expanded) :-
    dict_pairs(Dictionary, json, Pairs),
    expand_dictionary_pairs(Pairs,Prefixes,Expanded_Pairs),
    dict_pairs(Expanded, json, Expanded_Pairs).

check_type_pair(Key,_Range,_Database,_Prefixes,Dictionary,Annotated),
has_at(Key) =>
    Dictionary = Annotated.
check_type_pair(Key,Type,Database,Prefixes,Dictionary,Annotated),
atom(Type) =>
    get_dict(Key,Dictionary,Value),
    check_value_type(Database, Prefixes, Value, Type, Annotated_Value),
    put_dict(Key,Dictionary,Annotated_Value,Annotated).
check_type_pair(Key,Range,Database,Prefixes,Dictionary,Annotated),
_{ '@subdocument' : [], '@class' : Type} :< Range =>
    get_dict(Key,Dictionary,Value),
    check_value_type(Database, Prefixes, Value, Type, Annotated).
check_type_pair(Key,Range,Database,Prefixes,Dictionary,Annotated),
_{ '@type' : "Set", '@class' : Type} :< Range =>
    (   get_dict(Key,Dictionary,Values)
    ->  maplist(
            {Database,Prefixes,Type}/
            [Value,Exp]>>check_value_type(Database,Prefixes,Value,Type,Exp),
            Values,Expanded),
        put_dict(Key,Dictionary,
                 json{ '@container' : "@set",
                       '@type' : Type,
                       '@value' : Expanded },
                 Annotated)
    ;   Dictionary = Annotated
    ).
check_type_pair(Key,Range,Database,Prefixes,Dictionary,Annotated),
_{ '@type' : "Optional", '@class' : Class } :< Range =>
    (   get_dict(Key, Dictionary, Value)
    ->  check_value_type(Database, Prefixes, Value, Class, Annotated)
    ;   Dictionary = Annotated
    ).
check_type_pair(Key,Range,Database,Prefixes,Dictionary,Annotated),
_{ '@type' : Collection, '@class' : Type } :< Range,
member(Collection, ["Array", "List"]) =>
    get_dict(Key,Dictionary,Values),
    maplist(
        {Database,Prefixes,Type}/
        [Value,Exp]>>check_value_type(Database,Prefixes,Value,Type,Exp),
        Values,Expanded),
    (   Collection = "Array"
    ->  Container = "@array"
    ;   Collection = "List"
    ->  Container = "@list"
    ),
    put_dict(Key,Dictionary,
             json{ '@container' : Container,
                   '@type' : Type,
                   '@value' : Expanded },
             Annotated).
/*
check_type_pair(Key,Range,Database,Prefixes,Dictionary,Annotated),
_{ '@id' : _, '@type' : Type } :< Range =>
    get_dict(Key,Dictionary,Value),
    check_value_type(Database, Prefixes, Value, Type).
*/

check_value_type(Database,Prefixes,Value,Type,Annotated),
is_dict(Value) =>
    infer_type(Database, Prefixes, Type, Value, Type, Annotated).
check_value_type(_Database,_Prefixes,Value,_Type,_Annotated),
is_list(Value) =>
    fail.
check_value_type(_Database,Prefixes,Value,Type, Annotated) =>
    prefix_expand(Type, Prefixes,Type_Ex),
    catch(
        json_value_cast_type(Value,Type,_),
        _,
        fail
    ),
    Annotated = json{'@type' : Type_Ex,
                     '@value' : Value }.

check_type_pairs([],_,_,Dictionary,Dictionary).
check_type_pairs([Key-Range|Pairs],Database,Prefixes,Dictionary,Annotated) :-
    check_type_pair(Key,Range,Database,Prefixes,Dictionary,Dictionary0),
    check_type_pairs(Pairs,Database,Prefixes,Dictionary0,Annotated).

candidate_subsumed(Database,'http://terminusdb.com/schema/sys#Top', Candidate) =>
    is_simple_class(Database, Candidate),
    \+ is_subdocument(Database, Candidate),
    \+ is_abstract(Database, Candidate).
candidate_subsumed(Database, Super, Candidate) =>
    class_subsumed(Database, Super, Candidate).

infer_type(Database, Super, Dictionary, Type, Annotated) :-
    database_prefixes(Database, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = (Default_Prefixes.put(DB_Prefixes)),
    infer_type(Database, Prefixes, Super, Dictionary, Type, Annotated).

infer_type(Database, _Prefixes, Super, Dictionary, Type, Dictionary),
get_dict('@type', Dictionary, Type) =>
    class_subsumed(Database, Super, Type).
infer_type(Database, Prefixes, Super, Dictionary, Type, Annotated) =>
    expand_dictionary_keys(Dictionary,Prefixes,Dictionary_Expanded),
    findall(Candidate-Annotated0,
            (   candidate_subsumed(Database, Super, Candidate),
                check_type(Database,Prefixes,Dictionary_Expanded,Candidate,Annotated0)
            ),
            [Type-Annotated0]),
    put_dict(json{'@type' : Type}, Annotated0, Annotated).

infer_type(Database, Dictionary, Type, Annotated) :-
    infer_type(Database, 'http://terminusdb.com/schema/sys#Top', Dictionary, Type, Annotated).

:- begin_tests(infer).
:- use_module(core(util/test_utils)).

multi('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "Class",
  "@id": "Multi",
  "set_decimal": { "@type" : "Set",
                   "@class" : "xsd:decimal" },
  "option_string": { "@type" : "Optional",
                     "@class" : "xsd:string" },
  "mandatory_string": "xsd:string",
  "mandatory_decimal": "xsd:decimal"
}

{ "@type" : "Class",
  "@id" : "MultiSub",
  "@subdocument" : [],
  "@key" : { "@type" : "Random"},
  "set_decimal": { "@type" : "Set",
                   "@class" : "xsd:decimal" },
  "option_string": { "@type" : "Optional",
                     "@class" : "xsd:string" },
  "mandatory_decimal": "xsd:decimal"
}

{ "@type" : "Class",
  "@id" : "HasSubdocument",
  "subdocument" : "MultiSub",
}

{ "@type" : "Class",
  "@id" : "NonUnique",
  "name" : "xsd:string"
}

{ "@type" : "Class",
  "@id" : "NonUniqueA",
  "@inherits" : "NonUnique"
}

{ "@type" : "Class",
  "@id" : "NonUniqueB",
  "@inherits" : "NonUnique"
}
').

test(infer_multi_success,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        'set_decimal' : [1,3,3],
        'mandatory_string' : "asdf",
        'mandatory_decimal' : 30
    },
    open_descriptor(Desc,Database),
    infer_type(Database,Document,Type,Annotated),
    Type = 'terminusdb:///schema#Multi',
    Annotated = json{'@type':'terminusdb:///schema#Multi',
                     'terminusdb:///schema#mandatory_decimal':
                     json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                          '@value':30},
                     'terminusdb:///schema#mandatory_string':
                     json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                          '@value':"asdf"},
                     'terminusdb:///schema#set_decimal':
                     json{'@container':"@set",
                          '@type':'xsd:decimal',
                          '@value':[json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                                         '@value':1},
                                    json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                                         '@value':3},
                                    json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                                         '@value':3}]}}.

test(infer_multisub_success,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        subdocument : json{
                          'set_decimal' : [1,3,3],
                          'mandatory_decimal' : 30
                      }
    },
    open_descriptor(Desc,Database),
    infer_type(Database,Document,Type,Annotated),
    Type = 'terminusdb:///schema#HasSubdocument',
    Annotated = json{'@type':'terminusdb:///schema#HasSubdocument',
                     'terminusdb:///schema#mandatory_decimal':
                     json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                          '@value':30},
                     'terminusdb:///schema#set_decimal':
                     json{'@container':"@set",
                          '@type':'xsd:decimal',
                          '@value':[json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                                         '@value':1},
                                    json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                                         '@value':3},
                                    json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                                         '@value':3}]}}.


test(infer_nonunique_failure,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        name : "Goober"
    },
    open_descriptor(Desc,Database),
    \+ infer_type(Database,Document,_Type,_Annotated).


:- end_tests(infer).
