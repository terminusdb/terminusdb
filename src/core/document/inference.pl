:- module('document/inference', [
              infer_type/4,
              infer_type/5,
              check_type/5
          ]).

:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(apply)).

:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core('document/schema')).
:- use_module(core('document/json')).
:- use_module(core(util)).
:- use_module(core(triple)).

/*

Todo:

[X] Add disjoint union
[X] Add enum
[X] Add unit
[X] Add ID captures

*/


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


% DB, Prefixes |- Value <= Type
check_type(Database,Prefixes,Value,Type,Annotated) :-
    \+ is_abstract(Database, Type),
    class_frame(Database, Type, false, Frame),
    check_frame(Frame,Database,Prefixes,Value,Type,Annotated).

check_frame(Frame,_Database,_Prefixes,Enum_Value,Type,Annotated),
is_dict(Frame),
_{'@type' : 'http://terminusdb.com/schema/sys#Enum',
  '@values': Enums
 } :< Frame =>

    (   string(Enum_Value)
    ->  (   enum_value(Type,Enum_Value,Value),
            get_dict('@values', Frame, Enums),
            memberchk(Value, Enums)
        ->  Annotated = success(json{ '@type' : "@id", '@id' : Value})
        ;   Annotated = witness(json{ '@type' : not_a_valid_enum,
                                      enum : Type,
                                      value : Enum_Value}))
    ;   Annotated = witness(json{ '@type' : not_a_valid_enum,
                                  enum : Type,
                                  value : Enum_Value})
    ).
check_frame(Frame,Database,Prefixes,Value,_Type,Annotated),
is_dict(Frame),
get_dict('@type', Frame, 'http://terminusdb.com/schema/sys#Class') =>
    dict_pairs(Frame,json,Pairs),
    check_type_pairs(Pairs,Database,Prefixes,success(Value),Annotated).

expand_dictionary_pairs([],_Prefixes,[]).
expand_dictionary_pairs([Key-Value|Pairs],Prefixes,[Key_Ex-Value_Ex|Expanded_Pairs]) :-
    (   Key = '@id'
    ->  Key = Key_Ex,
        prefix_expand(Value, Prefixes, Value_Ex)
    ;   prefix_expand_schema(Key, Prefixes, Key_Ex),
        Value = Value_Ex
    ),
    expand_dictionary_pairs(Pairs,Prefixes,Expanded_Pairs).

expand_dictionary_pairs([Key-Value|Pairs],Prefixes,[Key_Ex-Value|Expanded_Pairs]) :-
    prefix_expand_schema(Key, Prefixes, Key_Ex),
    expand_dictionary_pairs(Pairs,Prefixes,Expanded_Pairs).

expand_dictionary_keys(Dictionary, Prefixes, Expanded) :-
    dict_pairs(Dictionary, json, Pairs),
    expand_dictionary_pairs(Pairs,Prefixes,Expanded_Pairs),
    dict_pairs(Expanded, json, Expanded_Pairs).

promote_result_list(List, Promoted) :-
    (   maplist([success(Success),Success]>>true, List, Successes)
    ->  Promoted = success(Successes)
    ;   maplist([Result,Witness]>>
                (   Result = success(_)
                ->  Witness = null
                ;   Result = witness(Witness)
                ), List, Witnesses),
        Promoted = witness(Witnesses)
    ).

process_choices([],_Database,_Prefixes,Result,Result).
process_choices([_|_],_Database,_Prefixes,witness(Witness),witness(Witness)).
process_choices([Choice|Choices],Database,Prefixes,success(Dictionary),Annotated) :-
    findall(
        Key-Result,
        (   get_dict(Key,Choice,Type),
            get_dict(Key,Dictionary,Value),
            check_type(Database,Prefixes,Value,Type,Result)
        ),
        Results),
    (   Results = [Key-Result]
    ->  (   Result = success(D)
        ->  put_dict(Key,Dictionary,D,OutDict),
            process_choices(Choices,Database,Prefixes,success(OutDict),Annotated)
        ;   Result = witness(D)
        ->  dict_pairs(Witness, json, [Key-D]),
            Annotated = witness(Witness)
        )
    ;   Results = []
    ->  Annotated =
        witness(json{'@type' : no_choice_is_cardinality_one,
                     choice : Choice,
                     document : Dictionary})
    ;   Annotated =
        witness(json{'@type' : choice_has_too_many_answers,
                     choice : Choice,
                     document : Dictionary})
    ).

check_type_pair(_Key,_Range,_Database,_Prefixes,witness(Failure),Annotated) =>
    witness(Failure) = Annotated.
check_type_pair('@oneOf',Range,Database,Prefixes,success(Dictionary),Annotated) =>
    process_choices(Range,Database,Prefixes,success(Dictionary),Annotated).
check_type_pair(Key,_Range,_Database,_Prefixes,success(Dictionary),Annotated),
has_at(Key) =>
    success(Dictionary) = Annotated.
check_type_pair(_Key,Type,Database,_Prefixes,success(_Dictionary),_Annotated),
atom(Type),
is_enum(Database,Type) =>
    throw(error(checking_of_enum_unimplemented)).
check_type_pair(Key,Type,_Database,_Prefixes,success(Dictionary),Annotated),
Type = 'http://terminusdb.com/schema/sys#Unit' =>
    (   get_dict(Key, Dictionary, [])
    ->  Annotated = success(Dictionary)
    ;   Annotated = witness(json{ '@type' : not_a_sys_unit,
                                  key : Key,
                                  document : Dictionary })
    ).
check_type_pair(Key,Type,Database,Prefixes,success(Dictionary),Annotated),
atom(Type) =>
    prefix_expand_schema(Type, Prefixes, Type_Ex),
    (   get_dict(Key,Dictionary,Value)
    ->  (   check_value_type(Database, Prefixes, Value, Type_Ex, Annotated_Value)
        ->  (   Annotated_Value = success(Success_Value)
            ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
                Annotated = success(Annotated_Success)
            ;   Annotated_Value = witness(Witness_Value)
            ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                Annotated = witness(Witness)
            )
        ;   Annotated = witness(json{ '@type' : value_invalid_at_type,
                                      document : Dictionary,
                                      value : Value,
                                      type : Type_Ex })
        )
    ;   Annotated = witness(json{ '@type' : mandatory_key_does_not_exist_in_document,
                                  document : Dictionary,
                                  key : Key })
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated),
_{ '@subdocument' : [], '@class' : Type} :< Range =>
    (   get_dict(Key,Dictionary,Value)
    ->  check_value_type(Database, Prefixes, Value, Type, Annotated)
    ;   Annotated = witness(json{ '@type' : missing_property,
                                  '@property' : Key})
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated),
_{ '@type' : 'http://terminusdb.com/schema/sys#Set', '@class' : Type} :< Range =>
    (   get_dict(Key,Dictionary,Values)
    ->  maplist(
            {Database,Prefixes,Type}/
            [Value,Exp]>>check_simple_or_compound_type(Database,Prefixes,Value,Type,Exp),
            Values,Expanded),
        promote_result_list(Expanded,Result_List),
        (   Result_List = witness(_)
        ->  Annotated = Result_List
        ;   Result_List = success(List),
            put_dict(Key,Dictionary,
                     json{ '@container' : "@set",
                           '@value' : List },
                     Annotated_Dict),
            success(Annotated_Dict) = Annotated)
    ;   success(Dictionary) = Annotated
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated),
_{ '@type' : 'http://terminusdb.com/schema/sys#Optional', '@class' : Class } :< Range =>
    (   get_dict(Key, Dictionary, Value)
    ->  check_simple_or_compound_type(Database, Prefixes, Value, Class, Annotated_Value),
        (   Annotated_Value = success(Success_Value)
        ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
            Annotated = success(Annotated_Success)
        ;   Annotated_Value = witness(Witness_Value)
        ->  dict_pairs(Witness, json, [Key-Witness_Value]),
            Annotated = witness(Witness)
        )
    ;   success(Dictionary) = Annotated
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated),
_{ '@type' : Collection, '@class' : Type } :< Range,
member(Collection, ['http://terminusdb.com/schema/sys#Array',
                    'http://terminusdb.com/schema/sys#List']) =>
    get_dict(Key,Dictionary,Values),
    maplist(
        {Database,Prefixes,Type}/
        [Value,Exp]>>check_simple_or_compound_type(Database,Prefixes,Value,Type,Exp),
        Values,Expanded),
    promote_result_list(Expanded,Result_List),
    (   Result_List = witness(_)
    ->  Annotated = Result_List
    ;   Result_List = success(List)
    ->  (   Collection = 'http://terminusdb.com/schema/sys#Array'
        ->  Container = "@array"
        ;   Collection = 'http://terminusdb.com/schema/sys#List'
        ->  Container = "@list"
        ),
        put_dict(Key,Dictionary,
                 json{ '@container' : Container,
                       '@type' : Type,
                       '@value' : List },
                 Annotated_Dictionary),
        Annotated = success(Annotated_Dictionary)
    ).

check_simple_or_compound_type(Database,Prefixes,Value,Type,Annotated),
atom(Type) =>
    prefix_expand_schema(Type, Prefixes, Type_Ex),
    check_value_type(Database,Prefixes,Value,Type_Ex,Annotated).
check_simple_or_compound_type(Database,Prefixes,Value,Type,Annotated),
is_dict(Type) =>
    get_dict('@id',Type,Type_Id),
    check_frame(Type,Database,Prefixes,Value,Type_Id,Annotated).

check_value_type(_Database,_Prefixes,Value,_Type,Annotated),
is_dict(Value),
get_dict('@ref', Value, _) =>
    put_dict('@type', Value, "@id", Result),
    Annotated = success(Result).
check_value_type(Database,Prefixes,Value,Type,Annotated),
is_dict(Value) =>
    infer_type(Database, Prefixes, Type, Value, Type, Annotated).
check_value_type(_Database,_Prefixes,Value,_Type,_Annotated),
is_list(Value) =>
    fail.
check_value_type(_Database,_Prefixes,Value,Type,Annotated),
is_base_type(Type) =>
    catch(
        (   json_value_cast_type(Value,Type,_),
            Annotated = success(json{'@type' : Type,
                                     '@value' : Value })
        ),
        error(casting_error(Val, Type), _),
        Annotated = witness(json{'@type':could_not_interpret_as_type,
                                 'value': Val,
                                 'type' : Type })
    ).
check_value_type(Database,Prefixes,Value,Type,Annotated) =>
    (   is_simple_class(Database,Type)
    ->  (   (   string(Value)
            ;   atom(Value))
        ->  prefix_expand(Value,Prefixes,Exp),
            Annotated = success(json{ '@type' : "@id",
                                      '@id' : Exp })
        ;   is_dict(Value),
            get_dict('@type', Value, "@id")
        ->  Annotated = success(Value)
        ;   Annotated = witness(json{ '@type' : not_an_object_identifier,
                                      value : Value,
                                      type : Type })
        )
    ;   Annotated = witness(json{ '@type' : invalid_class,
                                  value : Value,
                                  type : Type })
    ).

check_type_pairs([],_,_,Dictionary,Dictionary).
check_type_pairs([Key-Range|Pairs],Database,Prefixes,Dictionary,Annotated) :-
    check_type_pair(Key,Range,Database,Prefixes,Dictionary,Dictionary0),
    check_type_pairs(Pairs,Database,Prefixes,Dictionary0,Annotated).

:- table schema_class_has_property/3 as private.
schema_class_has_property(Schema, Class,Property) :-
    xrdf(Schema,Class,Property,_).
schema_class_has_property(Schema, Class, Property) :-
    xrdf(Schema,Class,sys:inherits,Parent),
    schema_class_has_property(Schema, Parent, Property).
schema_class_has_property(Schema, Class, Property) :-
    xrdf(Schema,Class,sys:oneOf,OneOf),
    xrdf(Schema,OneOf,Property,_).

schema_matches_shape(Schema,Candidate,Dictionary) :-
    dict_keys(Dictionary,Keys),
    exclude(has_at,Keys,Properties),
    maplist({Schema,Candidate}/[Prop]>>schema_class_has_property(Schema,Candidate,Prop),
            Properties).

matches_shape(Database,Candidate,Dictionary) :-
    database_schema(Database,Schema),
    schema_matches_shape(Schema,Candidate,Dictionary).

candidate_subsumed(Database,'http://terminusdb.com/schema/sys#Top', Candidate, Dictionary) =>
    matches_shape(Database, Candidate, Dictionary),
    is_simple_class(Database, Candidate),
    \+ is_subdocument(Database, Candidate),
    \+ is_abstract(Database, Candidate).
candidate_subsumed(Database, Super, Candidate, Dictionary) =>
    matches_shape(Database, Candidate, Dictionary),
    class_subsumed(Database, Super, Candidate).

infer_type(Database, Prefixes, Dictionary, Type, Annotated) :-
    infer_type(Database, Prefixes,
               'http://terminusdb.com/schema/sys#Top', Dictionary, Type, Annotated).

infer_type(Database, Prefixes, Super, Dictionary, Inferred_Type, Annotated),
get_dict('@type', Dictionary, Type) =>
    prefix_expand_schema(Type, Prefixes, Type_Ex),
    (   (   class_subsumed(Database, Super, Type_Ex)
        ;   Super = 'http://terminusdb.com/schema/sys#Top'
        )
    ->  put_dict('@type', Dictionary, Type_Ex, New_Dictionary),
        expand_dictionary_keys(New_Dictionary,Prefixes,Dictionary_Expanded),
        check_type(Database,Prefixes,Dictionary_Expanded,Type_Ex,Annotated),
        Inferred_Type = Type_Ex
    ;   Annotated = witness(json{ '@type' : ascribed_type_not_subsumed,
                                  'document' : Dictionary,
                                  'ascribed_type' : Type_Ex,
                                  'required_type' : Super})
    ).
infer_type(Database, Prefixes, Super, Dictionary, Type, Annotated) =>
    expand_dictionary_keys(Dictionary,Prefixes,Dictionary_Expanded),
    findall(Candidate-Annotated0,
            (   candidate_subsumed(Database, Super, Candidate, Dictionary_Expanded),
                check_type(Database,Prefixes,Dictionary_Expanded,Candidate,Annotated0)
            ),
            Results),
    exclude([_-witness(_)]>>true, Results, Successes),
    (   Successes = [Type-success(Annotated0)]
    ->  put_dict(json{'@type' : Type}, Annotated0, Annotated1),
        Annotated = success(Annotated1)
    ;   Successes = []
    ->  (   [_-witness(Witness)] = Results
        ->  Annotated = witness(json{ '@type' : no_unique_type_for_document,
                                      'reason' : Witness,
                                      'document' : Dictionary})
        ;   Annotated = witness(json{ '@type' : no_unique_type_for_document,
                                      'document' : Dictionary})
        )
    ;   maplist([Type-_,Type]>>true,Successes,Types)
    ->  Annotated = witness(json{ '@type' : no_unique_type_for_document,
                                  'document' : Dictionary,
                                  'candidates' : Types})
    ;   Annotated = witness(json{ '@type' : no_unique_type_for_document,
                                  'document' : Dictionary})
    ).

infer_type(Database, Dictionary, Type, Annotated) :-
    database_prefixes(Database, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = (Default_Prefixes.put(DB_Prefixes)),
    infer_type(Database, Prefixes, Dictionary, Type, Annotated).

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

{ "@type" : "Class",
  "@id" : "Mentions",
  "mentions" : "Mentioned"
}

{ "@type" : "Class",
  "@id" : "Mentioned",
  "thing" : "xsd:string"
}

{ "@type" : "Enum",
  "@id" : "Rocks",
  "@value" : [ "big", "medium", "small" ]
}

{ "@type" : "Enum",
  "@id" : "Gas",
  "@value" : [ "light", "medium", "heavy" ]
}

{ "@type" : "Class",
  "@id" : "Planet",
  "@oneOf" : { "rocks" : "Rocks",
               "gas" : "Gas" }
}

{ "@type" : "TaggedUnion",
  "@id" : "ThisOrThat",
  "this" : "Rocks",
  "that" : "Gas"
}

{ "@type" : "Class",
  "@id" : "UnitTest",
  "unit" : "sys:Unit"
}

{ "@type" : "Class",
  "@id" : "Person",
  "forename" : "xsd:string",
  "surname" : "xsd:string",
  "rival" : { "@type" : "Optional", "@class" : "Person" }
}

{ "@type" : "Class",
  "@id" : "EnumSet",
  "enum" : { "@type" : "Set", "@class" : "Rocks"}
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
    infer_type(Database,Document,Type,success(Annotated)),

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
    infer_type(Database,Document,Type,success(Annotated)),
    Type = 'terminusdb:///schema#HasSubdocument',
    Annotated = json{'@type':'terminusdb:///schema#HasSubdocument',
                     'terminusdb:///schema#mandatory_decimal':
                     json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                          '@value':30},
                     'terminusdb:///schema#set_decimal':
                     json{'@container':"@set",
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
    infer_type(Database,Document,_Type,witness(Witness)),
    Witness = json{'@type':no_unique_type_for_document,
                   candidates:Candidates,
                   document:json{name:"Goober"}},
    sort(Candidates,['terminusdb:///schema#NonUnique',
                     'terminusdb:///schema#NonUniqueA',
                     'terminusdb:///schema#NonUniqueB']).

test(annotated_success,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@type' : 'NonUnique',
        name : "Goober"
    },
    open_descriptor(Desc,Database),
    infer_type(Database,Document,_Type,success(Annotated)),
    Annotated = json{'@type':'NonUnique',
                     'terminusdb:///schema#name'
                     :json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                           '@value':"Goober"}}.


test(reference_success,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        mentions : "Mentioned/something_or_other"
    },
    open_descriptor(Desc,Database),
    infer_type(Database,Document,Type,success(Annotated)),
    Type = 'terminusdb:///schema#Mentions',
    Annotated = json{'@type':'terminusdb:///schema#Mentions',
                     'terminusdb:///schema#mentions':
                     json{'@id':'terminusdb:///data/Mentioned/something_or_other',
                          '@type':"@id"}}.

test(planet_choice,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),

    Document =
    json{
        rocks : "big"
    },
    infer_type(Database,Document,Type,success(Annotated0)),
    Type = 'terminusdb:///schema#Planet',
    Annotated0 = json{'@type':'terminusdb:///schema#Planet',
                      'terminusdb:///schema#rocks':
                      json{ '@type' : "@id",
                            '@id' : 'terminusdb:///schema#Rocks/big'
                          }},
    Document1 =
    json{
        gas : "light"
    },
    infer_type(Database,Document1,Type1,success(Annotated1)),
    Type1 = 'terminusdb:///schema#Planet',
    Annotated1 = json{'@type':'terminusdb:///schema#Planet',
                      'terminusdb:///schema#gas':
                      json{'@type' : "@id",
                           '@id' : 'terminusdb:///schema#Gas/light'}},

    Document2 =
    json{
        rocks : "big",
        gas : "light"
    },
    infer_type(Database,Document2,_,Result2),
    Result2 =
    witness(
        json{'@type':no_unique_type_for_document,
             document:json{gas:"light",rocks:"big"},
             reason:
             json{'@type':choice_has_too_many_answers,
                  choice:
                  tagged_union{
                      'terminusdb:///schema#gas':'terminusdb:///schema#Gas',
                      'terminusdb:///schema#rocks':'terminusdb:///schema#Rocks'},
                  document:json{'terminusdb:///schema#gas':"light",
                                'terminusdb:///schema#rocks':"big"}}}),

    Document3 =
    json{
        rocks : "not a rock"
    },

    infer_type(Database,Document3,_,Result3),
    Result3 =
    witness(json{'@type':no_unique_type_for_document,
                 document:json{rocks:"not a rock"},
                 reason:json{'terminusdb:///schema#rocks':
                             json{'@type':not_a_valid_enum,
                                  enum:'terminusdb:///schema#Rocks',
                                  value:"not a rock"}}}),

    Document4 =
    json{
        '@type' : "Planet",
        rocks : "not a rock"
    },

    infer_type(Database,Document4,_,Result4),
    Result4 =
    witness(json{'terminusdb:///schema#rocks':
                 json{'@type':not_a_valid_enum,
                      enum:'terminusdb:///schema#Rocks',
                      value:"not a rock"}}).

test(this_or_that,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),

    Document =
    json{
        this : "big"
    },
    infer_type(Database,Document,Type,success(Annotated)),
    Type = 'terminusdb:///schema#ThisOrThat',

    Annotated = json{'@type':'terminusdb:///schema#ThisOrThat',
                     'terminusdb:///schema#this':
                     json{ '@id' : 'terminusdb:///schema#Rocks/big',
                           '@type' : "@id"}}.

test(unit,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),

    Document =
    json{
        unit : []
    },
    infer_type(Database,Document,Type,
               success(json{'@type':'terminusdb:///schema#UnitTest',
                            'terminusdb:///schema#unit':[]})),
    Type = 'terminusdb:///schema#UnitTest'.


test(capture_ref,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),

    Document0 =
    json{
        '@type':"Person",
        '@capture' : "Id_Tom",
        forename : "Tom",
        surname : "Sawyer"
    },
    infer_type(Database,Document0,Type0,success(Result0)),
    Type0 = 'terminusdb:///schema#Person',
    Result0 = json{ '@capture':"Id_Tom",
				    '@type':"Person",
				    'terminusdb:///schema#forename':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Tom"
					    },
				    'terminusdb:///schema#surname':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Sawyer"
					    }
				  },
    Document1 =
    json{ '@type': "Person",
          forename: "Jerry",
          surname: "Lewis",
          rival: json{'@ref': "Id_Tom"} },

    infer_type(Database,Document1,Type1,success(Result1)),
    Type1 = 'terminusdb:///schema#Person',
    Result1 = json{ '@type':"Person",
	                'terminusdb:///schema#forename':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Jerry"
						},
	                'terminusdb:///schema#rival':
                    json{ '@ref':"Id_Tom",
						  '@type':"@id"
					    },
	                'terminusdb:///schema#surname':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Lewis"
						}
	              }.

test(infer_enum_set,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),

    Document1 = json{
        enum : ["big", "medium", "small"]
    },

    infer_type(Database,Document1,Type1,Result),
    Result =
    success(
        json{'@type':'terminusdb:///schema#EnumSet',
             'terminusdb:///schema#enum':
             json{'@container':"@set",
                  '@value':[json{'@id':'terminusdb:///schema#Rocks/big',
                                 '@type':"@id"},
                            json{'@id':'terminusdb:///schema#Rocks/medium',
                                 '@type':"@id"},
                            json{'@id':'terminusdb:///schema#Rocks/small',
                                 '@type':"@id"}]}}),
    Type1 = 'terminusdb:///schema#EnumSet'.

test(system_capability,
     [setup((setup_temp_store(State)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    open_descriptor(system_descriptor{},System_Database),

    Document = json{'@id':"Capability/server_access",
                    '@type':"Capability",
                    role:"Role/admin",
                    scope:"Organization/admin"},

    infer_type(System_Database,Document,Type,Result),
    writeq(Result).

:- end_tests(infer).
