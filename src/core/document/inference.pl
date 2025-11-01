:- module('document/inference', [
              infer_type/4,
              infer_type/5,
              infer_type/6,
              check_type/6
          ]).

:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(assoc)).

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

update_document_links_monadic([], [], _Subdoc, _Prefixes, _Id, captures(In, D-D, S-S, In)).
update_document_links_monadic([Parent|Parents], [ParentOut|ParentsOut], Subdoc, Prefixes, Id, captures(In, DepH-DepT, SubH-SubT, Out)) :-
    do_or_die(get_dict('@property', Parent, Property),
              error(no_property_specified_in_link(Parent),_)),
    prefix_expand_schema(Property, Prefixes, Property_Ex),
    (   get_dict('@id', Parent, Parent_Id0)
    ->  (   is_dict(Parent_Id0)
        ->  do_or_die(get_dict('@ref', Parent_Id0, Ref),
                      error(no_ref_in_link(Parent),_)),
            capture_ref(In, Ref, Parent_Id, Mid),
            (   Subdoc = true
            ->  DepH = [Parent_Id|DepMid]
            ;   DepMid = DepH),
            put_dict(_{'@id': Parent_Id}, Parent, ParentOut)
        ;   do_or_die(
                (   \+ is_list(Parent_Id0),
                    text(Parent_Id0),
                    ground(Parent_Id0)),
                error(link_id_specified_but_not_valid(Parent),_)),
            ParentOut = Parent,
            Out = In,
            DepMid = DepH,
            Parent_Id = Parent_Id0)
    ;   get_dict('@ref', Parent, Ref)
    ->  capture_ref(In, Ref, Parent_Id, Mid),
        (   Subdoc = true
        ->  DepH = [Parent_Id|DepMid]
        ;   DepMid = DepH),
        del_dict('@ref', Parent, _, ParentMid),
        put_dict(_{'@id': Parent_Id}, ParentMid, ParentOut)
    ;   throw(error(no_ref_or_id_in_link(Parent),_))),

    SubH=[link(Parent_Id,Property_Ex,Id)|SubMid],

    update_document_links_monadic(Parents, ParentsOut, Subdoc, Prefixes, Id, captures(Mid, DepMid-DepT, SubMid-SubT, Out)).

update_document_links(Value, ValueOut, Database, Prefixes, Type, Captures),
is_dict(Value),
get_dict('@linked-by', Value, Parent) =>
    update_id_field(Value, Prefixes, ValueMid),
    get_dict('@id', ValueMid, Id),

    (   is_list(Parent)
    ->  Parents = Parent
    ;   Parents = [Parent]),

    (   is_subdocument(Database, Type)
    ->  do_or_die(Parents = [_],
                  error(not_one_parent_of_subdocument(Parents),_)),
        Subdoc = true
    ;   Subdoc = false),

    update_document_links_monadic(Parents, ParentsOut, Subdoc, Prefixes, Id, Captures),

    put_dict(_{'@linked-by': ParentsOut}, ValueMid, ValueOut).
update_document_links(Value, ValueOut, _Database, _Prefixes, _Type, captures(In, DepH-DepT, SubH-SubT, Out)) =>
    ValueOut = Value,
    DepH = DepT,
    SubH = SubT,
    In = Out.

foreign_class(Foreign,Class) :-
    is_dict(Foreign),
    _{ '@type' : 'http://terminusdb.com/schema/sys#Foreign',
       '@class' : Class} :< Foreign.

expand_type(Type, Prefixes, Type_Ex),
text(Type) =>
    prefix_expand_schema(Type, Prefixes, Type_Ex).
expand_type(Type, Prefixes, Type_Ex),
foreign_class(Type,Class) =>
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    Type_Ex = _{ '@type' : 'http://terminusdb.com/schema/sys#Foreign',
                 '@class' : Class_Ex}.

% DB, Prefixes |- Value <= Type
check_type(Database,Prefixes,Value,Type,Annotated,captures(In, DepH-DepT, SubH-SubT, Out)) :-
    \+ is_abstract(Database, Type),
    update_document_links(Value, ValueOut, Database, Prefixes, Type, captures(In, DepH-DepMid, SubH-SubMid, Mid)),
    NextCaptures = captures(Mid, DepMid-DepT, SubMid-SubT, Out),
    class_frame(Database, Type, Frame, [expand_abstract(false),
                                        compress_ids(false)]),
    (   is_dict(Value),
        shape_mismatch(Database,Type,ValueOut,Properties)
    ->  no_captures(NextCaptures),
        missing_property_witness(ValueOut,Properties,Type,Annotated)
    ;   check_frame(Frame,Database,Prefixes,ValueOut,Type,Annotated,NextCaptures)
    ).

missing_property_witness(Dictionary,Properties,Type,witness(Result)) :-
    Result = json{ '@type' : unknown_property_for_type,
                   property : Properties,
                   document : Dictionary,
                   type : Type }.

no_captures(captures(C,T-T,S-S,C)).

check_enum(Enum_Value,Type,Enums,Annotated),
text(Enum_Value) =>
    (   enum_value(Type,Enum_Value,Value),
        memberchk(Value, Enums)
    ->  Annotated = success(json{ '@type' : "@id", '@id' : Value})
    ;   Annotated = witness(json{ '@type' : not_a_valid_enum,
                                  enum : Type,
                                  value : Enum_Value})
    ).
check_enum(Enum_Value,Type,_,Annotated) =>
    Annotated = witness(json{ '@type' : not_a_valid_enum,
                              enum : Type,
                              value : Enum_Value}).

get_dict_not_null(Key,Dict,Value) :-
    get_dict(Key,Dict,Value),
    \+ Value = null.

drop_key(Key,Dict,New) :-
    (   del_dict(Key,Dict, _, New)
    ->  true
    ;   Dict = New
    ).

check_frame(_Frame,_Database,_Prefixes,Value,Type,Annotated,Captures),
Type = 'http://terminusdb.com/schema/sys#Unit' =>
    no_captures(Captures),
    (   Value = []
    ->  Annotated = success([])
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : not_a_sys_unit,
                                  document : Value })
    ).
check_frame(Frame,_Database,_Prefixes,Enum_Value,Type,Annotated,Captures),
is_dict(Frame),
_{'@type' : 'http://terminusdb.com/schema/sys#Enum',
  '@values': Enums
 } :< Frame =>
    no_captures(Captures),
    check_enum(Enum_Value,Type,Enums,Annotated).
check_frame(Frame,Database,Prefixes,Value,_Type,Annotated,Captures),
is_dict(Frame),
get_dict('@type', Frame, 'http://terminusdb.com/schema/sys#Class') =>
    dict_pairs(Frame,json,Pairs),
    (   is_dict(Value)
    ->  expand_dictionary_keys(Value,Prefixes,Expanded)
    ;   Value = Expanded),
    check_type_pairs(Pairs,Database,Prefixes,success(Expanded),Annotated,Captures).

expand_dictionary_pairs([],_Prefixes,[]).
expand_dictionary_pairs([Key-Value|Pairs],Prefixes,[Key_Ex-Value_Ex|Expanded_Pairs]) :-
    (   Key = '@id'
    ->  Key = Key_Ex,
        prefix_expand(Value, Prefixes, Value_Ex)
    ;   prefix_expand_schema(Key, Prefixes, Key_Ex),
        Value = Value_Ex
    ),
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

process_choices([],_Database,_Prefixes,Result,Result,captures(In,T-T,S-S,In)).
process_choices([_|_],_Database,_Prefixes,witness(Witness),witness(Witness),captures(In,T-T,S-S,In)).
process_choices([Choice|Choices],Database,Prefixes,success(Dictionary),Annotated,Captures) :-
    Captures = captures(In,DepH-DepT,SubH-SubT,Out),
    findall(
        Key-Result-captures(In,DepH-DepT0,SubH-SubT0,Out0),
        (   get_dict(Key,Choice,Type),
            get_dict(Key,Dictionary,_Value),
            check_type_pair(Key,Type,Database,Prefixes,success(Dictionary),Result,captures(In,DepH-DepT0,SubH-SubT0,Out0))
        ),
        Results),
    (   Results = [Key-Result-captures(_,DepH-DepT0,SubH-SubT0,C1)]
    ->  (   Result = success(OutDict)
        ->  %put_dict(Key,Dictionary,D,OutDict),
            Capture1 = captures(C1,DepT0-DepT,SubT0-SubT,Out),
            process_choices(Choices,Database,Prefixes,success(OutDict),Annotated,Capture1)
        ;   Result = witness(Witness)
        ->  no_captures(Captures),
            %dict_pairs(Witness, json, [Key-D]),
            Annotated = witness(Witness)
        )
    ;   Results = []
    ->  no_captures(Captures),
        Annotated =
        witness(json{'@type' : no_choice_is_cardinality_one,
                     choice : Choice,
                     document : Dictionary})
    ;   no_captures(Captures),
        Annotated =
        witness(json{'@type' : choice_has_too_many_answers,
                     choice : Choice,
                     document : Dictionary})
    ).

check_type_pairs([],_,_,Dictionary,Dictionary,captures(In,T-T,S-S,In)).
check_type_pairs([Key-Range|Pairs],Database,Prefixes,Dictionary,Annotated,captures(In,DepH-DepT,SubH-SubT,Out)) :-
    check_type_pair(Key,Range,Database,Prefixes,Dictionary,Dictionary0,captures(In,DepH-DepM,SubH-SubM,Middle)),
    check_type_pairs(Pairs,Database,Prefixes,Dictionary0,Annotated,captures(Middle,DepM-DepT,SubM-SubT,Out)).

check_type_pair(_Key,_Range,_Database,_Prefixes,witness(Failure),Annotated,Captures) =>
    no_captures(Captures),
    witness(Failure) = Annotated.
check_type_pair('@oneOf',Range,Database,Prefixes,success(Dictionary),Annotated,Captures) =>
    process_choices(Range,Database,Prefixes,success(Dictionary),Annotated,Captures).
check_type_pair(Key,_Range,_Database,_Prefixes,success(Dictionary),Annotated,Captures),
has_at(Key) =>
    no_captures(Captures),
    success(Dictionary) = Annotated.
check_type_pair(Key,Type,_Database,_Prefixes,success(Dictionary),Annotated,Captures),
Type = 'http://terminusdb.com/schema/sys#Unit' =>
    no_captures(Captures),
    (   get_dict(Key, Dictionary, [])
    ->  Annotated = success(Dictionary)
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : not_a_sys_unit,
                                  field : Key,
                                  document : Dictionary })
    ).
check_type_pair(Key,Type,Database,Prefixes,success(Dictionary),Annotated,Captures),
(   atom(Type)
;   foreign_class(Type, _)) =>
    expand_type(Type, Prefixes, Type_Ex),
    (   get_dict_not_null(Key,Dictionary,Value)
    ->  (   check_value_type(Database, Prefixes, Value, Type_Ex, Annotated_Value,Captures)
        ->  (   Annotated_Value = success(Success_Value)
            ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
                Annotated = success(Annotated_Success)
            ;   Annotated_Value = witness(Witness_Value)
            ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                Annotated = witness(Witness)
            )
        ;   no_captures(Captures),
            Annotated = witness(json{ '@type' : value_invalid_at_type,
                                      document : Dictionary,
                                      value : Value,
                                      type : Type_Ex })
        )
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : required_field_does_not_exist_in_document,
                                  document : Dictionary,
                                  field : Key })
    ).
check_type_pair(Key,Range,_Database,_Prefixes,success(Dictionary),Annotated,Captures),
_{ '@type':'http://terminusdb.com/schema/sys#Enum', '@id' : Enum,
   '@values' : Enums} :< Range =>
    no_captures(Captures),
    (   get_dict_not_null(Key,Dictionary,Value)
    ->  check_enum(Value,Enum,Enums,Annotated_Value),
        (   Annotated_Value = success(Success_Value)
        ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
            Annotated = success(Annotated_Success)
        ;   Annotated_Value = witness(Witness_Value)
        ->  dict_pairs(Witness, json, [Key-Witness_Value]),
            Annotated = witness(Witness)
        )
    ;   Annotated = witness(json{ '@type' : required_field_does_not_exist_in_document,
                                  document : Dictionary,
                                  field : Key })
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{ '@subdocument' : [], '@class' : Type} :< Range =>
    (   get_dict_not_null(Key,Dictionary,Value)
    ->  check_value_type(Database, Prefixes, Value, Type, Annotated_Value, Captures),
        (   Annotated_Value = success(Success_Value)
        ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
            Annotated = success(Annotated_Success)
        ;   Annotated_Value = witness(Witness_Value)
        ->  dict_pairs(Witness, json, [Key-Witness_Value]),
            Annotated = witness(Witness)
        )
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : required_field_does_not_exist_in_document,
                                  document : Dictionary,
                                  field : Key })
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{ '@unfoldable' : [], '@class' : Type} :< Range =>
    (   get_dict_not_null(Key,Dictionary,Value)
    ->  check_value_type(Database, Prefixes, Value, Type, Annotated_Value, Captures),
        (   Annotated_Value = success(Success_Value)
        ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
            Annotated = success(Annotated_Success)
        ;   Annotated_Value = witness(Witness_Value)
        ->  dict_pairs(Witness, json, [Key-Witness_Value]),
            Annotated = witness(Witness)
        )
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : required_field_does_not_exist_in_document,
                                  document : Dictionary,
                                  field : Key })
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{'@type':'http://terminusdb.com/schema/sys#Cardinality',
  '@max':Max,'@min':Min,'@class': Type} :< Range =>
    % Note: witness here should really have more context given.
    (   get_dict_not_null(Key,Dictionary,Values)
    ->  (   is_list(Values),
            length(Values, N)
        ->  (   N >= Min, N =< Max
            ->  Captures = captures(In,DepH-DepT,SubH-SubT,Out),
                mapm(
                    {Database,Prefixes,Type}/
                    [Value,Exp,DepH0-SubH0-In0,DepT0-SubT0-Out0]>>(
                        Capture0 = captures(In0,DepH0-DepT0,SubH0-SubT0,Out0),
                        check_simple_or_compound_type(Database,Prefixes,Value,Type,Exp,Capture0)
                    ),
                    Values,Expanded,DepH-SubH-In,DepT-SubT-Out),
                promote_result_list(Expanded,Result_List),
                (   Result_List = witness(Witness_Value)
                ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                    Annotated = witness(Witness)
                ;   Result_List = success(List),
                    presentation_type(Type, Presentation),
                    put_dict(Key,Dictionary,
                             json{ '@container' : "@set",
                                   '@type' : Presentation,
                                   '@value' : List },
                             Annotated_Dict),
                    success(Annotated_Dict) = Annotated
                )
            ;   no_captures(Captures),
                Annotated = witness(json{ '@type' : field_has_wrong_cardinality,
                                          min: Min,
                                          max: Max,
                                          actual: N,
                                          document : Dictionary,
                                          field : Key })
            )
        ;   check_simple_or_compound_type(Database,Prefixes,Values,Type,Result,Captures),
            (   Result = witness(Witness_Value)
            ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                Annotated = witness(Witness)
            ;   Result = success(Term),
                presentation_type(Type, Presentation),
                put_dict(Key,Dictionary,
                         json{ '@container' : "@set",
                               '@type' : Presentation,
                               '@value' : [Term] },
                         Annotated_Dict),
                success(Annotated_Dict) = Annotated
            )
        )
    ;   Min =< 0
    ->  no_captures(Captures),
        drop_key(Key,Dictionary,New),
        success(New) = Annotated
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : required_field_does_not_exist_in_document,
                                  document : Dictionary,
                                  field : Key })
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{ '@type' : 'http://terminusdb.com/schema/sys#Set', '@class' : Type} :< Range =>
    % Note: witness here should really have more context given.
    (   get_dict_not_null(Key,Dictionary,Values)
    ->  (   is_list(Values)
        ->  Captures = captures(In,DepH-DepT,SubH-SubT,Out),
            mapm(
                {Database,Prefixes,Type}/
                [Value,Exp,DepH0-SubH0-In0,DepT0-SubT0-Out0]>>(
                    Capture0 = captures(In0,DepH0-DepT0,SubH0-SubT0,Out0),
                    check_simple_or_compound_type(Database,Prefixes,Value,Type,Exp,Capture0)
                ),
                Values,Expanded,DepH-SubH-In,DepT-SubT-Out),
            promote_result_list(Expanded,Result_List),
            (   Result_List = witness(Witness_Value)
            ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                Annotated = witness(Witness)
            ;   Result_List = success(List),
                presentation_type(Type, Presentation),
                put_dict(Key,Dictionary,
                         json{ '@container' : "@set",
                               '@type' : Presentation,
                               '@value' : List },
                         Annotated_Dict),
                success(Annotated_Dict) = Annotated
            )
        ;   check_simple_or_compound_type(Database,Prefixes,Values,Type,Result,Captures),
            (   Result = witness(Witness_Value)
            ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                Annotated = witness(Witness)
            ;   Result = success(Term),
                presentation_type(Type, Presentation),
                put_dict(Key,Dictionary,
                         json{ '@container' : "@set",
                               '@type' : Presentation,
                               '@value' : [Term] },
                         Annotated_Dict),
                success(Annotated_Dict) = Annotated
            )
        )
    ;   no_captures(Captures),
        drop_key(Key,Dictionary,New),
        success(New) = Annotated
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{ '@type' : 'http://terminusdb.com/schema/sys#Optional', '@class' : Class } :< Range =>
    (   get_dict_not_null(Key, Dictionary, Value)
    ->  (   foreign_class(Range,Type)
        ->  check_type_pair(Key,Type,Database,Prefixes,success(Dictionary),Annotated,Captures)
        ;   check_simple_or_compound_type(Database, Prefixes, Value, Class, Annotated_Value,Captures),
            (   Annotated_Value = success(Success_Value)
            ->  put_dict(Key,Dictionary,Success_Value,Annotated_Success),
                Annotated = success(Annotated_Success)
            ;   Annotated_Value = witness(Witness_Value)
            ->  dict_pairs(Witness, json, [Key-Witness_Value]),
                Annotated = witness(Witness)
            )
        )
    ;   no_captures(Captures),
        drop_key(Key,Dictionary,New),
        success(New) = Annotated
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{ '@type' : 'http://terminusdb.com/schema/sys#Array', '@class' : Type,
   '@dimensions' : N } :< Range =>
    (   get_dict_not_null(Key,Dictionary,Values)
    ->  check_n_dim_array(Values, N, Database, Prefixes, Type, Result_List, Captures),
        (   Result_List = witness(_)
        ->  Annotated = Result_List
        ;   Result_List = success(List)
        ->  presentation_type(Type, Presentation),
            put_dict(Key,Dictionary,
                     json{ '@container' : "@array",
                           '@dimensions' : N,
                           '@type' : Presentation,
                           '@value' : List },
                     Annotated_Dictionary),
            Annotated = success(Annotated_Dictionary)
        )
    ;   no_captures(Captures),
        presentation_type(Type, Presentation),
        put_dict(Key,Dictionary,
                 json{ '@container' : "@array",
                       '@dimensions' : N,
                       '@type' : Presentation,
                       '@value' : [] },
                 Annotated_Dictionary),
        Annotated = success(Annotated_Dictionary)
    ).
check_type_pair(Key,Range,Database,Prefixes,success(Dictionary),Annotated,Captures),
_{ '@type' : 'http://terminusdb.com/schema/sys#List',
   '@class' : Type} :< Range =>
    (   get_dict_not_null(Key,Dictionary,Values)
    ->  Captures = captures(In,DepH-DepT,SubH-SubT,Out),
        mapm(
            {Database,Prefixes,Type}/
            [Value,Exp,DepH0-SubH0-In0,DepT0-SubT0-Out0]>>(
                Capture0 = captures(In0,DepH0-DepT0,SubH0-SubT0,Out0),
                check_simple_or_compound_type(Database,Prefixes,Value,Type,Exp,Capture0)
            ),
            Values,Expanded,DepH-SubH-In,DepT-SubT-Out),
        promote_result_list(Expanded,Result_List),
        (   Result_List = witness(_)
        ->  Annotated = Result_List
        ;   Result_List = success(List)
        ->  presentation_type(Type, Presentation),
            put_dict(Key,Dictionary,
                     json{ '@container' : "@list",
                           '@type' : Presentation,
                           '@value' : List },
                     Annotated_Dictionary),
            Annotated = success(Annotated_Dictionary)
        )
    ;   no_captures(Captures),
        Annotated = witness(json{ '@type' : required_field_does_not_exist_in_document,
                                  document : Dictionary,
                                  field : Key })
    ).

presentation_type(Type, Presentation),
is_dict(Type),
_{ '@type' : 'http://terminusdb.com/schema/sys#Enum', '@id' : Result} :< Type =>
    Presentation = Result.
presentation_type(Type, Presentation),
is_dict(Type),
_{ '@subdocument' : [], '@class' : Class} :< Type =>
    presentation_type(Class, Presentation).
presentation_type(Type, Presentation),
foreign_class(Type, Class) =>
    presentation_type(Class, Presentation).
presentation_type(Type, Presentation),
atom(Type) =>
    Presentation = Type.

has_n_dim([], _).
has_n_dim([V|_], 1) :-
    \+ is_list(V),
    !.
has_n_dim([V|_], N) :-
    has_n_dim(V, M),
    N is M + 1.

check_n_dim_array(Values, N, Database, Prefixes, Type, Expanded, Captures) :-
    (   has_n_dim(Values,N)
    ->  catch(
            (   check_n_dim_array_(Values, N, Database, Prefixes, Type,
                                   Expanded_Candidate, Captures),
                Expanded = success(Expanded_Candidate)
            ),
            witness(W),
            (   no_captures(Captures),
                Expanded = witness(W)
            )
        )
    ;   no_captures(Captures),
        Expanded = witness(json{ '@type' : invalid_array_dimensions,
                                 array : Values,
                                 dimensions : N })
    ).

is_witness(witness(_)).

check_n_dim_array_([], _, _Database, _Prefixes, _Type, [], captures(In,T-T,S-S,In)).
check_n_dim_array_([null|Values], 1, Database, Prefixes, Type, [null|Expanded],
                   captures(In,H-T,SH-ST,Out)) :-
    !,
    check_n_dim_array_(Values, 1, Database, Prefixes, Type, Expanded, captures(In,H-T,SH-ST,Out)).
check_n_dim_array_([Value|Values], 1, Database, Prefixes, Type, [E|Expanded],
                   captures(In,H-T,SH-ST,Out)) :-
    !,
    check_simple_or_compound_type(Database, Prefixes,Value,Type, Exp, captures(In,H-MT,SH-SMT,Mid)),
    (   is_witness(Exp)
    ->  throw(Exp)
    ;   Exp = success(E)
    ),
    check_n_dim_array_(Values, 1, Database, Prefixes, Type, Expanded, captures(Mid,MT-T,SMT-ST,Out)).
check_n_dim_array_([null|Values], N, Database, Prefixes, Type, [null|Expanded],
                   captures(In,H-T,SH-ST,Out)) :-
    !,
    check_n_dim_array_(Values, N, Database, Prefixes, Type, Expanded,
                       captures(In,H-T,SH-ST,Out)).
check_n_dim_array_([Value|Values], N, Database, Prefixes, Type, [Exp|Expanded],
                   captures(In,H-T,SH-ST,Out)) :-
    M is N - 1,
    check_n_dim_array_(Value, M, Database, Prefixes, Type, Exp,
                       captures(In,H-MT,SH-SMT,Mid)),
    check_n_dim_array_(Values, N, Database, Prefixes, Type, Expanded,
                       captures(Mid,MT-T,SMT-ST,Out)).

check_simple_or_compound_type(Database,Prefixes,Value,Type,Annotated,Captures),
(   atom(Type)
;   foreign_class(Type,_)) =>
    expand_type(Type, Prefixes, Type_Ex),
    check_value_type(Database,Prefixes,Value,Type_Ex,Annotated,Captures).
check_simple_or_compound_type(Database,Prefixes,Value,Type,Annotated,Captures),
_{ '@subdocument' : _, '@class' : Class} :< Type =>
    check_simple_or_compound_type(Database,Prefixes,Value,Class,Annotated,Captures).
check_simple_or_compound_type(Database,Prefixes,Value,Type,Annotated,Captures),
_{ '@id' : Type_Id } :< Type =>
    check_frame(Type,Database,Prefixes,Value,Type_Id,Annotated,Captures).

check_value_type(_Database,_Prefixes,Value,_Type,Annotated,captures(Capture_In,Dep-DepT,Sub-SubT,Capture_Out)),
is_dict(Value),
get_dict('@ref', Value, Ref) =>
    Sub=SubT,
    capture_ref(Capture_In, Ref, Capture_Var, Capture_Out),
    put_dict(_{'@id' : Capture_Var, '@type': "@id"},Value, Result),
    Dep = [Capture_Var|DepT],
    Annotated = success(Result).
check_value_type(_Database,Prefixes,Value,Type,Annotated,Captures),
foreign_class(Type,Class) =>
    no_captures(Captures),
    (   text(Value)
    ->  prefix_expand(Value, Prefixes, Value_Ex),
        Result = json{ '@type' : "@id",
                       '@id' : Value_Ex,
                       '@foreign' : Class},
        Annotated = success(Result)
    ;   Error = json{ '@type' : foreign_type_not_id,
                      value : Value },
        Annotated = witness(Error)
    ).
check_value_type(Database,Prefixes,Value,Type,Annotated,Captures),
is_dict(Value) =>
    infer_type(Database, Prefixes, Type, Value, _, Annotated, Captures).
check_value_type(_Database,_Prefixes,Value,Type,Annotated,Captures),
Type = 'http://terminusdb.com/schema/sys#Unit',
is_list(Value) =>
    no_captures(Captures),
    Annotated = success([]).
check_value_type(_Database,_Prefixes,Value,Type,Annotated,Captures),
Type = 'http://terminusdb.com/schema/sys#JSON',
is_list(Value) =>
    % Arrays are valid JSON values for sys:JSON
    no_captures(Captures),
    Annotated = success(json{ '@type' : Type,
                              '@value' : Value }).
check_value_type(_Database,_Prefixes,Value,Type,_Annotated,_Captures),
is_list(Value) =>
    throw(error(schema_check_failure([witness{ '@type' : unexpected_list,
                                               value: Value, type : Type }]), _)).
check_value_type(_Database,_Prefixes,Value,Type,Annotated,Captures),
Type = 'http://terminusdb.com/schema/sys#JSON',
(Value = _Val^^_ValType ; string(Value) ; number(Value) ; memberchk(Value, [true, false, null])) =>
    % Accept primitive types (typed or untyped) for sys:JSON
    no_captures(Captures),
    Annotated = success(json{'@type' : Type,
                             '@value' : Value }).
check_value_type(_Database,_Prefixes,Value,Type,Annotated,Captures),
is_base_type(Type) =>
    no_captures(Captures),
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
check_value_type(Database,Prefixes,Value,Type,Annotated,Captures),
is_enum(Database,Type) =>
    check_type(Database, Prefixes, Value, Type, Annotated, Captures).
check_value_type(Database,Prefixes,Value,Type,Annotated,Captures) =>
    no_captures(Captures),
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

shape_mismatch(Database,Candidate,Dictionary,Properties) :-
    database_schema(Database,Schema),
    dict_keys(Dictionary,Keys),
    exclude(has_at,Keys,Props),
    exclude({Schema,Candidate}/[Prop]>>
            schema_class_has_property(Schema,Candidate,Prop),
            Props, Properties),
    \+ Properties = [].

candidate_subsumed(Database,'http://terminusdb.com/schema/sys#Top', Candidate, Dictionary) =>
    matches_shape(Database, Candidate, Dictionary),
    is_simple_class(Database, Candidate),
    \+ is_subdocument(Database, Candidate),
    \+ is_abstract(Database, Candidate).
candidate_subsumed(Database, Super, Candidate, Dictionary) =>
    matches_shape(Database, Candidate, Dictionary),
    class_subsumed(Database, Candidate, Super).

infer_type(Database, Prefixes, Dictionary, Type, Annotated) :-
    empty_assoc(In),
    infer_type(Database, Prefixes, Dictionary, Type, Annotated,
               captures(In,_H-_T,_H2-_T2,_Out)).

infer_type(Database, Prefixes, Dictionary, Type, Annotated, Captures) :-
    infer_type(Database, Prefixes, 'http://terminusdb.com/schema/sys#Top', Dictionary,
               Type, Annotated, Captures).

infer_type(Database, Prefixes, Super, Dictionary, Type, Annotated, captures(In,DepH-DepT,SubH-SubT,Out)) :-
    infer_type_or_check(Database, Prefixes, Super, Dictionary, Type, Annotated0,
                        captures(In,DepH-DepT,SubH-SubT,Middle)),
    (   success(Dict) = Annotated0
    ->  update_id_field(Dict,Prefixes,Result),
        update_captures(Result,Middle,Out),
        Annotated = success(Result)
    ;   Annotated = Annotated0,
        In = Out
    ).

infer_type_or_check(_Database, Prefixes, _Super, Dictionary, _Inferred_Type, Annotated, Captures),
get_dict('@type', Dictionary, "@id"),
get_dict('@id', Dictionary, Id),
text(Id) =>
    no_captures(Captures),
    prefix_expand(Id, Prefixes, Id_Ex),
    Annotated = success(json{ '@type' : "@id",
                              '@id' : Id_Ex }).
infer_type_or_check(_Database, _Prefixes, Super, Value, _Inferred_Type, Annotated, Captures),
Super = 'http://terminusdb.com/schema/sys#JSON',
\+ is_dict(Value) =>
    % Handle primitive JSON types: strings, numbers, booleans, null
    no_captures(Captures),
    Annotated = success(json{ '@type' : Super,
                              '@value' : Value }).
infer_type_or_check(_Database, _Prefixes, Super, Dictionary, _Inferred_Type, Annotated, Captures),
memberchk(Super, ['http://terminusdb.com/schema/sys#JSON',
                  'http://terminusdb.com/schema/sys#JSONDocument']) =>
    no_captures(Captures),
    put_dict(json{ '@type' : Super },
             Dictionary,
             Result),
    Annotated = success(Result).
infer_type_or_check(Database, Prefixes, Super, Dictionary, Inferred_Type, Annotated,Captures),
get_dict('@type', Dictionary, Type) =>
    prefix_expand_schema(Type, Prefixes, Type_Ex),
    (   (   is_base_type(Type_Ex),
            basetype_subsumption_of(Type_Ex,Super)
        ;   class_subsumed(Database, Type_Ex, Super)
        ;   Super = 'http://terminusdb.com/schema/sys#Top',
            is_simple_class(Database, Type_Ex)
        )
    ->  put_dict('@type', Dictionary, Type_Ex, New_Dictionary),
        expand_dictionary_keys(New_Dictionary,Prefixes,Dictionary_Expanded),
        check_type(Database,Prefixes,Dictionary_Expanded,Type_Ex,Annotated,Captures),
        Inferred_Type = Type_Ex
    ;   no_captures(Captures),
        (   (   is_simple_class(Database, Type_Ex)
            ;   is_base_type(Type_Ex))
        ->      Annotated = witness(json{ '@type' : ascribed_type_not_subsumed,
                                          'document' : Dictionary,
                                          'ascribed_type' : Type_Ex,
                                          'required_type' : Super})
        ;   Annotated = witness(json{ '@type' : ascribed_type_does_not_exist,
                                      'document' : Dictionary,
                                      'ascribed_type' : Type_Ex})
        )
    ).
infer_type_or_check(Database, Prefixes, Super, Dictionary, Type, Annotated,captures(In,DepH-DepT,SubH-SubT,Out)) =>
    expand_dictionary_keys(Dictionary,Prefixes,Dictionary_Expanded),
    findall(Candidate-Annotated0-captures(In,DepH-DepT0,SubH-SubT0,Out0),
            (   Captures0 = captures(In,DepH-DepT0,SubH-SubT0, Out0),
                candidate_subsumed(Database, Super, Candidate, Dictionary_Expanded),
                check_type(Database,Prefixes,Dictionary_Expanded,Candidate,Annotated0,Captures0)
            ),
            Results),
    exclude([_-witness(_)-_]>>true, Results, Successes),
    (   Successes = [Type-success(Annotated0)-captures(In,DepH-DepT,SubH-SubT,Out)]
    ->  put_dict(json{'@type' : Type}, Annotated0, Annotated1),
        Annotated = success(Annotated1)
    ;   Successes = []
    ->  (   [_-witness(Witness)-_] = Results
        ->  Annotated = witness(json{ '@type' : no_unique_type_for_document,
                                      'reason' : Witness,
                                      'document' : Dictionary})
        ;   Annotated = witness(json{ '@type' : no_unique_type_for_document,
                                      'document' : Dictionary})
        )
    ;   maplist([Type-_-_,Type]>>true,Successes,Types)
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

:- begin_tests(infer,[concurrent(true)]).
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

{ "@type" : "Class",
  "@id" : "HasArray",
  "array" : { "@type" : "Array",
              "@class" : "Person",
              "@dimensions" : 2 }
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
    Annotated = json{'@id' : _,
                     '@type':'terminusdb:///schema#Multi',
                     'terminusdb:///schema#mandatory_decimal':
                     json{'@type':'http://www.w3.org/2001/XMLSchema#decimal',
                          '@value':30},
                     'terminusdb:///schema#mandatory_string':
                     json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                          '@value':"asdf"},
                     'terminusdb:///schema#set_decimal':
                     json{'@container':"@set",
                          '@type' : 'http://www.w3.org/2001/XMLSchema#decimal',
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
    Annotated =
    json{'@id':_27242,
     '@type':'terminusdb:///schema#HasSubdocument',
     'terminusdb:///schema#subdocument':
     json{'@id':_27080,
          '@type':'terminusdb:///schema#MultiSub',
          'terminusdb:///schema#mandatory_decimal':
          json{'@type':'http://www.w3.org/2001/XMLSchema#decimal','@value':30},
          'terminusdb:///schema#set_decimal':
          json{'@container':"@set",'@type':'http://www.w3.org/2001/XMLSchema#decimal',
               '@value':[json{'@type':'http://www.w3.org/2001/XMLSchema#decimal','@value':1},
                         json{'@type':'http://www.w3.org/2001/XMLSchema#decimal','@value':3},
                         json{'@type':'http://www.w3.org/2001/XMLSchema#decimal','@value':3}]}}}.

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
    infer_type(Database,Document,Type,success(Annotated)),
    Type = 'terminusdb:///schema#NonUnique',
    Annotated = json{'@id' : _,
                     '@type':'terminusdb:///schema#NonUnique',
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
    Annotated = json{'@id':_,
                     '@type':'terminusdb:///schema#Mentions',
                     'terminusdb:///schema#mentions':
                     json{'@id':'terminusdb:///data/Mentioned/something_or_other',
                          '@type':"@id"}}.

test(expanded_enum,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),

    Document =
    json{
        rocks : 'terminusdb:///schema#Rocks/big'
    },
    infer_type(Database,Document,Type,success(_)),
    Type = 'terminusdb:///schema#Planet'.

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
    Annotated0 = json{'@id':_,
                      '@type':'terminusdb:///schema#Planet',
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
    Annotated1 = json{'@id':_,
                      '@type':'terminusdb:///schema#Planet',
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
                  choice:json{ 'terminusdb:///schema#gas':
                               json{ '@id':'terminusdb:///schema#Gas',
									 '@type':'http://terminusdb.com/schema/sys#Enum',
									 '@values':[ 'terminusdb:///schema#Gas/light',
												 'terminusdb:///schema#Gas/medium',
												 'terminusdb:///schema#Gas/heavy'
											   ]
								   },
							   'terminusdb:///schema#rocks':
                               json{ '@id':'terminusdb:///schema#Rocks',
									 '@type':'http://terminusdb.com/schema/sys#Enum',
									 '@values':[ 'terminusdb:///schema#Rocks/big',
												 'terminusdb:///schema#Rocks/medium',
												 'terminusdb:///schema#Rocks/small'
											   ]
								   }
							 },
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

    Annotated = json{'@id':_,
                     '@type':'terminusdb:///schema#ThisOrThat',
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
               success(json{'@id':_,
                            '@type':'terminusdb:///schema#UnitTest',
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

    empty_assoc(In),
    database_prefixes(Database,Prefixes),
    infer_type(Database,Prefixes,Document0,Type0,success(Result0),captures(In,[]-[],[]-[],Out)),
    assoc_to_list(Out,["Id_Tom"-Tom]),

    Type0 = 'terminusdb:///schema#Person',
    Result0 = json{ '@id': Tom1,
                    '@capture':"Id_Tom",
				    '@type':'terminusdb:///schema#Person',
				    'terminusdb:///schema#forename':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Tom"
					    },
				    'terminusdb:///schema#surname':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Sawyer"
					    }
				  },
    Tom1 == Tom,
    Document1 =
    json{ '@type': "Person",
          forename: "Jerry",
          surname: "Lewis",
          rival: json{'@ref': "Id_Tom"} },

    infer_type(Database,Prefixes,Document1,Type1,success(Result1),captures(Out,Dep2H-Dep2T,[]-[],Out)),
    Dep2T = [],
    Dep2H = [Tom2],
    Tom2 == Tom,
    Type1 = 'terminusdb:///schema#Person',
    Result1 = json{ '@id':_,
                    '@type':'terminusdb:///schema#Person',
	                'terminusdb:///schema#forename':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Jerry"
						},
	                'terminusdb:///schema#rival':
                    json{ '@ref':"Id_Tom",
                          '@id' : Tom3,
						  '@type':"@id"
					    },
	                'terminusdb:///schema#surname':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						  '@value':"Lewis"
						}
	              },
    Tom3 == Tom.

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
        json{'@id': _,
             '@type':'terminusdb:///schema#EnumSet',
             'terminusdb:///schema#enum':
             json{'@container':"@set",
                  '@type' : 'terminusdb:///schema#Rocks',
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
    !,
    infer_type(System_Database,Document,Type,Result),
    Type = 'http://terminusdb.com/schema/system#Capability',
    Result = success(
                 json{'@id':'terminusdb://system/data/Capability/server_access',
                      '@type':'http://terminusdb.com/schema/system#Capability',
                      'http://terminusdb.com/schema/system#role':
                      json{'@container':"@set",
                           '@type' : 'http://terminusdb.com/schema/system#Role',
                           '@value':[json{'@id':'terminusdb://system/data/Role/admin',
                                          '@type':"@id"}]},
                 'http://terminusdb.com/schema/system#scope':
                 json{'@id':'terminusdb://system/data/Organization/admin',
                      '@type':"@id"}}).


test(array_of_obj,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),
    Document =
    json{
        array : [
            [ json{ forename: "Jerry",
                    surname: "Lewis"},
              json{ forename: "Jerry",
                    surname: "Lewis"}
            ]
        ]
    },
    infer_type(Database,Document,Type,Result),

    Type = 'terminusdb:///schema#HasArray',
    Result = success(
                 json{'@id':_,
                      '@type':'terminusdb:///schema#HasArray',
                      'terminusdb:///schema#array':
                      json{'@container':"@array",'@dimensions':2,
                           '@type':'terminusdb:///schema#Person',
                           '@value':
                           [[json{'@id':_,
                                  '@type':'terminusdb:///schema#Person',
                                  'terminusdb:///schema#forename':
                                  json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                       '@value':"Jerry"},
                                  'terminusdb:///schema#surname':
                                  json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                       '@value':"Lewis"}},
                             json{'@id':_,'@type':'terminusdb:///schema#Person',
                                  'terminusdb:///schema#forename':
                                  json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                       '@value':"Jerry"},
                                  'terminusdb:///schema#surname':
                                  json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                       '@value':"Lewis"}}]]}}).


test(array_of_obj_failure,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),
    Document =
    json{
        array : [
            [ json{ forename: "Jerry",
                    surname: "Lewis"},
              json{ rock: "big"}
            ]
        ]
    },
    infer_type(Database,Document,_,Result),

    Result = witness(
                 json{'@type':no_unique_type_for_document,
                      document:
                      json{array:[[json{forename:"Jerry",surname:"Lewis"},
                                   json{rock:"big"}]]},
                      reason:json{'@type':no_unique_type_for_document,
                                  document:json{rock:"big"}}}).

test(obj_captures,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),
    Document =
    json{ forename: "Jerry",
          surname: "Lewis"},
    database_prefixes(Database,Prefixes),
    empty_assoc(In),
    infer_type(Database,Prefixes,Document,_Type,_Result,captures(In,H-T,[]-[],Out)),
    H == T,
    In == Out.

test(obj_captures_has_capture,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),
    Document =
    json{ '@capture' : "Jer-Bear",
          forename: "Jerry",
          surname: "Lewis"},
    database_prefixes(Database,Prefixes),
    empty_assoc(In),
    infer_type(Database,Prefixes,Document,_Type,success(Result),captures(In,H-T,[]-[],Out)),
    T = [],
    H = [],
    assoc_to_list(Out,["Jer-Bear"-Jer]),
    get_dict('@id', Result, Jer1),
    Jer == Jer1.

test(array_of_obj_captures,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multi,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc,Database),
    Document =
    json{
        array : [
            [ json{ forename: "Jerry",
                    surname: "Lewis"},
              json{ forename: "Jerry",
                    surname: "Lewis"}
            ]
        ]
    },
    database_prefixes(Database,Prefixes),
    empty_assoc(In),
    infer_type(Database,Prefixes,Document,_Type,_Result,captures(In,H-T,[]-[],Out)),
    In == Out,
    H == T.

:- end_tests(infer).
