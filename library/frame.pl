:- module(frame, [
              %% Give a class frame for a given class.
              class_frame/3,
              % Various class/entity queries 
              all_entities/2,
              all_classes/2,
              class_properties/3,
              class_property_frame/4,
              %% Fill a given class frame with data.
              fill_class_frame/4,
              entity_filled_frame/3,
              all_entity_instances/2,
              all_entity_iris/2,
              % Get an object as described by a frame.
              % (should this be exported?)
              % entity_object/4,
              % As JSON-LD
              entity_jsonld/3,
              % As JSON-LD with a context 
              entity_jsonld/4,
              % As JSON-LD with a context and depth parameter
              entity_jsonld/5,
              class_frame_jsonld/3,
              object_edges/3,
              delete_object/2,
              update_object/2,
              update_object/3
          ]).

/** <module> Frames
 * 
 * Frame code for generating either objects based on ontology information 
 * or descriptors of the object type - or the combination of the two 
 * coupled together.
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(library(utils)).
:- use_module(library(base_type)).
:- use_module(library(validate_schema), except([entity/2])).
:- use_module(library(validate_instance)).
:- use_module(library(inference)).
:- use_module(library(database)).
:- use_module(library(triplestore)).
:- use_module(library(schema), []).
:- use_module(library(types)).
:- use_module(library(frame_types)).
:- use_module(library(json_ld)).

class_record(Graph,Class,[class=Class|L]) :-
    maybe_meta(Class,Graph,L).
            
property_record(Graph,Property,L) :-
    maybe_meta(Property,Graph,L).

all_entities(Graph,AER) :-
    unique_solutions(E,schema:entity(E,Graph),AE),
    maplist(class_record(Graph),AE,AER).

all_classes(Graph,ACR) :-
    unique_solutions(C,class(C,Graph),AC),
    maplist(class_record(Graph),AC,ACR).

/** 
 * get_label(Entity,Graph,Label) is semidet.
 */
get_label(Entity,Graph,Label) :-
    database_instance(Graph,Collection),
    database_instance(Graph,Instance),
    global_prefix_expand(rdfs:label,LabelProp),
    xrdf(Collection,Instance,Entity,LabelProp,literal(lang(_,Label))),
    !.
get_label(Entity,Graph,Label) :-
    database_instance(Graph,Collection),
    database_instance(Graph,Instance),
    global_prefix_expand(rdfs:label,LabelProp),
    xrdf(Collection,Instance,Entity,LabelProp,literal(type(_,Label))),
    !.

get_some_label(E,Graph,L) :-
    (   get_label(E,Graph,L)
    ->  true
    ;   L=E).

/** 
 * all_entity_instances(Graph,Ae) is semidet.
 * 
 * Returns a list of URI=[type=Class,label=Label] elements, where 
 * Class is an entity class and Label is a string.
 */
all_entity_instances(Graph,AE) :-
    database_name(Graph,Collection),    
    database_instance(Graph,Instance),
    unique_solutions(E=[type=C,label=L],
                     (   xrdf(Collection,Instance,
                              E,rdf:type,C),
                         schema_util:entity(C,Graph),
                         get_some_label(E,Graph,L)),
                     AE).

all_entity_iris(Graph, IRIs) :-
    database_name(Graph,Collection),            
    database_instance(Graph, Instance),
    findall(IRI,
            (   xrdf(Collection,Instance,
                     IRI,rdf:type,C),
                schema_util:entity(C, Graph)
            ),
            IRIs).

add_most_specific_property(Graph,P,PList,Rp) :-
    % write('P: '),writeq(P),nl,
    member(Q,PList), % write('Q: '),writeq(Q),nl,
    (   subsumptionPropertiesOf(P,Q,Graph)
    *-> select(Q,PList,PListp), Rp=[P|PListp] % ,write('P < Q'),nl
    ;   subsumptionPropertiesOf(Q,P,Graph)
    *-> Rp=PList % ,write('Q < P'), nl
    ;   Rp=[P|PList]),
    % Hail mary - makes me so uncomfortable.
    !.
add_most_specific_property(_Graph,P,PList,[P|PList]).

most_specific_properties(Graph,Properties,SpecificProperties) :-
    foldl(add_most_specific_property(Graph),Properties,[],SpecificProperties). 

%:- rdf_meta class_properties(r,o,r).
class_properties(Class, Graph, PropertiesPrime) :-
    (   schema:entity(Class,Graph)
    ->  EntityProperties=['http://www.w3.org/2000/01/rdf-schema#label', 
                          'http://www.w3.org/2000/01/rdf-schema#comment']
    ;   EntityProperties=[]),
    (   setof(Super,schema:subsumptionOf(Class,Super,Graph),Classes),
        setof(P,
              S^(member(S,Classes),
                 validate_schema:domain(P,S,Graph)),
              Properties)        
    ->  most_specific_properties(Graph,Properties,MSProperties),
        append(MSProperties,EntityProperties,PropertiesWithAbstract),
        database_name(Graph,Collection),                
        database_schema(Graph,Schema),
        exclude({Schema,Collection}/[X]>>(
                    xrdf(Collection,
                         Schema,
                         X,dcog:tag,dcog:abstract)),
                PropertiesWithAbstract,
                PropertiesPrime)
    ;   PropertiesPrime=EntityProperties).

%:- rdf_meta has_formula(r,o).
has_formula(Class,Graph) :-
    (   subClassOf(Class,_,Graph)
    ;   intersectionOf(Class,_,Graph)
    ;   unionOf(Class,_,Graph)
    ;   disjointUnionOf(Class,_,Graph)
    ;   oneOfList(Class,_,Graph)
    ).

% restriction_type(+Class:uri, -Restriction:restriction_formula, +Graph:graph) is nondet.
%
% Get the restriction formula for a restriction class.
%
%:- rdf_meta restriction_type(r,t,?).
restriction_type(CR,restriction([uri=CR,property=OP,someValuesFrom=C]),Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:someValuesFrom,C).
restriction_type(CR,restriction([uri=CR,property=OP,allValuesFrom=C]),Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:allValuesFrom,C).
restriction_type(CR,restriction([uri=CR,property=OP,minCardinality=N]),Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:minCardinality,literal(type(_Type, CardStr))),
    (number(CardStr)-> CardStr=N ; atom_number(CardStr,N)).
restriction_type(CR,restriction([uri=CR,property=OP,maxCardinality=N]),Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:maxCardinality,literal(type(_Type, CardStr))),
    (number(CardStr)-> CardStr=N ; atom_number(CardStr,N)).
restriction_type(CR,restriction([uri=CR,property=OP,cardinality=N]),Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:cardinality,literal(type(_Type, CardStr))),
    (number(CardStr)-> CardStr=N ; atom_number(CardStr,N)).
restriction_type(CR,restriction([uri=CR,property=OP,minQualifiedCardinality=N,onClass=C]), Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:minQualifiedCardinality,literal(type(_Type, CardStr))),
    xrdf(Collection,Schema,CR,owl:onClass,C),
    (number(CardStr)-> CardStr=N ; atom_number(CardStr,N)).
restriction_type(CR,restriction([uri=CR,property=OP,maxQualifiedCardinality=N,onClass=C]), Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:maxQualifiedCardinality,literal(type(_Type,CardStr))),
    xrdf(Collection,Schema,CR,owl:onClass,C),
    (number(CardStr)-> CardStr=N ; atom_number(CardStr,N)).
restriction_type(CR,restriction([uri=CR,property=OP,qualifiedCardinality=N,onClass=C]), Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:qualifiedCardinality,literal(type(_Type,CardStr))),
    xrdf(Collection,Schema,CR,owl:onClass,C),
    (number(CardStr)-> CardStr=N ; atom_number(CardStr,N)).
restriction_type(CR,restriction([uri=CR,property=OP,hasValue=V]), Graph) :-
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,CR,owl:onProperty,OP),
    xrdf(Collection,Schema,CR,owl:hasValue,V).

% ignore modeline
% is_class_formula(+Formula:any) is semidet. 
%  
% True if Formula is a class formula 
is_class_formula(Class<Superclasses) :-
    is_uri(Class),
    exclude(is_class_formula,Superclasses,[]).
is_class_formula(Class=and(Sols)) :-
    is_uri(Class),
    exclude(is_class_formula,Sols,[]).
is_class_formula(Class=or(Sols)) :-
    is_uri(Class),
    exclude(is_class_formula,Sols,[]).
is_class_formula(Class=xor(Sols)) :-
    is_uri(Class),
    exclude(is_class_formula,Sols,[]).
is_class_formula(Class=oneOf(OneList)) :-
    is_uri(Class),
    exclude(is_uri,OneList,[]).
is_class_formula(restriction(Restriction)) :-
    is_property_restriction(Restriction).
is_class_formula(class(Class)) :-
    is_uri(Class).

error:has_type(class_formula,X) :-
    is_class_formula(X).

/* 
 * class_assertion(+Graph:graph, +Class:uri, -Formula:class_formula) is nondet.
 *
 * Get one solution of the OWL class assertions
 * 
 * @param Graph A graph object identifying the current schema graph.
 * @param Class Atom URI identifier of rfds or owl Class.
 * @param Formula Term describing the class relationships.
 * 
 * TODO: Formula should be a type
 */
%:- rdf_meta class_assertion(o,r,t).
class_assertion(Graph, Class, (Class<Solns)) :-
    %class(Class),
    setof(Sol, Y^(subClassOf(Class,Y,Graph),
                  class_formula(Y, Graph, Sol)),
          Solns).
class_assertion(Graph, Class, (Class=and(Solns))) :-
    %class(Class,Graph),
    setof(Sol,Y^(intersectionOf(Class,Y,Graph),
                 class_formula(Y,Graph,Sol)),
          Solns).
class_assertion(Graph,Class,(Class=or(Solns))) :-
    %class(Class,Graph),
    setof(Sol,Y^(unionOf(Class,Y,Graph),
                 class_formula(Y,Graph,Sol)),
          Solns).
class_assertion(Graph,Class,(Class=xor(Solns))) :-
    %class(Class,Graph),
    setof(Sol,Y^(disjointUnionOf(Class,Y,Graph),
                 class_formula(Y,Graph,Sol)),
          Solns).
class_assertion(Graph,Class,(Class=oneOf(OneList))) :-
    oneOfList(Class,OneList,Graph).
class_assertion(Graph,Class,RType) :-
    restriction(Class,Graph),
    restriction_type(Class,RType,Graph).
class_assertion(Graph,Class,class(Class)) :-
    immediateClass(Class,Graph),
    \+ restriction(Class,Graph),
    \+ has_formula(Class,Graph).

/* 
 * class_formula(+Class:uri, +Graph:graph, -Formula:class_formula) is semidet.
 * 
 * Create a formula describing the manner in which the class is
 * defined.
 *
 * @param Graph A graph idntifying the current schema graph.
 * @param Class Atom URI identifier of rfds or owl Class.
 * @param Formula Term describing the class relationships.
 */
class_formula(Class,Graph,F) :-
    setof(Sol,class_assertion(Graph,Class,Sol), Solns),
    (   [F]=Solns
    ->  true
    ;   F=(Class=and(Solns))
    ).

maybe_label(C,Graph,[label=L]) :-
    label(C,L,Graph), !.
maybe_label(_,_,[]).

maybe_comment(C,Graph,[comment=Comment]) :-
    comment(C,Comment,Graph), !.
maybe_comment(_,_,[]). 

maybe_dcog_tag(C,Graph,[dcogtag=DC]) :-
    dcog_tag(C,DC,Graph), !.
maybe_dcog_tag(_,_,[]).

maybe_meta(C,Graph,LCD) :- 
    maybe_label(C,Graph,Label),
    maybe_comment(C,Graph,Comment),
    maybe_dcog_tag(C,Graph,DCOGTag),
    append(Label, Comment, LC),
    append(LC,DCOGTag,LCD).

decorate_elements([],_Graph,[]).
decorate_elements([Elt|Rest],Graph,[MLC|Restp]) :-
    class_record(Graph,Elt,MLC),
    decorate_elements(Rest,Graph,Restp).

/**
 * property_restriction(+Property:uri,+Graph:graph,PR:property_restriction) is det. 
 * 
 * Obtains a property restriction which is intrinsic to the property rather than 
 * a result of a restriction class.
 */
property_restriction(P,Graph,R) :-
    (   functionalProperty(P,Graph)
    ->  R=[uri=P,property=P,cardinality=1]
    ;   R=true
    ).


/** 
 * classes_below(+Class:uri, +Graph, -Below:list(uri)) is det.
 * 
 * Get all classes below us in the subsumption hierarchy, 
 * excluding owl:Nothing or abstract classes.
 */
classes_below(Class,Graph,BelowList) :-
    unique_solutions(Below,schema:subsumptionOf(Below,Class,Graph),Classes),
    exclude([X]>>(X='http://www.w3.org/2002/07/owl#Nothing'), Classes, ClassesNoBottom),
    database_name(Graph,Collection),
    database_schema(Graph,Schema),
    exclude({Collection, Schema}/[X]>>(
                xrdf(Collection, Schema,
                     X,dcog:tag,dcog:abstract)),
            ClassesNoBottom,
            BelowList).
                                   
simplify_restriction_list(T,R,S) :-
    (   R = [S]
    ->  true
    ;   R = []
    ->  S=true
    ;   S=[type=T,
           operands=R]).

/** 
 * normalise_restriction(+Restriction:property_restriction,-N:property_restriction) is det. 
 *
 * Disjunctive normal form for restriction.
 */ 
normalise_restriction([type=sub, operands=Results],N) :-
    !,
    maplist([X,Y]>>(normalise_restriction(X,Y)),Results,Normalised),
    exclude([X]>>(X=true),Normalised,Result),
    debug(normalise,'normalise_restriction sub: ~p',[Result]),
    (   exclude([F]>>(F=[type=T|_], member(T,[and,sub])), Result, [])
    ->  foldl([F,R,O]>>(F=[type=_,operands=Fs], append(Fs,R,O)),Result,[],Acc)
    ;   Acc=Result),
    simplify_restriction_list(sub,Acc,N).
normalise_restriction([type=and, operands=Results],N) :-
    !,
    maplist([X,Y]>>(normalise_restriction(X,Y)),Results,Normalised),
    exclude([X]>>(X=true),Normalised,Result),
    debug(normalise,'normalise_restriction and: ~p',[Result]),
    (   exclude([F]>>(F=[type=T|_], member(T,[and,sub])), Result, [])
    ->  foldl([F,R,O]>>(F=[type=_,operands=Fs], append(Fs,R,O)),Result,[],Acc)
    ;   Acc=Result),
    simplify_restriction_list(and,Acc,N).
normalise_restriction([type=or, operands=Results],N) :-
    !,
    maplist([X,Y]>>(normalise_restriction(X,Y)),Results,Normalised),
    simplify_restriction_list(or,Normalised,N).
normalise_restriction([type=xor, operands=Results],N) :-
    !,
    maplist([X,Y]>>(normalise_restriction(X,Y)),Results,Normalised),
    exclude([X]>>(X=true),Normalised,Result),
    simplify_restriction_list(xor,Result,N).
normalise_restriction([uri=U|Res],[uri=U|Res]) :-
    !.
normalise_restriction(true,true) :-
    !.

/* 
 * restriction_formula(+Formula:class_formula,+Graph:graph,
 *                     -RestrictionFormula:property_restriction) is det.
 * 
 * Calculate the formula of restrictions for a class.
 */
restriction_formula(_<L,Graph,S) :-
    !,
    maplist({Graph}/[C,F]>>(restriction_formula(C,Graph,F)),
            L,R),
    simplify_restriction_list(sub,R,S).
restriction_formula(_=or(L),Graph,S) :-
    !,
    maplist({Graph}/[C,F]>>(restriction_formula(C,Graph,F)),
            L,R),
    simplify_restriction_list(or,R,S).
restriction_formula(_=and(L),Graph,S) :-
    !,
    maplist({Graph}/[C,F]>>(restriction_formula(C,Graph,F)),
            L,R),
    simplify_restriction_list(and,R,S).
restriction_formula(_=xor(L),Graph,S) :-
    !,
    maplist({Graph}/[C,F]>>(restriction_formula(C,Graph,F)),
            L,R),
    simplify_restriction_list(xor,R,S).
restriction_formula(class(_),_,true) :-
    !.
restriction_formula(restriction(L),_,L) :-
    !.
restriction_formula(_=oneOf(_),_,true) :-
    !.

/** 
 * select_restriction(+P:uri,+R:property_restriction,+Graph:graph,-S:property_restriction) is det.
 */ 
select_restriction(P,[type=or,operands=Operands],G,Restriction) :-
    !,
    convlist({P,G}/[R,S]>>select_restriction(P,R,G,S), Operands, NewOperands),
    simplify_restriction_list(or,NewOperands,Restriction).
select_restriction(P,[type=sub,operands=Operands],G,Restriction) :-
    !,
    convlist({P,G}/[R,S]>>select_restriction(P,R,G,S), Operands, NewOperands),
    simplify_restriction_list(sub,NewOperands,Restriction).
select_restriction(P,[type=and,operands=Operands],G,Restriction) :-
    !,
    convlist({P,G}/[R,S]>>select_restriction(P,R,G,S), Operands, NewOperands),
    simplify_restriction_list(and,NewOperands,Restriction).
select_restriction(P,[type=xor,operands=Operands],G,Restriction) :-
    !,
    convlist({P,G}/[R,S]>>select_restriction(P,R,G,S), Operands, NewOperands),
    simplify_restriction_list(xor,NewOperands,Restriction).
select_restriction(P,[uri=U,property=Q|R],G,[uri=U,property=P|R]) :-
    subsumptionPropertiesOf(P,Q,G),
    !.
select_restriction(_,[uri=_,property=_|_],_,true) :-
    !. % \+ subsumptionPropertiesOf(P,Q,Graph).    
select_restriction(_,true,_,true).


/** 
 * calculate_property_restriction(+Property:uri,+Restriction_Formula:property_restriction, 
 *                                +Graph:graph,-Restriction:property_restriction)  is det.
 * 
 * Calculate the full restriction on a given property, from the formula and 
 * constraints directly on the property.
 */
calculate_property_restriction(Property,Restriction_Formula,Graph,Restriction) :- 
    select_restriction(Property,Restriction_Formula,Graph,CalculatedRestriction),
    property_restriction(Property,Graph,PropRestriction),
    normalise_restriction([type=and,operands=[PropRestriction,CalculatedRestriction]],
                          Restriction).

/** 
 * apply_restriction(+Class:uri,+Property:uri,+Graph:graph,
 *                   +RestrictionFormula:property_restriction,Frame) is semidet.
 * 
 * 
 */
%:- rdf_meta apply_restriction(r,r,o,o,t).
apply_restriction(Class,Property,Graph,Restriction_Formula,
                  [type=datatypeProperty,
                   property=Property,
                   domain=Class,
                   restriction=Restriction,
                   range=Range
                   |RecordRemainder]) :-
    % Checking for pseudo_properties, which denote annotations and other opaque linking
    % we don't want to generate a frame here at all.
    annotationProperty(Property,Graph),
    !,
    mostSpecificRange(Property,Range,Graph),    
    property_record(Graph,Property,RecordRemainder),
    calculate_property_restriction(Property,Restriction_Formula,Graph,Restriction).
apply_restriction(Class,Property,Graph,Restriction_Formula,
                  [type=datatypeProperty,
                   property=Property,
                   domain=Class,
                   restriction=Restriction,
                   range=Range
                   |RecordRemainder]) :-
    mostSpecificRange(Property,Range,Graph),
    datatype(Range,Graph),
    % We only just found out that we've no alternatives now after testing data type..
    !,
    property_record(Graph,Property,RecordRemainder),
    calculate_property_restriction(Property,Restriction_Formula,Graph,Restriction).
apply_restriction(Class,Property,Graph,Restriction_Formula,
                  [type=objectProperty,
                   property=Property,
                   domain=Class,
                   range=Range,
                   restriction=Restriction,
                   frame=[type=entity,class=Range|RTail]
                   |RecordRemainder]) :- 
    mostSpecificRange(Property,Range,Graph),
    class(Range,Graph),
    schema:entity(Range,Graph),
    % We are an entity frame.
    !,
    property_record(Graph,Property,RecordRemainder),    
    maybe_label(Range,Graph,RLabel),
    maybe_comment(Range,Graph,RComment),
    append(RLabel, RComment, RTail),
    calculate_property_restriction(Property,Restriction_Formula,Graph,Restriction).
apply_restriction(Class,Property,Graph,Restriction_Formula,
                  [type=objectProperty,
                   property=Property,
                   domain=Class,
                   range=Range,
                   restriction=Restriction,
                   frame=Frame
                   |RecordRemainder]) :- 
    mostSpecificRange(Property,Range,Graph),
    class(Range,Graph),
    oneOfList(Range,OneList,Graph),
    % We are an entity frame.
    !,
    Frame = [type=oneOf, elements=DecoratedOneList],
    decorate_elements(OneList,Graph,DecoratedOneList),        
    property_record(Graph,Property,RecordRemainder),
    % These always come together as a triplet, so should probably be one thing.
    calculate_property_restriction(Property,Restriction_Formula,Graph,Restriction).
apply_restriction(Class,Property,Graph,Restriction_Formula,
                  [type=objectProperty,
                   property=Property,
                   domain=Class,
                   range=Range,
                   frame=Frame,
                   restriction=Restriction
                   |RecordRemainder]) :-
    mostSpecificRange(Property,Range,Graph),
    class(Range,Graph),
    % Object property but not an entity
    !,
    property_record(Graph,Property,RecordRemainder),
    calculate_property_restriction(Property,Restriction_Formula,Graph,Restriction),

    classes_below(Range,Graph,Below),
    (   [NextClass] = Below
        % singleton choice of class class should just be rendered.
    ->  class_frame_aux(NextClass,Graph,Frame)
        % We can't decide from here...
    ;   convlist({Graph}/[C,F]>>(class_frame_aux(C,Graph,F)),Below,Frames),
        Frame=[type=class_choice,operands=Frames]
    ).

/* 
 * calculate_frame(+Class:uri,+Properties:list(uri),
 *                 +Restriction_Formula:property_restriction, Graph:graph, -Frame) is det.
 * 
 * Calculate the application of the restriction formula to the properties. 
333 */
calculate_frame(Class,Properties,Restriction_Formula,Graph,Frames) :- 
    maplist({Class,Graph,Restriction_Formula}/[Property,Property_Frame]
            >>(apply_restriction(Class,Property,Graph,Restriction_Formula,Property_Frame)),
            Properties, Frames).

/*
 * We can't actually check types here because tabling doesn't work with 
 * attributed variables.
 * 
 * class_frame_aux(+Class:uri,Graph:graph,OutputFrame:frame) is semidet. 
 * Generate the frame associated with a given class. 
 *
 * Fails if the class doesn't exist.
 */
%:- if(current_prolog_flag(optimise,true)).
%:- table class_frame/3.
%:- endif.
class_frame(Class,Graph,Frame) :-
    class_frame_aux(Class,Graph,Frame).

class_frame_aux(Class, Graph, Frame) :-
    (   class_formula(Class,Graph,Formula)
    ->  restriction_formula(Formula,Graph,RestrictionFormula),
        class_properties(Class,Graph,Properties),
        * format('Class: ~q~n Formula: ~q~n Properties ~q~n',[Class,Formula,Properties]),
        debug(restriction,'Restriction: ~p',[RestrictionFormula]),
        calculate_frame(Class,Properties,RestrictionFormula,Graph,Frame)
    ;   Frame = [type=failure, message='No Class Formula!', class=Class]).


/********************************************************************
 * Warning - Filled Class Frames are unlikely to work at the moment.*
 * They are left as documentation.                                  *
 ********************************************************************/

/** 
 * fill_class_frame_aux(+Elt,+Graph,-Frame,-Filled) is det.
 * 
 * Fill a class frame with values to recreate the LDO structure.
 *
 * NOTE: This should probably be obsoleted in favour of realisers
 */ 
fill_class_frame(_,_,[],[]) :- !.
fill_class_frame(Elt,Graph,[[type=objectProperty|P]|Rest], Frames) :-
    % object property
    !,
    member(property=Prop, P),
    select(frame=Frame,P,Pp),
    Prefix=[type=objectProperty,domainValue=Elt|Pp],
    (   setof([frame=FilledFrame],
              V^(inferredEdge(Elt, Prop, V, Graph),
                 fill_class_frame(V,Graph,Frame,FilledFrame)),
              FrameSuffixes)
    ->  maplist(append(Prefix),FrameSuffixes,PrefixFrames),
        fill_class_frame(Elt,Graph,Rest,F),
        append(PrefixFrames,F,Frames)
    ;   fill_class_frame(Elt,Graph,Rest,Frames)).
fill_class_frame(Elt,Graph,[[type=datatypeProperty|P]|Rest],Frames) :-
    % datatype property
    !,
    member(property=Prop, P),
    Prefix = [type=datatypeProperty,domainValue=Elt|P],
    (   setof([rangeValue=V], inferredEdge(Elt, Prop, V, Graph), Vs)
    ->  maplist(append(Prefix),Vs,PrefixFrames),
        fill_class_frame(Elt,Graph,Rest,F),
        append(PrefixFrames,F,Frames)
    ;   fill_class_frame(Elt,Graph,Rest,Frames)).
fill_class_frame(Elt,Graph,[[type=restriction|_]|Rest],Frames) :-
    % a bare restriction (restricts nothing)
    !, 
    fill_class_frame(Elt,Graph,Rest,Frames).
fill_class_frame(Elt,Graph,C,[type=Type,frames=Fsp]) :-
    member(type=Type, C), 
    member(operands=Fs, C),
    % An operand
    !,
    maplist({Elt,Graph}/[Fin,Fout]>>(fill_class_frame(Elt,Graph,Fin,Fout)),Fs,Fsp).
fill_class_frame(Elt,_,F,EntityFrame) :-
    member(type=Type, F),
    member(Type,[oneOf,entity]),
    % Just need one type
    !,
    append(F,[domainValue=Elt],EntityFrame).

choose_property(Graph,Property,[type=and,operands=R], Result) :-
    member(F,R),
    !,
    choose_property(Graph,Property,F,Result).
choose_property(Graph,Property,[type=or,operands=R], Result) :-
    member(F,R),
    !,
    choose_property(Graph,Property,F,Result).
choose_property(Graph,Property,[type=xor,operands=R], Result) :-
    member(F,R),
    !,
    choose_property(Graph,Property,F,Result).
choose_property(Graph,P,[R|Rest], [R|Result]) :-
    member(property=Q,R),
    !,
    % Can this be correct?
    subsumptionPropertiesOf(P,Q,Graph), !,
    choose_property(Graph,P,Rest,Result).
choose_property(Graph,P,[R|FrameRest], Result) :-
    \+ (member(property=Q,R), subsumptionPropertiesOf(P,Q,Graph)),
    choose_property(Graph,P,FrameRest,Result), !.
choose_property(_Graph,_P,[], []) :- !.

% Property Frame
class_property_frame(Class, Property, Graph, PropertyFrame) :-
    class_frame(Class, Graph, Frame),
    choose_property(Graph,Property,Frame,PropertyFrame).
    
% get filled frame for entity
entity_filled_frame(Entity,Graph,Filled) :-
    instanceClass(Entity,Class,Graph),
    class_frame(Class,Graph,Frame),
    fill_class_frame(Entity,Graph,Frame,Filled).

/*
 * realiser(+Entity:uri,+Frame:frame,+Graph:graph,+Depth:number,-Realiser:any) is det.
 * 
 * Synthesise the concrete representative of the schema class, 
 * showing inhabitation unfolding entities up to a depth of Depth. 
 * 
 * Does not actually appear to be det!
 */
realiser(Elt,Frame,Graph,Depth,['@type'=Class,
                                '@id'=Elt
                                |Realisers]) :-
    instanceClass(Elt,Class,Graph),
    realise_frame(Elt,Frame,Graph,Depth,Realisers).

/* 
 * realise_frame(Elt,Frame,Graph,Depth,Quasi_JSONLD) is det.
 * 
 * Traverse frame synthesising realisers.
 */
realise_frame(_,[],_,_,[]) :-
    !.
realise_frame(Elt,[[type=objectProperty|P]|Rest],Graph,Depth,Realisers) :-
    !, % no turning back if we are an object property
    member(property=Prop, P),
    select(frame=Frame,P,_FrameLessP),
    (   setof(New_Realiser,
              V^(inferredEdge(Elt, Prop, V, Graph),
                 (   schema:entity(V,Graph)
                 ->  (   Depth =< 1
                     ->  New_Realiser=[V]
                     ;   New_Depth is Depth-1,
                         entity_object(V,Graph,New_Depth,Object),
                         Object = [_Type,_Id|New_Realiser])
                 ;   realiser(V,Frame,Graph,Depth,New_Realiser))),
              RealiserValues)
    ->  (   RealiserValues = [Val]
        ->  Realisers = [Prop=Val|RealiserTail]
        ;   Realisers = [Prop=RealiserValues|RealiserTail]
        ),
        realise_frame(Elt,Rest,Graph,Depth,RealiserTail)
    ;   realise_frame(Elt,Rest,Graph,Depth,Realisers)
    ).
realise_frame(Elt,[[type=datatypeProperty|P]|Rest],Graph,Depth,Realisers) :-
    !, % no turning back if we are a datatype property
    member(property=Prop, P),
    (   setof(V,
              inferredEdge(Elt, Prop, V, Graph),
              RealiserValues)
    ->  (   RealiserValues = [Val]
        ->  Realisers = [Prop=Val|RealiserTail]
        ;   Realisers = [Prop=RealiserValues|RealiserTail]
        ),
        realise_frame(Elt,Rest,Graph,Depth,RealiserTail)
    ;   realise_frame(Elt,Rest,Graph,Depth,Realisers)
    ).
realise_frame(Elt,[[type=restriction|_R]|Rest],Graph,Depth,Realisers) :-
    % We are a bare restriction, not applied to any property
    !, 
    realise_frame(Elt,Rest,Graph,Depth,Realisers).
realise_frame(Elt,[[type=class_choice,operands=_]|Rest],Graph,Depth,[Realiser|Realisers]) :-
    % We are a bare class_choice, not applied to any property
    !,
    entity_object(Elt,Graph,Depth,Realiser),
    realise_frame(Elt,Rest,Graph,Depth,Realisers).
realise_frame(Elt,Frame,Graph,Depth,Realisers) :-
    % We should be able to assume correctness of operator here...
    % member(type=Type, Frame), 
    member(operands=Fs, Frame),
    !, % We're an operator, so stick with it
    maplist({Elt,Graph,Depth}/[TheFrame,New_Realiser]
            >>(realiser(Elt,TheFrame,Graph,Depth,New_Realiser)),Fs,Realisers).
realise_frame(_Elt,F,_Graph,_Depth,[]) :-
    memberchk(type=oneOf, F),
    !.
realise_frame(Elt, Frame, Graph, Depth, New_Realiser) :-
    memberchk(type=entity, Frame),
    (   Depth =< 1
    ->  New_Realiser=[]
    ;   New_Depth is Depth-1,
        entity_object(Elt,Graph,New_Depth,Object),
        Object = [_Type,_Id|New_Realiser]).


/* 
 * realise_triples(Elt,Frame,Graph,Realiser) is det.
 *
 * The triple realiser must be kept in complete lock step with the definition above. 
 * This makes me wonder if we shouldn't keep the method fused or derived!
 * 
 * It may be desirable to have a depth parameter here as well?
 */ 
realise_triples(_,[],_,[]) :-
    !.
realise_triples(Elt,[[type=objectProperty|P]|Rest],Graph,Realiser) :-
    !, % no turning back if we are an object property
    database_name(Graph,C),
    database_instance(Graph,G),
    
    member(property=Prop, P),
    select(frame=Frame,P,_FrameLessP),
    (   setof(New_Realiser,
              V^(inferredEdge(Elt, Prop, V, Graph),
                 (   schema:entity(V,Graph)
                 ->  New_Realiser=[(C,G,Elt,Prop,V)]
                 ;   realise_triples(V,Frame,Graph,Below),
                     New_Realiser=[(C,G,Elt,Prop,V)|Below])),
              RealiserLists)
    ->  append(RealiserLists,Realisers_on_P),
        realise_triples(Elt,Rest,Graph,Realiser_Tail),
        append(Realisers_on_P, Realiser_Tail, Realiser)
    ;   realise_triples(Elt,Rest,Graph,Realiser)
    ).
realise_triples(Elt,[[type=datatypeProperty|P]|Rest],Graph,Realiser) :-
    !, % no turning back if we are a datatype property
    database_name(Graph,C),
    database_instance(Graph,G),

    member(property=Prop, P),
    (   setof((C,G,Elt,Prop,V),
              inferredEdge(Elt, Prop, V, Graph),
              Realisers_on_P)
    ->  realise_triples(Elt,Rest,Graph,Realiser_Tail),
        append(Realisers_on_P,Realiser_Tail,Realiser)
    ;   realise_triples(Elt,Rest,Graph,Realiser)
    ).
realise_triples(Elt,[[type=restriction|_R]|Rest],Graph,Realiser) :-
    % We are a bare restriction, not applied to any property
    !, 
    realise_triples(Elt,Rest,Graph,Realiser).
realise_triples(Elt,[[type=class_choice,operands=_]|Rest],Graph,Realiser) :-
    % We are a bare class choice, not applied to any property
    !,
    object_edges(Elt,Graph,Edges),
    realise_triples(Elt,Rest,Graph,Realiser_Tail),
    append(Edges,Realiser_Tail,Realiser).
realise_triples(Elt,Frame,Graph,Realisers) :-
    % We should be able to assume correctness of operator here...
    % member(type=Type, Frame), 
    member(operands=Fs, Frame),
    !, % We're an operator, so stick with it
    maplist({Elt,Graph}/[TheFrame,New_Realiser]
            >>(realise_triples(Elt,TheFrame,Graph,New_Realiser)),Fs,Realiser_List),
    append(Realiser_List,Realisers).
realise_triples(_Elt,F,_Graph,[]) :-
    member(type=Type, F),
    member(Type,[oneOf,entity]),
    % is a one-of or entity (don't backtrack over member)
    !.

/*
 * entity_object(+Entity:uri,+Graph:graph,+Depth,-Realiser) is semidet.
 * 
 * Gets the realiser for the frame associated with the class of 
 * Entity
 */ 
entity_object(Entity,Graph,Depth,Realiser) :-
    most_specific_type(Entity,Class,Graph),
    class_frame(Class,Graph,Frame),
    realiser(Entity,Frame,Graph,Depth,Realiser).

/* 
 * get_collection_jsonld_context(Collection,Ctx) is det. 
 * 
 * Get a JSON-LD dictonary holding the context for this db.
 */
get_collection_jsonld_context(Collection, Ctx) :-
    get_collection_prefix_list(Collection,Ctx_Obj),
    term_jsonld(Ctx_Obj, Ctx).

/*
 * entity_jsonld(+Entity:uri,+Ctx:any,+Graph:graph,-Realiser) is semidet.
 * 
 * Gets the realiser for the frame associated with the class of 
 * Entity in a JSON_LD format using a supplied context.
 */ 
entity_jsonld(Entity,Ctx,Graph,JSON_LD) :-
    entity_object(Entity, Graph, 1, Realiser),
    term_jsonld(Realiser, JSON_Ex),
    
    database_name(Graph, Collection),
    get_collection_jsonld_context(Collection,Ctx_Graph),
    merge_dictionaries(Ctx,Ctx_Graph,Ctx_Total),

    compress(JSON_Ex,Ctx_Total,JSON_LD).

/*
 * entity_jsonld(+Entity:uri,+Ctx:any,+Graph:graph,+Depth,-Realiser) is semidet.
 * 
 * Gets the realiser for the frame associated with the class of 
 * Entity in a JSON-LD format using a supplied context and unfolding 
 * up to depth Depth
 */ 
entity_jsonld(Entity,Ctx,Graph,Depth,JSON_LD) :-
    entity_object(Entity, Graph, Depth, Realiser),
    term_jsonld(Realiser, JSON_Ex),
    
    database_name(Graph, Collection),
    get_collection_jsonld_context(Collection,Ctx_Graph),
    merge_dictionaries(Ctx,Ctx_Graph,Ctx_Total),

    compress(JSON_Ex,Ctx_Total,JSON_LD).


/*
 * entity_jsonld(+Entity:uri,+Graph:graph,-Realiser) is semidet.
 * 
 * Gets the realiser for the frame associated with the class of 
 * Entity in a JSON_LD format.
 */ 
entity_jsonld(Entity,Graph,JSON_LD) :-
    entity_jsonld(Entity,_{},Graph,JSON_LD).


/* 
 * class_frame_jsonld(Class,Graph,JSON_Frame) is det.
 * 
 */
class_frame_jsonld(Class,Graph,JSON_Frame) :-
    class_frame(Class,Graph,Frame),
    term_jsonld(Frame,JSON_LD),
    
    database_name(Graph, Collection),
    get_collection_jsonld_context(Collection,Ctx),
    
    compress(JSON_LD,Ctx,JSON_Frame).

/* 
 * object_edges(URI,Graph,Edges) is det.
 * 
 * Is there any way to make this so that everyting is derived from 
 * the same source? 
 */ 
object_edges(URI,Graph,Edges) :-
    most_specific_type(URI,Class,Graph),
    class_frame(Class,Graph,Frame),
    realise_triples(URI,Frame,Graph,Edges).

/* 
 * object_references(URI,Graph,Edges) is det.
 * 
 * Get the set of references to a given object.
 */ 
object_references(URI,Graph,Edges) :-
    database_name(Graph,C),
    database_instance(Graph,G),

    findall((C,G,Elt,Prop,URI),
            inferredEdge(Elt, Prop, URI, Graph),
            Edges).

/* 
 * delete_object(URI,Graph) is det.
 * 
 */
delete_object(URI,Graph) :-
    object_edges(URI,Graph,Object_Edges),
    object_references(URI,Graph,References),
    append(Object_Edges,References,Edges),
    
    maplist([(C,G,X,Y,Z)]>>(delete(C,G,X,Y,Z)), Edges).

/* 
 * update_object(Obj:dict,Graph) is det.
 * 
 * This should extract the object from the database
 * and set up inserts / deletes as:
 * 
 * Inserts := triples(New) / triples(Old)
 * Deletes := triples(New) / triples(New)
 */
update_object(Obj, Graph) :-
    jsonld_id(Obj,ID),
    update_object(ID,Obj,Graph).


/* 
 * update_object(ID:url,Obj:dict,Graph) is det.
 * 
 * Does the actual updating using ID. 
 */
update_object(ID, Obj, Graph) :-
    put_dict('@id', Obj, ID, New_Obj),

    database_name(Graph,Collection),
    get_collection_jsonld_context(Collection,Ctx),
    
    jsonld_triples(New_Obj,Ctx,Graph,New),
    object_edges(ID,Graph,Old),
    % Don't back out now.  both above should be det so we don't have to do this. 
    !,
    subtract(New,Old,Inserts),
    subtract(Old,New,Deletes),

    maplist([(C,G,X,Y,Z)]>>(insert(C,G,X,Y,Z)), Inserts),
    
    maplist([(C,G,X,Y,Z)]>>(delete(C,G,X,Y,Z)), Deletes).
