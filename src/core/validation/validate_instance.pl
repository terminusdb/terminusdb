:- module(validate_instance,[
              instance_class/3,
              most_specific_type/3,
              refute_insertion/5,
              refute_deletion/5,
              refute_basetype_elt/3
          ]).

/** <module> Instance Validation
 *
 * This module deals with instance checking as against a given schema.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(validate_schema).

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(http/json)).

/**
 * most_specific_type(+Document, -Sorted, +Database)
 *
 * Gets the most specific type for a class
 **/
most_specific_type(Document, Document_Type, Database):-
    get_ordered_instance_classes(Document, [Document_Type|_], Database).

/**
 * get_ordered_instance_classes(+Document, -Sorted, +Database)
 *
 * Gets all classes for a specific document and returns
 * them ordered by subsumption.
 **/
get_ordered_instance_classes(Document, Sorted, Database) :-
    findall(
        Class,
        instance_class(Document, Class, Database),
        Classes
    ),

    predsort(
        {Database}/[Delta,C1,C2]>>
        (   strict_subsumption_of(C1, C2, Database)
        ->  Delta = (<)
        ;   strict_subsumption_of(C2, C1, Database)
        ->  Delta = (>)
        ;   C1 = C2
        ->  Delta = (=)
        ), Classes, Sorted
    ).

/**
 * instance_class(?X:uri, ?C:uri, +Database:database) is nondet.
 *
 * Determines the class C identified with the instance X.
 */
instance_class(X, Y, Database) :-
    database_instance(Database,Instance),
    xrdf(Instance, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y).
instance_class(X, Y, Database) :-
    database_schema(Database,Schema), % instances can also exist in the schema
    xrdf(Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y).

% X has cardinality N at property OP
card(X,OP,Y,Database,N) :-
    (   setof(Y,inferredEdge(X,OP,Y,Database), ListX)
    ->  ListX = L
    ;   L = []),
    length(L,N).

% X has qualified cardinality N at property OP and class C
qualifiedCard(X,OP,Y,C,Database,N) :-
    (   setof(Y,
              (   inferredEdge(X,OP,Y,Database),
	              \+ refute_node_at_range(Y,C,Database,_)
              ),
	          ListX)
    ->  ListX = L
    ;   L = []),
    length(L,N).

/*
 * refute_insertion(+Database, +Graph, +X, +P, +Y, -Reason)
 *

For readability:

S,G |- X : C  :=  \+ refute_node_at(S,G,X,C,Reason)

inserts follow this pattern:

forall (x p y) \in Inserts.
  p = 'rdf:type' => S,G |- x : y
  p /= 'rdf:type' =>
    S,G |- x : dom(p) /\ S,G |- y : rng(p)

  forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

*/
refute_insertion(Database,
                 X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',Class,
                 Reason) :-
    !,
    refute_node_at_class(Database,X,Class,Reason).
refute_insertion(_Database,
                 X,'http://www.w3.org/2000/01/rdf-schema#label',Label,
                 Reason) :-
    !,
    refute_basetype_elt(Label,'http://www.w3.org/2001/XMLSchema#string',Base_Reason),
    Reason = Base_Reason.put(_{'vio:subject' : X,
                               'vio:property' : 'http://www.w3.org/2000/01/rdf-schema#label'}).
refute_insertion(_Database,
                 X,'http://www.w3.org/2000/01/rdf-schema#comment',Label,
                 Reason) :-
    !,
    refute_basetype_elt(Label,'http://www.w3.org/2001/XMLSchema#string',Base_Reason),
    Reason = Base_Reason.put(_{'vio:subject' : X,
                               'vio:property' : 'http://www.w3.org/2000/01/rdf-schema#label'}).
refute_insertion(Database,X,P,_Y,Reason) :-
    % \+ P = 'rdf:type'
    (   domain(P,Domain,Database)
    ->  refute_node_at_class(Database,X,Domain,Sub_Reason),
        Reason = Sub_Reason.put(_{ 'vio:property' : _{ '@value' : P,
                                                       '@type' : 'xsd:anyURI' }})
    ;   interpolate(['The property ', P,' has an undefined domain.'],Message),
        Reason = _{
                     '@type' : 'vio:PropertyWithUndefinedDomain',
                     'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                     'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
                 }
    ).
refute_insertion(Database,_X,P,Y,Reason) :-
    % \+ P = 'rdf:type'
    (   range(P,Range,Database)
    ->  refute_node_at_range(Database,Y,Range,Sub_Reason),
        Reason = Sub_Reason.put(_{ 'vio:property' : _{ '@value' : P,
                                                       '@type' : 'xsd:anyURI' }})
    ;   format(atom(Message),'The property ~q has an undefined domain.',[P]),
        Reason = _{
                     '@type' : 'vio:PropertyWithUndefinedRange',
                     'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                     'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
                 }
    ).
refute_insertion(Database,X,P,_Y,Reason) :-
    % Note: This does too much work in the case that we are not a newly inserted object.
    refute_all_restrictions(Database,X,P,Reason).
refute_insertion(Database,X,P,Y,Reason) :-
    refute_functional_property(X,P,Y,Database,Reason).
refute_insertion(Database,X,P,Y,Reason) :-
    refute_inverse_functional_property(X,P,Y,Database,Reason).

/*
 * refute_node_at_class(+D,+G,+X,+C,Reason) is nondet.
 *
 * Refute that X is at class C.
 */
refute_node_at_class(Database,X,Class,Reason) :-
    (   instance_class(X,IC,Database)
    ->  (   class(IC, Database)
        ->  (   subsumption_of(IC,Class,Database)
            ->  subsumption_of(IC,Super,Database),
                domain(P,Super,Database),
                refute_all_restrictions(Database,X,P,Reason)
            ;   format(atom(Message),'The subject ~q has a class ~q not subsumed by ~q.',[X,IC,Class]),
                Reason = _{
                             '@type' : 'vio:InstanceSubsumptionViolation',
                             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                             'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                             'vio:class' : _{ '@value' : IC, '@type' : 'xsd:anyURI' },
                             'vio:parent' : _{ '@value' : Class, '@type' : 'xsd:anyURI' }
                         })
        ;   (   \+ atom(Class)
            ->  format(atom(Message), 'The term ~q is not a valid class', [Class]),
                term_string(Class, Term_String),
                Reason = _{
                             '@type' : 'vio:InvalidClassViolation',
                             'vio:message' : _{ '@value' : Message,
                                                '@type' : 'xsd:string'},
                             'vio:class' : _{ '@value' : Term_String,
                                              '@type' : 'xsd:anyURI' }
                         }
            ;   format(atom(Message), 'The class ~q is not found in the schema', [Class]),
                Reason = _{
                             '@type' : 'vio:InvalidClassViolation',
                             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                             'vio:class' : _{ '@value' : Class, '@type' : 'xsd:anyURI' }
                         }))
    ;   format(atom(Message),'The subject ~q has no defined class.',[X]),
        Reason = _{
                     '@type' : 'vio:UntypedInstance',
                     'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' }
                 }).

/*
 * refute_node_at_range(+Database,+Graph,+X,+Class,-Reason) is nondet.
 *
 * This does extra work for datatypes, the passes to the generic class check.
 */
refute_node_at_range(_Database,X,Class,Reason) :-
    is_literal(X),
    !,
    refute_basetype_elt(X,Class,Reason).
refute_node_at_range(Database,X,Class,Reason) :-
    % \+ is_literal(X)
    refute_node_at_class(Database,X,Class,Reason).

/*
 * refute_sp_restriction(+Database,+Graph,+X,+P,-Reason) is nondet.
 *

forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

 */
refute_all_restrictions(Database,X,P,Reason) :-
    % Domain existence must be checked separately
    instance_class(X,C,Database),
    restriction_on_property(CR,P,Database),
    subsumption_of(C,CR,Database),
    % check to see does X satisfy each restriction
    refute_restriction(Database,X,CR,P,Reason).

/*
 * refute_deletion(Database,X,P,Y,Reason) is nondet.
 *
 * We no longer need to show satisfaction of domain and range
 * as we have been taken off the charts, but we do have to show
 * that cardinalities have not been violated.

deletes following the following pattern:

forall (x p y) \in Deletes.

  forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

*/
refute_deletion(Database,
                X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',_Class,
                Reason) :-
    !,
    database_instance(Database,Instance),
    \+ xrdf(Instance,X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',_SomeClass),
    % And yet something exists...
    xrdf(Instance,X,_,_),
    interpolate(['The subject ',X,' has no defined class.'],Message),
    Reason = _{
                 '@type' : 'vio:UntypedInstance',
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' }
             }.
refute_deletion(Database,X,P,_Y,Reason) :-
    % Note: This only needs to happen if there is an object deletion
    refute_all_restrictions(Database,X,P,Reason).
refute_deletion(Database,X,P,Y,Reason) :-
    refute_functional_property(X,P,Y,Database,Reason).
refute_deletion(Database,X,P,Y,Reason) :-
    refute_inverse_functional_property(X,P,Y,Database,Reason).

%%%%%%%%%%%%%%%%%%%%%%
%%   RESTRICTIONS   %%
%%%%%%%%%%%%%%%%%%%%%%

/*
 * refute_restriction(X,CR,Reason)
 *
 * X is not an element of the restriction CR at P (for Reason)
 */
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:someValuesFrom,C),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    forall(inferredEdge(X,P_Actual,Y,Database),
           (   \+ refute_node_at_range(Database,Y,C,_Reason)
           )),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedRestrictionViolation',
			     'vio:message' : 'No values from restriction class',
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:allValuesFrom,C),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    inferredEdge(X,P_Actual,Y,Database),
    refute_node_at_range(Database,Y,C,Reason),
    interpolate(['Some values not from restriction class: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:minCardinality,CardStr^^xsd:nonNegativeInteger),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    coerce_number(CardStr,N),
    card(X,P_Actual,_,Database,M),
    M < N, atom_number(A,M),
    interpolate(['Cardinality not great enough for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:maxCardinality,CardStr^^xsd:nonNegativeInteger),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    coerce_number(CardStr,N),
    card(X,P_Actual,_,Database,M),
    N < M, atom_number(A,M),
    interpolate(['Cardinality too great for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:cardinality, CardStr^^xsd:nonNegativeInteger),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    coerce_number(CardStr,N),
    card(X,P_Actual,_,Database,M),
    N \= M, atom_number(A,M),
    interpolate(['Cardinality does not match for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:minQualifiedCardinality, CardStr^^xsd:nonNegativeInteger),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    xrdf(Schema,CR,owl:onClass,C),
    coerce_number(CardStr,N),
    qualifiedCard(X,P_Actual,_,C,Database,M),
    M < N, atom_number(A,M),
    interpolate(['Cardinality too low for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:maxQualifiedCardinality, CardStr^^xsd:nonNegativeInteger),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    xrdf(Schema,CR,owl:onClass,C),
    coerce_number(CardStr,N),
    qualifiedCard(X,P_Actual,_,C,Database,M),
    N < M, atom_number(A,M),
    interpolate(['Cardinality too high for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:qualifiedCardinality, CardStr^^xsd:nonNegativeInteger),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    xrdf(Schema,CR,owl:onClass,C),
    coerce_number(CardStr,N),
    qualifiedCard(X,P_Actual,_,C,Database,N),
    N \= M, atom_number(A,M),
    interpolate(['Cardinality unequal on restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Schema,CR,owl:hasValue,V),
    xrdf(Schema,CR,owl:onProperty,P_Actual),
    inferredEdge(X,P_Actual,Y,Database),
    Y \= V,
    interpolate(['Wrong value on restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceValueRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:value' : _{ '@value' : V, '@type' : 'xsd:anyURI' }
             }.

/**
 * refute_functional_property(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:vio) is nondet.
 *
 * Determines of ?P is actually a functional property or not.
 */
refute_functional_property(X,P,Y,Database,Reason) :-
    functional_property(P,Database),
    database_instance(Database,Instance),
    xrdf(Instance,X,P,_),
    card(X,P,_,Database,N),
    N \= 1,
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = _{
                 '@type' : 'vio:FunctionalPropertyViolation',
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.

/**
 * refute_inverse_functional_property(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:vio) is nondet.
 *
 * Determines of ?P is actually an inverse functional property or not.
 */
refute_inverse_functional_property(X,P,Y,Database,Reason) :-
    database_instance(Database,Instance),
    inverse_functional_property(P,Database),
    xrdf(Instance,_,P,Y),
    card(_,P,Y,Database,N),
    N \= 1,
    interpolate(['Inverse Functional Property ',P,' is not inverse functional.'],Message),
    Reason = _{
                 '@type' : 'vio:FunctionalPropertyViolation',
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.

%%%%%%%%%%%%%%%%%%%%%%
%%  BASETYPES ONLY  %%
%%%%%%%%%%%%%%%%%%%%%%

/*
 * refute_basetype_elt(+Literal,+Type,-Reason)
 */
refute_basetype_elt(L,T,R) :-
    (   L = _^^T2,
        \+ basetype_subsumption_of(T,T2)
    ->  R = _{
                '@type' : 'vio:DataTypeSubsumptionViolation',
                'vio:message' : 'Could not subsume type1:required_type with type2:found_type',
			    'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : T},
			    'vio:parent_type' : _{ '@type' : 'xsd:string', '@value' : T2}
            }
    ;   refute_basetype_elt_(T,L,R)
    ).

refute_basetype_elt_('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',S@L,Reason) :-
    (   \+ (atom(S) ; string(S)),
        term_to_atom(S@L,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom or string for language value, found term.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',S@L,Reason) :-
    (   \+ atom(L),
        term_to_atom(S@L,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom in language section, found term.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S@L,Reason) :-
    (   \+ atom(L),
        term_to_atom(S@L,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom in language section, found term.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S@L,Reason) :-
    (   \+ (atom(S) ; string(S)),
        term_to_atom(S@L,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom or string for language value, found term.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S^^_,Reason) :-
    (   \+ (atom(S) ; string(S)),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom, found term as element.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S^^T,Reason) :-
    (   \+ atom(T),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom, found term as type.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#coordinatePolygon',S^^_, Reason) :-
    (   \+ is_coordinate_polygon(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate polygon',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:coordinatePolygon'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#coordinatePolyline',S^^_, Reason) :-
    (   \+ is_coordinate_polygon(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate polyline',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:coordinatePolyline'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#coordinate',S^^_, Reason) :-
    (   \+ is_point(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:coordinate'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#dateRange',S^^_, Reason) :-
    (   \+ is_date_range(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed dateRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:dateRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#integerRange',S^^_, Reason) :-
    (   \+ is_integer_range(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed integerRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:integerRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#decimalRange',S^^_, Reason) :-
    (   \+ is_decimal_range(S),
        term_to_atom(S,A)
	->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed decimalRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:decimalRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#gYearRange',S^^_, Reason) :-
    (   \+ is_gyear_range(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed gYearRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:gYearRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#url',S^^_, Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:url,C,[])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid URL',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:url'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#email',S^^_, Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:email,C,[])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid email address',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:email'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#json',S^^_, Reason) :-
    (   \+ (catch(atom_json_dict(S,_,[]),_,fail)),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid json object',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:json'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#boolean',S^^_,Reason) :-
    (   \+ is_boolean(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed boolean.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:boolean'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#decimal',S^^_,Reason) :-
    (   \+ number(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed decimal.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:decimal'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#integer',S^^_,Reason) :-
    (   \+ integer(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed integer.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:integer'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#double',S^^_,Reason) :-
    (   \+ float(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed double.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:double'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#float',S^^_,Reason) :-
    (   \+ float(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed float.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:float'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#time',S^^_,Reason) :-
    (   \+ is_time(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:time',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:time'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#date',S^^_,Reason) :-
    (   \+ is_date(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:date.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:date'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#dateTime', S^^_,Reason) :-
    (   \+ is_date_time(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:dateTime : parameter out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:dateTime'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gYear',S^^_,Reason) :-
    (   \+ is_gyear(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYear',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gYear'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gMonth',S^^_,Reason) :-
    (   \+ is_gmonth(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:Month',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gDay',S^^_,Reason) :-
    (   \+ is_gday(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gMonth',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gYearMonth',S^^_,Reason) :-
    (   \+ is_gyear_month(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gYearMonth'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gMonthDay',S^^_,Reason) :-
    (   \+ is_gmonth_day(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonthDay'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#duration',S^^_,Reason) :-
    (   \+ is_duration(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:duration',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:duration'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#byte',S^^_,Reason) :-
    (   \+ is_byte(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:byte',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:byte'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#short',S^^_,Reason) :-
    (   \+ is_short(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:short',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:short'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#int',S^^_,Reason) :-
    (   \+ is_int(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:int',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:int'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#long',S^^_,Reason) :-
    (   \+ is_long(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:long',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:long'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedByte',S^^_,Reason) :-
    (   \+ is_unsigned_byte(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedByte',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedByte'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedShort',S^^_,Reason) :-
    (   \+ is_unsigned_short(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedShort',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedShort'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedInt',S^^_,Reason) :-
    (   \+ is_unsigned_int(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedInt',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedInt'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedLong',S^^_,Reason) :-
    (   \+ is_unsigned_long(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedLong',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedLong'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#positiveInteger',S^^_,Reason) :-
    (   \+ is_positive_integer(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:positiveInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:positiveInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',S^^_,Reason) :-
    (   \+ is_nonnegative_integer(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:nonNegativeInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
	                 'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:nonNegativeInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#negativeInteger',S^^_,Reason) :-
    (   \+ is_negative_integer(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:negativeInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:negativeInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#nonPositiveInteger',S^^_,Reason) :-
    (   \+ is_nonpositive_integer(S),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:nonPositiveInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:nonPositiveInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#base64Binary',S^^_,Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:base64Binary,C,[])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:base64Binary',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:base64Binary'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#anyURI',S^^_,Reason) :-
    (   \+ uri_components(S,_),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:anyUri',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:anyURI'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#language',S^^_,Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:language, C, [])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:language',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:language'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#normalizedString',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:normalizedString,C, [])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:normalizedString',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:normalizedString'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#token',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:normalizedString,C, [])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:token',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:token'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#NMTOKEN',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:nmtoken,C, [])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NMTOKEN',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:NMTOKEN'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#Name',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:name,C, [])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:Name',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:Name'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#NCName',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:ncname,C, [])),
        term_to_atom(S,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NCName',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:NCName'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral',T,Reason) :-
    (   \+ (atom(T) ; string(T)),
        term_to_atom(T,A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed rdf:PlainLiteral',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'rdf:PlainLiteral'}
                 }
    ).

		 /*******************************
		 *  MISSING PREDICATES          *
		 *******************************/

strictSubsumptionOf(A,B,C) :-
    throw(error(system_error,
      context(strictSubsumptionOf(A, B, C),
              'strictSubsumptionOf/3 does not exist'))).
