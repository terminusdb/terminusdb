:- module('query/restrictions', [
              run_restriction_named/5,
              ids_for_restriction/4
          ]).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document)).
:- use_module(core(query/algebra)).
:- use_module(core(query/constraints)).

:- use_module(library(terminus_store)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module(library(solution_sequences)).

interpret_restriction(and(A,B),DB) :-
    interpret_restriction(A,DB),
    interpret_restriction(B,DB).
interpret_restriction(or(A,_),DB) :-
    interpret_restriction(A, DB).
interpret_restriction(or(_,B), DB) :-
    interpret_restriction(B, DB).
interpret_restriction(impl(A,B), DB) :-
    (   interpret_restriction(A, DB)
    *-> interpret_restriction(B, DB)
    ;   true
    ).
interpret_restriction(op(Op,A,B),_) :-
    effective_operator(Op, Operator),
    unannotated(A, ARaw),
    unannotated(B, BRaw),
    call(Operator, ARaw, BRaw).
interpret_restriction(t(A,P,C), DB) :-
    DB = database(Schema,Instance),
    database_schema_prefixes(Schema,Prefixes),
    prefix_expand_schema(P, Prefixes, PEx),
    xrdf(Instance, A, PEx, C).
interpret_restriction(nt(A,P,C), DB) :-
    DB = database(Schema,Instance),
    database_schema_prefixes(Schema,Prefixes),
    prefix_expand_schema(P, Prefixes, PEx),
    \+ xrdf(Instance, A, PEx, C).
interpret_restriction(isa(X,C), DB) :-
    DB = database(Schema,Instance),
    database_schema_prefixes(Schema,Prefixes),
    prefix_expand_schema(C, Prefixes, CEx),
    global_prefix_expand(rdf:type, RDF_Type),
    xrdf(Instance, X, RDF_Type, CEx).

compile_restriction(Dictionary, _Schema, Subject, Expression, Reason),
is_dict(Dictionary),
get_dict('@having', Dictionary, Having) =>
    get_dict('@on', Dictionary, Type),
    atom_string(Type_Atom, Type),
    get_dict('_id', Dictionary, Id),
    (   get_dict('@comment', Dictionary, Message)
    ->  true
    ;   Id = Message
    ),
    compile_constraint_rule(Having, Subject, Rule_Expression, _{}, _),
    Expression = and(isa(Subject, Type_Atom),
                     and(Rule_Expression,
                         op(=,Reason, json{ id: Id, message: Message}))).
compile_restriction(Dictionary, Schema, Subject, Expression, Reason),
is_dict(Dictionary),
get_dict('@anyOf', Dictionary, Restrictions) =>
    maplist({Schema, Subject, R}/[Restriction_Name, Expression]>>(
                get_restriction_named(Restriction_Name, Schema, Restriction),
                compile_restriction(Restriction, Schema, Subject, Expression, R)
            ),
            Restrictions,
            Expressions),
    get_dict('_id', Dictionary, Id),
    (   get_dict('@comment', Dictionary, Message)
    ->  true
    ;   Id = Message
    ),
    termlist_disjunction(Expressions, Or_Expression),
    Expression = and(Or_Expression,
                     op(=,Reason,
                        json{ id: Id, message: Message, reason: R}
                       )).

get_restriction_named(Name, Schema, Restriction) :-
    database_schema_context_object(Schema, Context),
    get_dict('@metadata', Context, Metadata),
    get_dict('restrictions', Metadata, Restrictions),
    include({Name}/[R]>>get_dict('_id', R, Name),
            Restrictions,
            [Restriction|_]).

run_restriction_named(Schema, Instance, Name, Subject, Reasons) :-
    get_restriction_named(Name, Schema, Restriction),
    compile_restriction(Restriction, Schema, Subject, Expression, Reason),
    group_by(Subject, Reason,
            interpret_restriction(Expression, database(Schema, Instance)),
            Reasons).

ids_for_restriction(Transaction, Restriction_Name, Id, Reason) :-
    ground(Id),
    !,
    database_instance(Transaction, Instance),
    database_schema(Transaction, Schema),
    Instance = [Instance_Obj],
    get_dict(read, Instance_Obj, Layer),
    ground(Layer),
    subject_id(Layer, IRI, Id),
    run_restriction_named(Schema, Instance, Restriction_Name, IRI, Reason_JSON),
    atom_json_dict(Reason, Reason_JSON, [as(string)]).
ids_for_restriction(Transaction, Restriction_Name, Id, Reason) :-
    database_instance(Transaction, Instance),
    database_schema(Transaction, Schema),
    Instance = [Instance_Obj],
    get_dict(read, Instance_Obj, Layer),
    ground(Layer),
    run_restriction_named(Schema, Instance, Restriction_Name, IRI, Reason_JSON),
    subject_id(Layer, IRI, Id),
    atom_json_dict(Reason, Reason_JSON, [as(string)]).


:- begin_tests(restrictions).
:- use_module(core(util/test_utils)).

insurance_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "@metadata" : {
      "restrictions" : [
            { "_type" : "Restriction",
              "_id" : "NoDeathCert",
              "@comment" : "There was no death certificate in this claims case",
              "@on" : "ClaimCase",
              "@having" : { "death_certificate" : null }
            },
            { "_type" : "Restriction",
              "_id" : "NoPolicy",
              "@on" : "ClaimCase",
              "@having" : { "policy" : null }
            },
            { "_type" : "Restriction",
              "_id" : "NeedsService",
              "@on" : "ClaimCase",
              "@anyOf" : ["NoDeathCert", "NoPolicy"]
            },
            { "_type" : "Restriction",
              "_id" : "NamesDontMatch",
              "@on" : "ClaimCase",
              "@having" :
              { "@and" : [{ "death_certificate" : { "name" : { "@var" : "DeathCertName"}},
                            "policy" : { "life_assured_name" : { "@var" : "PolicyName"}}},
                          { "@with" : "DeathCertName",
                            "@ne" : {"@var" : "PolicyName"}}]
              }
            },
            { "_type" : "Restriction",
              "_id" : "NeedsAssessment",
              "@on" : "ClaimCase",
              "@anyOf" : ["NamesDontMatch"]
            }
        ]
  }
}
{ "@id": "Policy",
  "@type": "Class",
  "beneficiary": "Beneficiary",
  "country_of_issue": "Country",
  "covered_countries": {
      "@class": "Country",
      "@type": "Set"
  },
  "effective_date" : "xsd:dateTime",
  "life_assured_date_of_birth": "xsd:string",
  "life_assured_name": "xsd:string",
  "premium_paid_to_date": "xsd:double",
  "sum_assured": "xsd:double"
}
{ "@id": "Refund",
  "@type": "Class",
  "refunded_to": "Beneficiary",
}
{ "@id": "Beneficiary",
  "@type": "Class",
  "bank_account": "xsd:string",
  "date_of_birth": "xsd:dateTime",
  "is_flagged": "xsd:boolean",
  "name": "xsd:string"
}
{ "@id": "ClaimCase",
  "@type": "Class",
  "death_certificate":
  { "@class": "DeathCertificate",
    "@type": "Optional"
  },
  "incur":
  { "@class": "Refund",
    "@type": "Optional"
  },
  "policy":
  { "@class": "Policy",
    "@type": "Optional"
  }
}
{ "@id": "DeathCertificate",
  "@type": "Class",
  "country_of_death": "Country",
  "date_of_birth": "xsd:dateTime",
  "date_of_death": "xsd:dateTime",
  "name": "xsd:string"
}
{ "@id": "Country",
  "@key":
  { "@fields": ["name"],
    "@type": "Lexical"
  },
  "@type": "Class",
  "name": "xsd:string"
}
').

insurance_database(
    [
        json{ '@type' : "Country", name : "Germany" },
        json{ '@type' : "ClaimCase" },
        json{ '@type' : "ClaimCase",
              death_certificate: json{ '@type' : "DeathCertificate",
                                       name : "Joe",
                                       country_of_death: "Country/Germany",
                                       date_of_birth: "2012-01-01T00:00:00Z",
                                       date_of_death: "2013-01-01T00:00:00Z"}}
    ]
).

insert_insurance_documents(Desc) :-
    insurance_database(Documents),

    with_test_transaction(
        Desc,
        C1,
        (   empty_assoc(Captures_In),
            api_document:api_insert_document_from_lazy_list(
                             Documents, instance, false, C1,
                             Captures_In,
                             _Captures_Out,
                             _Backlinks,
                             _Ids))
    ).

test(compile_needs_service,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    open_descriptor(Desc, Db),
    database_schema(Db, Schema),

    get_restriction_named("NeedsService", Schema, Named),
    compile_restriction(Named, Schema, Subject, Expression, Reason),

    Expression =
    and(or(and(and(isa(NoDeathCert,'ClaimCase'),
				   op(=,
					  R1,
					  json{ id:"NoDeathCert",
							message:"There was no death certificate in this claims case"
						  })),
			   nt(A,death_certificate,_)),
		   and(and(isa(NoPolicy,'ClaimCase'),
				   op(=,
					  R2,
					  json{ id:"NoPolicy",
							message:"NoPolicy"
						  })),
			   nt(A,policy,_))),
		op(=,
		   R,
		   json{ id:"NeedsService",
				 message:"NeedsService",
				 reason:RInner
			   })),

    NoDeathCert == Subject,
    NoPolicy == Subject,
    R == Reason,
    RInner = R1,
    RInner = R2.

test(compile_names_dont_match,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    open_descriptor(Desc, Db),
    database_schema(Db, Schema),

    get_restriction_named("NamesDontMatch", Schema, Named),
    compile_restriction(Named, Schema, Subject, Expression, Reason),

    Expression = and(isa(A,'ClaimCase'),
					 and(and(and(and(and(t(A,
									       death_certificate,
									       B),
									     t(A,policy,C)),
								     t(B,name,D)),
								 t(C,life_assured_name,E)),
							 op(\=,D,E)),
						 op(=,
							R,
							json{ id:"NamesDontMatch",
								  message:"NamesDontMatch"
								}))),
    A == Subject,
    R == Reason.

test(interpret_names_dont_match,[]) :-

    Term = and(op(\=, "Duck", "Goat"),
               op(=,Reason,"Foo")),
    interpret_restriction(Term, fake_db),
    Reason == "Foo".

test(interpret_restrictions,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    insert_insurance_documents(Desc),

    open_descriptor(Desc, Db),

    Restriction_Name = "NeedsService",
    findall(Id-Reason,
            ids_for_restriction(Db, Restriction_Name, Id, Reason),
            All),

    All = [ 1 - "[\n  {\n    \"id\":\"NeedsService\",\n    \"message\":\"NeedsService\",\n    \"reason\": {\"id\":\"NoPolicy\", \"message\":\"NoPolicy\"}\n  }\n]",
            2 - "[\n  {\n    \"id\":\"NeedsService\",\n    \"message\":\"NeedsService\",\n    \"reason\": {\n      \"id\":\"NoDeathCert\",\n      \"message\":\"There was no death certificate in this claims case\"\n    }\n  },\n  {\n    \"id\":\"NeedsService\",\n    \"message\":\"NeedsService\",\n    \"reason\": {\"id\":\"NoPolicy\", \"message\":\"NoPolicy\"}\n  }\n]"
          ].


:- end_tests(restrictions).

