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
:- use_module(core(query/jsonld)).

:- use_module(library(terminus_store)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module(library(solution_sequences)).

is_enum(A,_) :-
    atom(A).

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
interpret_restriction(op(Op,A,B),Db) :-
    is_enum(A,Db),
    !,
    Db = database(Schema, _),
    database_schema_prefixes(Schema,Prefixes),
    prefix_expand(B, Prefixes, BEx),
    effective_operator(Op, Operator),
    call(Operator, A, BEx).
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
interpret_restriction(not(P), DB) :-
    \+ interpret_restriction(P, DB).

compile_any_of(Dictionary, Schema, Subject, Expression, Reason) :-
    (   get_dict('@anyOf', Dictionary, Restrictions)
    ->  maplist({Schema, Subject, R}/[Restriction_Name, Expression]>>(
                    get_restriction_named(Restriction_Name, Schema, Restriction),
                    compile_restriction(Restriction, Schema, Subject, Expression, R)
                ),
                Restrictions,
                Expressions),
        termlist_disjunction(Expressions, Expression),
        Reason = json{ reason: R }
    ;   Expression = true,
        Reason = json{}
    ).

compile_none_of(Dictionary, Schema, Subject, Expression, Reasons) :-
    (   get_dict('@noneOf', Dictionary, Restrictions)
    ->  maplist({Schema, Subject}/[Restriction_Name, not(Expression)]>>(
                get_restriction_named(Restriction_Name, Schema, Restriction),
                compile_restriction(Restriction, Schema, Subject, Expression, _)
            ),
            Restrictions,
            Expressions),
        termlist_conjunction(Expressions, Expression),
        Reasons = json{ none_of: Restrictions }
    ;   Expression = true,
        Reasons = json{}
    ).

compile_has(Dictionary, Subject, Expression, Bindings) :-
    (   get_dict('@has', Dictionary, Restriction)
    ->  compile_constraint_rule(Restriction, Subject, Expression, Bindings, _)
    ;   Expression = true
    ).

compile_given(Dictionary, Subject, Expression, Bindings) :-
    (   get_dict('@given', Dictionary, Restrictions)
    ->  mapm({Subject}/[Restriction, Exp, BIn, BOut]>>(
                 compile_constraint_rule(Restriction,Subject,Exp, BIn, BOut)
             ),
             Restrictions,
             Expressions,
             _{},
             Bindings),
        termlist_conjunction(Expressions, Expression)
    ;   Expression = true,
        Bindings = _{}
    ).

restriction_message(Dictionary, Message) :-
    get_dict('_id', Dictionary, Id),
    (   get_dict('@comment', Dictionary, Message)
    ->  true
    ;   Id = Message
    ).

compile_restriction(Dictionary, Schema, Subject, Expression, Reason),
is_dict(Dictionary) =>
    get_dict('@on', Dictionary, Type),
    atom_string(Type_Atom, Type),
    get_dict('_id', Dictionary, Id),
    restriction_message(Dictionary, Message),
    compile_given(Dictionary, Subject, Given_Expression, Bindings),
    compile_has(Dictionary, Subject, Has_Expression, Bindings),
    compile_any_of(Dictionary, Schema, Subject, Any_Of_Expression, Reasons1),
    compile_none_of(Dictionary, Schema, Subject, None_Of_Expression, Reasons2),
    Reason_Dict0 = json{ id: Id, message: Message },
    put_dict(Reason_Dict0, Reasons1, Reason_Dict1),
    put_dict(Reason_Dict1, Reasons2, Reason_Dict2),
    Conjuncts = [isa(Subject, Type_Atom),
                 Given_Expression,
                 Has_Expression,
                 Any_Of_Expression,
                 None_Of_Expression,
                 op(=,Reason,
                    Reason_Dict2)],
    termlist_conjunction(Conjuncts, Expression).

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
    atom_json_dict(Reason, Reason_JSON, [as(string), width(0)]).
ids_for_restriction(Transaction, Restriction_Name, Id, Reason) :-
    database_instance(Transaction, Instance),
    database_schema(Transaction, Schema),
    Instance = [Instance_Obj],
    get_dict(read, Instance_Obj, Layer),
    ground(Layer),
    run_restriction_named(Schema, Instance, Restriction_Name, IRI, Reason_JSON),
    subject_id(Layer, IRI, Id),
    atom_json_dict(Reason, Reason_JSON, [as(string), width(0)]).


:- begin_tests(restrictions).
:- use_module(core(util/test_utils)).

insurance_schema('
{
    "@base": "terminusdb:///data/",
    "@metadata": {
        "restrictions": [
            {   "@on" : "ClaimCase",
                "_id" : "CountryOfDeath",
                "@comment" : "The Country of death is not the same as the country of issue",
                "_type" : "Restriction",
                "@given" : [{ "death_certificate_for_claim" :
                              { "country_of_death" : {"@var" : "CountryOfDeath"}},
                              "policy_of_claim" :
                              { "country_of_issue" : {"@var" : "CountryOfIssue"}}}],
                "@has" : { "@with" : "CountryOfDeath",
                           "@neq" : {"@var" : "CountryOfIssue"}}
            },
            {   "@on" : "ClaimCase",
                "_id" : "CauseOfDeathAssessed",
                "@comment" : "Cause is one of manslaughter, murder, accidental, suicide",
                "_type" : "Restriction",
                "@has" : { "death_certificate_for_claim" :
                              { "cause_of_death" :
                                { "@or" : [{ "@eq" : "@schema:CauseOfDeath/manslaughter" },
                                           { "@eq" : "@schema:CauseOfDeath/murder" },
                                           { "@eq" : "@schema:CauseOfDeath/accidental" },
                                           { "@eq" : "@schema:CauseOfDeath/suicide" }
                                          ]}}}
            },
            {
                "@anyOf": ["CauseOfDeathAssessed", "CountryOfDeath"],
                "@on": "ClaimCase",
                "@comment" : "Claim requires assessment",
                "_id": "ReferralForAssessment",
                "_type": "Restriction"
            },
            {
                "@given" : [
                    { "death_certificate_for_claim" : {
                        "date_of_death" : { "@var" : "DateOfDeath" }},
                      "policy_of_claim" : {
                        "moratorium_end" : { "@var" : "MoratoriumEnd" },
                        "moratorium_start" : { "@var" : "MoratoriumStart" }}}],
                "@has": { "@with" : "DateOfDeath",
                          "@ge": { "@var" : "MoratoriumStart" },
                          "@le": { "@var" : "MoratoriumEnd" }
                        },
                "@on": "ClaimCase",
                "@noneOf" : ["ReferralForAssessment","ReferralForService",
                             "ClaimClosed"],
                "@comment" : "Claim is eligible for a premium refund",
                "_id": "PremiumRefund",
                "_type": "Restriction"
            },
            {
                "@on": "ClaimCase",
                "@comment" : "Claim is eligible for a sum assured refund",
                "@noneOf" : ["ReferralForAssessment","ReferralForService",
                             "PremiumRefund",
                             "ClaimClosed"],
                "_id": "SumAssuredRefund",
                "_type": "Restriction"
            },
            {
                "@has" : { "incur" : { "@var" : "_"}},
                "@on": "ClaimCase",
                "_id": "ClaimClosed",
                "_type": "Restriction"
            },
            {
                "@has": {
                    "death_certificate_for_claim": null
                },
                "@on": "ClaimCase",
                "@comment" : "Claim had no associated death certificate",
                "_id": "NoDeathCert",
                "_type": "Restriction"
            },
            {
                "@has": {
                    "policy_of_claim": null
                },
                "@on": "ClaimCase",
                "@comment" : "Claim had no associated policy",
                "_id": "NoPolicy",
                "_type": "Restriction"
            },
            {
                "@given": [{"policy_of_claim":
                               { "start_date" : { "@var": "StartDate" }},
                               "death_certificate_for_claim":
                               { "date_of_death" : { "@var": "DateOfDeath" }}}],
                "@has": { "@with" : "StartDate",
                          "@le" : { "@var" : "DateOfDeath" }},
                "@on": "ClaimCase",
                "@comment" : "Date of death on certificate is prior to start date of policy.",
                "_id": "DeathAfterStart",
                "_type": "Restriction"
            },
            {
                "@has": {"policy_of_claim":
                            { "beneficiary" : { "flagged" : { "@eq" : true }}}},
                "@on": "ClaimCase",
                "@comment" : "The beneficiary is flagged.",
                "_id": "BeneficiaryIsFlagged",
                "_type": "Restriction"
            },
            {
                "@anyOf": [
                    "NoDeathCert",
                    "NoPolicy",
                    "NamesDontMatch",
                    "DateOfBirthDoesntMatch",
                    "DeathAfterStart",
                    "BeneficiaryIsFlagged"
                ],
                "@on": "ClaimCase",
                "@comment" : "Claim requires servicing",
                "_id": "ReferralForService",
                "_type": "Restriction"
            },
            {
                "@given" : [{
                              "death_certificate_for_claim": {
                                  "name": {
                                      "@var": "DeathCertName"
                                  }
                               },
                              "policy_of_claim": {
                                  "life_assured_name": {
                                      "@var": "PolicyName"
                                  }
                              }
                            }],
                "@has": {
                            "@ne": {
                                "@var": "PolicyName"
                            },
                            "@with": "DeathCertName"
                        },
                "@on": "ClaimCase",
                "@comment" : "The death certificate and policy names do not match",
                "_id": "NamesDontMatch",
                "_type": "Restriction"
            },
            {
                "@given" : [
                        {
                            "death_certificate_for_claim": {
                                "date_of_birth": {
                                    "@var": "DeathCertDateOfBirth"
                                }
                            },
                            "policy_of_claim": {
                                "life_assured_date_of_birth": {
                                    "@var": "PolicyDateOfBirth"
                                }
                            }
                        }],
                "@has": {
                            "@ne": {
                                "@var": "PolicyDateOfBirth"
                            },
                            "@with": "DeathCertDateOfBirth"
                        },
                "@on": "ClaimCase",
                "@comment" : "The death certificate and policy dates of birth do not match",
                "_id": "DateOfBirthDoesntMatch",
                "_type": "Restriction"
            }
        ]
    },
    "@schema": "terminusdb:///schema#",
    "@type": "@context"
}
{
    "@id": "Refund",
    "@type": "Enum",
    "@value": [
        "SumAssured",
        "Premium",
        "Denied"
    ]
}
{
    "@id": "ClaimCase",
    "@type": "Class",
    "death_certificate_for_claim": {
        "@class": "DeathCertificate",
        "@type": "Optional"
    },
    "incur": {
        "@class": "Refund",
        "@type": "Optional"
    },
    "policy_of_claim": {
        "@class": "Policy",
        "@type": "Optional"
    }
}
{
    "@id": "Beneficiary",
    "@type": "Class",
    "@unfoldable": [],
    "bank_account": "xsd:string",
    "date_of_birth": {
        "@class": "xsd:dateTime",
        "@type": "Optional"
    },
    "is_flagged": "xsd:boolean",
    "name": "xsd:string"
}
{
    "@id": "Policy",
    "@type": "Class",
    "@unfoldable": [],
    "beneficiary": "Beneficiary",
    "country_of_issue": "Country",
    "covered_countries": {
        "@class": "Country",
        "@type": "Set"
    },
    "life_assured_date_of_birth": {
        "@class": "xsd:dateTime",
        "@type": "Optional"
    },
    "start_date" : { "@type" : "Optional",
                     "@class" : "xsd:dateTime" },
    "life_assured_name": "xsd:string",
    "premium_paid_to_date": "xsd:double",
    "sum_assured": "xsd:double",
    "moratorium_start" : { "@type" : "Optional",
                           "@class" : "xsd:dateTime"},
    "moratorium_end" : { "@type" : "Optional",
                         "@class" : "xsd:dateTime" }
}
{
    "@id": "DeathCertificate",
    "@type": "Class",
    "@unfoldable": [],
    "country_of_death": "Country",
    "date_of_birth": "xsd:dateTime",
    "date_of_death": "xsd:dateTime",
    "name": "xsd:string",
    "cause_of_death" : { "@type" : "Optional",
                         "@class" : "CauseOfDeath" }
}
{
    "@id" : "CauseOfDeath",
    "@type" : "Enum",
    "@value" : ["natural",
                "manslaughter",
                "murder",
                "accidental",
                "suicide"]
}
{
    "@id": "Country",
    "@key": {
        "@fields": [
            "name"
        ],
        "@type": "Lexical"
    },
    "@type": "Class",
    "name": "xsd:string"
}
').

insurance_database(
    [
        json{ '@type' : "Country", name : "Germany" },
        json{ '@type' : "ClaimCase",
              '@id' : "ClaimCase/1" },
        json{ '@type' : "ClaimCase",
              '@id' : "ClaimCase/2",
              death_certificate_for_claim:
              json{ '@type' : "DeathCertificate",
                    name : "Jack",
                    country_of_death: "Country/Germany",
                    date_of_birth: "2012-01-01T00:00:00Z",
                    date_of_death: "2013-01-01T00:00:00Z"}},
        json{ '@type' : "ClaimCase",
              '@id' : "ClaimCase/3",
              policy_of_claim:
              json{ '@type' : "Policy",
                    beneficiary:
                    json{ '@type' : "Beneficiary",
                          bank_account: "B",
                          date_of_birth : "2000-01-01T00:00:00Z",
                          is_flagged: false,
                          name: "Spok" },
                    country_of_issue: "Country/Germany",
                    life_assured_date_of_birth: "2012-01-01T00:00:00Z",
                    start_date: "2020-01-01T00:00:00Z",
                    life_assured_name : "Cathrine",
                    premium_paid_to_date: "334.43",
                    sum_assured : "21.34",
                    moratorium_start: "2020-01-01T00:00:00Z",
                    moratorium_end: "2021-01-01T00:00:00Z"
                  }
            },
        json{ '@type' : "ClaimCase",
              '@id' : "ClaimCase/4",
              death_certificate_for_claim:
              json{ '@type' : "DeathCertificate",
                    name : "Joe",
                    cause_of_death: "murder",
                    country_of_death: "Country/Germany",
                    date_of_birth: "2012-01-01T00:00:00Z",
                    date_of_death: "2013-01-01T00:00:00Z"},
              policy_of_claim:
              json{ '@type' : "Policy",
                    beneficiary:
                    json{ '@type' : "Beneficiary",
                          bank_account: "B",
                          date_of_birth : "2000-01-01T00:00:00Z",
                          is_flagged: false,
                          name: "Kirk" },
                    country_of_issue: "Country/Germany",
                    life_assured_date_of_birth: "2012-01-01T00:00:00Z",
                    start_date: "2020-01-01T00:00:00Z",
                    life_assured_name : "Joe",
                    premium_paid_to_date: "334.43",
                    sum_assured : "21.34",
                    moratorium_start: "2012-01-01T00:00:00Z",
                    moratorium_end: "2021-01-01T00:00:00Z"
                  }
            }
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

    get_restriction_named("ReferralForService", Schema, Named),
    compile_restriction(Named, Schema, _Subject, Expression, Reason),

    Expression = and(_,
                     op(=,R,_)),
    R == Reason,

    Expression =@=
    and(and(isa(A,'ClaimCase'),
	        or(or(or(or(or(and(and(isa(A,'ClaimCase'),
			                       op(=,
				                      B,
				                      json{ id:"NoDeathCert",
					                        message:"Claim had no associated death certificate"
				                          })),
			                   nt(A,death_certificate_for_claim,_)),
		                   and(and(isa(A,'ClaimCase'),
			                       op(=,
				                      B,
				                      json{id:"NoPolicy",message:"Claim had no associated policy"})),
			                   nt(A,policy_of_claim,_))),
		                and(and(and(and(and(and(t(A,death_certificate_for_claim,C),
					                            t(A,policy_of_claim,D)),
					                        t(C,name,E)),
				                        t(D,life_assured_name,F)),
				                    isa(A,'ClaimCase')),
			                    op(=,
			                       B,
			                       json{ id:"NamesDontMatch",
				                         message:"The death certificate and policy names do not match"
				                       })),
			                op(\=,E,F))),
		             and(and(and(and(and(and(t(A,death_certificate_for_claim,G),
					                         t(A,policy_of_claim,H)),
				                         t(G,date_of_birth,I)),
				                     t(H,life_assured_date_of_birth,J)),
			                     isa(A,'ClaimCase')),
			                 op(=,
			                    B,
			                    json{ id:"DateOfBirthDoesntMatch",
				                      message:"The death certificate and policy dates of birth do not match"
				                    })),
		                 op(\=,I,J))),
	              and(and(and(and(and(and(t(A,death_certificate_for_claim,K),
				                          t(A,policy_of_claim,L)),
				                      t(K,date_of_death,M)),
			                      t(L,start_date,N)),
			                  isa(A,'ClaimCase')),
		                  op(=,
			                 B,
			                 json{ id:"DeathAfterStart",
			                       message:"Date of death on certificate is prior to start date of policy."
			                     })),
		              op(=<,N,M))),
	           and(and(and(and(and(t(A,policy_of_claim,O),
                                   t(O,beneficiary,P)),
                               t(P,flagged,Q)),
		                   isa(A,'ClaimCase')),
		               op(=,
		                  B,
		                  json{id:"BeneficiaryIsFlagged",message:"The beneficiary is flagged."})),
	               op(=,Q,true)))),
        op(=,_,json{id:"ReferralForService",message:"Claim requires servicing",reason:B})).

test(compile_names_dont_match,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    open_descriptor(Desc, Db),
    database_schema(Db, Schema),

    get_restriction_named("NamesDontMatch", Schema, Named),
    compile_restriction(Named, Schema, _Subject, Expression, _Reason),

    Expression =@=
    and(and(and(and(and(and(t(A,death_certificate_for_claim,B),
                            t(A,policy_of_claim,C)),
		                t(B,name,D)),
		            t(C,life_assured_name,E)),
	            isa(A,'ClaimCase')),
	        op(=,
	           _,
	           json{ id:"NamesDontMatch",
		             message:"The death certificate and policy names do not match"
	               })),
        op(\=,D,E)).

test(interpret_names_dont_match,[]) :-

    Term = and(op(\=, "Duck", "Goat"),
               op(=,Reason,"Foo")),
    interpret_restriction(Term, fake_db),
    Reason == "Foo".

test(referral_for_service,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    insert_insurance_documents(Desc),

    open_descriptor(Desc, Db),

    Restriction_Name = "ReferralForService",
    findall(Id-Reason,
            ids_for_restriction(Db, Restriction_Name, Id, Reason),
            All),
    All = [
        3 - "[ {\"id\":\"ReferralForService\", \"message\":\"Claim requires servicing\", \"reason\": {\"id\":\"NoDeathCert\", \"message\":\"Claim had no associated death certificate\"}},  {\"id\":\"ReferralForService\", \"message\":\"Claim requires servicing\", \"reason\": {\"id\":\"NoPolicy\", \"message\":\"Claim had no associated policy\"}} ]",
		4 - "[ {\"id\":\"ReferralForService\", \"message\":\"Claim requires servicing\", \"reason\": {\"id\":\"NoPolicy\", \"message\":\"Claim had no associated policy\"}} ]",
		5 - "[ {\"id\":\"ReferralForService\", \"message\":\"Claim requires servicing\", \"reason\": {\"id\":\"NoDeathCert\", \"message\":\"Claim had no associated death certificate\"}} ]"
	].

test(referral_for_assessment,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    insert_insurance_documents(Desc),

    open_descriptor(Desc, Db),

    Restriction_Name = "ReferralForAssessment",
    findall(Id-Reason,
            ids_for_restriction(Db, Restriction_Name, Id, Reason),
            All),
    All = [
        6 - "[ {\"id\":\"ReferralForAssessment\", \"message\":\"Claim requires assessment\", \"reason\": {\"id\":\"CauseOfDeathAssessed\", \"message\":\"Cause is one of manslaughter, murder, accidental, suicide\"}} ]"
	].

test(cause_of_death,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]) :-

    insert_insurance_documents(Desc),

    open_descriptor(Desc, Db),

    Restriction_Name = "CauseOfDeathAssessed",
    findall(Id-Reason,
            ids_for_restriction(Db, Restriction_Name, Id, Reason),
            All),
    All = [
        6 - "[ {\"id\":\"CauseOfDeathAssessed\", \"message\":\"Cause is one of manslaughter, murder, accidental, suicide\"} ]"

    ].


:- end_tests(restrictions).

