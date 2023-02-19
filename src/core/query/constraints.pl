:- module(constraints,
          [
              run/3,
              run/4,
              run_report/3,
              check_constraint_document/3
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document)).

/*

Term language for constraints
—————————————————————————————
Op ∈ Literal comparitors
V ∈ Literal Values
L,M,N ∈ Literal Variables
O ∈ Literal or Variable value := L | V
G ∈ Node
X,Y,Z ∈ Node Variables
W ∈ Node or Variable value := X | G
S,T ∈ Types
P,Q,R ∈ Field names
C,D,E ∈ Constraint := comp(Op,X,O)
                     | t(X,P,Y)
                     | and(C,D)
                     | or(C,D)
                     | not(C)
                     | impl(C,D)
                     | isa(X,T)


The term representing the "MidLifeInsurance" constraint is:

impl(and(isa(X,'Policy'),
         t(X,insurance_product,Y),
         isa(Y,'MidLifeInsurance')),
     and(t(X,customer,C),
         t(C,age,Z),
         Z < 70,
         Z > 35))

For each term, we also have its one hole contexts, which allows us to
do clause selection, and to have a partially completed context.

*/

negate(true,false).
negate(false,true).

inverse_op(<,>=).
inverse_op(>,<=).
inverse_op(>=,<).
inverse_op(=<,>).

select_redex(or(A,B),Remaining,Stack,Selected,Polarity) =>
    negate(Polarity,Negated),
    select_redex(A,[or1(B)|Remaining], Stack, Selected, Negated).
select_redex(and(A,B),Remaining,Stack,Selected,Polarity) =>
    select_redex(A,[and1(B)|Remaining], Stack, Selected, Polarity).
select_redex(impl(Ante,Con), Remaining, Stack, Selected, Polarity) =>
    negate(Polarity,Negated),
    select_redex(Ante, [impl1(Con)|Remaining], Stack, Selected, Negated).
select_redex(t(X,E,Y), Remaining, Stack, Selected, Polarity) =>
    Selected = t(X,E,Y)-Polarity,
    Stack = Remaining.
select_redex(op(Op,X,Y), Remaining, Stack, Selected, Polarity) =>
    Selected = op(Op,X,Y)-Polarity,
    Stack = Remaining.
select_redex(isa(X,T), Remaining, Stack, Selected, Polarity) =>
    Selected = isa(X,T)-Polarity,
    Stack = Remaining.

% plug the one hole context, and find the next Clause and Stack
step([and1(B)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    select_redex(B,[and2(Result)|T], Next_Stack, Next_Clause, Polarity).
step([and2(A)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    step(T, and(A,Result), Next_Stack, Next_Clause, Polarity).
step([or1(B)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    (   step(T, or(Result,B), Next_Stack, Next_Clause, Polarity)
    *-> true
    ;   negate(Polarity,Negated),
        select_redex(B, [or2(Result)|T], Next_Stack, Next_Clause, Negated)
    ).
step([and2(A)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    step(T, and(A,Result), Next_Stack, Next_Clause, Polarity).
step([impl1(Consequent)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    select_redex(Consequent,[impl2(Result)|T], Next_Stack, Next_Clause, Polarity).
step([impl2(Antecedent)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    step(T, impl(Antecedent, Result), Next_Stack, Next_Clause, Polarity).

run(Db, Constraint, Failed_At) :-
    select_redex(Constraint, [], Remaining, Clause, true),
    run(Clause, Remaining, Db, Failed_At).

raw(X^^_, X) :-
    !.
raw(X,X).

run_clause(t(X,P,Y), Db) :-
    database_prefixes(Db, Prefixes),
    prefix_expand_schema(P, Prefixes, PEx),
    database_instance(Db, G),
    xrdf(G,X,PEx,Y).
run_clause(op(Op,X,Y), _) :-
    raw(X,XRaw),
    raw(Y,YRaw),
    call(Op,XRaw,YRaw).
run_clause(isa(X,T), Db) :-
    database_prefixes(Db, Prefixes),
    prefix_expand_schema(T, Prefixes, TEx),
    global_prefix_expand(rdf:type, RDF_Type),
    run_clause(t(X,RDF_Type,TEx),Db).

run(Clause-false, Remaining, Db, Failed_At) :-
    (   run_clause(Clause, Db)
    *-> step(Remaining,Clause,Next_Stack, Next_Clause, false),
        run(Next_Clause, Next_Stack, Db, Failed_At)
    ;   step(Remaining,Clause,Next_Stack, Next_Clause, false),
        run(Next_Clause, Next_Stack, Db, Failed_At)
    ).
run(Clause-true, Remaining, Db, failed_at(Clause,Remaining)) :-
    \+ run_clause(Clause, Db).
run(Clause-true, Remaining, Db, Failed_At) :-
    run_clause(Clause, Db),
    step(Remaining,Clause,Next_Stack,Next_Clause, true),
    run(Next_Clause, Next_Stack, Db, Failed_At).

context_hole_term([],Term,Term).
context_hole_term([and1(B)|T],A,Term) :-
    context_hole_term(T,and(A,B),Term).
context_hole_term([and2(A)|T],B,Term) :-
    context_hole_term(T,and(A,B),Term).
context_hole_term([or1(B)|T],A,Term) :-
    context_hole_term(T,or(A,B),Term).
context_hole_term([or2(A)|T],B,Term) :-
    context_hole_term(T,or(A,B),Term).
context_hole_term([impl2(A)|T],B,Term) :-
    context_hole_term(T,impl(A,B),Term).

failure_report(failed_at(Clause,Remaining),Report) :-
    context_hole_term(Remaining,Clause,Term),
    !,
    render_constraint(Clause,Clause_String),
    render_constraint(Term,Term_String),
    format(string(Report),"Failed to satisfy: ~w~n~n    In the Constraint:~n~n~w~n",
           [Clause_String, Term_String]).

run_report(Db, Constraint, Report) :-
    (   run(Db, Constraint, Failed_At)
    ->  failure_report(Failed_At, Report)
    ).

% Some folds here with or...  Let's think about it in a minute
/*
step([or(Before,[Next_Constraint|After])|T], Result, Next_Stack, Next_Clause) :-
    select_redex(Next_Constraint,[or([Result|Before],After)|T], Next_Stack, Next_Clause).
*/

render_constraint(Constraint,String) :-
    render_constraint(Constraint, String, 0).

render_constraint(isa(X,T), String, Indent) :-
    pad('',' ',Indent,Pad),
    format(string(String), "~q:~q~n~s", [X,T,Pad]).
render_constraint(op(Op,B,C), String, Indent) :-
    raw(B,BRaw),
    raw(C,CRaw),
    pad('',' ',Indent,Pad),
    format(string(String),
           "~q ~s ~q~n~s", [BRaw,Op,CRaw,Pad]).
render_constraint(t(A,B,C), String, Indent) :-
    raw(C,CRaw),
    pad('',' ',Indent,Pad),
    format(string(String),
           "~q =[~q]> ~q~n~s", [A,B,CRaw,Pad]).
render_constraint(or(A,B), String, Indent) :-
    render_constraint(A, StringA, Indent),
    render_constraint(B, StringB, Indent),
    format(string(String),
           "(~s) ∨ (~s)", [StringA,StringB]).
render_constraint(and(A,B), String, Indent) :-
    render_constraint(A, StringA, Indent),
    render_constraint(B, StringB, Indent),
    format(string(String),
           "~s ∧ ~s", [StringA,StringB]).
render_constraint(impl(A,B), String, Indent) :-
    Indent_First is Indent + 2,
    render_constraint(A, StringA, Indent_First),
    Indent_Next is Indent + 4,
    render_constraint(B, StringB, Indent_Next),
    pad('',' ',Indent_Next,Pad),
    format(string(String),
           "( ~s ) ⇒~n~s~s", [StringA,Pad,StringB]).

check_constraint_document(_,_,_) :-
    fail.

:- begin_tests(constraints).

:- use_module(core(util/test_utils)).

test(or_test1, [fail]) :-
    % Success is failure
    Or_Test = or(op(>,10,12),
                 op(<,10,12)),
    run(fake_db, Or_Test, _Failed_At).

test(or_test2, []) :-
    Or_Test = or(op(>,10,100),
                 op(>,10,30)),
    run(fake_db, Or_Test, Failed_At),

    Failed_At = failed_at(Op,Ctx),
    Op = op(>,10,30),
    Ctx = [or2(op(>,10,100))],

    context_hole_term(Ctx, Op, Term),
    Term = or(op(>,10,100),op(>,10,30)).

insurance_schema('
{ "@type" : "@context",
  "@base" : "iri://insurance/",
  "@schema" : "iri://insurance#",
  "@metadata" :
  { "constraints" : [
      { "@name" : "MidLifeInsurance",
        "@rules" : [{ "@when" : { "@isa" : "Policy",
                                  "insurance_product" : { "@isa" : "MidLifeInsurance" }},
                      "@then" : { "customer" : { "age" : { "@and" : [{ "@gt" : 30 },
                                                                     { "@lt" : 60 }]}}}}]
      }
    ]
  }
}
{ "@type" : "Class",
  "@id" : "Customer",
  "@key" : { "@type" : "Lexical", "@fields" : ["forename", "surname", "id"]},
  "forename" : "xsd:string",
  "surname" : "xsd:string",
  "id" : "xsd:long",
  "age" : "xsd:nonNegativeInteger"
}
{ "@type" : "Class",
  "@id" : "InsuranceProduct",
  "@abstract" : [],
  "name" : "xsd:string"
}
{ "@type" : "Class",
  "@id" : "MidLifeInsurance",
  "@inherits" : "InsuranceProduct",
  "@key" : { "@type" : "Lexical", "@fields" : ["name"]}
}
{ "@type" : "Class",
  "@id" : "Policy",
  "customer" : "Customer",
  "insurance_product" : "InsuranceProduct",
  "start_date" : "xsd:dateTime",
  "end_date" : "xsd:dateTime"
}
').

insurance_database(
    [
        json{ '@type' : "MidLifeInsurance",
              '@capture' : "Product/midlife",
              name : "Mid-Life Insurance Product" },
        json{ '@type' : "Customer",
              '@capture' : "Customer/jim",
              forename : "Jim",
              surname : "McCoy",
              id : 1,
              age : 40
            },
        json{ '@type' : "Customer",
              '@capture' : "Customer/jill",
              forename : "Jill",
              surname : "Curry",
              id : 2,
              age : 12
            },
        json{ '@type' : "Policy",
              insurance_product : json{ '@ref' : "Product/midlife" },
              customer : json{ '@ref' : "Customer/jim" },
              start_date : "2020-01-01T00:00:00Z",
              end_date : "2050-01-01T00:00:00Z"
            },
        json{ '@type' : "Policy",
              insurance_product : json{ '@ref' : "Product/midlife" },
              customer : json{ '@ref' : "Customer/jim" },
              start_date : "2030-01-01T00:00:00Z",
              end_date : "2060-01-01T00:00:00Z"
            },
        json{ '@type' : "Policy",
              insurance_product : json{ '@ref' : "Product/midlife" },
              customer : json{ '@ref' : "Customer/jill" },
              start_date : "2030-01-01T00:00:00Z",
              end_date : "2060-01-01T00:00:00Z"
            }
    ]
).

test(or_impl_test,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    insurance_database(Documents),

    with_test_transaction(
        Desc,
        C1,
        forall(member(Doc, Documents),
                   insert_document(C1, Doc, _))
    ),

    open_descriptor(Desc, Db),

    Or_Impl = impl(isa(Customer, 'Customer'),
                   and(t(Customer, age, Age),
                       or(op(>, Age, 30),
                          op(<, Age, 10)))),
    run(Db, Or_Impl, Failed_At),
    !,
    Failed_At = failed_at(Op, Ctx),
    Op = op(>,12 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',30),
    Ctx = [ or1(op(<,
				   12 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
				   10)),
			and2(t('iri://insurance/Customer/Jill+Curry+2',
				   age,
				   12 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger')),
			impl2(isa('iri://insurance/Customer/Jill+Curry+2',
					  'Customer'))
		  ],

    context_hole_term(Ctx,Op,Term),
    !,
    Term = impl(isa('iri://insurance/Customer/Jill+Curry+2',
					'Customer'),
				and(t('iri://insurance/Customer/Jill+Curry+2',
					  age,
					  12 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
					or(op(>,
						  12 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
						  30),
					   op(<,
						  12 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
						  10)))),

    failure_report(Failed_At, String),
    String = "Failed to satisfy: 12 > 30\n\n\n    In the Constraint:\n\n( 'iri://insurance/Customer/Jill+Curry+2':'Customer'\n   ) ⇒\n    'iri://insurance/Customer/Jill+Curry+2' =[age]> 12\n     ∧ (12 > 30\n    ) ∨ (12 < 10\n    )\n".


test(midlife_insurance,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(insurance_schema, Desc))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

/*

Example constraint1:

{ "@type": "Constraint",
  "@id": "MidLifeInsuranceAge",
  "@doc": "People with MidLifeInsurance have to be older than 35, and younger than 70",
  "@on": "Policy",
  "@constraints": [{"insurance_product": {"@type": "MidLifeInsurance"}},
                   {"customer": {"age": [{"@lt": 70},
                                         {"@gt": 35}]}}],
}

*/
    Midlife = impl(and(isa(Policy,'Policy'),
                       and(t(Policy,insurance_product,Product),
                           isa(Product,'MidLifeInsurance'))),
                   and(t(Policy,customer,Customer),
                       and(t(Customer,age,Age),
                           and(op(<,Age,70),
                               op(>,Age,35))))).


test(insurance_no_overlap, []) :-
/*
Example constraint2:

Example constraint:

{ "@type": "Constraint",
  "@id": "InsuranceNoOverlap",
  "@doc": "People with an insurance policy can't have that same product in an overlapping duration",
  "@on": "Policy",
  "@constraints": [{"@id": {"@var": "policy_id"}},
                   {"@not": [{"insurance_product": {"@type": {"@var": "product_type_1"}}},
                             {"customer": {"@linked-by": {"@type": "Policy", "@property": "customer},
                                           "@id": {"@var": "policy_id_2"}}},
                             {"@ne": [{"@var": "policy_id"},
                                      {"@var": "policy_id_2"}]},
                             {"@match": {"@id": {"@var": "policy_id_2"},
                                         "insurance_product": {"@var": "product_type_1"}}},
                             {"@match": {"@id": {"@var": "policy_id"},
                                         "start_date": {"@var": "policy_1_start_date"},
                                         "end_date": {"@var": "policy_1_start_date"}}},
                             {"@match": {"@id": {"@var": "policy_id_2"},
                                         "start_date": {"@var": "policy_2_start_date"},
                                         "end_date": {"@var": "policy_2_start_date"}}},
                             {"@or": [{"@between": {"@from": {"@var": "policy_1_start_date"},
                                                    "@to": {"@var": "policy_1_end_date"},
                                                    "@element: {"@var": "policy_2_start_date"}}},
                                      {"@between": {"@from": {"@var": "policy_1_start_date"},
                                                    "@to": {"@var": "policy_1_end_date"},
                                                    "@element: {"@var": "policy_2_end_date"}}}]}]}]
*/

    No_Overlap = impl(and(isa(Policy1,'Policy'),
                          and(t(Policy1,insurance_product,Product),
                              and(t(Policy1,customer,Customer),
                                  and(t(Policy2,customer,Customer),
                                      and(op(\=,Policy1,Policy2),
                                          t(Policy2, insurance_product, Product)))))),
                      and(t(Policy1,start_date,Start_Date1),
                          and(t(Policy2,start_date,Start_Date2),
                              and(t(Policy1,end_date,End_Date1),
                                  and(t(Policy2, end_date, End_Date2),
                                      or(and(op(<,Start_Date1, End_Date2),
                                             op(<,End_Date2, End_Date1)),
                                         and(op(<,Start_Date1, Start_Date2),
                                             op(<,Start_Date2, End_Date1)))))))).

:- end_tests(constraints).
