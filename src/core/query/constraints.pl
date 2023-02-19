:- module(constraints,
          [
              select/5,
              step/5,
              run/2,
              run/3,
              run_report/1,
              my_constraint1/1
          ]).

:- use_module(core(util)).

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

select(or(A,B),Remaining,Stack,Selected,Polarity) :-
    negate(Polarity,Negated),
    select(A,[or1(B)|Remaining], Stack, Selected, Negated).
select(and(A,B),Remaining,Stack,Selected,Polarity) :-
    select(A,[and1(B)|Remaining], Stack, Selected, Polarity).
select(impl(Ante,Con), Remaining, Stack, Selected, Polarity) :-
    negate(Polarity,Negated),
    select(Ante, [impl1(Con)|Remaining], Stack, Selected, Negated).
select(t(X,E,Y), Remaining, Remaining, t(X,E,Y)-Polarity, Polarity).
select(op(Op,X,Y), Remaining, Remaining, op(Op,X,Y)-Polarity, Polarity).
select(isa(X,T), Remaining, Remaining, isa(X,T)-Polarity, Polarity).

% plug the one hole context, and find the next Clause and Stack
step([and1(B)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    select(B,[and2(Result)|T], Next_Stack, Next_Clause, Polarity).
step([and2(A)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    step(T, and(A,Result), Next_Stack, Next_Clause, Polarity).
step([or1(B)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    (   step(T, or(Result,B), Next_Stack, Next_Clause, Polarity)
    ->  true
    ;   select(B, [or2(Result)], Next_Stack, Next_Clause, Polarity)
    ).
step([and2(A)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    step(T, and(A,Result), Next_Stack, Next_Clause, Polarity).
step([impl1(Consequent)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    select(Consequent,[impl2(Result)|T], Next_Stack, Next_Clause, Polarity).
step([impl2(Antecedent)|T], Result, Next_Stack, Next_Clause, Polarity) :-
    step(T, impl(Antecedent, Result), Next_Stack, Next_Clause, Polarity).

run(Constraint, Failed_At) :-
    select(Constraint, [], Remaining, Clause, true),
    run(Clause, Remaining, Failed_At).

run_clause(t(X,P,Y), Db) :- database_instance(Db, G), xrdf(X,P,Y,G).
run_clause(op(Op,X,Y), _) :- call(Op,X,Y).
run_clause(isa(X,T), Db) :- run_clause(t(X,rdf:type,T),Db).

run(Clause-false, Remaining, Db, Failed_At) :-
    (   run_clause(Clause, Db)
    *-> step(Remaining,Clause,Next_Stack, Next_Clause, true),
        run(Next_Clause, Next_Stack, Db, Failed_At)
    ;   step(Remaining,Clause,Next_Stack, Next_Clause, true),
        run(Next_Clause, Next_Stack, Db, Failed_At)
    ).
run(Clause-true, Remaining, Db, failed_at(Clause,Remaining)) :-
    \+ run_clause(Clause, Db).
run(Clause-true, Remaining, Failed_At) :-
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

report_failure(failed_at(Clause,Remaining)) :-
    context_hole_term(Remaining,Clause,Term),
    print_term(Remaining, []),
    print_term(Term, []),
    !,
    render_constraint(Clause,Clause_String),
    render_constraint(Term,Term_String),
    format(user_output,"Failed to satisfy: ~w~n~n    In the Constraint:~n~n~w~n",
           [Clause_String, Term_String]).

run_report(Constraint) :-
    (   run(Constraint, Failed_At)
    ->  report_failure(Failed_At)
    ;   format(user_output, "Successfully satisfied constraint", [])
    ).

or_test(or(op(>,10,12),
           op(<,10,12))).

or_test2(or(op(>,10,100),
            op(>,10,30))).

or_impl_test(impl(isa(Customer, 'Customer'),
                  and(t(Customer, age, Age),
                      or(op(>, Age, 30),
                         op(<, Age, 10))))).

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
my_constraint1(impl(and(isa(Policy,'Policy'),
                        and(t(Policy,insurance_product,Product),
                            isa(Product,'MidLifeInsurance'))),
                    and(t(Policy,customer,Customer),
                        and(t(Customer,age,Age),
                            and(op(<,Age,70),
                                op(>,Age,35)))))).


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

my_constraint2(impl(and(isa(Policy1,'Policy'),
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
                                           op(<,Start_Date2, End_Date1))))))))).

% Some folds here with or...  Let's think about it in a minute
/*
step([or(Before,[Next_Constraint|After])|T], Result, Next_Stack, Next_Clause) :-
    select(Next_Constraint,[or([Result|Before],After)|T], Next_Stack, Next_Clause).
*/

render_constraint(Constraint,String) :-
    render_constraint(Constraint, String, 0).

render_constraint(isa(X,T), String, Indent) :-
    pad('',' ',Indent,Pad),
    format(string(String), "~q:~q~n~s", [X,T,Pad]).
render_constraint(op(Op,B,C), String, Indent) :-
    pad('',' ',Indent,Pad),
    format(string(String),
           "~q ~s ~q~n~s", [B,Op,C,Pad]).
render_constraint(t(A,B,C), String, Indent) :-
    pad('',' ',Indent,Pad),
    format(string(String),
           "~q =[~q]> ~q~n~s", [A,B,C,Pad]).
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

