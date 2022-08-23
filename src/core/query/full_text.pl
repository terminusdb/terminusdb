:- module(full_text,[tf_idf/4]).

:- use_module(core(query)).
:- use_module(library(pcre)).
:- use_module(core(util)).

tokenize_query(Query,Tokens) :-
    re_foldl([Dict,List,[Res|List]]>>
             get_dict(1,Dict,Res),
             '[\\s\\W]*(\\p{Xan}+)[\\s\\W]*',
             Query, [], Reverse, []),
    reverse(Reverse,Tokens).

bigramize([],[]).
bigramize([_],[]).
bigramize([Term,Next|Rest],[Term-Next|Bigrams]) :-
    bigramize(Rest,Bigrams).

run_text_query(Askable, Query, Results) :-
    tokenize_query(Query, Tokens),
    bigramize(Tokens,Pairs),
    maplist([First-Second,Result]>>(
                atomic_list_concat([First,' ',Second],Atom),
                atom_string(Atom,Result)
            ),
            Pairs,Bigrams),
    append(Tokens,Bigrams,List),
    clumped(List,Bag),
    scored_documents(Askable, Bag, Results).

scored_documents(Askable, Bag, Results) :-
    findall(
        DocumentId-Scores,
        group_by(
            DocumentId,
            Score,
            (   member(Term-Count,Bag),
                tf_idf(Askable,Term,DocumentId,TF_IDF),
                Score is Count * TF_IDF),
            Scores),
        DocumentId_Scores),
    maplist([Doc-Scores,Doc-Final]>>sum_list(Scores,Final), DocumentId_Scores, Pre_Sort),
    sort(2,@>=, Pre_Sort, Results).

show_top_ten(Askable, Query) :-
    run_text_query(Askable, Query, Results),
    length(L,10),
    append(L,_,Results),
    index_list(L,Idx),
    maplist({Askable}/[Doc-L,I]>>(
                ask(Askable,
                    t(Doc,text, Text^^xsd:string)),
                N is I + 1,
                format(current_output, '~d. ~w~n~n', [N,Text])),
            L,Idx).

%% tf-idf(t, d) = tf(t, d) * log(N/(df + 1))
tf_idf(Askable,Term,DocumentId,TF_IDF) :-
    (   ask(Askable,
            (   t(TermId,term,Term^^xsd:string),
                t(TermId,documents,DocTF_IDF),
                t(DocTF_IDF,document,DocumentId),
                t(DocTF_IDF,tf_idf,TF_IDF^^xsd:decimal)
            ))
    *->  true
    ;   \+ var(DocumentId),
        TF_IDF = 0).
