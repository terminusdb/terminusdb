:- module(json_clean_slate, []).
:- use_module(core(query)).

json_data_prefix('terminusdb:///json/').

json_type_rdf_type(X,R),
string(X) =>
    global_prefix_expand(xsd:string, T),
    R = X^^T.
json_type_rdf_type(X,R),
number(X) =>
    global_prefix_expand(xsd:decimal, T),
    R = X^^T.
json_type_rdf_type(X,R),
atom(X),
memberchk(X,[true,false]) =>
    global_prefix_expand(xsd:boolean, T),
    R = X^^T.
json_type_rdf_type(X,R),
atom(X),
X = null =>
    global_prefix_expand(xsd:token, T),
    R = X^^T.


:- if(false).
json_hash_init("").

json_hash_expand(Context_In, Val, Hash_Out, Context_Out),
string(Val) =>
    format(string(Hash_Out), "~s~s", [Context_In,Val]),
    Context_Out = Hash_Out.

json_hash_extract(Context, Hash) :-
    Hash = Context.

:- else.
json_hash_init(Ctx) :-
    sha_new_ctx(Ctx, []).

json_hash_expand(Context_In, Val, Hash_Out, Context_Out),
string(Val) =>
    sha_hash_ctx(Context_In, Val, Context_Out, Hash_List),
    hash_atom(Hash_List, Hash_Out_Atom),
    atom_string(Hash_Out_Atom, Hash_Out).

json_hash_extract(Context, Hash) :-
    sha_hash_ctx(Context, "", _, Hash_List),
    hash_atom(Hash_List, Hash_Atom),
    atom_string(Hash_Atom, Hash).
:- endif.

json_hash_init(Val, Context) :-
    json_hash_init(Context_1),
    json_hash_expand(Context_1, Val, _, Context).

json_hash(Val, Hash) :-
    json_hash_init(Val, Hash).

json_document_triple(Dict, Id, Triple),
is_dict(Dict) =>
    global_prefix_expand(sys:'JSONDocument', Sys_JSON_Document),
    global_prefix_expand(rdf:type, Rdf_Type),

    (   Triple = t(Id, Rdf_Type, Sys_JSON_Document)
    ;   dict_pairs(Dict, _, Pairs),
        member(Property-Value, Pairs),
        uri_encoded(segment, Property, Encoded_Property),
        global_prefix_expand(json:Encoded_Property, Expanded_Property),

        json_subdocument_triple(Value, X),
        (   X = t(_,_,_)
        ->  Triple = X
        ;   X = hash(_Inner_Hash, Link),
            Triple = t(Id, Expanded_Property, Link))).

json_subdocument_triple(Dict, Triple_Or_Hash),
is_dict(Dict) =>
    json_hash_init("Dict(", Init_Hash),
    State = state(Init_Hash,[]),
    dict_pairs(Dict, _, Pairs),
    (   member(Property-Value, Pairs),
        json_subdocument_triple(Value, X),
        (   X = t(_,_,_)
        ->  Triple_Or_Hash = X
        ;   X = hash(Inner_Hash, Link),
            State = state(Context_In,Members_In),
            uri_encoded(segment, Property, Encoded_Property),
            global_prefix_expand(json:Encoded_Property, Expanded_Property),
            atom_string(Encoded_Property, Encoded_Property_String),
            term_string(Encoded_Property_String, Encoded_Property_String_Quoted),
            json_hash_expand(Context_In, Encoded_Property_String_Quoted, _, Context_Out1),
            json_hash_expand(Context_Out1, "-", _, Context_Out2),
            json_hash_expand(Context_Out2, Inner_Hash, _, Context_Out),
            Members_Out = [Expanded_Property-Link|Members_In],
            nb_setarg(1, State, Context_Out),
            nb_setarg(2, State, Members_Out),
            fail)
    ;   State = state(Context, Members),
        json_hash_expand(Context, ")", Hash, _),
        json_data_prefix(Data_Prefix),
        format(atom(Node), "~sJSON/SHA1/~s", [Data_Prefix, Hash]),
        (   global_prefix_expand(rdf:type, Rdf_Type),
            global_prefix_expand(sys:'JSON', Json_Type),
            Triple_Or_Hash = t(Node, Rdf_Type, Json_Type)
        ;   member(Property-Link, Members),
            Triple_Or_Hash = t(Node, Property, Link)
        ;   Triple_Or_Hash = hash(Hash, Node))).
json_subdocument_triple(List, Triple_Or_Hash),
is_list(List) =>
    reverse(List, Rev),
    json_hash_init("List(", Hash_In),
    global_prefix_expand(rdf:nil, Rdf_Nil),
    Hash = hash(Hash_In, Rdf_Nil),
    json_list_triple(Rev, Hash, Triple_Or_Hash).
json_subdocument_triple(Val, Triple_Or_Hash) =>
    json_hash_init("val(", Context),
    format(string(Val_Quoted), "~q", [Val]),
    json_hash_expand(Context, Val_Quoted, _, Context2),
    json_hash_expand(Context2, ")", Hash, _),
    json_type_rdf_type(Val, Rdf_Val),
    Triple_Or_Hash = hash(Hash, Rdf_Val).

json_list_triple(Nil, Hash, Hash2),
Nil == [] =>
    Hash2 = Hash.
json_list_triple([First|Rest], hash(Context_In, Node_In), Triple_Or_Hash) =>
    json_subdocument_triple(First, X),
    (   X = t(_,_,_)
    ->  Triple_Or_Hash = X
    ;   X = hash(Document_Hash, Document_Node),
        json_hash_expand(Context_In, Document_Hash, _, Context_Out),
        json_hash_expand(Context_Out, ")", Hash_Out, _),
        json_data_prefix(Data_Prefix),
        format(atom(Node_Out), "~sCons/SHA1/~s", [Data_Prefix,Hash_Out]),
        global_prefix_expand(rdf:type, Rdf_Type),
        global_prefix_expand(rdf:first, Rdf_First),
        global_prefix_expand(rdf:rest, Rdf_Rest),
        global_prefix_expand(rdf:'List', Rdf_List),
        (   Triple_Or_Hash = t(Node_Out, Rdf_Type, Rdf_List)
        ;   Triple_Or_Hash = t(Node_Out, Rdf_First, Document_Node)
        ;   Triple_Or_Hash = t(Node_Out, Rdf_Rest, Node_In)
        ;   json_list_triple(Rest, hash(Context_Out, Node_Out), Triple_Or_Hash))).
