:- module('document/json_rdf', [
              is_json_document_type/1,
              is_json_subdocument_type/1,
              json_document_triple/3,
              json_subdocument_triple/4,
              assign_json_document_id/2,
              get_json_object/3,
              graph_get_json_object/3,
              insert_json_object/3,
              delete_json_object/3,
              delete_json_object/4
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document/json)).
:- use_module(core(document/instance)).
:- use_module(core(document/schema)).

:- use_module(library(sha)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(plunit)).
% higher order
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

is_json_document_type(Type) :-
    global_prefix_expand(sys:'JSONDocument', Type).

is_json_subdocument_type(Type) :-
    global_prefix_expand(sys:'JSON', Type).

/** <module> JSON RDF
 **/
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

json_subdocument_triple(Id, Property, Dict, Triple) :-
    json_subdocument_triple(Dict, Triple_Or_Hash),
    (   Triple_Or_Hash = t(_,_,_)
    ->  Triple = Triple_Or_Hash
    ;   Triple_Or_Hash = hash(_, Node),
        Triple = t(Id, Property, Node)).

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


assign_json_document_id(Prefixes,Id) :-
    get_dict('@base', Prefixes, Base),
    atomic_list_concat([Base,'JSONDocument/'],Obj_Base),
    idgen_random(Obj_Base, Id).

bind_vars(Object) :-
    term_variables(Object, Vars),
    maplist([json]>>true, Vars).

compress_json_field(Key, Prop) :-
    re_matchsub('^http://terminusdb.com/schema/json#(.*)',
                Key, Sub, []),
    get_dict(1, Sub, Encoded_Key),
    uri_encoded(segment, Prop, Encoded_Key).

get_json_object(Desc, Id, JSON) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc, Trans),
    get_json_object(Trans, Id, JSON).
get_json_object(Query_Context, Id, JSON) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_json_object(TO, Id, JSON).
get_json_object(Transaction, Id, JSON) :-
    (   is_transaction(Transaction)
    ;   is_validation_object(Transaction)
    ),
    !,
    database_instance(Transaction, Instance),
    graph_get_json_object(Instance, Id, JSON).

marshall_value(_^^'http://www.w3.org/2001/XMLSchema#token', null) :-
    !.
marshall_value(Value^^_, Value).

graph_get_json_object(_Graph, Id, []) :-
    global_prefix_expand(rdf:nil, Id),
    !.
graph_get_json_object(Graph, Id, [Head|Tail]) :-
    xrdf(Graph, Id, rdf:type, rdf:'List'),
    !,
    xrdf(Graph, Id, rdf:first, First),
    (   marshall_value(First,Head)
    ->  true
    ;   graph_get_json_object(Graph, First, Head)
    ),
    xrdf(Graph, Id, rdf:rest, Rest),
    graph_get_json_object(Graph, Rest, Tail).
graph_get_json_object(Graph, Id, Document) :-
    (   xrdf(Graph, Id, rdf:type, sys:'JSONDocument')
    ;   xrdf(Graph, Id, rdf:type, sys:'JSON')),
    !,
    findall(Prop-Value,
            (   xrdf(Graph, Id, Key, Val_or_Uri),
                (   Key = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
                ->  fail % ignore type field
                ;   marshall_value(Val_or_Uri,Value)
                ->  true % strip type from value
                ;   graph_get_json_object(Graph, Val_or_Uri, Value) % subdocument or list
                ),
                compress_json_field(Key,Prop)
            ),
            Pairs),
    dict_pairs(Document, json, Pairs).

insert_json_object(Query_Context, JSON, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_json_object(TO, JSON, Id).
insert_json_object(Transaction, JSON, Id) :-
    database_instance(Transaction, [Instance]),
    database_prefixes(Transaction,Prefixes),
    (   var(Id)
    ->  assign_json_document_id(Prefixes,Id)
    ;   true),
    % insert
    forall(
        json_document_triple(JSON, Id, t(S,P,O)),
        insert(Instance, S, P, O, _)
    ).

has_no_other_link(DB, Id, V) :-
    database_instance(DB, I),
    forall(
        xrdf(I, Some, _, V),
        atom_string(Id,Some)
    ).

delete_json_subobject(DB, Id, V) :-
    (   atom(V),
        instance_of(DB, V, C)
    ->  (   has_no_other_link(DB, Id, V)
        ->  (   global_prefix_expand(sys:'JSON', C)
            ->  delete_json_object(DB, V)
            ;   is_list_type(C)
            ->  delete_json_object(DB, V)
            ;   true)
        ;   true
        )
    ;   true).

delete_json_object(Transaction, Id) :-
    delete_json_object(Transaction, true, Id).

delete_json_object(Transaction, Unlink, Id) :-
    is_transaction(Transaction),
    !,
    database_prefixes(Transaction, Prefixes),
    delete_json_object(Transaction, Prefixes, Unlink, Id).
delete_json_object(Query_Context, Unlink, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    delete_json_object(TO, Unlink, Id).

/* Basically use a reference count for deletion. */
delete_json_object(Transaction, Prefixes, Unlink, Id) :-
    database_instance(Transaction, Instance),
    prefix_expand(Id,Prefixes,Id_Ex),
    (   xrdf(Instance, Id_Ex, rdf:type, _)
    ->  true
    ;   throw(error(document_not_found(Id), _))
    ),
    forall(
        xquad(Instance, G, Id_Ex, P, V),
        (   delete(G, Id_Ex, P, V, _),
            delete_json_subobject(Transaction,Id_Ex,V)
        )
    ),
    (   Unlink = true
    ->  unlink_object(Instance, Id_Ex)
    ;   true).

:- begin_tests(json_rdf, [concurrent(true)]).

test(assign_json_document_id,[]) :-
    Prefixes = _{ '@base' : 'foo/' },
    assign_json_document_id(Prefixes,Id),
    atom_concat('foo/JSONDocument', _, Id).

test(generate_data_triples,[]) :-
    JSON = json{
               name : "Gavin",
               age : 45,
               hobby: null,
               sex : true
           },
    Prefixes = _{ '@base' : 'foo/' },
    assign_json_document_id(Prefixes,Id),
    findall(
        Triple,
        json_document_triple(JSON, Id, Triple),
        Triples),

    Triples =
    [t(Id,
       'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
       'http://terminusdb.com/schema/sys#JSONDocument'),
     t(Id,
       'http://terminusdb.com/schema/json#age',
       45^^'http://www.w3.org/2001/XMLSchema#decimal'),
     t(Id,
       'http://terminusdb.com/schema/json#hobby',
       null^^'http://www.w3.org/2001/XMLSchema#token'),
     t(Id,
       'http://terminusdb.com/schema/json#name',
       "Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
     t(Id,
       'http://terminusdb.com/schema/json#sex',
       true^^'http://www.w3.org/2001/XMLSchema#boolean')
    ].

test(generate_subdocument_triples,[]) :-
    JSON = json{
               name : "Susan",
               friend : json{
                            name : "Gavin",
                            age : 45,
                            hobby: null,
                            sex : true
                        }
           },
    Prefixes = _{ '@base' : 'foo/' },
    assign_json_document_id(Prefixes,Id),
    findall(
        Triple,
        json_document_triple(JSON, Id, Triple),
        Triples),

    Triples =
    [t(Id,
       'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
       'http://terminusdb.com/schema/sys#JSONDocument'),
     t('terminusdb:///json/JSON/SHA1/b75cadefaf8ec772ec68f8534ab2d2fd9465cf6d',
       'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
       'http://terminusdb.com/schema/sys#JSON'),
     t('terminusdb:///json/JSON/SHA1/b75cadefaf8ec772ec68f8534ab2d2fd9465cf6d',
       'http://terminusdb.com/schema/json#sex',
       true^^'http://www.w3.org/2001/XMLSchema#boolean'),
     t('terminusdb:///json/JSON/SHA1/b75cadefaf8ec772ec68f8534ab2d2fd9465cf6d',
       'http://terminusdb.com/schema/json#name',
       "Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
     t('terminusdb:///json/JSON/SHA1/b75cadefaf8ec772ec68f8534ab2d2fd9465cf6d',
       'http://terminusdb.com/schema/json#hobby',
       null^^'http://www.w3.org/2001/XMLSchema#token'),
     t('terminusdb:///json/JSON/SHA1/b75cadefaf8ec772ec68f8534ab2d2fd9465cf6d','http://terminusdb.com/schema/json#age',45^^'http://www.w3.org/2001/XMLSchema#decimal'),
     t(Id,
       'http://terminusdb.com/schema/json#friend',
       'terminusdb:///json/JSON/SHA1/b75cadefaf8ec772ec68f8534ab2d2fd9465cf6d'),
     t(Id,
       'http://terminusdb.com/schema/json#name',
       "Susan"^^'http://www.w3.org/2001/XMLSchema#string')
    ].

test(generate_list_triples,[]) :-
    JSON = json{
               name : "Susan",
               friends : [json{name : "Gavin"},json{name : "Tim"}]
           },
    Prefixes = _{ '@base' : 'foo/' },
    assign_json_document_id(Prefixes,Id),
    findall(
        Triple,
        json_document_triple(JSON, Id, Triple),
        Triples),
    Triples =
    [ t(Id,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSONDocument'),
	  t('terminusdb:///json/JSON/SHA1/c23061edfc1f51ee7a7bf85600e735037010c0d8',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/SHA1/c23061edfc1f51ee7a7bf85600e735037010c0d8',
		'http://terminusdb.com/schema/json#name',
		"Tim"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/Cons/SHA1/e6dba5bead04d2eab971eed16379d3b915c811b1',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t('terminusdb:///json/Cons/SHA1/e6dba5bead04d2eab971eed16379d3b915c811b1',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		'terminusdb:///json/JSON/SHA1/c23061edfc1f51ee7a7bf85600e735037010c0d8'),
	  t('terminusdb:///json/Cons/SHA1/e6dba5bead04d2eab971eed16379d3b915c811b1',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
	  t('terminusdb:///json/JSON/SHA1/b17a67cefa99604c8fb9840bc22198a4bda93cff',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/SHA1/b17a67cefa99604c8fb9840bc22198a4bda93cff',
		'http://terminusdb.com/schema/json#name',
		"Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/Cons/SHA1/1ef375511fd860b6716d56a865ebb6573bcee9aa',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t('terminusdb:///json/Cons/SHA1/1ef375511fd860b6716d56a865ebb6573bcee9aa',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		'terminusdb:///json/JSON/SHA1/b17a67cefa99604c8fb9840bc22198a4bda93cff'),
	  t('terminusdb:///json/Cons/SHA1/1ef375511fd860b6716d56a865ebb6573bcee9aa',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'terminusdb:///json/Cons/SHA1/e6dba5bead04d2eab971eed16379d3b915c811b1'),
	  t(Id,
		'http://terminusdb.com/schema/json#friends',
		'terminusdb:///json/Cons/SHA1/1ef375511fd860b6716d56a865ebb6573bcee9aa'),
	  t(Id,
		'http://terminusdb.com/schema/json#name',
		"Susan"^^'http://www.w3.org/2001/XMLSchema#string')
	].

:- use_module(core(util/test_utils)).
test(round_trip_complex,[
         setup((setup_temp_store(State),
                create_db_without_schema(admin,test))),
         cleanup(teardown_temp_store(State))
     ]) :-

    JSON =
    json{
        name : "Susan",
        friends : [json{name : "Gavin",
                        height: 1.67,
                        year: 2012},
                   json{name : "Tim",
                        instrument: null}]
    },

    resolve_absolute_string_descriptor('admin/test', Descriptor),
    create_context(Descriptor,commit_info{ author : "automated test framework",
                                           message : "testing"}, Context),

    with_transaction(
        Context,
        insert_json_object(Context, JSON, Id),
        _
    ),

    get_json_object(Descriptor, Id, Document),

    Document =
    json{ friends:[ json{name:"Gavin",height:1.67,year:2012},
				    json{instrument:null,name:"Tim"}
				  ],
		  name:"Susan"
		}.


test(delete_complex,[
         setup((setup_temp_store(State),
                create_db_without_schema(admin,test))),
         cleanup(teardown_temp_store(State))
     ]) :-

    JSON =
    json{
        name : "Susan",
        friends : [json{name : "Gavin",
                        height: 1.67,
                        year: 2012},
                   json{name : "Tim",
                        instrument: null}]
    },

    resolve_absolute_string_descriptor('admin/test', Descriptor),
    create_context(Descriptor,commit_info{ author : "automated test framework",
                                           message : "testing"}, Context),

    with_transaction(
        Context,
        insert_json_object(Context, JSON, Id),
        _
    ),
    create_context(Descriptor,commit_info{ author : "automated test framework",
                                           message : "testing"}, Context2),

    with_transaction(
        Context2,
        delete_json_object(Context2, Id),
        _
    ),

    findall(
        t(X,Y,Z),
        ask(Descriptor, t(X,Y,Z)),
        []).

test(delete_overlap,[
         setup((setup_temp_store(State),
                create_db_without_schema(admin,test))),
         cleanup(teardown_temp_store(State))
     ]) :-

    JSON1 =
    json{
        name : "Susan",
        friends : [json{name : "Gavin",
                        height: 1.67,
                        year: 2012}]
    },

    JSON2 =
    json{
        name : "Dick",
        friends : [json{name : "Gavin",
                        height: 1.67,
                        year: 2012}]
    },

    resolve_absolute_string_descriptor('admin/test', Descriptor),
    create_context(Descriptor,commit_info{ author : "automated test framework",
                                           message : "testing"}, Context),

    with_transaction(
        Context,
        (   insert_json_object(Context, JSON1, Id1),
            insert_json_object(Context, JSON2, Id2)
        ),
        _
    ),
    create_context(Descriptor,commit_info{ author : "automated test framework",
                                           message : "testing"}, Context2),

    with_transaction(
        Context2,
        delete_json_object(Context2, Id1),
        _
    ),

    get_json_object(Descriptor, Id2, Document),
    Document =
    json{ friends:[json{height:1.67,name:"Gavin",year:2012}],
		  name:"Dick"
		},

    create_context(Descriptor,commit_info{ author : "automated test framework",
                                           message : "testing"}, Context3),

    with_transaction(
        Context3,
        delete_json_object(Context3, Id2),
        _
    ),

    findall(
        t(X,Y,Z),
        ask(Descriptor, t(X,Y,Z)),
        []).

:- end_tests(json_rdf).
