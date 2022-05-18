:- module('document/json_rdf', [
              json_object_triple/3,
              json_object_triple/4,
              assign_json_object_id/2,
              get_json_object/3,
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

:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(plunit)).
% higher order
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).



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

json_object_triple(Parent_Id, Parent_Prop, Object, Triple),
is_dict(Object) =>
    variant_sha1(Object, Hash),
    json_data_prefix(Data),
    atomic_list_concat([Data,'JSON/',Hash], Id),
    global_prefix_expand(rdf:type, RDF_Type),
    global_prefix_expand(sys:'JSON', JSON),
    % Generate triples from fields
    (   Triple = t(Parent_Id, Parent_Prop, Id)
    ;   Triple = t(Id, RDF_Type, JSON)
    ;   get_dict(Key,Object,Value),
        uri_encoded(segment, Key, Encoded_Key),
        global_prefix_expand(json:Encoded_Key, Prop),
        json_object_triple(Id,Prop,Value,Triple)
    ).
json_object_triple(Parent_Id, Parent_Prop, Object, Triple),
is_list(Object),
Object = [Head|Tail] =>
    variant_sha1(Object, Hash),
    json_data_prefix(Data),
    atomic_list_concat([Data,'Cons/',Hash], Id),
    global_prefix_expand(rdf:type, RDF_Type),
    global_prefix_expand(rdf:first, RDF_First),
    global_prefix_expand(rdf:rest, RDF_Rest),
    global_prefix_expand(rdf:'List', RDF_List),
    (   Triple = t(Parent_Id, Parent_Prop, Id)
    ;   Triple = t(Id,RDF_Type,RDF_List)
    ;   json_object_triple(Id,RDF_First,Head,Triple)
    ;   json_object_triple(Id,RDF_Rest,Tail,Triple)
    ).
json_object_triple(Parent_Id, Parent_Prop, Object, Triple),
is_list(Object),
Object = [] =>
    global_prefix_expand(rdf:nil, RDF_Nil),
    Triple = t(Parent_Id,Parent_Prop, RDF_Nil).
json_object_triple(Parent_Id, Parent_Prop, Object, Triple) =>
    json_type_rdf_type(Object, Value),
    Triple = t(Parent_Id, Parent_Prop, Value).

assign_json_object_id(Object, Id) :-
    bind_vars(Object),
    variant_sha1(Object, Hash),
    json_data_prefix(Data),
    atomic_list_concat([Data,'JSONDocument/',Hash], Id).

bind_vars(Object) :-
    term_variables(Object, Vars),
    maplist([json]>>true, Vars).

/* Top level entry point */
json_object_triple(Object,Id,Triple),
is_dict(Object) =>
    % We need stable hashs - so bind all vars.
    global_prefix_expand(rdf:type, RDF_Type),
    global_prefix_expand(sys:'JSONDocument', JSON),
    % Generate triples from fields
    (   Triple = t(Id, RDF_Type, JSON)
    ;   get_dict(Key,Object,Value),
        uri_encoded(segment, Key, Encoded_Key),
        global_prefix_expand(json:Encoded_Key, Prop),
        json_object_triple(Id,Prop,Value,Triple)
    ).

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
    get_json_object_(Transaction, Id, JSON).

get_json_object_(_Transaction, Id, []) :-
    global_prefix_expand(rdf:nil, Id),
    !.
get_json_object_(Transaction, Id, [Head|Tail]) :-
    database_instance(Transaction, Instance),
    xrdf(Instance, Id, rdf:type, rdf:'List'),
    !,
    xrdf(Instance, Id, rdf:first, First),
    get_json_object_(Transaction, First, Head),
    xrdf(Instance, Id, rdf:rest, Rest),
    get_json_object_(Transaction, Rest, Tail).
get_json_object_(Transaction, Id, Document) :-
    database_instance(Transaction, Instance),
    (   xrdf(Instance, Id, rdf:type, sys:'JSONDocument')
    ;   xrdf(Instance, Id, rdf:type, sys:'JSON')),
    !,
    findall(Prop-Value,
            (   xrdf(Instance, Id, Key, Val_or_Uri),
                (   Key = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
                ->  fail % ignore type field
                ;   Val_or_Uri = _^^'http://www.w3.org/2001/XMLSchema#token'
                ->  Value = null % add null for token
                ;   Val_or_Uri = Value^^_
                ->  true % strip value
                ;   get_json_object_(Transaction, Val_or_Uri, Value) % subdocument or list
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
    (   var(Id)
    ->  assign_json_object_id(JSON, Id)
    ;   true),
    % insert
    forall(
        json_object_triple(JSON, Id, t(S,P,O)),
        insert(Instance, S, P, O, _)
    ).

has_no_other_link(DB, Id, V) :-
    database_instance(DB, I),
    forall(
        xrdf(I, Some, _, V),
        Id = Some
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

test(generate_data_triples,[]) :-
    JSON = json{
               name : "Gavin",
               age : 45,
               hobby: null,
               sex : true
           },
    assign_json_object_id(JSON, Id),
    findall(
        Triple,
        json_object_triple(JSON, Id, Triple),
        Triples),

    Triples =
    [ t('terminusdb:///json/JSONDocument/037676793775f3250a1c2109c440387eb2583a36',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSONDocument'),
	  t('terminusdb:///json/JSONDocument/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#name',
		"Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/JSONDocument/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#age',
		45^^'http://www.w3.org/2001/XMLSchema#decimal'),
	  t('terminusdb:///json/JSONDocument/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#hobby',
		null^^'http://www.w3.org/2001/XMLSchema#token'),
	  t('terminusdb:///json/JSONDocument/037676793775f3250a1c2109c440387eb2583a36',
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
    assign_json_object_id(JSON, Id),
    findall(
        Triple,
        json_object_triple(JSON, Id, Triple),
        Triples),
    Triples =
    [ t('terminusdb:///json/JSONDocument/5492d856e43ffe25196a86ad9027791380cc4d2d',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSONDocument'),
	  t('terminusdb:///json/JSONDocument/5492d856e43ffe25196a86ad9027791380cc4d2d',
		'http://terminusdb.com/schema/json#name',
		"Susan"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/JSONDocument/5492d856e43ffe25196a86ad9027791380cc4d2d',
		'http://terminusdb.com/schema/json#friend',
		'terminusdb:///json/JSON/037676793775f3250a1c2109c440387eb2583a36'),
	  t('terminusdb:///json/JSON/037676793775f3250a1c2109c440387eb2583a36',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#name',
		"Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/JSON/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#age',
		45^^'http://www.w3.org/2001/XMLSchema#decimal'),
	  t('terminusdb:///json/JSON/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#hobby',
		null^^'http://www.w3.org/2001/XMLSchema#token'),
	  t('terminusdb:///json/JSON/037676793775f3250a1c2109c440387eb2583a36',
		'http://terminusdb.com/schema/json#sex',
		true^^'http://www.w3.org/2001/XMLSchema#boolean')
	].

test(generate_list_triples,[]) :-
    JSON = json{
               name : "Susan",
               friends : [json{name : "Gavin"},json{name : "Tim"}]
           },
    assign_json_object_id(JSON, Id),
    findall(
        Triple,
        json_object_triple(JSON, Id, Triple),
        Triples),
    Triples =
    [ t('terminusdb:///json/JSONDocument/3f8090f584ef91ecf987325540b3ef189cc3835c',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSONDocument'),
	  t('terminusdb:///json/JSONDocument/3f8090f584ef91ecf987325540b3ef189cc3835c',
		'http://terminusdb.com/schema/json#name',
		"Susan"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/JSONDocument/3f8090f584ef91ecf987325540b3ef189cc3835c',
		'http://terminusdb.com/schema/json#friends',
		'terminusdb:///json/Cons/5f8ccf7fade4412b027d456b81eae97b6c9b79d2'),
	  t('terminusdb:///json/Cons/5f8ccf7fade4412b027d456b81eae97b6c9b79d2',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t('terminusdb:///json/Cons/5f8ccf7fade4412b027d456b81eae97b6c9b79d2',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		'terminusdb:///json/JSON/90bfb26e463835fd207b23b1d9774391c540257c'),
	  t('terminusdb:///json/JSON/90bfb26e463835fd207b23b1d9774391c540257c',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/90bfb26e463835fd207b23b1d9774391c540257c',
		'http://terminusdb.com/schema/json#name',
		"Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/Cons/5f8ccf7fade4412b027d456b81eae97b6c9b79d2',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'terminusdb:///json/Cons/8aa912f4be20fd8d26af0787261e45ed53e956e9'),
	  t('terminusdb:///json/Cons/8aa912f4be20fd8d26af0787261e45ed53e956e9',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t('terminusdb:///json/Cons/8aa912f4be20fd8d26af0787261e45ed53e956e9',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		'terminusdb:///json/JSON/70d8e985ade1df14a4a51b960ff2e1f0858bf2c8'),
	  t('terminusdb:///json/JSON/70d8e985ade1df14a4a51b960ff2e1f0858bf2c8',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/70d8e985ade1df14a4a51b960ff2e1f0858bf2c8',
		'http://terminusdb.com/schema/json#name',
		"Tim"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/Cons/8aa912f4be20fd8d26af0787261e45ed53e956e9',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil') ].

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
