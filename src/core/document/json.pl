:- module('document/json', [
              idgen_random/2,
              idgen_hash/3,
              idgen_lexical/3,
              context_triple/2,
              json_elaborate/3,
              json_elaborate/6,
              json_schema_triple/3,
              json_schema_elaborate/3,
              get_document/3,
              get_document/5,
              get_document_uri/3,
              get_schema_document/3,
              get_schema_document_uri/2,
              get_document_by_type/3,
              get_document_uri_by_type/3,
              get_schema_document_uri_by_type/3,
              delete_document/2,
              insert_document/3,
              insert_document/7,
              insert_document_unsafe/7,
              replace_document/2,
              replace_document/3,
              replace_document/5,
              replace_document/8,
              nuke_documents/1,
              insert_schema_document/2,
              insert_schema_document_unsafe/3,
              delete_schema_document/2,
              replace_schema_document/2,
              replace_schema_document/3,
              replace_schema_document/4,
              nuke_schema_documents/1,
              json_read_required_context/3,
              insert_context_document/2,
              replace_context_document/2,
              database_prefixes/2,
              database_schema_prefixes/2,
              run_insert_document/4,
              create_graph_from_json/5,
              write_json_stream_to_builder/3,
              write_json_stream_to_schema/2,
              write_json_stream_to_instance/2,
              write_json_string_to_schema/2,
              write_json_string_to_instance/2,
              replace_json_schema/2,
              class_frame/3,
              class_frame/4,
              all_class_frames/2,
              class_property_dictionary/3,
              class_property_dictionary/4,
              prefix_expand_schema/3,
              type_context/4,
              enum_value/3,
              update_id_field/3,
              update_captures/3,
              capture_ref/4,
              schema_document_exists/2,
              document_exists/2
          ]).

:- use_module(instance).
:- use_module(schema).

:- use_module(library(assoc)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(crypto)).
:- use_module(library(when)).
:- use_module(library(option)).

% performance
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
:- use_module(library(plunit)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(util/tables)).

:- use_module(core(document/inference)).
:- use_module(core(document/json_rdf)).

verify_languages(Docs) :-
    length(Docs,N),
    (   N > 1
    ->  convlist([D,R]>>get_dict('@language',D,R), Docs, Langs),
        do_or_die(
            length(Langs,N),
            error(no_language_tag_for_multilingual, _)),
        die_if(
            (   duplicates(Langs,Repeating),
                \+ Repeating = []),
            error(language_tags_repeated(Repeating), _))
    ;   true).

encode_id_fragment(Elt, Encoded) :-
    ground(Elt),
    !,
    (   Elt = optional(none)
    ->  Encoded = "+none+"
    ;   Elt = list(List)
    ->  maplist(encode_id_fragment, List, List_Encoded),
        merge_separator_split(Encoded, '++', List_Encoded)
    ;   format(atom(Fragment), '~w', [Elt]),
        uri_encoded(segment, Fragment, Part_Encoded),
        re_replace('\\+'/g, '%2B', Part_Encoded, Encoded)).
encode_id_fragment(Fragment, Encoded) :-
    ground(Encoded),
    !,
    (   Encoded = "+"
    ->  Fragment = optional(none)
    ;   uri_encoded(segment, Fragment, Encoded)).
encode_id_fragment(_, _) :-
    throw(error(instantiation_error, _)).

value_type_to_json_type(X, T, X, T) :-
    number(X),
    !.
value_type_to_json_type(X, T, S, T) :-
    string(X),
    !,
    atom_string(X, S).
value_type_to_json_type(X, T, X, T) :-
    T = 'http://www.w3.org/2001/XMLSchema#boolean',
    !.
value_type_to_json_type(X, T, S, T) :-
    typecast(X^^T, 'http://www.w3.org/2001/XMLSchema#string', [], S^^_).

json_type_to_value_type(J, T, J, T) :-
    number(J),
    !.
json_type_to_value_type(J, T, J, T) :-
    memberchk(J, [false, true]),
    !,
    do_or_die(
        T = 'http://www.w3.org/2001/XMLSchema#boolean',
        error(unexpected_boolean_value(J, T), _)).
json_type_to_value_type(J, T, X, T) :-
    typecast(J^^'http://www.w3.org/2001/XMLSchema#string', T, [], X^^_).


value_type_json_type(V,T,JV,JT) :-
    ground(V),
    ground(T),
    !,
    value_type_to_json_type(V,T,JV,JT).
value_type_json_type(V,T,JV,JT) :-
    json_type_to_value_type(JV,JT,V,T).

value_json(X,O) :-
    O = json{
            '@type': "@id",
            '@id': X
        },
    string(X),
    !.
value_json(X,O) :-
    ground(O), % this looks dubious...
    O = json{
            '@type': "@id",
            '@id': X,
            '@foreign' : _
        },
    string(X),
    !.
value_json(RDF_Nil,[]) :-
    global_prefix_expand(rdf:nil,RDF_Nil),
    !.
value_json(V^^T,json{ '@value' : JV, '@type' : JT}) :-
    value_type_json_type(V,T,JV,JT),
    !.
value_json(X@Lang,O) :-
    O = json{
            '@lang': Lang,
            '@value': X
        },
    !,
    do_or_die(
        iana(Lang,_),
        error(unknown_language_tag(Lang),_)).
value_json(X@Lang,O) :-
    global_prefix_expand(rdf:langString,RDF_LangString),
    O = json{
            '@lang': Lang,
            '@value': X,
            '@type' : RDF_LangString
        },
    !,
    do_or_die(
        iana(Lang,_),
        error(unknown_language_tag(Lang),_)).
value_json(X,X) :-
    atom(X).

get_all_path_values(JSON,Path_Values) :-
    findall(Path-Value,
            get_path_value(JSON,Path,Value),
            Path_Values).

% TODO: Arrays
get_value([], []) :-
    !.
get_value(List, Value) :-
    is_list(List),
    !,
    member(Elt,List),
    get_value(Elt,Value).
get_value(Elaborated, Value) :-
    is_dict(Elaborated),
    get_dict('@type', Elaborated, "@id"),
    !,
    get_dict('@id', Elaborated, Value).
get_value(Elaborated,Value) :-
    is_dict(Elaborated),
    get_dict('@container',Elaborated,List_Type),
    memberchk(List_Type, ["@array", "@list", "@set"]),
    !,
    get_dict('@value',Elaborated, List),
    member(Elt,List),
    get_value(Elt,Value).
get_value(Elaborated,Value) :-
    is_dict(Elaborated),
    get_dict('@container',Elaborated,Table_Type),
    memberchk(Table_Type, ["@table"]),
    !,
    get_dict('@value',Elaborated, Table),
    member(List,Table),
    member(Elt,List),
    get_value(Elt,Value).
get_value(Elaborated,Value) :-
    is_dict(Elaborated),
    get_dict('@value',Elaborated,_),
    !,
    value_json(Value,Elaborated).

get_path_value(Elaborated,Path,Value) :-
    is_dict(Elaborated),
    get_dict('@type',Elaborated,_),
    !,
    dict_pairs(Elaborated,json,Pairs),
    % Remove ID if it exists
    (   select('@id'-_,Pairs,Pairs1)
    ->  true
    ;   Pairs = Pairs1),
    % remove type?
    % select('@type'-_,Pairs1,Pairs2),

    member(P-V,Pairs1),

    (   P = '@type',
        atom(V)
    ->  Path = [P],
        V = Value
    ;   get_value(V,Value)
    *->  Path = [P]
    ;   get_path_value(V,Sub_Path,Value),
        Path = [P|Sub_Path]
    ).
get_path_value(Elaborated,[I|Path],Value) :-
    is_list(Elaborated),
    sort(Elaborated, Sorted),
    !,
    % We must be sorted to ensure stability.
    % NOTE: This probably breaks if there are unbound dictionary tags!
    nth0(I, Sorted, Elt),
    get_path_value(Elt, Path, Value).

get_field_values(JSON,DB,Context,Fields,Values) :-
    findall(
        Value,
        (   member(Field,Fields),
            prefix_expand_schema(Field,Context,Field_Ex),
            (   get_dict(Field_Ex,JSON,Value)
            ->  true
            %   key field not found! We'll have to find out if that's allowable or not
            ;   get_dict('@type',JSON,Type),
                prefix_expand_schema(Type, Context, Type_Ex),
                prefix_expand_schema(Field, Context, Field_Ex),
                class_predicate_type(DB, Type_Ex, Field_Ex, Field_Type),
                memberchk(Field_Type, [optional(_), set(_), array(_,_)])
            ->  Value = optional(none)
            ;   throw(error(key_missing_required_field(Field),_))
            )
        ),
        Values).

untyped_typecast(V, Type, Val, Val_Type) :-
    (   string(V)
    ->  typecast(V^^xsd:string,
                 Type, [], Val^^Val_Type)
    ;   atom(V),
        atom_string(V,String)
    ->  typecast(String^^xsd:string,
                 Type, [], Val^^Val_Type)
    ;   number(V)
    ->  typecast(V^^xsd:decimal,
                 Type, [], Val^^Val_Type)).

normalize_json_value(V, Type, Val) :-
    global_prefix_expand_safe(Type, TE),

    untyped_typecast(V, TE, Casted, Casted_Type),
    typecast(Casted^^Casted_Type, xsd:string, [], Val^^_).

raw(JValue,Value) :-
    is_dict(JValue),
    !,
    (   get_dict('@type', JValue, "@id")
    ->  get_dict('@id', JValue, Value)
    ;   get_dict('@container', JValue, Container_Type)
    ->  get_dict('@value', JValue, V),
        (   [] = V
        ->  Value = optional(none)
        ;   [Single_Value] = V
        ->  raw(Single_Value, Value)
        ;   \+ is_list(V)
        ->  get_dict('@type', JValue, Value_Type),
            normalize_json_value(V, Value_Type, Value)
        ;   Container_Type = "@table"
        ->  maplist([V1,V1_Raw]>>maplist(raw, V1, V1_Raw),
                    V,V_Raw),
            Value = table(V_Raw)
        ;   (   Container_Type = "@set"
            ->  sort(V, V_1)
            ;   V_1 = V),
            maplist(raw, V_1, V_Raw),
            Value = list(V_Raw))
    ;   get_dict('@type', JValue, Value_Type)
    ->  get_dict('@value', JValue, Uncasted_Value),
        normalize_json_value(Uncasted_Value, Value_Type, Value)
    ;   get_dict('@value', JValue, Value)
    ).
raw(JValue,JValue).

idgen_suffix(Values, Suffix) :-
    maplist(raw,Values,Raw),
    maplist(encode_id_fragment,Raw,Encoded),
    merge_separator_split(Suffix, '+', Encoded).

idgen_lexical(Base,Values,ID) :-
    idgen_suffix(Values, Suffix),
    format(string(ID), '~w~w', [Base,Suffix]).

idgen_hash(Base,Values,ID) :-
    idgen_suffix(Values, Suffix),
    crypto_data_hash(Suffix, Hash, [algorithm(sha256)]),
    format(string(ID), "~w~w", [Base,Hash]).

idgen_path_values_hash(Base,Path,ID) :-
    format(string(A), '~q', [Path]),
    crypto_data_hash(A, Hash, [algorithm(sha256)]),
    format(string(ID), "~w~w", [Base,Hash]).

idgen_random(Base,ID) :-
    random(X),
    format(string(S), '~w', [X]),
    crypto_data_hash(S, Hash, [algorithm(sha256)]),
    format(string(ID),'~w~w',[Base,Hash]).

path_strings_([], _Prefixes, []).
path_strings_([index(N)|Path], Prefixes, [N_String|Strings]) :-
    format(atom(N_String), '~q', [N]),
    path_strings_(Path, Prefixes, Strings).
path_strings_([node(X)|Path], Prefixes, [URI|Strings]) :-
    compress_dict_uri(X, Prefixes, Compressed),
    (   (   uri_has_protocol(Compressed)
        ;   uri_has_prefix(Compressed))
    ->  prefix_expand(X, Prefixes, URI)
    ;   Compressed = URI
    ),
    path_strings_(Path, Prefixes, Strings).
path_strings_([property(X)|Path], Prefixes, [URI|Strings]) :-
    % We have *very* late binding of IDs, need to do this after its done.
    compress_schema_uri(X, Prefixes, Compressed),
    (   (   uri_has_protocol(Compressed)
        ;   uri_has_prefix(Compressed))
    ->  prefix_expand_schema(X, Prefixes, URI)
    ;   Compressed = URI
    ),
    path_strings_(Path, Prefixes, Strings).
path_strings_([type(X)|Path], Prefixes, [URI|Strings]) :-
    % We have *very* late binding of IDs, need to do this after its done.
    compress_schema_uri(X, Prefixes, Compressed),
    (   (   uri_has_protocol(Compressed)
        ;   uri_has_prefix(Compressed))
    ->  prefix_expand_schema(X, Prefixes, URI)
    ;   Compressed = URI
    ),
    path_strings_(Path, Prefixes, Strings).

path_strings(Elts, Prefixes, Strings) :-
    %reverse(Elts, Descending),
    path_strings_(Elts, Prefixes, Strings).

path_component([], _Prefixes, []) :-
    !.
path_component(Path, Prefixes, [Path_String]) :-
    reverse(Path,Rev),
    path_strings(Rev, Prefixes, Strings),
    merge_separator_split(Path_String, '/', Strings).

json_idgen(Descriptor, JSON, DB, Context, Path, Id) :-
    json_idgen_(Descriptor, JSON, DB, Context, Path, Id),
    !.
json_idgen(Descriptor, _JSON, _DB, _Context, _Path, _Id) :-
    throw(error(unexpected_argument_instantiation(json_idgen, Descriptor), _)).

json_idgen_(lexical(Base, Fields), JSON, DB, Context, Path, Id) :-
    get_field_values(JSON, DB, Context, Fields, Values),
    path_component([type(Base)|Path], Context, [Path_Base]),
    idgen_lexical(Path_Base, Values, Id).
json_idgen_(hash(Base, Fields), JSON, DB, Context, Path, Id) :-
    get_field_values(JSON, DB, Context, Fields, Values),
    path_component([type(Base)|Path], Context, [Path_Base]),
    idgen_hash(Path_Base, Values, Id).
json_idgen_(value_hash(Base), JSON, _DB, _Context, _Path, Id) :-
    get_all_path_values(JSON, Path_Values),
    idgen_path_values_hash(Base, Path_Values, Id).
json_idgen_(random(Base), JSON, _DB, Context, Path, Id) :-
    json_idgen_base(Base, JSON, Context, Path, Id).
json_idgen_(base(Base), JSON, _DB, Context, Path, Id) :-
    json_idgen_base(Base, JSON, Context, Path, Id).

json_idgen_base(Base, JSON, Context, Path, Id) :-
    (   get_dict('@id', JSON, Submitted_Id),
        ground(Submitted_Id)
    ->  path_component([type(Base)|Path], Context, [Path_Base]),
        idgen_check_base(Submitted_Id, Path_Base, Context),
        Id = Submitted_Id
    ;   path_component([type(Base)|Path], Context, [Path_Base]),
        idgen_random(Path_Base, Id)
    ).

idgen_check_base(Submitted_ID, Base, Context) :-
    prefix_expand(Submitted_ID, Context, Submitted_ID_Ex),
    prefix_expand(Base, Context, Base_Ex),
    do_or_die(atom_concat(Base_Ex, _, Submitted_ID_Ex),
              error(submitted_document_id_does_not_have_expected_prefix(Submitted_ID_Ex, Base_Ex),_)).

check_submitted_id_against_generated_id(Context, Generated_Id, Id) :-
    ground(Id),
    !,
    prefix_expand(Id, Context, Id_Ex),
    prefix_expand(Generated_Id, Context, Generated_Id_Ex),
    do_or_die(
        Id_Ex = Generated_Id_Ex,
        error(submitted_id_does_not_match_generated_id(Id_Ex, Generated_Id_Ex), _)
    ).
check_submitted_id_against_generated_id(Context, Id, Id_Ex) :-
    prefix_expand(Id, Context, Id_Ex).

class_descriptor_image(unit,json{ '@type': SysUnit}) :-
    global_prefix_expand(sys:'Unit', SysUnit).
class_descriptor_image(class(_),json{ '@type' : "@id" }).
class_descriptor_image(foreign(C),json{ '@type' : "@id", '@foreign' : C}).
class_descriptor_image(optional(C),json{ '@type' : C }).
class_descriptor_image(tagged_union(_,_),json{ '@type' : "@id" }).
class_descriptor_image(base_class(C),json{ '@type' : C }).
class_descriptor_image(enum(C,_),json{ '@type' : C }).
class_descriptor_image(list(C),json{ '@container' : "@list",
                                     '@type' : C }).
class_descriptor_image(table(C),json{ '@container' : "@table",
                                      '@type' : C }).
class_descriptor_image(array(C,D),json{ '@container' : "@array",
                                        '@dimensions' : D,
                                        '@type' : C }).
class_descriptor_image(set(C),json{ '@container' : "@set",
                                    '@type' : C }).
class_descriptor_image(cardinality(C,_,_), json{ '@container' : "@set",
                                                 '@type' : C }).

get_context_metadata(DB, ID, Metadata) :-
    metadata_descriptor(DB, ID, metadata(Metadata)).

get_context_documentation(DB, ID, Doc) :-
    database_schema(DB, Schema),
    findall(
        Documentation,
        (   xrdf(Schema, ID, sys:documentation, Doc_ID),
            Documentation0 = json{},
            (   xrdf(Schema, Doc_ID, sys:title, Title^^_)
            ->  put_dict(_{ '@title': Title}, Documentation0, Documentation1)
            ;   Documentation0 = Documentation1),
            (   xrdf(Schema, Doc_ID, sys:description, Desc^^_)
            ->  put_dict(_{ '@description': Desc}, Documentation1, Documentation2)
            ;   Documentation1 = Documentation2),
            (   xrdf(Schema, Doc_ID, sys:authors, Author_ID)
            ->  rdf_list_list(Schema, Author_ID, Authors),
                maplist([Author^^_,Author]>>true, Authors, Authors_List),
                put_dict(_{ '@authors': Authors_List}, Documentation2, Documentation3)
            ;   Documentation2 = Documentation3),
            (   xrdf(Schema, Doc_ID, sys:language, Language^^_)
            ->  put_dict(_{ '@language': Language}, Documentation3, Documentation)
            ;   Documentation3 = Documentation)
        ),
        Documentations
    ),
    (   Documentations = []
    ->  fail
    ;   Documentations = [Doc]
    ->  true
    ;   Documentations = Doc
    ).

database_context_object(DB,Prefixes) :-
    is_transaction(DB),
    is_schemaless(DB),
    !,
    database_prefixes(DB, Prefixes).
database_context_object(DB,Context) :-
    is_transaction(DB),
    !,
    database_prefixes(DB, Prefixes),
    database_schema(DB, Schema),
    % This should always exist according to schema correctness criteria?
    xrdf(Schema, ID, rdf:type, sys:'Context'),
    xrdf(Schema, ID, sys:base, Base_String^^_),
    xrdf(Schema, ID, sys:schema, Schema_String^^_),
    (   get_context_documentation(DB, ID, Documentation)
    ->  put_dict(
            _{ '@base' : Base_String,
               '@schema' : Schema_String,
               '@documentation' : Documentation},
            Prefixes, Context0)
    ;   put_dict(
            _{ '@base' : Base_String,
               '@schema' : Schema_String},
            Prefixes, Context0)
    ),
    (   get_context_metadata(DB, ID, Metadata)
    ->  put_dict(_{'@metadata' : Metadata}, Context0, Context)
    ;   Context = Context0
    ).
database_context_object(Query_Context,Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    database_context_object(TO, Context).
database_context_object(Askable,Context) :-
    create_context(Askable, Query_Context),
    database_context_object(Query_Context, Context).

:- table database_schema_prefixes/2 as private.
database_schema_prefixes(Schema,Context) :-
    once(
        (   xrdf(Schema, ID, rdf:type, sys:'Context')
        ->  id_schema_json(Schema,ID,Pre_Context),
            findall(
                Key-URI,
                (   xrdf(Schema, ID, sys:prefix_pair, Prefix_Pair),
                    xrdf(Schema, Prefix_Pair, sys:prefix, Key_String^^_),
                    xrdf(Schema, Prefix_Pair, sys:url, URI^^_),
                    atom_string(Key,Key_String)
                ),
                Pairs),
            dict_pairs(Prefixes, context, Pairs),
            !,
            Context_With_ID = (Pre_Context.put(Prefixes)),
            select_dict(json{'@id' : _ }, Context_With_ID, Context)
        ;   Context = _{})
    ).

database_prefixes(DB,Context) :-
    (   is_transaction(DB)
    ;   is_validation_object(DB)),
    !,
    database_schema(DB,Schema),
    database_schema_prefixes(Schema,Context).
database_prefixes(Query_Context, Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    database_prefixes(TO, Context).
database_prefixes(Askable, Context) :-
    create_context(Askable, Query_Context),
    database_prefixes(Query_Context, Context).

predicate_map(P, json{ '@id' : P }).

type_context(_DB, "@id", _, json{}) :- !.
type_context(_DB, Base_Type, _, json{}) :-
    is_base_type(Base_Type),
    !.
type_context(DB,Type,Prefixes,Context) :-
    prefix_expand_schema(Type,Prefixes,TypeEx),
    is_simple_class(DB, TypeEx),
    findall(P - C,
          (
              class_predicate_type(DB, TypeEx, P, Desc),
              class_descriptor_image(Desc, Image),
              predicate_map(P, Map),
              put_dict(Map,Image,C)
          ),
          Edges),
    % eliminate duplicates
    sort(Edges,Sorted_Edges),
    catch(
        json_dict_create(Context,Sorted_Edges),
        error(duplicate_key(P),_),
        throw(error(violation_of_diamond_property(Type,P)))
    ).

prefix_expand_schema(Node,Context,NodeEx) :-
    (   get_dict('@schema', Context, Schema),
        put_dict(_{'@base' : Schema}, Context, New_Context)
    ->  true
    ;   Context = New_Context),
    prefix_expand(Node, New_Context, NodeEx).

property_expand_key_value(Prop,Value,DB,Context,Captures_In,P,V,Dependencies,Captures_Out) :-
    get_dict(Prop, Context, Full_Expansion),
    is_dict(Full_Expansion),
    !,
    expansion_key(Prop,Full_Expansion,P,Expansion),
    context_value_expand(DB,Context,Value,Expansion,Captures_In,V,Dependencies,Captures_Out).
property_expand_key_value(Prop,Value,DB,Context,Captures_In,P,V,Dependencies,Captures_Out) :-
    prefix_expand_schema(Prop, Context, Prop_Ex),
    Prop \= Prop_Ex,
    property_expand_key_value(Prop_Ex,Value,DB,Context,Captures_In,P,V,Dependencies,Captures_Out).

json_elaborate(DB,JSON,Elaborated) :-
    empty_assoc(Captures_In),
    json_elaborate(DB,JSON,Captures_In,Elaborated,_Dependencies,_Captures_Out).
json_elaborate(DB,JSON,Captures_In,Elaborated,Dependencies,Captures_Out) :-
    database_prefixes(DB,Context),
    json_elaborate(DB,JSON,Context,Captures_In,Elaborated, Dependencies, Captures_Out).

:- use_module(core(document/inference)).
json_elaborate(DB,JSON,Context,Captures_In,Elaborated,Dependencies,Captures_Out) :-
    %json_elaborate_(DB,JSON,Context,Captures_In,Elaborated,Dependencies,Captures_Out),
    infer_type(DB,Context,JSON,_,Result,captures(Captures_In,Dependencies-[],Captures_Out)),
    (   Result = witness(Witness)
    ->  term_variables(Witness, Vars),
        maplist([null]>>true,Vars),
        throw(error(schema_check_failure([Witness]),_))
    ;   Result = success(Elaborated)
    ),
    do_or_die(
        json_assign_ids(DB,Context,Elaborated),
        error(unable_to_assign_ids)).

/*
 * Check for JSON values that should not found in a string field.
 */
check_json_string(_, Val) :-
    atom(Val),
    \+ memberchk(Val, ['', null, false, true]),
    !.
check_json_string(_, Val) :-
    string(Val),
    Val \= "",
    !.
check_json_string(Field, Val) :-
    throw(error(bad_field_value(Field, Val), _)).

json_elaborate_(DB,JSON,Context,Captures_In,Result, Dependencies, Captures_Out) :-
    is_dict(JSON),
    !,

    % Look for @type. If it is not there but @id is, assume that the expanded
    % @type should be @id. If @id is not there, throw an error.
    % See <https://github.com/terminusdb/terminusdb/issues/622>
    (    get_dict('@type', JSON, Type)
    ->   New_JSON = JSON
    ;    (   do_or_die(
                 get_dict('@id', JSON, _Id),
                 % Note that, even though we check for @id here, we primarily
                 % expect a @type, so that is the reported error.
                 error(missing_field('@type', JSON), _)),
             put_dict('@type', JSON, "@id", New_JSON),
             Type = "@id")),

    do_or_die(
        type_context(DB,Type,Context,Type_Context),
        error(type_not_found(Type),_)),
    put_dict(Type_Context,Context,New_Context),
    json_context_elaborate(DB,New_JSON,New_Context,Captures_In,Elaborated,Dependencies,Captures_Out_1),

    % Insert an id. If id was part of the input document, it is
    % prefix-expanded. If not, it is kept as a variable, to be unified
    % with what it should be later on.
    update_id_field(Elaborated,Context,Result),

    %% do we need to capture something?
    update_captures(Result, Captures_Out_1, Captures_Out).

update_id_field(Elaborated,Context,Result) :-
    (   get_dict('@value', Elaborated, _) % no id on values
    ->  Elaborated = Result
    ;   get_dict('@type', Elaborated, Type),
        memberchk(Type, ['http://terminusdb.com/schema/sys#JSONDocument',
                         'http://terminusdb.com/schema/sys#JSON'])
    ->  Elaborated = Result
    ;   (   get_dict('@id', Elaborated, Id)
        ->  prefix_expand(Id, Context, Id_Ex)
        ;   Id_Ex = _),
        put_dict('@id', Elaborated, Id_Ex, Result)
    ).

update_captures(Elaborated,In,Out) :-
    get_dict('@id', Elaborated, Id),
    get_dict('@capture', Elaborated, Capture_Group),
    !,
    do_or_die(string(Capture_Group),
              error(capture_is_not_a_string(Capture_Group), _)),
    (   get_assoc(Capture_Group, In, Capture_Var)
    ->  do_or_die(var(Capture_Var),
                  error(capture_already_bound(Capture_Group), _)),
        Capture_Var = Id,
        Out = In
    ;   put_assoc(Capture_Group, In, Id, Out)
    ).
update_captures(_Elaborated,Capture,Capture).

json_assign_ids(DB,Context,JSON) :-
    json_assign_ids(DB,Context,JSON,[]).

json_assign_ids(_DB,_Context,JSON,_Path) :-
    \+ is_dict(JSON),
    !.
json_assign_ids(_DB,_Context,JSON,_Path) :-
    get_dict('@type',JSON,"@id"),
    !.
json_assign_ids(DB,Context,JSON,Path) :-
    get_dict('@id',JSON,ID),
    !,

    get_dict('@type',JSON,Type),
    (   is_subdocument(DB, Type)
    ->  Next_Path = Path
    ;   Next_Path = []),

    key_descriptor(DB, Context, Type, Descriptor),
    json_idgen(Descriptor, JSON, DB, Context, Next_Path, Generated_Id),
    check_submitted_id_against_generated_id(Context, Generated_Id, ID),

    dict_pairs(JSON, _, Pairs),
    maplist({DB, Context, ID, Next_Path}/[Property-Value]>>(
                json_assign_ids(DB, Context, Value, [property(Property),node(ID)|Next_Path])
            ),
            Pairs).
json_assign_ids(DB,Context,JSON,Path) :-
    get_dict('@container',JSON, "@set"),
    !,
    get_dict('@value', JSON, Values),
    maplist({DB,Context,Path}/[Value]>>(
                json_assign_ids(DB, Context, Value, Path)
            ),
            Values).
json_assign_ids(DB,Context,JSON,Path) :-
    get_dict('@container',JSON, "@array"),
    !,
    get_dict('@value', JSON, Values),
    array_assign_ids(Values,DB,Context,Path).
json_assign_ids(DB,Context,JSON,Path) :-
    get_dict('@container',JSON, _),
    !,
    get_dict('@value', JSON, Values),
    (   Values = []
    ->  Numlist = []
    ;   length(Values, Value_Len),
        Max_Index is Value_Len - 1,
        numlist(0, Max_Index, Numlist)
    ),

    maplist({DB,Context,Path}/[Value,Index]>>(
                New_Path=[index(Index)|Path],
                json_assign_ids(DB, Context, Value, New_Path)
            ),
            Values,
            Numlist).
json_assign_ids(_DB,_Context,_JSON,_Path).

array_assign_ids([],_,_,_) :-
    !.
array_assign_ids(Values,DB,Context,Path) :-
    length(Values, Value_Len),
    Max_Index is Value_Len - 1,
    numlist(0, Max_Index, Numlist),
    array_assign_ids(Values,Numlist,DB,Context,Path).

array_assign_ids([], [], _, _, _).
array_assign_ids([H|T], [I|Idx], DB, Context, Path) :-
    New_Path=[index(I)|Path],
    (   is_list(H)
    ->  array_assign_ids(H,DB,Context,New_Path)
    ;   json_assign_ids(DB,Context,H,New_Path)
    ),
    array_assign_ids(T,Idx,DB,Context,Path).

expansion_key(Key,Expansion,Prop,Cleaned) :-
    (   select_dict(json{'@id' : Prop}, Expansion, Cleaned)
    ->  true
    ;   Key = Prop,
        Expansion = Cleaned
    ).

capture_ref(Captures_In, Ref, Capture_Var, Captures_Out) :-
    do_or_die(var(Capture_Var),
              error(capture_var_was_ground_unexpectedly(Ref, Capture_Var),_ )),
    do_or_die(string(Ref),
              error(capture_is_not_a_string(Ref), _)),
    (   get_assoc(Ref, Captures_In, Capture_Var)
    ->  Captures_In = Captures_Out
    ;   put_assoc(Ref, Captures_In, Capture_Var, Captures_Out)).

value_expand_list([], _DB, _Context, _Elt_Type, Captures, [], [], Captures).
value_expand_list([Value|Vs], DB, Context, Elt_Type, Captures_In, [Expanded|Exs], Dependencies, Captures_Out) :-
    (   is_enum(DB,Elt_Type)
    ->  enum_value(Elt_Type,Value,Enum_Value),
        prefix_expand_schema(Enum_Value, Context, Enum_Value_Ex),
        Prepared = json{'@id' : Enum_Value_Ex,
                        '@type' : "@id"}
    ;   is_dict(Value)
    ->  (   get_dict('@ref', Value, Ref)
        ->  capture_ref(Captures_In, Ref, Capture_Var, Captures_In_1),
            Prepared = json{'@type': "@id", '@id' : Capture_Var},
            Dependencies_1 = [Capture_Var]
        ;   get_dict('@type', Value, _)
        ->  Value = Prepared % existing type could be more specific
        ;   put_dict(json{'@type':Elt_Type}, Value, Prepared))
    ;   is_base_type(Elt_Type)
    ->  Prepared = json{'@value' : Value,
                        '@type': Elt_Type}
    ;   Prepared = json{'@id' : Value,
                        '@type': "@id"}),

    % set up capture defaults if they're unbound at this point
    ignore((Captures_In_1 = Captures_In,
            Dependencies_1 = [])),

    json_elaborate_(DB, Prepared, Context, Captures_In_1, Expanded, Dependencies_2, Captures_In_2),
    value_expand_list(Vs, DB, Context, Elt_Type, Captures_In_2, Exs, Dependencies_3, Captures_Out),
    append([Dependencies_1, Dependencies_2, Dependencies_3], Dependencies).


value_expand_array([Value|Vs], DB, Context, Elt_Type, Captures_In, [Expanded|Exs], Dependencies, Captures_Out) :-
    is_list(Value),
    !,
    value_expand_array(Value, DB, Context, Elt_Type, Captures_In, Expanded, Dependencies_1, Captures_1),
    value_expand_array(Vs, DB, Context, Elt_Type, Captures_1, Exs, Dependencies_2, Captures_Out),
    append([Dependencies_1, Dependencies_2], Dependencies).
value_expand_array(In, DB, Context, Elt_Type, Captures_In, Expanded, Dependencies, Captures_Out) :-
    value_expand_list(In, DB, Context, Elt_Type, Captures_In, Expanded, Dependencies, Captures_Out).

% Unit type expansion
context_value_expand(_,_,Value,json{'@type': Unit_Type},Captures,[],[],Captures) :-
    global_prefix_expand(sys:'Unit', Unit_Type),
    !,
    do_or_die(Value = [],
              error(not_a_unit_type(Value), _)).

context_value_expand(_,_,null,_,Captures,null,[],Captures) :-
    !.
context_value_expand(DB,Context,Value,Expansion,Captures_In,V,Dependencies,Captures_Out) :-
    get_dict('@container', Expansion, "@array"),
    !,
    % Container type
    get_dict('@type', Expansion, Elt_Type),
    (   is_list(Value)
    ->  Value_List = Value
    ;   get_dict('@value',Value,Value_List),
        is_list(Value_List)
    ->  true
    ),
    value_expand_array(Value_List, DB, Context, Elt_Type, Captures_In, Expanded_List, Dependencies, Captures_Out),
    V = (Expansion.put(json{'@value' : Expanded_List})).
context_value_expand(DB,Context,Value,Expansion,Captures_In,V,Dependencies,Captures_Out) :-
    get_dict('@container', Expansion, _),
    !,
    % Container type
    get_dict('@type', Expansion, Elt_Type),
    (   is_list(Value)
    ->  Value_List = Value
    ;   string(Value)
    ->  Value_List = [Value]
    ;   is_dict(Value),
        get_dict('@value',Value,Value_List)
    ->  true
    %   fallthrough case - we were expecting a container but we have
    %   single dictionary which is not a direct value. It must be a
    %   single object which we'll treat as a single element list.
    ;   Value_List = [Value]),
    value_expand_list(Value_List, DB, Context, Elt_Type, Captures_In, Expanded_List, Dependencies, Captures_Out),
    V = (Expansion.put(json{'@value' : Expanded_List})).
context_value_expand(DB,Context,Value,Expansion,Captures_In,V,Dependencies,Captures_Out) :-
    % A possible reference
    get_dict('@type', Expansion, Type),
    (   Type = "@id"
    ;   \+ is_base_type(Type),
        \+ is_enum(DB, Type)),

    !,
    (   is_dict(Value)
    ->  (   get_dict('@ref', Value, Ref)
        % This is a capture reference
        ->  capture_ref(Captures_In, Ref, Capture_Var, Captures_Out),
            V = _{'@id' : Capture_Var, '@type': "@id"},
            Dependencies = [Capture_Var]
        ;   json_elaborate_(DB, Value, Context, Captures_In, V, Dependencies,Captures_Out))
    ;   is_list(Value)
    ->  Value = [Val],
        context_value_expand(DB,Context,Val,Expansion,Captures_In,V,Dependencies,Captures_Out)
    ;   prefix_expand(Value,Context,Value_Ex),
        put_dict(_{'@type': "@id", '@id' : Value_Ex}, Expansion, V),
        % V = _{'@type': "@id", '@id': Value_Ex},
        Dependencies = [],
        Captures_Out = Captures_In
    ).
context_value_expand(_,Context,Value,_Expansion,Captures,V,[],Captures) :-
    % An already expanded typed value
    is_dict(Value),
    get_dict('@value',Value, Elt),
    get_dict('@type',Value, Type),
    prefix_expand(Type,Context,Type_Ex),
    !,
    V = json{'@value' : Elt, '@type' : Type_Ex}.
context_value_expand(DB,Context,Value,Expansion,Captures,V,[],Captures) :-
    get_dict('@type', Expansion, Type),
    is_enum(DB,Type),
    !,
    enum_value(Type,Value,Enum_Value),
    prefix_expand_schema(Enum_Value, Context, Enum_Value_Ex),
    V = json{'@id' : Enum_Value_Ex,
             '@type' : "@id"}.
context_value_expand(DB,Context,Value,Expansion,Captures_In,V,Dependencies,Captures_Out) :-
    get_dict('@type', Expansion, Type),
    \+ is_base_type(Type),
    !,
    (   is_dict(Value)
    ->  json_elaborate_(DB, Value, Context, Captures_In, V, Dependencies,Captures_Out)
    ;   throw(error(this_case_should_not_exist,_)), % while reading this code it seems like this case should not exist. At this point value should have been a dictionary always
        prefix_expand(Value,Context,Value_Ex),
        V = json{ '@type' : "@id", '@id' : Value_Ex},
        Captures_Out = Captures_In,
        Dependencies = []
    ).
context_value_expand(DB,Context,Value,Expansion,Captures_In,V,Dependencies,Captures_Out) :-
    % An unexpanded typed value
    New_Expansion = (Expansion.put(json{'@value' : Value})),
    json_elaborate_(DB,New_Expansion, Context, Captures_In,V,Dependencies,Captures_Out).

enum_value(Type,Value,ID) :-
    ground(Type),
    ground(Value),
    !,
    % First check to see if type is a prefix of value
    atom_string(Value_Atom, Value),
    (   atom_concat(Type, _, Value_Atom)
    ->  ID = Value_Atom
    ;   encode_id_fragment(Value, Encoded_Value),
        atomic_list_concat([Type, '/', Encoded_Value], ID)
    ).
enum_value(Type,Value,ID) :-
    ground(Type),
    !,
    atom_concat(Type,'/',Prefix),
    atom_concat(Prefix,Encoded_Value,ID),
    encode_id_fragment(Value, Encoded_Value).


oneof_value(Val,Context,NewPath,Transformed) :-
    findall(
        Prop-Value,
        (   get_dict(P_Choice,Val,V_Choice),
            json_schema_predicate_value(P_Choice,V_Choice,Context,NewPath,Prop,Value)
        ),
        PVs),
    dict_pairs(Elaborated,json,PVs),
    property_id(Val,Context,NewPath,ID),
    global_prefix_expand(sys:'Choice',Choice_Type),
    put_dict(_{'@id' : ID, '@type' : Choice_Type}, Elaborated, Transformed).

json_context_elaborate(DB, JSON, Context, Captures_In, Expanded, [], Captures_In) :-
    is_dict(JSON),
    get_dict('@type',JSON,Type),
    prefix_expand_schema(Type,Context,Type_Ex),
    is_enum(DB,Type_Ex),
    !,
    get_dict('@value',JSON,Value),
    enum_value(Type,Value,Full_ID),
    Expanded = json{ '@type' : "@id",
                     '@id' : Full_ID }.
json_context_elaborate(DB, JSON, Context, Captures_In, Expanded, Dependencies, Captures_Out) :-
    is_dict(JSON),
    !,
    dict_pairs(JSON,json,Pairs),
    mapm({DB,JSON,Context}/[Prop-Value,P-V,Dependencies,Captures, New_Captures]>>
         (   (   property_expand_key_value(Prop,Value,DB,Context,Captures,P,V,Dependencies,New_Captures)
             ->  true
             ;   Prop = '@type'
             ->  P = Prop,
                 prefix_expand_schema(Value, Context, V)
             ;   has_at(Prop)
             ->  P = Prop,
                 V = Value
             ;   (   get_dict('@type', JSON, Type)
                 ->  throw(error(unrecognized_property(Type,Prop,Value), _))
                 ;   throw(error(unrecognized_untyped_property(Prop,Value), _)))
             ),
             (   var(Dependencies)
             ->  Dependencies = []
             ;   true),
             (   var(New_Captures)
             ->  New_Captures = Captures
             ;   true)
         ),
         Pairs,
         PVs,
         Dependencies_List,
         Captures_In,
         Captures_Out),
    append(Dependencies_List, Dependencies),
    dict_pairs(Expanded,json,PVs).

json_prefix_access(JSON,Edge,Type) :-
    global_prefix_expand(Edge,Expanded),
    get_dict(Expanded,JSON,Type).

json_type(JSON,_Context,Type) :-
    json_prefix_access(JSON,rdf:type,Type).

is_type_family(Dict) :-
    get_dict('@type',Dict,Type_Constructor),
    maybe_expand_schema_type(Type_Constructor,Expanded),
    type_family_constructor(Expanded).

type_family_parts(JSON,['Cardinality',Class,Min_Cardinality,Max_Cardinality]) :-
    get_dict('@type',JSON,"Cardinality"),
    !,
    get_dict('@class',JSON, Class),
    (   get_dict('@cardinality',JSON, Cardinality)
    ->  Min_Cardinality = Cardinality,
        Max_Cardinality = Cardinality
    ;   (   get_dict('@min_cardinality',JSON, Min_Cardinality)
        ->  true
        ;   Min_Cardinality = 0
        ),
        (   get_dict('@max_cardinality',JSON, Max_Cardinality)
        ->  true
        ;   Max_Cardinality = inf
        )
    ).
type_family_parts(JSON,[Family,Class]) :-
    get_dict('@type',JSON, Family),
    get_dict('@class',JSON, Class).

type_family_id(JSON,Context,Path,ID) :-
    path_component(Path,Context,Component),
    type_family_parts(JSON,Parts),
    maplist(encode_id_fragment,Parts,Encoded),
    merge_separator_split(Type_String,'+',Encoded),
    append(Component, [Type_String], Total),
    merge_separator_split(Merged,'/',Total),
    prefix_expand_schema(Merged,Context,ID).

key_parts(JSON,Type,Fields) :-
    get_dict('@type',JSON,Type),
    get_dict('@fields',JSON,Fields),
    !.
key_parts(JSON,Type,[]) :-
    get_dict('@type',JSON,Type).

key_id(JSON,Context,Path,ID) :-
    key_parts(JSON,Type,Fields),
    path_component([type(Type),property(key)|Path], Context, [Path_String]),
    (   Fields = []
    ->  Total = [Path_String]
    ;   maplist(encode_id_fragment,Fields,Encoded),
        merge_separator_split(Fields_String,'+',Encoded),
        Total = [Path_String,Fields_String]
    ),
    merge_separator_split(Merged,'/',Total),
    prefix_expand_schema(Merged,Context,ID).

property_part(JSON,Keys) :-
    dict_pairs(JSON, _, Pairs),
    convlist([Key-_,Key]>>(\+ memberchk(Key, ['@id', '@type'])), Pairs, Keys).

property_id(JSON,Context,Path,ID) :-
    path_component(Path,Context,Component),
    property_part(JSON,Keys),
    maplist(encode_id_fragment,Keys,Encoded),
    merge_separator_split(Key_String,'+',Encoded),
    append(Component,[Key_String], Total),
    merge_separator_split(Merged,'/',Total),
    prefix_expand_schema(Merged,Context,ID).

documentation_id(Context,Path,ID) :-
    path_component([type('Documentation'),property('documentation')|Path], Context, Encoded),
    merge_separator_split(Merged,'/',Encoded),
    prefix_expand_schema(Merged,Context,ID).

maybe_expand_schema_type(Type, Expanded) :-
    (   re_match('.*:.*', Type)
    ->  Type = Expanded
    ;   global_prefix_expand(sys:Type,Expanded)
    ).

is_context(JSON) :-
    get_dict('@type', JSON, "@context").

% NOTE: We probably need the prefixes in play here...
is_type_enum(JSON) :-
    get_dict('@type', JSON, "Enum"),
    !.
is_type_enum(JSON) :-
    global_prefix_expand(sys:'Enum', Enum),
    get_dict('@type', JSON, Enum).

context_triple(JSON,Triple) :-
    context_elaborate(JSON,Elaborated),
    expand(Elaborated,json{
                          sys:'http://terminusdb.com/schema/sys#',
                          xsd:'http://www.w3.org/2001/XMLSchema#',
                          xdd:'http://terminusdb.com/schema/xdd#'
                      },
           Expanded),
    % A small white lie...
    do_or_die(
        get_dict('@base', JSON, Base),
        error(
            schema_check_failure(
                [witness{
                    '@type': context_missing_system_prefix,
                    prefix_name: 'http://terminusdb.com/schema/sys#base'
                }]), _)),
    json_triple_(Expanded,_{'@base' : Base},Triple).

context_keyword_value_map('@type',"@context",'@type','sys:Context').
context_keyword_value_map('@base',Value,'sys:base',json{'@type' : "xsd:string", '@value' : Value}).
context_keyword_value_map('@schema',Value,'sys:schema',json{'@type' : "xsd:string", '@value' : Value}).
context_keyword_value_map('@metadata',JSON,'sys:metadata',Value) :-
    Value = (JSON.put('@type', "sys:JSON")).
context_keyword_value_map('@documentation',Documentation,'sys:documentation',Result) :-
    (   is_list(Documentation) % multilingual
    ->  DocSet = Documentation
    ;   DocSet = [Documentation]
    ),

    verify_languages(DocSet),

    index_list(DocSet,Indexes),
    maplist([Doc,Idx,Res]>>(
                dict_pairs(Doc, json, Pairs),
                findall(P-V,
                        (   member(Keyword-Value,Pairs),
                            \+ Keyword = '@type',
                            context_documentation_value_map(Keyword,Value,P,V)
                        ),
                        PVs),
                atomic_list_concat(
                    ["terminusdb://context/SchemaDocumentation/",Idx], Id),
                dict_pairs(Res,json,['@id'-Id,
                                     '@type'-"sys:SchemaDocumentation"|PVs])),
            DocSet,Indexes,ValueSet),
    Result = json{ '@container' : "@set",
                   '@type' : 'sys:SchemaDocumentation',
                   '@value' : ValueSet }.

context_documentation_value_map('@language',Value,'sys:language',
                                json{'@type' : "xsd:language", '@value' : Value}).
context_documentation_value_map('@title',Value,'sys:title',
                                json{'@type' : "xsd:string", '@value' : Value}).
context_documentation_value_map('@description',Value,'sys:description',
                                json{'@type' : "xsd:string", '@value' : Value}).
context_documentation_value_map('@authors',Value,'sys:authors',json{ '@container' : "@list",
                                                                     '@type' : "xsd:string",
                                                                     '@value' : Value_List }) :-
    maplist([V,JSON]>>(JSON = json{ '@value' : V, '@type' : "xsd:string"}), Value,Value_List).

context_elaborate(JSON,Elaborated) :-
    is_context(JSON),
    !,
    dict_pairs(JSON,json,Pairs),
    partition([P-_]>>(member(P, ['@type', '@base', '@schema',
                                 '@documentation', '@metadata'])),
              Pairs, Keyword_Values, Prop_Values),
    findall(
        P-V,
        (   member(Keyword-Value,Keyword_Values),
            context_keyword_value_map(Keyword,Value,P,V)
        ),
        PVs),

    findall(
        Prefix_Pair,
        (   member(Prop-Value, Prop_Values),
            idgen_hash('terminusdb://Prefix_Pair/',[json{'@value' : Prop},
                                                    json{'@value' : Value}], HashURI),
            Prefix_Pair = json{'@type' : 'sys:Prefix',
                               '@id' : HashURI,
                               'sys:prefix' : json{ '@value' : Prop,
                                                    '@type' : "xsd:string"},
                               'sys:url' : json{ '@value' : Value,
                                                 '@type' : "xsd:string"}
                              }
        ),
        Prefix_Pair_List),

    dict_pairs(Elaborated,json,['@id'-"terminusdb://context",
                                'sys:prefix_pair'-json{ '@container' : "@set",
                                                        '@type' : "sys:Prefix",
                                                        '@value' : Prefix_Pair_List }
                                |PVs]).

wrap_id(ID, json{'@type' : "@id",
                 '@id' : ID}) :-
    (   atom(ID)
    ;   string(ID)),
    !.
wrap_id(ID, ID).

wrap_text(Text, json{'@type' : XSD,
                     '@value' : String}) :-
    global_prefix_expand(xsd:string, XSD),
    (   atom(Text)
    ;   string(Text)),
    !,
    atom_string(Text,String).
wrap_text(Text, Text) :-
    is_dict(Text).

expand_match_system(Key,Term,Key_Ex) :-
    global_prefixes(sys,Prefix),
    global_prefix_expand(sys:Term,Key_Ex),
    prefix_expand(Key, _{'@base' : Prefix}, Key_Ex).

json_schema_elaborate_key(V,_,json{'@type':Value}) :-
    atom(V),
    !,
    global_prefixes(sys,Prefix),
    prefix_expand(V, _{'@base' : Prefix}, Value).
json_schema_elaborate_key(V,Context,Value) :-
    get_dict('@type', V, Candidate),
    (   expand_match_system(Candidate, 'Lexical', Type)
    ;   expand_match_system(Candidate, 'Hash', Type)
    ),
    !,
    (   get_dict('@fields', V, Fields)
    ->  json_schema_elaborate_key_fields(Context, Fields, Fields_Wrapped),
        global_prefix_expand(sys:fields,Field),
        Type_Value = json{ '@type' : Type },
        Value = (Type_Value.put(Field,
                                json{
                                    '@container' : "@list",
                                    '@type' : "@id",
                                    '@value' : Fields_Wrapped
                                }))
    ;   throw(error(key_missing_fields(Candidate),_))
    ).
json_schema_elaborate_key(V,_,json{ '@type' : Type}) :-
    get_dict('@type', V, ValueHash),
    expand_match_system(ValueHash, 'ValueHash', Type),
    !.
json_schema_elaborate_key(V,_,json{ '@type' : Type}) :-
    get_dict('@type', V, Random),
    expand_match_system(Random, 'Random', Type),
    !.
json_schema_elaborate_key(V,_,_) :-
    get_dict('@type', V, Type),
    !,
    throw(error(document_key_type_unknown(Type), _)).
json_schema_elaborate_key(V,_,_) :-
    atom_json_dict(Atom, V, []),
    throw(error(document_key_type_missing(Atom), _)).

json_schema_elaborate_key_fields(_, Fields, _) :-
    \+ is_list(Fields),
    !,
    throw(error(key_fields_not_an_array(Fields), _)).
json_schema_elaborate_key_fields(_, [], _) :-
    throw(error(key_fields_is_empty, _)).
json_schema_elaborate_key_fields(Context, Fields, Fields_Wrapped) :-
    maplist(
        {Context}/[Elt, Elt_ID]>>(
            prefix_expand_schema(Elt, Context, Elt_Ex),
            wrap_id(Elt_Ex,Elt_ID)),
        Fields,
        Fields_Wrapped).

json_schema_elaborate_property_documentation(Context, Path, Dict, Out) :-
    global_prefix_expand(sys:'PropertyDocumentation',Property_Ex),
    property_id(Dict, Context, Path, Id),
    dict_pairs(Dict, json, In_Pairs),
    findall(
        Key-Value,
        (
            member(K-V, In_Pairs),
            (   K = '@id'
            ->  fail
            ;   K = '@type'
            ->  fail
            ;   is_dict(V),
                (   get_dict('@comment', V, _)
                ;   get_dict('@label', V, _))
            ->  prefix_expand_schema(K, Context, Key),
                json_schema_elaborate_documentation_value(Context,
                                                          [property(Key)
                                                           |Path],
                                                          V, Value)
            ;   is_dict(V)
            ->  prefix_expand_schema(K, Context, Key),
                Value = V
            ;   prefix_expand_schema(K, Context, Key),
                wrap_text(V,Value)
            )
        ),
        Pairs),
    dict_pairs(Out, json, ['@id'-Id,'@type'-Property_Ex|Pairs]).

json_schema_elaborate_enum_documentation(Context, Path, Dict, Out) :-
    global_prefix_expand(sys:'EnumDocumentation',Enum_Ex),
    property_id(Dict, Context, Path, Id),
    dict_pairs(Dict, json, In_Pairs),
    reverse(Path,[type(Type)|_]),
    findall(
        Key-Value,
        (
            member(K-V, In_Pairs),
            (   K = '@id'
            ->  fail
            ;   K = '@type'
            ->  fail
            ;   is_dict(V),
                (   get_dict('@comment', V, _)
                ;   get_dict('@label', V, _))
            ->  enum_value(Type,K,Enum_Value),
                prefix_expand_schema(Enum_Value, Context, Key),
                json_schema_elaborate_documentation_value(Context,
                                                          [property(Key)
                                                           |Path],
                                                          V, Value)
            ;   is_dict(V)
            ->  enum_value(Type,K,Enum_Value),
                prefix_expand_schema(Enum_Value, Context, Key),
                Value = V
            ;   enum_value(Type,K,Enum_Value),
                prefix_expand_schema(Enum_Value, Context, Key),
                wrap_text(V,Value)
            )
        ),
        Pairs),
    dict_pairs(Out, json, ['@id'-Id,'@type'-Enum_Ex|Pairs]).

json_schema_elaborate_documentation_value(Context, Path, V, Value2) :-
    global_prefix_expand(sys:'DocumentationLabelComment',Doc_Ex),
    property_id(V, Context, Path, Id),
    global_prefix_expand(sys:'comment',Comment_Ex),
    global_prefix_expand(sys:'label',Label_Ex),
    Value = json{ '@id' : Id, '@type' : Doc_Ex },
    (   get_dict('@comment', V, Comment)
    ->  wrap_text(Comment, Comment_Wrapped),
        put_dict(Comment_Ex, Value, Comment_Wrapped, Value1)
    ;   Value = Value1
    ),
    (   get_dict('@label', V, Label)
    ->  wrap_text(Label, Label_Wrapped),
        put_dict(Label_Ex, Value1, Label_Wrapped, Value2)
    ;   Value2 = Value1
    ).

json_schema_elaborate_documentation(V,Context,Path,json{'@type' : Documentation_Ex,
                                                        '@id' : ID,
                                                        '@comment' :
                                                        json{ '@type' : XSD,
                                                              '@value' : V}}),
string(V) =>
    global_prefix_expand(sys:'Documentation',Documentation_Ex),
    global_prefix_expand(xsd:string, XSD),
    documentation_id(Context,Path,ID).
json_schema_elaborate_documentation(V,Context,Path,Result),
(   is_list(V)
;   is_dict(V)) =>
    global_prefix_expand(sys:'Documentation',Documentation_Ex),
    Result = json{ '@container' : "@set",
                   '@type' : Documentation_Ex,
                   '@value' : VSetElab },
    (   is_list(V)
    ->  VSet = V
    ;   VSet = [V]
    ),

    verify_languages(VSet),

    index_list(VSet, Indexes),
    maplist({Context,Path,Documentation_Ex}/[VElt,Idx,Res5]>>
            (   documentation_id(Context,[index(Idx)|Path],Doc_Id),
                Res = json{'@id' : Doc_Id,
                           '@type' : Documentation_Ex
                          },
                (   get_dict('@language', VElt, Lang)
                ->  do_or_die(
                        iana(Lang,_),
                        error(unknown_language_tag(Lang),_)),
                    global_prefix_expand(sys:language, LangTag),
                    global_prefix_expand(xsd:language, LangType),
                    put_dict(LangTag,Res,json{ '@value' : Lang,
                                               '@type' : LangType},
                             Res1)
                ;   Res1 = Res
                ),

                (   get_dict('@comment', VElt, Comment_Text)
                ->  wrap_text(Comment_Text, Comment),
                    global_prefix_expand(sys:'comment',CommentP),
                    put_dict(CommentP,Res1,Comment,Res2)
                ;   Res2 = Res1
                ),

                (   get_dict('@label', VElt, Label_Text)
                ->  wrap_text(Label_Text, Label),
                    global_prefix_expand(sys:'label',LabelP),
                    put_dict(LabelP,Res2,Label,Res3)
                ;   Res3 = Res2
                ),

                (   get_dict('@properties',VElt,Property_Obj)
                ->  global_prefix_expand(sys:'properties',PropertiesP),
                    json_schema_elaborate_property_documentation(
                        Context,
                        [property('properties'),
                         type('Documentation'),
                         property('documentation'),
                         index(Idx)
                         |Path],
                        Property_Obj,
                        Property_Obj2),
                    put_dict(PropertiesP, Res3, Property_Obj2, Res4)
                ;   Res4 = Res3),

                (   get_dict('@values',VElt,Property_Obj)
                ->  global_prefix_expand(sys:'values',PropertiesP),
                    json_schema_elaborate_enum_documentation(
                        Context,
                        [property('values'),
                         type('Documentation'),
                         property('documentation'),
                         index(Idx)
                         |Path],
                        Property_Obj,
                        Property_Obj2),
                    put_dict(PropertiesP, Res4, Property_Obj2, Res5)
                ;   Res5 = Res4)
            ),
            VSet, Indexes, VSetElab).

json_schema_predicate_value('@id',V,Context,_,'@id',V_Ex) :-
    !,
    prefix_expand_schema(V,Context,V_Ex).
json_schema_predicate_value('@cardinality',V,_,_,P,json{'@type' : Type,
                                                        '@value' : V }) :-
    global_prefix_expand(xsd:nonNegativeInteger,Type),
    !,
    (   global_prefix_expand(sys:max_cardinality, P)
    ;   global_prefix_expand(sys:min_cardinality, P)
    ).
json_schema_predicate_value('@min_cardinality',V,_,_,P,json{'@type' : Type,
                                                            '@value' : V }) :-
    !,
    global_prefix_expand(xsd:nonNegativeInteger,Type),
    global_prefix_expand(sys:min_cardinality, P).
json_schema_predicate_value('@max_cardinality',V,_,_,P,json{'@type' : Type,
                                                            '@value' : V }) :-
    !,
    global_prefix_expand(xsd:nonNegativeInteger,Type),
    global_prefix_expand(sys:max_cardinality, P).
json_schema_predicate_value('@dimensions',V,_,_,P,json{'@type' : Type,
                                                       '@value' : V }) :-
    !,
    global_prefix_expand(xsd:nonNegativeInteger,Type),
    global_prefix_expand(sys:dimensions, P).
json_schema_predicate_value('@key',V,Context,Path,P,Value) :-
    !,
    global_prefix_expand(sys:key, P),
    do_or_die(is_dict(V),
              error(document_key_not_object(V), _)),
    json_schema_elaborate_key(V,Context,Elab),
    key_id(V,Context,Path,ID),
    put_dict(_{'@id' : ID}, Elab, Value).
json_schema_predicate_value('@oneOf',V,Context,Path,P,Set) :-
    !,
    global_prefix_expand(sys:oneOf, P),
    do_or_die((   is_dict(V)
              ->  Vs = [V]
              ;   is_list(V)
              ->  Vs = V),
              error(document_oneof_not_set(V), _)),
    NewPath = [property('oneOf')|Path],
    maplist({Context,NewPath}/[Val,Transformed]>>(
                oneof_value(Val,Context,NewPath,Transformed)
            ),
            Vs,
            Value_List),
    global_prefix_expand(sys:'Choice',Choice_Type),
    Set = json{ '@container' : "@set",
                '@type' : Choice_Type,
                '@value' : Value_List }.
json_schema_predicate_value('@documentation',V,Context,Path,P,Value) :-
    !,
    global_prefix_expand(sys:documentation, P),
    json_schema_elaborate_documentation(V,Context,Path,Value).
json_schema_predicate_value('@abstract',[],_,_,P,[]) :-
    !,
    global_prefix_expand(sys:abstract, P).
json_schema_predicate_value('@subdocument',[],_,_,P,[]) :-
    !,
    global_prefix_expand(sys:subdocument, P).
json_schema_predicate_value('@metadata',V,_,_,P,Value) :-
    !,
    global_prefix_expand(sys:metadata, P),
    global_prefix_expand(sys:'JSON', Type),
    Value = (V.put('@type', Type)).
json_schema_predicate_value('@unfoldable',[],_,_,P,[]) :-
    !,
    global_prefix_expand(sys:unfoldable, P).
json_schema_predicate_value('@base',V,_,_,P,Value) :-
    !,
    global_prefix_expand(sys:base, P),
    (   is_dict(V)
    ->  Value = V
    ;   global_prefix_expand(xsd:string, XSD),
        Value = json{ '@type' : XSD,
                      '@value' : V }
    ).
json_schema_predicate_value('@type',V,_,_,'@type',Value) :-
    !,
    maybe_expand_schema_type(V,Value).
json_schema_predicate_value('@class',V,Context,_,Class,json{'@type' : "@id",
                                                            '@id' : VEx}) :-
    !,
    prefix_expand_schema(V,Context,VEx),
    global_prefix_expand(sys:class, Class).
json_schema_predicate_value(P,V,Context,Path,Prop,Value) :-
    is_dict(V),
    !,
    prefix_expand_schema(P,Context,Prop),
    json_schema_elaborate(V, Context, [property(P)|Path], Value).
json_schema_predicate_value(P,List,Context,_,Prop,Set) :-
    is_list(List),
    !,
    prefix_expand_schema(P,Context,Prop),
    maplist({Context}/[V,Value]>>prefix_expand_schema(V,Context,Value),
            List, Value_List),
    Set = json{ '@container' : "@set",
                '@type' : "@id",
                '@value' : Value_List }.
json_schema_predicate_value(P,V,Context,_,Prop,json{'@type' : "@id",
                                                    '@id' : VEx }) :-
    prefix_expand_schema(P,Context,Prop),
    prefix_expand_schema(V,Context,VEx),
    die_if(
        global_prefix_expand(sys:'JSONDocument',VEx),
        error(json_document_as_range(P,V),_)).


json_schema_elaborate(JSON,Context,Path,Elaborated) :-
    do_or_die(
        get_dict('@type', JSON, Type),
        error(missing_field('@type', JSON), _)),
    check_json_string('@type', Type),
    compress_system_uri(Type,Context,Type_Min),
    do_or_die(
        json_schema_elaborate_(Type_Min,JSON,Context,Path,Elaborated),
        error(schema_type_unknown(Type_Min),_)
    ).

json_schema_elaborate_('Enum',JSON,Context,Old_Path,Elaborated) :-
    !,
    get_dict('@id', JSON, ID),
    prefix_expand_schema(ID,Context,ID_Ex),
    get_dict('@type', JSON, Type),
    maybe_expand_schema_type(Type,Expanded),
    do_or_die(
        (   get_dict('@value', JSON, List),
            maplist({ID_Ex}/[Elt,json{'@type' : "@id",
                                      '@id' : V}]>>(
                        enum_value(ID_Ex,Elt,V)
                    ),List,New_List)
        ),
        error(invalid_enum_values(JSON),_)),

    Type_ID = json{ '@id' : ID_Ex,
                    '@type' : Expanded
                  },
    (   get_dict('@documentation', JSON, Documentation)
    ->  json_schema_predicate_value('@documentation', Documentation,
                                    Context,[type(ID_Ex)|Old_Path],
                                    Doc_Prop,Elaborated_Docs),
        put_dict(Doc_Prop, Type_ID, Elaborated_Docs, Schema_Obj0)
    ;   Schema_Obj0 = Type_ID),

    (   get_dict('@metadata', JSON, Metadata)
    ->  json_schema_predicate_value('@metadata', Metadata,
                                    Context,[],
                                    Metadata_Prop,
                                    Elaborated_Metadata),
        put_dict(Metadata_Prop, Schema_Obj0, Elaborated_Metadata, Schema_Obj)
    ;   Schema_Obj = Schema_Obj0),

    global_prefix_expand(sys:value, Sys_Value),
    Elaborated = (Schema_Obj.put(Sys_Value,
                                 json{ '@container' : "@list",
                                       '@type' : "@id",
                                       '@value' : New_List })).
json_schema_elaborate_(Type,JSON,Context,Old_Path,Elaborated) :-
    memberchk(Type,['Class','TaggedUnion',
                    'Set','List','Optional','Array', 'Cardinality',
                    'Table','Foreign']),
    is_dict(JSON),
    dict_pairs(JSON,json,Pre_Pairs),
    !,
    (   is_type_family(JSON)
    ->  type_family_id(JSON,Context,Old_Path,ID),
        Pairs = ['@id'-ID|Pre_Pairs]
    ;   get_dict('@id',JSON,ID)
    ->  Pairs = Pre_Pairs
    ;   throw(error(missing_field('@id', JSON), _))
    ),
    Path = [type(ID)|Old_Path],
    findall(
        Prop-Value,
        (   member(P-V,Pairs),
            json_schema_predicate_value(P,V,Context,Path,Prop,Value)
        ),
        PVs),
    dict_pairs(Elaborated,json,PVs),
    check_schema_document_restrictions(Elaborated).

check_schema_document_restrictions(Elaborated) :-
    global_prefix_expand(sys:subdocument, SubP),
    \+ get_dict(SubP, Elaborated, _),
    !.
check_schema_document_restrictions(Elaborated) :-
    global_prefix_expand(sys:abstract, AbsP),
    get_dict(AbsP, Elaborated, _),
    !.
check_schema_document_restrictions(Elaborated) :-
    global_prefix_expand(sys:key, KeyP),
    do_or_die(get_dict(KeyP, Elaborated, Key),
              error(subdocument_key_missing, _)),
    % We currently don't reach the following checks because
    % json_schema_elaborate_key throw errors for the same conditions.
    do_or_die(get_dict('@type', Key, Key_Type_String),
              error(subdocument_key_type_missing,_)),
    atom_string(Key_Type, Key_Type_String),
    do_or_die(
        % Is this exhaustive?
        (   global_prefix_expand(sys:'ValueHash',Key_Type)
        ;   global_prefix_expand(sys:'Hash',Key_Type)
        ;   global_prefix_expand(sys:'Lexical',Key_Type)
        ;   global_prefix_expand(sys:'Random',Key_Type)),
        error(subdocument_key_type_unknown(Key_Type_String),_)).

json_schema_elaborate(JSON,Context,JSON_Schema) :-
    json_schema_elaborate(JSON,Context,[],JSON_Schema).

json_schema_triple(JSON,Context,Triple) :-
    do_or_die(
        json_schema_elaborate(JSON,Context,[],JSON_Schema),
        error(unable_to_elaborate_schema_document(JSON),_)),
    json_triple_(JSON_Schema,Context,Triple).

/*
% Triple generator
json_triple(DB,JSON,Triple) :-
    database_prefixes(DB,Context),
    json_triple(DB,Context,JSON,Triple).

json_triple(DB,Context,JSON,Triple) :-
    empty_assoc(Captures),
    json_triple(DB, Context, JSON, Captures, Triple, _Dependencies, _Captures_Out).
json_triple(DB,Context,JSON,Captures_In, Triple, Dependencies, Captures_Out) :-
    json_elaborate(DB,JSON,Context,Captures_In,Elaborated,Dependencies, Captures_Out),
    when(ground(Dependencies),
         json_triple_(Elaborated,Context,Triple)).
*/

json_triples(DB,JSON,Triples) :-
    database_prefixes(DB,Context),
    empty_assoc(Captures),
    json_elaborate(DB,JSON,Context,Captures,Elaborated,Dependencies,_Captures_Out),
    do_or_die(ground(Dependencies),
              error(unbound_capture_groups(Dependencies),_)),
    findall(Triple,
            json_triple_(Elaborated,Context,Triple),
            Triples).

json_triple_(JSON,_,Triple) :-
    is_dict(JSON),
    get_dict('@type', JSON, 'http://terminusdb.com/schema/sys#JSONDocument'),
    !,
    get_dict('@id', JSON, Id),
    json_document_triple(JSON, Id, Triple).
json_triple_(JSON,_,_Triple) :-
    is_dict(JSON),
    get_dict('@value', JSON, _),
    \+ get_dict('@container', JSON, _),
    !,
    fail.
json_triple_(JSON,Context,Triple) :-
    is_dict(JSON),
    !,
    % NOTE: Need to do something with containers separately
    dict_keys(JSON,Keys),

    do_or_die(
        get_dict('@id', JSON, ID),
        error(missing_field('@id', JSON), _)),

    member(Key, Keys),
    get_dict(Key,JSON,Value),
    (   Key = '@id'
    ->  fail
    ;   Key = '@capture'
    ->  fail
    ;   Key = '@ref'
    ->  fail
    ;   Key = '@type', % this is a leaf
        Value = "@id"
    ->  fail
    ;   Key = '@foreign'
    ->  global_prefix_expand(sys:foreign_type, Foreign_Type),
        Triple = t(ID,Foreign_Type,Value)
    ;   Key = '@type'
    ->  global_prefix_expand(rdf:type, RDF_Type),
        Triple = t(ID,RDF_Type,Value)
    ;   Key = '@inherits'
    ->  global_prefix_expand(sys:inherits, SYS_Inherits),
        (   get_dict('@value',Value,Class)
        ->  (   is_dict(Class)
            ->  get_dict('@id', Class, Inherited),
                Triple = t(ID,SYS_Inherits,Inherited)
            ;   is_list(Class)
            ->  member(Inherited, Class),
                Triple = t(ID,SYS_Inherits,Inherited)
            ;   Triple = t(ID,SYS_Inherits,Class))
        ;   get_dict('@id', Value, Inherited)
        ->  Triple = t(ID,SYS_Inherits,Inherited))
    ;   (   Value = []
        ->  global_prefix_expand(rdf:nil,RDF_Nil),
            Triple = t(ID,Key,RDF_Nil)
        ;   Value = null
        ->  fail
        ;   get_dict('@id', Value, Value_ID)
        ->  (   json_triple_(Value, Context, Triple)
            ;   Triple = t(ID,Key,Value_ID)
            )
        ;   get_dict('@container', Value, "@list")
        ->  get_dict('@value', Value, List),
            list_id_key_context_triple(List,Value,ID,Key,Context,Triple)
        ;   get_dict('@container', Value, "@array")
        ->  get_dict('@value', Value, Array),
            get_dict('@dimensions', Value, Dimensions),
            array_id_key_context_triple(Array,Value,Dimensions,ID,Key,Context,Triple)
        ;   get_dict('@container', Value, "@set")
        ->  get_dict('@value', Value, Set),
            set_id_key_context_triple(Set,Value,ID,Key,Context,Triple)
        ;   is_json_datatype(Value)
        ->  del_dict('@type', Value, _, Pure),
            json_subdocument_triple(ID,Key,Pure,Triple)
        ;   value_json(Lit,Value),
            Triple = t(ID,Key,Lit)
        )
    ).

is_json_datatype(Object) :-
    global_prefix_expand(sys:'JSON', Sys_JSON),
    get_dict('@type', Object, Sys_JSON).

:- table level_predicate_name/2.
level_predicate_name(Level, Predicate) :-
    global_prefix_expand(sys:index, SYS_Index),
    (   Level = 1
    ->  Predicate = SYS_Index
    ;   format(atom(Predicate), '~w~q', [SYS_Index,Level])
    ).

array_id_key_context_triple([],_,_,_,_,_,_) :-
    !,
    fail.
array_id_key_context_triple(List,Object,Dimensions,ID,Key,Context,Triple) :-
    get_dict('@base', Context, Base),
    atomic_list_concat([Base,'Array_'], Base_Array),
    list_array_shape(List,Shape),
    do_or_die(length(Shape,Dimensions),
              error(wrong_array_dimensions(List,Dimensions), _)),
    global_prefix_expand(sys:'Array', SYS_Array),
    global_prefix_expand(sys:value, SYS_Value),
    global_prefix_expand(xsd:nonNegativeInteger, XSD_NonNegativeInteger),
    global_prefix_expand(rdf:type, RDF_Type),
    list_array_index_element(List,Indexes,Elt),
    idgen_random(Base_Array,New_ID),
    (   between(1,Dimensions,N),
        level_predicate_name(N,SYS_Index),
        nth1(N,Indexes,Index),
        Triple = t(New_ID, SYS_Index, Index^^XSD_NonNegativeInteger)
    ;   (   is_json_datatype(Object)
        ->  del_dict('@type', Elt, _, Pure),
            json_subdocument_triple(New_ID,SYS_Value,Pure,Triple)
        ;   reference(Elt,Ref),
            Triple = t(New_ID, SYS_Value, Ref)
        ;   json_triple_(Elt,Context,Triple)
        )
    ;   Triple = t(ID, Key, New_ID)
    ;   Triple = t(New_ID, RDF_Type, SYS_Array)
    ).

/* for now assumes uniformity */
list_array_shape([],[]).
list_array_shape([H|T],Dimensions) :-
    (   list_array_shape(H,D)
    ->  Dimensions = [N|D]
    ;   Dimensions = [N]
    ),
    length([H|T],N).

list_array_index_element([], _, _) :-
    !,
    fail.
list_array_index_element(List,Index,Element) :-
    list_array_shape(List,Dimensions),
    list_array_index_element(Dimensions,List,Rev_Index,Element),
    reverse(Rev_Index,Index).

list_array_index_element([],Elt,[],Elt).
list_array_index_element([N|D],L,[I|Idx],Elt) :-
    between(0,N,I),
    nth0(I,L,S),
    list_array_index_element(D,S,Idx,Elt).

set_id_key_context_triple([H|T],Object,ID,Key,Context,Triple) :-
    (   (   is_json_datatype(Object)
        ->  del_dict('@type', H, _, Pure),
            json_subdocument_triple(ID,Key,Pure,Triple)
        ;   reference(H,HRef),
            Triple = t(ID,Key,HRef)
        ;   json_triple_(H,Context,Triple)
        )
    ;   set_id_key_context_triple(T,Object,ID,Key,Context,Triple)
    ).

reference(Dict,ID) :-
    get_dict('@id',Dict, ID),
    !.
reference(Elt,V) :-
    value_json(V,Elt).

list_id_key_context_triple([],_Object,ID,Key,_Context,t(ID,Key,RDF_Nil)) :-
    global_prefix_expand(rdf:nil, RDF_Nil).
list_id_key_context_triple([H|T],Object,ID,Key,Context,Triple) :-
    get_dict('@base', Context, Base),
    atomic_list_concat([Base,'Cons/'], Base_Cons),
    idgen_random(Base_Cons,New_ID),
    (   Triple = t(ID,Key,New_ID)
    ;   global_prefix_expand(rdf:type, RDF_Type),
        global_prefix_expand(rdf:'List', RDF_List),
        Triple = t(New_ID, RDF_Type, RDF_List)
    ;   global_prefix_expand(rdf:first, RDF_First),
        (   is_json_datatype(Object)
        ->  del_dict('@type', H, _, Pure),
            json_subdocument_triple(New_ID,RDF_First,Pure,Triple)
        ;   reference(H,HRef),
            Triple = t(New_ID,RDF_First,HRef)
        ;   json_triple_(H,Context,Triple)
        )
    ;   global_prefix_expand(rdf:rest, RDF_Rest),
        list_id_key_context_triple(T,Object,New_ID,RDF_Rest,Context,Triple)
    ).

rdf_list_list(_Graph, RDF_Nil,[]) :-
    global_prefix_expand(rdf:nil,RDF_Nil),
    !.
rdf_list_list(Graph, Cons,[H|L]) :-
    xrdf(Graph, Cons, rdf:type, rdf:'List'),
    xrdf(Graph, Cons, rdf:first, H),
    xrdf(Graph, Cons, rdf:rest, Tail),
    rdf_list_list(Graph,Tail,L).

array_lists(DB,Id,P,Dimension,Lists) :-
    database_instance(DB,Instance),
    findall(
        Idxs-V,
        (   xrdf(Instance,Id,P,ArrayElement),
            xrdf(Instance,ArrayElement,rdf:type,sys:'Array'),
            xrdf(Instance,ArrayElement,sys:value,V),
            findall(I,
                    (   down_from(Dimension,1,D),
                        % Generate keys in reverse order of dimension
                        % for convenient lexical sorting (z, y, x)
                        level_predicate_name(D,Sys_Index),
                        xrdf(Instance,ArrayElement,Sys_Index,I^^_)
                    ),
                    Idxs)
        ),
        Index_List),
    index_list_array(Index_List,Dimension,Lists).

index_list_array(Index_List,Dimension,Lists) :-
    % dimension is one indexed, position is zero indexed
    % sanity check to avoid infinite recursions
    check_dimension(Index_List, Dimension),
    keysort(Index_List, Index_List_Sorted),
    index_list_array(Index_List_Sorted,[],1,Dimension,0,Lists).

check_dimension([], _).
check_dimension([H-_|_], Dimension) :-
    length(H,Dimension).

index_list_array([], [], _, _, _, []).
index_list_array([Idxs-V|T], Idx_Tail, Dimension, Dimension, I, [V|Result]) :-
    % highest dimension, innermost list
    nth1(Dimension,Idxs,I),
    % indicies match, add value
    !,
    J is I + 1,
    index_list_array(T, Idx_Tail, Dimension, Dimension, J, Result).
index_list_array([Idxs-V|T], [Idxs-V|T], Dimension, Dimension, I, []) :-
    nth1(Dimension,Idxs,K),
    % highest dimension, innermost list
    % indices have gone down again (moved to next dimension)
    K < I,
    !.
index_list_array([Idxs-V|T], Index_Tail, Dimension, Dimension, I, [null|Result]) :-
    !,
    % highest dimension, innermost list
    % indices still rising, add null and recurse
    J is I + 1,
    index_list_array([Idxs-V|T], Index_Tail, Dimension, Dimension, J, Result).
index_list_array([Idxs-V|T], Idx_Tail, D, Dimension, I, [Inner_Result|Result]) :-
    % an outer list, but indices match
    nth1(D,Idxs,I),
    !,
    J is I + 1,
    E is D + 1,
    index_list_array([Idxs-V|T], Idx_Tail1, E, Dimension, 0, Inner_Result),
    index_list_array(Idx_Tail1, Idx_Tail, D, Dimension, J, Result).
index_list_array([Idxs-V|T], [Idxs-V|T], D, _Dimension, I, []) :-
    nth1(D,Idxs,K),
    % an outer list but indices have gone down again
    % indices have gone down again (moved to next dimension)
    K < I,
    !.
index_list_array([Idxs-V|T], Idx_Tail, D, Dimension, I, [null|Result]) :-
    % an outer list, but indices do not match
    % indices do not match, add null and recurse
    J is I + 1,
    index_list_array([Idxs-V|T], Idx_Tail, D, Dimension, J, Result).

:- begin_tests(multidim_array,[concurrent(true)]).

test(one_d, []) :-
    Id_List = [[0]-a,
               [1]-b,
               [2]-c],
    Dim = 1,
    index_list_array(Id_List,Dim,Lists),
    Lists = [a,b,c].

test(one_d_gapped, []) :-
    Id_List = [[0]-a,
               [2]-c],
    Dim = 1,
    index_list_array(Id_List,Dim,Lists),
    Lists = [a,null,c].


test(two_d, []) :-
    Id_List = [[0,0]-a,
               [0,1]-b,
               [1,0]-c,
               [1,1]-d],
    Dim = 2,
    index_list_array(Id_List,Dim,Lists),
    Lists = [[a,b],[c,d]].

test(two_d_gapped, []) :-
    Id_List = [[0,0]-a,
               [0,2]-c,
               [2,0]-g,
               [2,1]-h,
               [2,2]-i],
    Dim = 2,
    index_list_array(Id_List,Dim,Lists),
    Lists = [[a,null,c],null,[g,h,i]].


%      y1        c  -  d
%   .          .      .
% 0  - x1    a  -  b   |
%
% |          |   g |   h
%              .     .
% z1         e  -  f

test(three_d, []) :-
    Id_List = [[0,0,0]-a, [1,0,0]-e,
               [0,1,0]-c, [1,1,0]-g,
               [0,0,1]-b, [1,0,1]-f,
               [0,1,1]-d, [1,1,1]-h],
    Dim = 3,
    index_list_array(Id_List,Dim,Lists),
    Lists = [[[a,b],[c,d]],
             [[e,f],[g,h]]].

:- end_tests(multidim_array).

set_list(DB,Id,P,Set) :-
    % NOTE: This will not give an empty list.
    database_instance(DB,Instance),
    setof(V,xrdf(Instance,Id,P,V),Set),
    !.

list_type_id_predicate_value([],_,_,_,_,_,_,_,_,[]).
list_type_id_predicate_value([O|T],C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,[V|L]) :-
    type_id_predicate_iri_value(C,Id,P,O,Recursion,DB,Prefixes,Compress_Ids,Unfold,V),
    list_type_id_predicate_value(T,C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,L).

array_type_id_predicate_value([],_,_,_,_,_,_,_,_,_,[]).
array_type_id_predicate_value(In,1,C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,Out) :-
    !,
    list_type_id_predicate_value(In,C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,Out).
array_type_id_predicate_value([O|T],D,C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,[V|L]) :-
    E is D - 1,
    array_type_id_predicate_value(O,E,C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,V),
    array_type_id_predicate_value(T,D,C,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,L).

type_id_predicate_iri_value(unit,_,_,_,_,_,_,_,_,[]).
type_id_predicate_iri_value(enum(C,_),_,_,V,_,_,_,_,_,O) :-
    enum_value(C, O, V).
type_id_predicate_iri_value(foreign(_),_,_,Id,_,_,Prefixes,Compress_Ids,_,Value) :-
    (   Compress_Ids = true
    ->  compress_dict_uri(Id, Prefixes, Value)
    ;   Value = Id
    ).
type_id_predicate_iri_value(list(C),Id,P,O,Recursion,DB,Prefixes,Compress_Ids,Unfold,L) :-
    % Probably need to treat enums...
    database_instance(DB,Instance),
    rdf_list_list(Instance,O,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,L).
type_id_predicate_iri_value(array(C,Dim),Id,P,_,Recursion,DB,Prefixes,Compress_Ids,Unfold,L) :-
    array_lists(DB,Id,P,Dim,V),
    type_descriptor(DB,C,Desc),
    array_type_id_predicate_value(V,Dim,Desc,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,L).
type_id_predicate_iri_value(set(C),Id,P,_,Recursion,DB,Prefixes,Compress_Ids,Unfold,L) :-
    set_list(DB,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,L).
type_id_predicate_iri_value(cardinality(C,_),Id,P,_,Recursion,DB,Prefixes,Compress_Ids,Unfold,L) :-
    set_list(DB,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,Recursion,DB,Prefixes,Compress_Ids,Unfold,L).
type_id_predicate_iri_value(class(_),_,_,Id,Recursion,DB,Prefixes,Compress_Ids,Unfold,Value) :-
    (   instance_of(DB, Id, C),
        is_subdocument(DB, C),
        Unfold = true
    ->  call(Recursion, DB, Prefixes, Compress_Ids, Unfold, Id, Value)
    ;   Compress_Ids = true
    ->  compress_dict_uri(Id, Prefixes, Value)
    ;   Value = Id
    ).
type_id_predicate_iri_value(tagged_union(C,_),_,_,Id,Recursion,DB,Prefixes,Compress_Ids,Unfold,Value) :-
    (   instance_of(DB, Id, C),
        is_subdocument(DB, C),
        Unfold = true
    ->  call(Recursion, DB, Prefixes, Compress_Ids, Unfold, Id, Value)
    ;   Compress_Ids = true
    ->  compress_dict_uri(Id, Prefixes, Value)
    ;   Value = Id
    ).
type_id_predicate_iri_value(optional(C),Id,P,O,Recursion,DB,Prefixes,Compress_Ids,Unfold,V) :-
    type_descriptor(DB,C,Desc),
    type_id_predicate_iri_value(Desc,Id,P,O,Recursion,DB,Prefixes,Compress_Ids,Unfold,V).
type_id_predicate_iri_value(base_class(C),_,_,Elt,_,DB,Prefixes,_Compress,_Unfold,V) :-
    % NOTE: This has to treat each variety of JSON value as natively
    % as possible.
    (   C = 'http://terminusdb.com/schema/sys#JSON'
    ->  get_json_object(DB, Elt, V)
    ;   Elt = X^^T
    ->  (   C = T % The type is not just subsumed but identical - no ambiguity.
        ->  value_type_json_type(X,T,V,_)
        ;   value_type_json_type(X,T,D,T2),
            % NOTE: We're always compressing, even if Compress_Ids is false
            % The reason here is that this is a datatype property, not a node of our own.
            % We may want to revisit this logic though.
            compress_dict_uri(T2,Prefixes,T2C),
            V = json{ '@type' : T2C, '@value' : D}
        )
    ;   Elt = X@L
    ->  V = json{ '@value' : X, '@lang' : L}
    ).

compress_dict_uri(URI, Dict, Folded_URI, Options) :-
    (   option(compress_ids(true),Options)
    ->  compress_dict_uri(URI, Dict, Folded_URI)
    ;   URI = Folded_URI).

compress_system_uri(IRI,Prefixes,IRI_Atom,Options) :-
    (   option(compress_ids(true),Options)
    ->  compress_system_uri(IRI,Prefixes,IRI_Atom)
    ;   IRI = IRI_Atom
    ).

expand_system_uri(Prefix:IRI,IRI_Atom,Options) =>
    (   option(compress_ids(true),Options)
    ->  IRI_Atom = IRI
    ;   global_prefix_expand(Prefix:IRI,IRI_Atom)
    ).

compress_system_uri(IRI,Prefixes,IRI_Atom) :-
    put_dict(_{'@base' : 'http://terminusdb.com/schema/sys#'}, Prefixes, Schema_Prefixes),
    compress_dict_uri(IRI,Schema_Prefixes,IRI_Comp),
    atom_string(IRI_Atom, IRI_Comp),
    !.
compress_system_uri(IRI,_Prefixes,IRI_Atom) :-
    atom_string(IRI_Atom, IRI).

compress_schema_uri(IRI,Prefixes,IRI_Atom,Options) :-
    (   option(compress_ids(true), Options)
    ->  compress_schema_uri(IRI,Prefixes,IRI_Atom)
    ;   IRI = IRI_Atom
    ).

compress_schema_uri(IRI,Prefixes,IRI_Comp) :-
    (   get_dict('@schema',Prefixes,Schema),
        put_dict(_{'@base' : Schema}, Prefixes, Schema_Prefixes)
    ->  true
    ;   Prefixes = Schema_Prefixes),
    compress_dict_uri(IRI,Schema_Prefixes,IRI_Comp),
    !.
compress_schema_uri(IRI,_Prefixes,IRI).

get_document_uri(Query_Context, Include_Subdocuments, ID) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document_uri(TO, Include_Subdocuments, ID).
get_document_uri(Desc, Include_Subdocuments, ID) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document_uri(Transaction, Include_Subdocuments, ID).
get_document_uri(DB, Include_Subdocuments, Uri) :-
    is_simple_class(DB, Class),
    (   Include_Subdocuments = true
    ->  true
    ;   \+ is_subdocument(DB, Class)),
    instance_of(DB, Uri, Class).
get_document_uri(DB, _Include_Subdocuments, Uri) :-
    Class = 'http://terminusdb.com/schema/sys#JSONDocument',
    instance_of(DB, Uri, Class).

get_document_uri_by_type(Query_Context, Type, Uri) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document_uri_by_type(TO, Type, Uri).
get_document_uri_by_type(Desc, Type, Uri) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document_by_type(Transaction, Type, Uri).
get_document_uri_by_type(DB, Type, Uri) :-
    database_prefixes(DB,Prefixes),
    (   sub_atom(Type, _, _, _, ':')
    ->  Prefixed_Type = Type
    ;   atomic_list_concat(['@schema', ':', Type], Prefixed_Type)),
    prefix_expand(Prefixed_Type,Prefixes,Type_Ex),

    is_instance2(DB, Uri, Type_Ex).

get_document_by_type(Query_Context, Type, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document_by_type(TO, Type, Document).
get_document_by_type(Desc, Type, Document) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document_by_type(Transaction, Type, Document).
get_document_by_type(DB, Type, Document) :-
    database_prefixes(DB,Prefixes),
    (   sub_atom(Type, _, _, _, ':')
    ->  Prefixed_Type = Type
    ;   atomic_list_concat(['@schema', ':', Type], Prefixed_Type)),
    prefix_expand(Prefixed_Type,Prefixes,Type_Ex),

    is_instance2(DB, Document_Uri, Type_Ex),

    get_document(DB, Document_Uri, Document).

get_document(Resource, Id, Document) :-
    Options = options{
                  compress_ids : true,
                  unfold: true,
                  keep_json_type: false
              },
    get_document(Resource, Id, Document, Options).

get_document(Query_Context, Id, Document, Options) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document(TO, Id, Document, Options).
get_document(Desc, Id, Document, Options) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document(Transaction, Id, Document, Options).
get_document(DB, Id, Document, Options) :-
    database_prefixes(DB,Prefixes),
    get_document(DB, Prefixes, Id, Document, Options).

get_document(DB, Prefixes, Id, Document, Options) :-
    database_instance(DB,Instance),
    prefix_expand(Id,Prefixes,Id_Ex),
    xrdf(Instance, Id_Ex, rdf:type, Class),
    (   Class = 'http://terminusdb.com/schema/sys#JSONDocument'
    ->  get_json_object(DB, Id_Ex, JSON0),
        (   option(keep_json_type(true), Options)
        ->  (   option(compress_ids(true), Options)
            ->  prefix_expand_schema(Class, Prefixes, Class_Ex)
            ;   Class = Class_Ex
            ),
            put_dict(_{'@type': Class_Ex}, JSON0, JSON)
        ;   JSON = JSON0
        ),
        put_dict(_{'@id' : Id}, JSON, Document)
    ;   option(compress_ids(Compress_Ids), Options, true),
        option(unfold(Unfold), Options, true),
        findall(
            Prop-Value,
            (   distinct([P],xrdf(Instance,Id_Ex,P,O)),
                \+ is_built_in(P),

                once(class_predicate_type(DB,Class,P,Type)),
                type_id_predicate_iri_value(Type,Id_Ex,P,O,get_document,DB,Prefixes,Compress_Ids,Unfold,Value),
                compress_schema_uri(P, Prefixes, Prop, Options)
            ),
            Data),
        !,
        compress_dict_uri(Id_Ex, Prefixes, Id_comp, Options),
        compress_schema_uri(Class, Prefixes, Class_comp, Options),
        json_dict_create(Document,['@id'-Id_comp,
                                   '@type'-Class_comp
                                   |Data])
    ).

key_descriptor_json(Descriptor, Prefixes, Result) :-
    key_descriptor_json(Descriptor, Prefixes, Result, [compress_ids(true)]).

% Note: Should strictly expand the lexical/hash/valuehash type under option: compress_ids(false)
key_descriptor_json(lexical(_, Fields), Prefixes, json{ '@type' : "Lexical",
                                                        '@fields' : Fields_Compressed }, Options) :-
    maplist(
        {Prefixes,Options}/[Field,Compressed]>>compress_schema_uri(Field, Prefixes, Compressed, Options),
        Fields,
        Fields_Compressed
    ).
key_descriptor_json(hash(_, Fields), Prefixes, json{ '@type' : "Hash",
                                                     '@fields' : Fields_Compressed },Options) :-
    maplist(
        {Prefixes,Options}/[Field,Compressed]>>compress_schema_uri(Field, Prefixes, Compressed, Options),
        Fields,
        Fields_Compressed
    ).
key_descriptor_json(value_hash(_), _, json{ '@type' : "ValueHash" },_).
key_descriptor_json(random(_), _, json{ '@type' : "Random" },_).

documentation_descriptor_json(Descriptor, Prefixes, Result) :-
    documentation_descriptor_json(Descriptor,Prefixes, Result, [compress_ids(true)]).

documentation_descriptor_json(enum_documentation(Type,Records),
                              Prefixes,
                              JSON, Options) :-
    (   option(compress_ids(true), Options)
    ->  findall(Result,
                (   member(Record, Records),
                    (   get_dict('@values',Record,Elements)
                    ->  dict_pairs(Elements, _, Pairs),
                        maplist({Type,Prefixes}/[Enum-X,Small-X]>>(
                                    enum_value(Type,Val,Enum),
                                    compress_schema_uri(Val, Prefixes, Small)
                                ),
                                Pairs,
                                JSON_Pairs),
                        dict_pairs(JSON,json,JSON_Pairs),
                        Result = (Record.put('@values', JSON))
                    ;   Result = Record
                    )
                ),
                Results)
    ;   Results = Records
    ),
    (   Results = [JSON]
    ->  true
    ;   Results = []
    ->  false
    ;   Results = JSON
    ).
documentation_descriptor_json(property_documentation(Records),
                              Prefixes,
                              JSON, Options) :-
    (   option(compress_ids(true), Options)
    ->  findall(Result,
                (   member(Record, Records),
                    (   get_dict('@properties',Record,Elements)
                    ->  dict_pairs(Elements, _, Pairs),
                        maplist({Prefixes}/[Prop-X,Small-X]>>(
                                    compress_schema_uri(Prop, Prefixes, Small)
                                ),
                                Pairs,
                                JSON_Pairs),
                        dict_pairs(JSON,json,JSON_Pairs),
                        Result = (Record.put('@properties', JSON))
                    ;   Result = Record
                    )
                ),
                Results)
    ;   Results = Records
    ),
    (   Results = [JSON]
    ->  true
    ;   Results = []
    ->  false
    ;   Results = JSON
    ).

oneof_descriptor_json(Descriptor, Prefixes, JSON) :-
    oneof_descriptor_json(Descriptor, Prefixes, JSON, [compress_ids(true)]).

oneof_descriptor_json(tagged_union(_, Map), Prefixes, JSON, Options) :-
    dict_pairs(Map, _, Pairs),
    maplist({Options,Prefixes}/[Prop-Val,Small-Small_Val]>>(
                compress_schema_uri(Prop, Prefixes, Small, Options),
                type_descriptor_json(Val, Prefixes, Small_Val, Options)
            ),
            Pairs,
            JSON_Pairs),
    dict_pairs(JSON,json,JSON_Pairs).

type_descriptor_json(Type, Prefix, JSON) :-
    type_descriptor_json(Type, Prefix, JSON, [compress_ids(true)]).

type_descriptor_json(unit, _Prefix, Unit, Options) :-
    (   option(compress_ids(true), Options)
    ->  Unit = 'sys:Unit'
    ;   global_prefix_expand(sys:'Unit', Unit)
    ).
type_descriptor_json(class(C), Prefixes, Class_Comp, Options) :-
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(foreign(C), Prefixes, json{ '@type' : Foreign,
                                                 '@class' : Class_Comp }, Options) :-
    expand_system_uri(sys:'Foreign', Foreign, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(base_class(C), Prefixes, Class_Comp, Options) :-
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(optional(C), Prefixes, json{ '@type' : Optional,
                                                  '@class' : Class_Comp }, Options) :-
    expand_system_uri(sys:'Optional', Optional, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(set(C), Prefixes, json{ '@type' : Set,
                                             '@class' : Class_Comp }, Options) :-
    expand_system_uri(sys:'Set', Set, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(array(C,D), Prefixes, json{ '@type' : Array,
                                                 '@dimensions' : D,
                                                 '@class' : Class_Comp }, Options) :-
    expand_system_uri(sys:'Array', Array, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(list(C), Prefixes, json{ '@type' : List,
                                              '@class' : Class_Comp }, Options) :-
    expand_system_uri(sys:'List', List, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(tagged_union(C,_), Prefixes, Class_Comp, Options) :-
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(enum(C,_),Prefixes, Class_Comp, Options) :-
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_json(cardinality(C,Min,Max), Prefixes, json{ '@type' : Card,
                                                             '@class' : Class_Comp,
                                                             '@min' : Min,
                                                             '@max' : Max
                                                           }, Options) :-
    expand_system_uri(sys:'Cardinality', Card, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).

schema_subject_predicate_object_key_value(_,_,_Id,P,O^^_,'@base',O) :-
    global_prefix_expand(sys:base,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,_,'@subdocument',[]) :-
    global_prefix_expand(sys:subdocument,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,_,'@unfoldable',[]) :-
    global_prefix_expand(sys:unfoldable,P),
    !.
schema_subject_predicate_object_key_value(Schema,Prefixes,Id,P,_,'@inherits',V) :-
    global_prefix_expand(sys:inherits,P),
    findall(Parent,
            (   xrdf(Schema, Id, sys:inherits, O),
                compress_schema_uri(O, Prefixes, Parent)
            ),
            Parent_List),
    (   Parent_List = [V]
    ->  true
    ;   Parent_List = V),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,O^^_,'@schema',O) :-
    global_prefix_expand(sys:schema,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,_,'@abstract',[]) :-
    global_prefix_expand(sys:abstract,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,O,'@class',O) :-
    global_prefix_expand(sys:class,P),
    !.
schema_subject_predicate_object_key_value(Schema,_,Id,P,O,'@value',Enum_List) :-
    global_prefix_expand(sys:value,P),
    !,
    rdf_list_list(Schema, O, L),
    maplist({Id}/[V,Enum]>>(
                enum_value(Id,Enum,V)
            ), L, Enum_List).
schema_subject_predicate_object_key_value(Schema,Prefixes,Id,P,_,'@key',V) :-
    global_prefix_expand(sys:key,P),
    !,
    schema_key_descriptor(Schema, Prefixes, Id, Key),
    key_descriptor_json(Key,Prefixes,V).
schema_subject_predicate_object_key_value(Schema,Prefixes,Id,P,_,'@documentation',V) :-
    global_prefix_expand(sys:documentation,P),
    !,
    schema_documentation_descriptor(Schema, Id, Documentation_Desc),
    documentation_descriptor_json(Documentation_Desc,Prefixes,V).
schema_subject_predicate_object_key_value(Schema,_Prefixes,Id,P,_,'@metadata',V) :-
    global_prefix_expand(sys:metadata,P),
    !,
    schema_metadata_descriptor(Schema,Id,metadata(V)).
schema_subject_predicate_object_key_value(Schema,Prefixes,Id,P,_,'@oneOf',V) :-
    global_prefix_expand(sys:oneOf,P),
    !,
    findall(
        Val,
        (   schema_oneof_descriptor(Schema, Id, Descriptor),
            oneof_descriptor_json(Descriptor, Prefixes, Val)),
        Vs),
    (   Vs = [V]
    ->  true
    ;   V = Vs).
schema_subject_predicate_object_key_value(Schema,Prefixes,_Id,P,O,K,JSON) :-
    compress_schema_uri(P, Prefixes, K),
    schema_type_descriptor(Schema, O, Descriptor),
    type_descriptor_json(Descriptor,Prefixes,JSON).

get_schema_document_uri(Query_Context, ID) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_schema_document_uri(TO, ID).
get_schema_document_uri(Desc, ID) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_schema_document_uri(Transaction, ID).
get_schema_document_uri(_DB, '@context').
get_schema_document_uri(DB, Uri) :-
    is_simple_class(DB, Uri).

get_schema_document(Query_Context, Id, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_schema_document(TO, Id, Document).
get_schema_document(Desc, Id, Document) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc, Transaction),
    get_schema_document(Transaction, Id, Document).
get_schema_document(DB, '@context', Document) :-
    !,
    database_context_object(DB, Context_Object),
    % TODO: should database_prefixes even return an object where type is Context instead of @context?
    Document = (Context_Object.put('@type', '@context')).
get_schema_document(DB, Id, Document) :-
    database_prefixes(DB, DB_Prefixes),
    default_prefixes(Defaults),
    Prefixes = (Defaults.put(DB_Prefixes)),
    database_schema(DB,Schema),
    id_schema_json(Schema, Prefixes, Id, Document).

get_schema_document_uri_by_type(DB, Type, Uri) :-
    default_prefixes(Prefixes),
    database_schema(DB,Schema),
    (   ground(Type)
    ->  prefix_expand_schema(Type, Prefixes, Type_Ex)
    ;   Type = Type_Ex
    ),

    xrdf(Schema, Uri, rdf:type, Type_Ex).

get_schema_document_by_type(DB, Type, Document) :-
    default_prefixes(Prefixes),
    database_schema(DB,Schema),
    (   ground(Type)
    ->  prefix_expand_schema(Type, Prefixes, Type_Ex)
    ;   Type = Type_Ex
    ),

    xrdf(Schema, Id_Ex, rdf:type, Type_Ex),
    id_schema_json(Schema, Prefixes, Id_Ex, Document).

id_schema_json(Schema, Id, JSON) :-
    default_prefixes(Defaults),
    id_schema_json(Schema, Defaults, Id, JSON).

id_schema_json(Schema, Prefixes, Id, JSON) :-
    (   ground(Id)
    ->  prefix_expand_schema(Id, Prefixes, Id_Ex)
    ;   Id = Id_Ex
    ),

    xrdf(Schema, Id_Ex, rdf:type, Class),

    findall(
        K-V,
        (   distinct([P],xrdf(Schema,Id_Ex,P,O)),
            schema_subject_predicate_object_key_value(Schema,Prefixes,Id_Ex,P,O,K,V)
        ),
        Data),
    !,

    compress_schema_uri(Id_Ex, Prefixes, Id_Compressed),
    compress_schema_uri(Class, Prefixes, Class_Compressed),
    (   atom_concat('sys:',Small_Class, Class_Compressed)
    ->  true
    ;   Small_Class = Class_Compressed),
    json_dict_create(JSON,['@id'-Id_Compressed,
                           '@type'-Small_Class
                           |Data]).

validate_created_graph(schema, Layer) :-
    Validation_Object = validation_object{
                            descriptor: fake{},
                            instance_objects: [],
                            schema_objects: [graph_validation_obj{
                                                 descriptor: fake{},
                                                 read: Layer,
                                                 changed: true
                                             }]
                        },

    (   refute_schema(Validation_Object, Witness)
    ->  throw(error(schema_validation_error(Witness), _))
    ;   true).
validate_created_graph(instance(Transaction), Layer) :-
    Validation_Object = validation_object{
                            descriptor: fake{},
                            schema_objects: (Transaction.schema_objects),
                            instance_objects: [graph_validation_obj{
                                                 descriptor: fake{},
                                                 read: Layer,
                                                 changed: true
                                             }]
                        },

    (   refute_instance(Validation_Object, Witness)
    ->  throw(error(instance_validation_error(Witness), _))
    ;   true).

%%
% create_graph_from_json(+Store,+Graph_ID,+JSON_Stream,+Type:graph_type,-Layer) is det.
%
% Type := instance | schema(Database)
%
create_graph_from_json(Store, Graph_ID, JSON_Stream, Type, Layer) :-
    safe_create_named_graph(Store,Graph_ID,Graph_Obj),
    open_write(Store, Builder),

    write_json_stream_to_builder(JSON_Stream, Builder, Type),
    % commit this builder to a temporary layer to perform a diff.
    nb_commit(Builder,Layer),
    validate_created_graph(Type, Layer),
    nb_set_head(Graph_Obj, Layer).

replace_json_schema(Transaction, Stream) :-
    is_transaction(Transaction),
    !,
    database_schema(Transaction, [Schema]),
    delete_all(Schema),
    read_write_obj_builder(Schema, Builder),
    write_json_stream_to_builder(Stream, Builder, schema).
replace_json_schema(Query_Context, Stream) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    replace_json_schema(TO, Stream).

json_read_required_context(Stream, Context, Tail_Stream) :-
    % Read a new context from the list or stream and replace the existing one.
    do_or_die(
        (   json_read_list_stream_head(Stream, Context, Tail_Stream),
            is_dict(Context),
            get_dict('@type', Context, "@context")
        ),
        error(no_context_found_in_schema, _)).

write_json_stream_to_builder(Stream, Builder, schema) :-
    !,
    json_read_required_context(Stream, Context, Tail_Stream),

    % TODO: if people submit garbage, this is the place we first encounter said garbage.
    % If it's not valid json, or somewhat valid json but not json of the expected format, this will most likely give a very cryptic error.
    % This error needs to be less cryptic.

    insert_into_builder_context_document(Builder, Context),

    default_prefixes(Prefixes),
    put_dict(Context,Prefixes,Expanded_Context),

    forall(
        json_read_tail_stream(Tail_Stream, Dict),
        (
            forall(
                json_schema_triple(Dict,Expanded_Context,t(S,P,O)),
                (
                    object_storage(O,OS),
                    nb_add_triple(Builder, S, P, OS)
                )
            )
        )
    ).
write_json_stream_to_builder(JSON_Stream, Builder, instance(DB)) :-
    database_prefixes(DB,Context),
    empty_assoc(Captures_In),
    write_json_instance_stream_to_builder(JSON_Stream, Builder, DB, Context, Captures_In, Captures_Out),
    do_or_die(ground(Captures_Out),
              error(not_all_captures_ground(Captures_Out),_)).

write_json_instance_stream_to_builder(JSON_Stream, Builder, DB, Context, Captures_In, Captures_Out) :-
    json_read_term(JSON_Stream, Dict),
    !,
    json_elaborate(DB,Dict,Context,Captures_In,Elaborated,Dependencies,New_Captures_In),

    when(ground(Dependencies),
         forall(
             json_triple_(Elaborated,Context,t(S,P,O)),
             (
                 object_storage(O,OS),
                 nb_add_triple(Builder, S, P, OS)
             )
         )),

    write_json_instance_stream_to_builder(JSON_Stream, Builder, DB, Context, New_Captures_In, Captures_Out).
write_json_instance_stream_to_builder(_JSON_Stream, _Builder, _DB, _Context, Captures, Captures).

write_json_stream_to_schema(Transaction, Stream) :-
    transaction_object{} :< Transaction,
    !,
    [RWO] = (Transaction.schema_objects),
    read_write_obj_builder(RWO, Builder),

    write_json_stream_to_builder(Stream, Builder, schema).

write_json_stream_to_schema(Context, Stream) :-
    query_context{transaction_objects: [Transaction]} :< Context,
    write_json_stream_to_schema(Transaction, Stream).

write_json_stream_to_instance(Transaction, Stream) :-
    transaction_object{} :< Transaction,
    !,
    [RWO] = (Transaction.instance_objects),
    read_write_obj_builder(RWO, Builder),

    write_json_stream_to_builder(Stream, Builder, schema(Transaction)).

write_json_stream_to_instance(Context, Stream) :-
    query_context{transaction_objects: [Transaction]} :< Context,
    write_json_stream_to_instance(Transaction, Stream).

write_json_string_to_schema(Context, String) :-
    open_string(String, Stream),
    write_json_stream_to_schema(Context, Stream).

write_json_string_to_instance(Context, String) :-
    open_string(String, Stream),
    write_json_stream_to_instance(Context, Stream).

json_to_database_type(D^^T, _) :-
    is_list(D),
    !,
    throw(error(unexpected_array_value(D,T),_)).
json_to_database_type(D^^T, OC) :-
    string(D),
    !,
    typecast(D^^'http://www.w3.org/2001/XMLSchema#string', T, [], OC).
json_to_database_type(D^^T, OC) :-
    number(D),
    !,
    typecast(D^^'http://www.w3.org/2001/XMLSchema#decimal', T, [], OC).
json_to_database_type(O, O).

%% Document insert / delete / update

run_delete_document(Desc, Commit, ID) :-
    create_context(Desc,Commit,Context),
    with_transaction(
        Context,
        delete_document(Context, ID),
        _).

delete_subdocument(DB, Prefixes, V) :-
    (   atom(V),
        instance_of(DB, V, C)
    ->  (   is_subdocument(DB, C)
        ->  key_descriptor(DB, Prefixes, C, Descriptor),
            (   memberchk(Descriptor,[lexical(_,_),hash(_,_),random(_)])
            ->  delete_document(DB, V)
            ;   true)
        ;   is_list_type(C)
        ->  delete_document(DB, V)
        ;   true
        )
    ;   true).

delete_document(DB, Prefixes, Unlink, Id) :-
    database_instance(DB,Instance),
    prefix_expand(Id,Prefixes,Id_Ex),
    (   xrdf(Instance, Id_Ex, rdf:type, Type)
    ->  true
    ;   throw(error(document_not_found(Id), _))
    ),
    (   Type = 'http://terminusdb.com/schema/sys#JSONDocument'
    ->  delete_json_object(DB, Prefixes, Unlink, Id)
    ;   forall(
            xquad(Instance, G, Id_Ex, P, V),
            (   delete(G, Id_Ex, P, V, _),
                delete_subdocument(DB,Prefixes,V)
            )
        ),
        (   Unlink = true
        ->  unlink_object(Instance, Id_Ex)
        ;   true)
    ).

delete_document(DB, Unlink, Id) :-
    is_transaction(DB),
    !,
    database_prefixes(DB,Prefixes),
    delete_document(DB, Prefixes, Unlink, Id).
delete_document(Query_Context, Unlink, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    delete_document(TO, Unlink, Id).

delete_document(DB, Id) :-
    delete_document(DB, true, Id).

nuke_documents(Transaction) :-
    is_transaction(Transaction),
    !,
    database_instance(Transaction, [Instance]),
    delete_all(Instance).
nuke_documents(Query_Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    nuke_documents(TO).

nuke_schema_documents(Transaction) :-
    is_transaction(Transaction),
    !,
    database_schema(Transaction, Schema),
    forall(
        (   xrdf(Schema, Id, rdf:type, Type),
            is_system_class(Type)),
        delete_schema_document(Transaction, Id)
    ).
nuke_schema_documents(Query_Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    nuke_documents(TO).

check_existing_document_status(Transaction, Document, Status) :-
    get_dict('@type', Document, Type),
    get_dict('@id', Document, Id),
    database_instance(Transaction, Instance),
    (   key_descriptor(Transaction, Type, value_hash(_))
    ->  (   xrdf(Instance, Id, _, _)
        ->  Status = equivalent
        ;   Status = not_present)
    ;   (   xrdf(Instance, Id, _, _)
        ->  Status = present
        ;   Status = not_present)
    ).

insert_document(Transaction, Document, ID) :-
    insert_document(Transaction, Document, false, ID).

insert_document(Transaction, Document, Raw_JSON, ID) :-
    empty_assoc(Captures_In),
    insert_document(Transaction, Document, Raw_JSON, Captures_In, ID, _Dependencies, _Captures_Out).

valid_json_id_or_die(Prefixes,Id) :-
    do_or_die(
        (   get_dict('@base', Prefixes, Base),
            atomic_list_concat(['^',Base,'JSONDocument/.*'],Re),
            re_match(Re,Id)),
        error(not_a_valid_json_object_id(Id),_)).

insert_document(Transaction, Pre_Document, Raw_JSON, Captures, Id, [], Captures) :-
    is_transaction(Transaction),
    (   Raw_JSON = true,
        Pre_Document = Document
    ;   get_dict('@type', Pre_Document, String_Type),
        prefix_expand(String_Type,
                      _{ 'sys' : 'http://terminusdb.com/schema/sys#'},
                      'http://terminusdb.com/schema/sys#JSONDocument'),
        del_dict('@type', Pre_Document, _, Document)
    ),
    !,
    (   del_dict('@id', Document, Id_Short, JSON)
    ->  database_prefixes(Transaction, Prefixes),
        prefix_expand(Id_Short,Prefixes,Id),
        valid_json_id_or_die(Prefixes,Id),
        insert_json_object(Transaction, JSON, Id)
    ;   insert_json_object(Transaction, Document, Id)
    ).
insert_document(Transaction, Document, false, Captures_In, ID, Dependencies, Captures_Out) :-
    is_transaction(Transaction),
    !,
    json_elaborate(Transaction, Document, Captures_In, Elaborated, Dependencies, Captures_Out),
    % Are we trying to insert a subdocument?
    do_or_die(
        get_dict('@type', Elaborated, Type),
        error(missing_field('@type', Elaborated), _)),
    die_if(is_subdocument(Transaction, Type),
           error(inserted_subdocument_as_document, _)),

    % After elaboration, the Elaborated document will have an '@id'
    do_or_die(
        get_dict('@id', Elaborated, ID),
        error(missing_field('@id', Elaborated), _)),

    ensure_transaction_has_builder(instance, Transaction),
    when(ground(Dependencies),
         (
             check_existing_document_status(Transaction, Elaborated, Status),
             (   Status = not_present
             ->  insert_document_expanded(Transaction, Elaborated, ID)
             ;   Status = equivalent
             ->  true
             ;   Status = present
             ->  throw(error(can_not_insert_existing_object_with_id(ID), _))
             )
         )).
insert_document(Query_Context, Document, Raw_JSON, Captures_In, ID, Dependencies, Captures_Out) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_document(TO, Document, Raw_JSON, Captures_In, ID, Dependencies, Captures_Out).

insert_document_unsafe(Transaction, Prefixes, Document, true, Captures, Id, Captures) :-
    (   del_dict('@id', Document, Id_Short, JSON)
    ->  prefix_expand(Id_Short,Prefixes,Id),
        valid_json_id_or_die(Prefixes,Id),
        insert_json_object(Transaction, JSON, Id)
    ;   insert_json_object(Transaction, Document, Id)
    ).
insert_document_unsafe(Transaction, Context, Document, false, Captures_In, Id, Captures_Out) :-
    json_elaborate(Transaction, Document, Context, Captures_In, Elaborated, _Dependencies, Captures_Out),
    % Are we trying to insert a subdocument?
    do_or_die(
        get_dict('@type', Elaborated, Type),
        error(missing_field('@type', Elaborated), _)),
    die_if(
        is_subdocument(Transaction, Type),
        error(inserted_subdocument_as_document, _)),
    % After elaboration, the Elaborated document will have an '@id'
    do_or_die(
        get_dict('@id', Elaborated, Id),
        error(missing_field('@id', Elaborated), _)),
    insert_document_expanded(Transaction, Elaborated, Id).

insert_document_expanded(Transaction, Elaborated, ID) :-
    get_dict('@id', Elaborated, ID),
    database_instance(Transaction, [Instance]),
    database_prefixes(Transaction, Context),
    % insert
    forall(
        json_triple_(Elaborated, Context, t(S,P,O)),
        (   json_to_database_type(O,OC),
            insert(Instance, S, P, OC, _))
    ).

run_insert_document(Desc, Commit, Document, ID) :-
    create_context(Desc,Commit,Context),
    with_transaction(
        Context,
        insert_document(Context, Document, ID),
        _).

replace_document(DB, Document) :-
    replace_document(DB, Document, false, false, _).

replace_document(DB, Document, Id) :-
    replace_document(DB, Document, false, false, Id).

replace_document(Transaction, Document, Create, Raw_JSON, Id) :-
    empty_assoc(Captures),
    replace_document(Transaction, Document, Create, Raw_JSON, Captures, Id, _Dependencies, _Captures_Out).

is_json_hash(Id) :-
    re_match('^terminusdb:///json/JSON/.*', Id).

replace_document(Transaction, Document, Create, true, Captures, Id, [], Captures) :-
    is_transaction(Transaction),
    !,
    database_prefixes(Transaction, Prefixes),
    do_or_die(
        (   del_dict('@id', Document, Id, JSON)
        ->  prefix_expand(Id, Prefixes, Id_Ex),
            \+ is_json_hash(Id_Ex)
        ),
        error(can_not_replace_at_hashed_id(Document), _)),
    catch(delete_json_object(Transaction, false, Id_Ex),
          error(document_not_found(_), _),
          do_or_die(
              Create = true,
              error(document_not_found(Id, Document), _))),
    insert_json_object(Transaction, JSON, Id_Ex).
replace_document(Transaction, Document, Create, false, Captures_In, Id, Dependencies, Captures_Out) :-
    is_transaction(Transaction),
    !,
    database_prefixes(Transaction, Context),
    json_elaborate(Transaction, Document, Context, Captures_In, Elaborated, Dependencies, Captures_Out),
    get_dict('@id', Elaborated, Elaborated_Id),
    check_submitted_id_against_generated_id(Context, Elaborated_Id, Id),
    catch(delete_document(Transaction, false, Id),
          error(document_not_found(_), _),
          (   Create = true
          % If we're creating a document, we gotta be sure that it is not a subdocument
          ->  get_dict('@type', Elaborated, Type),
              die_if(is_subdocument(Transaction, Type),
                     error(inserted_subdocument_as_document, _))
          ;   throw(error(document_not_found(Id, Document), _)))),
    ensure_transaction_has_builder(instance, Transaction),
    when(ground(Dependencies),
         insert_document_expanded(Transaction, Elaborated, Id)).
replace_document(Query_Context, Document, Create, Raw_JSON, Captures_In, Id, Dependencies, Captures_Out) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    replace_document(TO, Document, Create, Raw_JSON, Captures_In, Id, Dependencies, Captures_Out).

run_replace_document(Desc, Commit, Document, Id) :-
    create_context(Desc,Commit,Context),
    with_transaction(
        Context,
        replace_document(Context, Document, false, false, Id),
        _).

% Frames
type_descriptor_sub_frame(Type,DB,Prefix,Frame) :-
    type_descriptor_sub_frame(Type,DB,Prefix,true,Frame).

type_descriptor_sub_frame(unit, _DB, _Prefix, Unit, Options) :-
    (   option(compress_ids(true), Options)
    ->  Unit = 'sys:Unit'
    ;   global_prefix_expand(sys:'Unit', Unit)
    ).
type_descriptor_sub_frame(class(C), DB, Prefixes, Frame, Options) :-
    (   is_abstract(DB, C),
        option(expand_abstract(true), Options)
    ->  findall(F,
                (   concrete_subclass(DB,C,Class),
                    type_descriptor(DB, Class, Desc),
                    type_descriptor_sub_frame(Desc,DB,Prefixes,F,Options)
                ),
                Frame)
    ;   is_subdocument(DB,C)
    ->  compress_schema_uri(C, Prefixes, Class_Comp, Options),
        Frame = json{ '@class' : Class_Comp,
                      '@subdocument' : []}
    ;   compress_schema_uri(C, Prefixes, Frame, Options)
    ).
type_descriptor_sub_frame(base_class(C), _DB, Prefixes, Class_Comp, Options) :-
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_sub_frame(foreign(C), _DB, Prefixes, json{ '@type' : Foreign,
                                                           '@class' : Class_Comp },
                          Options) :-
    expand_system_uri(sys:'Foreign', Foreign, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options).
type_descriptor_sub_frame(optional(C), DB, Prefixes, json{ '@type' : Optional,
                                                           '@class' : Frame }, Options) :-
    expand_system_uri(sys:'Optional', Optional, Options),
    type_descriptor(DB, C, Desc),
    type_descriptor_sub_frame(Desc, DB, Prefixes, Frame, Options).
type_descriptor_sub_frame(set(C), DB, Prefixes, json{ '@type' : Set,
                                                      '@class' : Frame }, Options) :-
    expand_system_uri(sys:'Set', Set, Options),
    type_descriptor(DB, C, Desc),
    type_descriptor_sub_frame(Desc, DB, Prefixes, Frame, Options).
type_descriptor_sub_frame(array(C,Dim), DB, Prefixes, json{ '@type' : Array,
                                                            '@dimensions' : Dim,
                                                            '@class' : Frame }, Options) :-
    expand_system_uri(sys:'Array', Array, Options),
    type_descriptor(DB, C, Desc),
    type_descriptor_sub_frame(Desc, DB, Prefixes, Frame, Options).
type_descriptor_sub_frame(list(C), DB, Prefixes, json{ '@type' : List,
                                                       '@class' : Frame }, Options) :-
    expand_system_uri(sys:'List', List, Options),
    type_descriptor(DB, C, Desc),
    type_descriptor_sub_frame(Desc, DB, Prefixes, Frame, Options).
type_descriptor_sub_frame(cardinality(C,Min,Max), DB, Prefixes, json{ '@type' : Card,
                                                                      '@class' : Frame,
                                                                      '@min' : Min,
                                                                      '@max' : Max
                                                                    }, Options) :-
    expand_system_uri(sys:'Cardinality', Card, Options),
    type_descriptor(DB, C, Desc),
    type_descriptor_sub_frame(Desc, DB, Prefixes, Frame, Options).
type_descriptor_sub_frame(enum(C,List), _DB, Prefixes, json{ '@type' : Enum,
                                                             '@id' : Class_Comp,
                                                             '@values' : Enum_List}, Options) :-
    expand_system_uri(sys:'Enum', Enum, Options),
    compress_schema_uri(C, Prefixes, Class_Comp, Options),
    (   option(compress_ids(true), Options)
    ->  maplist({C}/[V,Enum]>>(
                    enum_value(C,Enum,V)
                ), List, Enum_List)
    ;   List = Enum_List
    ).

oneof_descriptor_subframe(tagged_union(_, Map), DB, Prefixes, JSON, Options) :-
    dict_pairs(Map, _, Pairs),
    maplist({DB,Options,Prefixes}/[Prop-Val,Small-Small_Val]>>(
                compress_schema_uri(Prop, Prefixes, Small, Options),
                type_descriptor_sub_frame(Val, DB, Prefixes, Small_Val, Options)
            ),
            Pairs,
            JSON_Pairs),
    dict_pairs(JSON,json,JSON_Pairs).

all_class_frames(Askable, Frames) :-
    all_class_frames(Askable, Frames, [compress_ids(true),expand_abstract(true)]).

all_class_frames(Transaction, Frames, Options) :-
    (   is_transaction(Transaction)
    ;   is_validation_object(Transaction)
    ),
    !,
    database_prefixes(Transaction, Prefixes),
    findall(
        Class_Comp-Frame,
        (   is_simple_class(Transaction, Class),
            compress_schema_uri(Class, Prefixes, Class_Comp, Options),
            class_frame(Transaction, Class, Frame, Options)),
        Data),
    database_context_object(Transaction, Context),
    dict_pairs(Frames, json, ['@context'-Context|Data]).
all_class_frames(Query_Context, Frames, Options) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    all_class_frames(TO, Frames, Options).

class_frame(Askable, Class, Frame) :-
    class_frame(Askable, Class, Frame, [compress_ids(true),expand_abstract(true)]).

:- table class_frame/4 as private.
class_frame(Transaction, Class, Frame, Options) :-
    (   is_transaction(Transaction)
    ;   is_validation_object(Transaction)
    ),
    !,
    database_prefixes(Transaction, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = (Default_Prefixes.put(DB_Prefixes)),
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    findall(
        Predicate_Comp-Subframe,
        (   class_predicate_conjunctive_type(Transaction, Class_Ex, Predicate, Type_Desc),
            type_descriptor_sub_frame(Type_Desc, Transaction, Prefixes, Subframe, Options),
            compress_schema_uri(Predicate, Prefixes, Predicate_Comp, Options)
        ),
        Pairs),
    % Subdocument
    (   is_subdocument(Transaction, Class_Ex)
    ->  Pairs2 = ['@subdocument'-[]|Pairs]
    ;   Pairs2 = Pairs),
    % abstract
    (   is_abstract(Transaction, Class_Ex)
    ->  Pairs3 = ['@abstract'-[]|Pairs2]
    ;   Pairs3 = Pairs2),
    % key
    (   key_descriptor(Transaction, Class_Ex, Key_Desc),
        key_descriptor_json(Key_Desc,Prefixes,Key_JSON)
    ->  Pairs4 = ['@key'-Key_JSON|Pairs3]
    ;   Pairs4 = Pairs3),
    % oneOf
    (   findall(JSON,
                (   oneof_descriptor(Transaction, Class_Ex, OneOf_Desc),
                    oneof_descriptor_subframe(OneOf_Desc,Transaction,Prefixes,JSON,Options)),
                OneOf_JSONs),
        OneOf_JSONs \= []
    ->  Pairs5 = ['@oneOf'-OneOf_JSONs|Pairs4]
    ;   Pairs5 = Pairs4),
    % documentation
    (   documentation_descriptor(Transaction, Class_Ex, Documentation_Desc),
	    documentation_descriptor_json(Documentation_Desc,Prefixes,Documentation_Json, Options)
    ->  Pairs6 = ['@documentation'-Documentation_Json|Pairs5]
    ;   Pairs6 = Pairs5),
    % metadata
    (   metadata_descriptor(Transaction, Class_Ex, metadata(Metadata))
    ->  Pairs7 = ['@metadata'-Metadata|Pairs6]
    ;   Pairs7 = Pairs6),
    % enum
    (   is_enum(Transaction,Class_Ex)
    ->  database_schema(Transaction, Schema),
        schema_type_descriptor(Schema, Class, enum(Class,List)),
        (   option(compress_ids(true), Options)
        ->  maplist({Class_Ex}/[Value,Enum_Value]>>enum_value(Class_Ex,Enum_Value,Value),
                    List, Enum_List)
        ;   List = Enum_List
        ),
        expand_system_uri(sys:'Enum', Enum, Options),
        Pairs8 = ['@type'-Enum,'@values'-Enum_List|Pairs7]
    ;   expand_system_uri(sys:'Class', C, Options),
        Pairs8 = ['@type'-C|Pairs7]
    ),
    sort(Pairs8, Sorted_Pairs),
    catch(
        json_dict_create(Frame,Sorted_Pairs),
        error(duplicate_key(Predicate),_),
        throw(error(violation_of_diamond_property(Class,Predicate),_))
    ).
class_frame(Query_Context, Class, Frame, Options) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    class_frame(TO, Class, Frame, Options).
class_frame(Desc, Class, Frame, Options) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc, Trans),
    class_frame(Trans, Class, Frame, Options).

class_property_dictionary(Transaction, Prefixes, Class, Frame) :-
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    findall(
        Predicate_Comp-Result,
        (   class_predicate_type(Transaction, Class_Ex, Predicate, Type_Desc),
            type_descriptor_json(Type_Desc, Prefixes, Result),
            compress_schema_uri(Predicate, Prefixes, Predicate_Comp)
        ),
        Pairs),
    sort(Pairs, Sorted_Pairs),
    catch(
        json_dict_create(Frame,Sorted_Pairs),
        error(duplicate_key(Predicate),_),
        throw(error(violation_of_diamond_property(Class,Predicate),_))
    ).

class_property_dictionary(Transaction, Class, Frame) :-
    (   is_transaction(Transaction)
    ;   is_validation_object(Transaction)
    ),
    !,
    database_prefixes(Transaction, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = (Default_Prefixes.put(DB_Prefixes)),
    class_property_dictionary(Transaction, Prefixes, Class, Frame).
class_property_dictionary(Query_Context, Class, Frame) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    class_property_dictionary(TO, Class, Frame).
class_property_dictionary(Desc, Class, Frame) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc, Trans),
    class_property_dictionary(Trans, Class, Frame).


insert_into_builder_context_document(Builder, Document) :-
    forall(
        context_triple(Document,t(S,P,O)),
        (
            object_storage(O,OS),
            nb_add_triple(Builder, S, P, OS)
        )
    ).

insert_context_document(Transaction, Document) :-
    is_transaction(Transaction),
    !,
    database_schema(Transaction, [Schema]),
    read_write_obj_builder(Schema, Builder),
    insert_into_builder_context_document(Builder, Document).
insert_context_document(Query_Context, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_context_document(TO, Document).

valid_schema_name(Prefixes,Name) :-
    atom_string(Id,Name),
    prefix_expand_schema('JSONDocument',Prefixes,JSON_Id),
    \+ Id = JSON_Id.

insert_schema_document(Transaction, Document) :-
    is_transaction(Transaction),
    !,

    do_or_die(
        get_dict('@id', Document, Id),
        error(missing_field('@id', Document), _)),
    check_json_string('@id', Id),
    database_prefixes(Transaction, Prefixes),
    database_schema(Transaction, Schema),
    prefix_expand_schema(Id,Prefixes,Id_Ex),
    do_or_die(
        valid_schema_name(Prefixes,Id_Ex),
        error(can_not_insert_class_with_reserve_name(Id), _)),
    do_or_die(
        \+ xrdf(Schema, Id_Ex, _, _),
        error(can_not_insert_existing_object_with_id(Id), _)),

    insert_schema_document_unsafe(Transaction, Document).
insert_schema_document(Query_Context, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_schema_document(TO, Document).

insert_schema_document_unsafe(Transaction, Document) :-
    database_prefixes(Transaction, Context),
    insert_schema_document_unsafe(Transaction, Context, Document).

insert_schema_document_unsafe(Transaction, Context, Document) :-
    is_transaction(Transaction),
    !,
    % Is this a context? If so do something else.
    database_schema(Transaction, [Schema]),

    default_prefixes(Prefixes),
    put_dict(Context,Prefixes,Expanded_Context),
    forall(
        json_schema_triple(Document, Expanded_Context, t(S,P,O)),
        insert(Schema, S, P, O, _)
    ).
insert_schema_document_unsafe(Query_Context, Context, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_schema_document_unsafe(TO, Context, Document).

delete_schema_list(_, _, RDF_Nil) :-
    global_prefix_expand(rdf:nil, RDF_Nil),
    !.
delete_schema_list(Transaction, Context, Id) :-
    database_schema(Transaction, [Schema]),
    delete(Schema, Id, rdf:type, rdf:'List', _),
    xrdf([Schema], Id, rdf:rest, Rest),
    %writeq('Rest: '),writeq(Rest),nl,
    delete(Schema, Id, rdf:rest, Rest, _),
    delete_schema_list(Transaction, Context, Rest),
    xrdf([Schema],Id, rdf:first, O),
    delete(Schema, Id, rdf:first, O, _),
    %writeq('First: '),writeq(O),nl,
    delete_schema_subdocument(Transaction, Context, O).

delete_schema_subdocument(Transaction, Context, Id) :-
    database_schema(Transaction, [Schema]),
    (   atom(Id)
    ->  (   xrdf([Schema], Id, rdf:type, C),
            (   is_list_type(C)
            ->  delete_schema_list(Transaction,Context,Id)
            ;   (   is_key(C)
                ;   is_documentation(C)
                ;   type_family_constructor(C)
                )
            ->  forall(
                    xrdf([Schema], Id, P, R),
                    (   delete(Schema, Id, P, R, _),
                        delete_schema_subdocument(Transaction, Context, R))
                )
            ;   true
            )
        % Enum
        % NOTE: This should probably have an ENUM type field.
        ;   true
        )
    ;   true
    ).

% NOTE: This leaves garbage! We need a way to collect the leaves which
% link to array elements or lists.
delete_schema_document(Transaction, Id) :-
    is_transaction(Transaction),
    !,
    database_prefixes(Transaction, Context),
    database_schema(Transaction, [Schema]),

    default_prefixes(Prefixes),
    put_dict(Context,Prefixes,Expanded_Context),
    prefix_expand_schema(Id, Expanded_Context, Id_Ex),
    (   xrdf([Schema], Id_Ex, rdf:type, _)
    ->  true
    ;   throw(error(document_not_found(Id), _))
    ),
    forall(
        xrdf([Schema], Id_Ex, P, O),
        (   delete(Schema, Id_Ex, P, O, _),
            delete_schema_subdocument(Transaction,Expanded_Context,O)
        )
    ).
delete_schema_document(Query_Context, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    delete_schema_document(TO, Id).

replace_context_document(Transaction, Context) :-
    delete_schema_document(Transaction, 'terminusdb://context'),
    insert_context_document(Transaction, Context).

replace_schema_document(DB, Document) :-
    replace_schema_document(DB, Document, false, _Id).

replace_schema_document(DB, Document, Id) :-
    replace_schema_document(DB, Document, false, Id).

replace_schema_document(Transaction, Document, Create, Id) :-
    is_transaction(Transaction),
    !,
    (   get_dict('@id', Document, Id)
    ->  catch(delete_schema_document(Transaction, Id),
              error(document_not_found(_), _),
              (   Create = true
              ->  true
              ;   throw(error(document_not_found(Id, Document), _)))),
        insert_schema_document_unsafe(Transaction, Document)
    ;   get_dict('@type', Document, "@context")
    ->  replace_context_document(Transaction, Document),
        Id='@context'
    ;   throw(error(missing_field('@id', Document), _))
    ).
replace_schema_document(Query_Context, Document, Create, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    replace_schema_document(TO, Document, Create, Id).

schema_document_exists(Transaction, Id) :-
    [Schema_RWO] = (Transaction.schema_objects),
    Schema_Layer = (Schema_RWO.read),
    ground(Schema_Layer),
    database_prefixes(Transaction, Prefixes),
    prefix_expand_schema(Id, Prefixes, Id_Ex),
    subject_id(Schema_Layer, Id_Ex, _).

document_exists(Transaction, Id) :-
    [Instance_RWO] = (Transaction.instance_objects),
    Instance_Layer = (Instance_RWO.read),
    ground(Instance_Layer),
    database_prefixes(Transaction, Prefixes),
    prefix_expand(Id, Prefixes, Id_Ex),
    subject_id(Instance_Layer, Id_Ex, _).

:- begin_tests(json_stream, [concurrent(true)]).
:- use_module(core(util)).
:- use_module(library(terminus_store)).
:- use_module(core(query), [ask/2]).

test(write_json_stream_to_builder, [
         setup(
             (   open_memory_store(Store),
                 open_write(Store,Builder)
             )
         )
     ]) :-

    open_string('
{ "@type" : "@context",
  "@base" : "terminusdb://system/data/",
  "@schema" : "http://terminusdb.com/system/schema#",
  "type" : "http://terminusdb.com/type#" }

{ "@id" : "User",
  "@type" : "Class",
  "key_hash" : "type:string",
  "capability" : { "@type" : "Set",
                   "@class" : "Capability" } }',Stream),

    write_json_stream_to_builder(Stream, Builder,schema),
    nb_commit(Builder,Layer),

    findall(
        t(X,Y,Z),
        triple(Layer,X,Y,Z),
        Triples),

    Triples = [
        t("http://terminusdb.com/system/schema#User",
          "http://terminusdb.com/system/schema#capability",
          node("http://terminusdb.com/system/schema#User/capability/Set+Capability")),
        t("http://terminusdb.com/system/schema#User",
          "http://terminusdb.com/system/schema#key_hash",
          node("http://terminusdb.com/type#string")),
        t("http://terminusdb.com/system/schema#User",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Class")),
        t("http://terminusdb.com/system/schema#User/capability/Set+Capability",
          "http://terminusdb.com/schema/sys#class",
          node("http://terminusdb.com/system/schema#Capability")),
        t("http://terminusdb.com/system/schema#User/capability/Set+Capability",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Set")),
        t("terminusdb://Prefix_Pair/a0430ae9e26cb0d348e34f4c85800cd44564b201a7feb9974c4a4fbbb6c843ea",
          "http://terminusdb.com/schema/sys#prefix",
          value("\"type\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://Prefix_Pair/a0430ae9e26cb0d348e34f4c85800cd44564b201a7feb9974c4a4fbbb6c843ea",
          "http://terminusdb.com/schema/sys#url",
          value("\"http://terminusdb.com/type#\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://Prefix_Pair/a0430ae9e26cb0d348e34f4c85800cd44564b201a7feb9974c4a4fbbb6c843ea",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Prefix")),
        t("terminusdb://context",
          "http://terminusdb.com/schema/sys#base",
          value("\"terminusdb://system/data/\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://context",
          "http://terminusdb.com/schema/sys#prefix_pair",
          node("terminusdb://Prefix_Pair/a0430ae9e26cb0d348e34f4c85800cd44564b201a7feb9974c4a4fbbb6c843ea")),
        t("terminusdb://context",
          "http://terminusdb.com/schema/sys#schema",
          value("\"http://terminusdb.com/system/schema#\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://context",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Context"))
    ].

:- end_tests(json_stream).

:- begin_tests(json,[concurrent(true)]).

:- use_module(core(util/test_utils)).

test(expand_context_with_documentation, []) :-

    Context =
    _{ '@type' : "@context",
       '@base' : "http://i/",
       '@schema' : "http://s/",
       '@documentation' :
       _{ '@title' : "WOQL schema",
          '@description' : "This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.",
          '@authors' : ["Gavin"]
        }
     },

    context_elaborate(
        Context,
        Elaborated),

    Elaborated =
    json{'@id':"terminusdb://context",
         '@type':'sys:Context',
         'sys:base':json{'@type':"xsd:string",'@value':"http://i/"},
         'sys:documentation':
         json{'@container':"@set",'@type':'sys:SchemaDocumentation',
              '@value':
              [json{'@id':'terminusdb://context/SchemaDocumentation/0',
                    '@type':"sys:SchemaDocumentation",
                    'sys:authors':
                    json{'@container':"@list",
                         '@type':"xsd:string",
                         '@value':[json{'@type':"xsd:string",
                                        '@value':"Gavin"}]},
                    'sys:description':json{'@type':"xsd:string",
                                           '@value':"This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products."},
                    'sys:title':json{'@type':"xsd:string",
                                     '@value':"WOQL schema"}}]},
         'sys:prefix_pair':json{'@container':"@set",
                                '@type':"sys:Prefix",'@value':[]},
         'sys:schema':json{'@type':"xsd:string",'@value':"http://s/"}},

    findall(Triple, context_triple(Context, Triple), Triples),

    Triples =
    [ t("terminusdb://context",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#Context'),
	  t("terminusdb://context",
		'http://terminusdb.com/schema/sys#base',
		"http://i/" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t("terminusdb://context",
		'http://terminusdb.com/schema/sys#documentation',
		'terminusdb://context/SchemaDocumentation/0'),
	  t('terminusdb://context/SchemaDocumentation/0',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#SchemaDocumentation'),
	  t('terminusdb://context/SchemaDocumentation/0',
		'http://terminusdb.com/schema/sys#authors',
		Cons),
	  t(Cons,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t(Cons,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		"Gavin" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(Cons,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
	  t('terminusdb://context/SchemaDocumentation/0',
		'http://terminusdb.com/schema/sys#description',
		"This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products." ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb://context/SchemaDocumentation/0',
		'http://terminusdb.com/schema/sys#title',
		"WOQL schema" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t("terminusdb://context",
		'http://terminusdb.com/schema/sys#schema',
		"http://s/" ^^ 'http://www.w3.org/2001/XMLSchema#string')
	].

context_schema('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/",
  "@documentation" :
  { "@title" : "WOQL schema",
    "@description" : "This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.",
    "@authors" : ["Gavin"]
  }
}').

test(insert_retrieve_context_with_documentation, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )
         ),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    write_schema(context_schema, Desc),

    open_descriptor(Desc, DB),
    database_context_object(DB,Context),

    Context =
    _{'@base':"http://i/",
      '@documentation':_{'@authors':["Gavin"],
                         '@description':"This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.",
                         '@title':"WOQL schema"},
      '@schema':"http://s/",
      '@type':'Context'}.

schema1('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "name" : "xsd:string",
  "birthdate" : "xsd:date",
  "friends" : { "@type" : "Set",
                "@class" : "Person" } }

{ "@id" : "Employee",
  "@type" : "Class",
  "@inherits" : "Person",
  "staff_number" : "xsd:string",
  "boss" : { "@type" : "Optional",
             "@class" : "Employee" },
  "tasks" : { "@type" : "List",
              "@class" : "Task" } }

{ "@id" : "Task",
  "@type" : "Class",
  "name" : "xsd:string" }

{ "@id" : "Criminal",
  "@type" : "Class",
  "@inherits" : "Person",
  "aliases" : { "@type" : "List",
                "@class" : "xsd:string" } }
').

test(get_field_values, []) :-
    Expanded = json{'@type':'http://s/Person',
                    'http://s/birthdate':
                    json{'@type':'http://www.w3.org/2001/XMLSchema#date',
                         '@value':"1979-12-28"},
                    'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                         '@value':"jane"}},
    get_field_values(Expanded, _, context{'@base':"http://i/",
                                          '@schema':"http://s/",
                                          '@type':'http://terminusdb.com/schema/sys#Context'},
                     [name, birthdate],
                     [ json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                            '@value':"jane"},
                       json{'@type':'http://www.w3.org/2001/XMLSchema#date',
                            '@value':"1979-12-28"}
                     ]).

test(create_database_prefixes,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Prefixes),
    type_context(DB,'Employee',Prefixes,Context),

    Context = json{ 'http://s/birthdate':json{ '@id':'http://s/birthdate',
                                               '@type':'http://www.w3.org/2001/XMLSchema#date'
                                             },
                    'http://s/boss':json{'@id':'http://s/boss','@type':'http://s/Employee'},
                    'http://s/friends':json{'@container':"@set",
                                            '@id':'http://s/friends',
                                            '@type':'http://s/Person'},
                    'http://s/name':json{ '@id':'http://s/name',
                                          '@type':'http://www.w3.org/2001/XMLSchema#string'
                                        },
                    'http://s/staff_number':json{ '@id':'http://s/staff_number',
                                                  '@type':'http://www.w3.org/2001/XMLSchema#string'
                                                },
                    'http://s/tasks':json{'@container':"@list",'@id':'http://s/tasks','@type':'http://s/Task'}
                  }.

test(elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : 'Criminal/gavin',
                   '@type' : 'Criminal',
                   name : "gavin",
                   birthdate : "1977-05-24",
                   aliases : ["gavino", "gosha"]
               },

    open_descriptor(Desc, DB),

    json_elaborate(DB, Document, Elaborated),

    Elaborated = json{ '@id':'http://i/Criminal/gavin',
                       '@type':'http://s/Criminal',
                       'http://s/aliases':
                       _{ '@container':"@list",
			              '@type':'http://www.w3.org/2001/XMLSchema#string',
			              '@value':[ json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
					                       '@value':"gavino"
					                     },
				                     json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
					                       '@value':"gosha"
					                     }
				                   ]
			            },
                       'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                                  '@value':"1977-05-24"
			                                    },
                       'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                                 '@value':"gavin"
			                               }
                     }.

test(id_expand,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : 'Employee/gavin',
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : 'Employee/jane',
                              '@type' : 'Employee',
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28",
                              tasks: []
                          },
                   tasks : []
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, Document, Elaborated),

    Elaborated =
    json{
        '@id':'http://i/Employee/gavin',
        '@type':'http://s/Employee',
        'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                   '@value':"1977-05-24"
			                     },
        'http://s/boss':json{ '@id':'http://i/Employee/jane',
			                  '@type':'http://s/Employee',
			                  'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
						                                 '@value':"1979-12-28"
						                               },
			                  'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						                            '@value':"jane"
						                          },
			                  'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
							                                '@value':"12"
							                              },
                              'http://s/tasks':json{ '@container':"@list",
									                 '@type':'http://s/Task',
									                 '@value':[]
                                                   }
			                },
        'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                  '@value':"gavin"
			                },
        'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				                      '@value':"13"
				                    },
        'http://s/tasks':json{ '@container':"@list",
							   '@type':'http://s/Task',
							   '@value':[]
                             }
    }.

test(triple_convert,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : 'Employee/gavin',
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : 'Employee/jane',
                              '@type' : 'Employee',
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28",
                              tasks: []
                          },
                   tasks: []
               },

    open_descriptor(Desc, DB),
    json_triples(DB, Document, Triples),

    sort(Triples, Sorted),
    Sorted = [
        t('http://i/Employee/gavin',
          'http://s/birthdate',
          date(1977,5,24,0)^^'http://www.w3.org/2001/XMLSchema#date'),
        t('http://i/Employee/gavin','http://s/boss','http://i/Employee/jane'),
        t('http://i/Employee/gavin',
          'http://s/name',
          "gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/Employee/gavin',
          'http://s/staff_number',
          "13"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/Employee/gavin',
          'http://s/tasks',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
        t('http://i/Employee/gavin',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Employee'),
        t('http://i/Employee/jane',
          'http://s/birthdate',
          date(1979,12,28,0)^^'http://www.w3.org/2001/XMLSchema#date'),
        t('http://i/Employee/jane',
          'http://s/name',
          "jane"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/Employee/jane',
          'http://s/staff_number',
          "12"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/Employee/jane',
          'http://s/tasks',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
        t('http://i/Employee/jane',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Employee')
    ].

test(extract_json,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : 'Employee/gavin',
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   tasks : [],
                   boss : json{
                              '@id' : 'Employee/jane',
                              '@type' : 'Employee',
                              name : "jane",
                              tasks : [],
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, Id),

    open_descriptor(Desc, DB),
    !, % NOTE: why does rolling back over this go mental?

    get_document(DB,Id,JSON1),

    !,
    JSON1 = json{'@id':'Employee/gavin',
                 '@type':'Employee',
                 birthdate:"1977-05-24",
                 boss:'Employee/jane',
                 tasks: [],
                 name:"gavin",
                 staff_number:"13"},

    get_document(DB,'Employee/jane',JSON2),
    !,
    JSON2 = json{ '@id':'Employee/jane',
                  '@type':'Employee',
                  birthdate:"1979-12-28",
                  tasks: [],
                  name:"jane",
                  staff_number:"12"
                }.

test(double_insert_document,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             can_not_insert_existing_object_with_id('http://i/Employee/jane')
         )
     ]) :-

    Document = json{
                   '@id' : 'Employee/gavin',
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   tasks : [],
                   boss : json{
                              '@id' : 'Employee/jane',
                              '@type' : 'Employee',
                              name : "jane",
                              tasks : [],
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _Id1),

    Jane_Again =
    json{
        '@id' : 'Employee/jane',
        '@type' : 'Employee',
        name : "jane",
        tasks : [],
        staff_number : "12",
        birthdate : "1979-12-28"
    },
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "bar" },
                        Jane_Again, _Id2).

test(get_value,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id':'Employee/jane',
                '@type':'Employee',
                birthdate:"1979-12-28",
                name:"jane",
                staff_number:"12",
                tasks:[]},

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,Elaborated),

    get_all_path_values(Elaborated,Values),

    Values = [['@type']-'http://s/Employee',
              ['http://s/birthdate']-(date(1979,12,28,0)^^'http://www.w3.org/2001/XMLSchema#date'),
              ['http://s/name']-("jane"^^'http://www.w3.org/2001/XMLSchema#string'),
              ['http://s/staff_number']-("12"^^'http://www.w3.org/2001/XMLSchema#string'),
              ['http://s/tasks','@type']-'http://s/Task',
	          ['http://s/tasks','@value']-[]].


schema2('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@base" : "Person/",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name", "birthdate" ] },
  "name" : "xsd:string",
  "birthdate" : "xsd:date",
  "friends" : { "@type" : "Set",
                "@class" : "Person" } }

{ "@id" : "Employee",
  "@type" : "Class",
  "@inherits" : "Person",
  "@base" : "Employee/",
  "@key" : { "@type" : "Hash",
             "@fields" : [ "name", "birthdate" ] },
  "staff_number" : "xsd:string",
  "boss" : { "@type" : "Optional",
             "@class" : "Employee" },
  "tasks" : { "@type" : "List",
              "@class" : "Task" } }

{ "@id" : "Task",
  "@type" : "Class",
  "@key" : { "@type" : "ValueHash" },
  "name" : "xsd:string" }

{ "@id" : "Criminal",
  "@type" : "Class",
  "@inherits" : "Person",
  "aliases" : { "@type" : "List",
                "@class" : "xsd:string" } }

{ "@id" : "Event",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "action" : "xsd:string",
  "timestamp" : "xsd:dateTime" }

{ "@id" : "Book",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string" }

{ "@id" : "BookClub",
  "@type" : "Class",
  "@base" : "BookClub/",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string",
  "people" : { "@type" : "Set",
               "@class" : "Person" },
  "book_list" : { "@type" : "Array",
                  "@class" : "Book" } }

{ "@id" : "Colour",
  "@type" : "Enum",
  "@value" : [ "red", "blue", "green" ] }

{ "@id" : "Dog",
  "@type" : "Class",
  "@base" : "Dog/",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "hair_colour" : "Colour" }

{ "@id" : "BinaryTree",
  "@type" : "TaggedUnion",
  "@base" : "binary_tree/",
  "@key" : { "@type" : "ValueHash" },
  "leaf" : "sys:Unit",
  "node" : "Node" }

{ "@id" : "Node",
  "@type" : "Class",
  "@key" : { "@type" : "ValueHash" },
  "value" : "xsd:integer",
  "left" : "BinaryTree",
  "right" : "BinaryTree" }

{ "@id" : "Human",
  "@type" : "Class",
  "mother" : "Human",
  "father" : "Human" }

{ "@id" : "Moo",
  "@type" : "Class",
  "name" : "xsd:string" }

{ "@id" : "Choice",
  "@type" : "TaggedUnion",
  "a" : "xsd:string",
  "b" : "xsd:integer" }

{ "@id" : "InheritsChoice",
  "@type" : "Class",
  "@inherits" : ["Choice"],
  "c" : "xsd:string" }

{ "@id" : "Choice2",
  "@type" : "Class",
  "@oneOf" : { "a" : "xsd:string",
               "b" : "xsd:integer" }}

{ "@id" : "Choice3",
  "@type" : "Class",
  "c" : "xsd:string",
  "@oneOf" : { "a" : "xsd:string",
               "b" : "xsd:integer" }}

{ "@id" : "InheritsChoice2",
  "@type" : "Class",
  "@inherits" : ["Choice2"],
  "c" : "xsd:string" }

{ "@id" : "Choice4",
  "@type" : "Class",
  "@inherits" : ["Choice2"],
  "@oneOf" : { "c" : "xsd:string",
               "d" : "xsd:integer" } }

{ "@id" : "InheritsChoices",
  "@type" : "Class",
  "@inherits" : ["Choice2", "Choice4"] }

{ "@id" : "DoubleChoice",
  "@type" : "Class",
  "@oneOf" : [ { "a" : "xsd:string",
                 "b" : "xsd:integer" },
               { "c" : "xsd:string",
                 "d" : "xsd:integer" }] }

{ "@id" : "EnumChoice",
  "@type" : "Class",
  "@oneOf" : { "a" : "sys:Unit",
               "b" : "sys:Unit",
               "c" : "sys:Unit",
               "d" : "sys:Unit" }}
').

test(schema_key_elaboration1, []) :-
    Doc = json{'@id':"Capability",
               '@key':json{'@type':"ValueHash"},
               '@type':"Class",
               role:json{'@class':"Role",
                         '@type':"Set"},
               scope:"Resource"},

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Doc, Context, Elaborate),

    Elaborate =
    json{ '@id':'https://s/Capability',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Capability/key/ValueHash',
				'@type':'http://terminusdb.com/schema/sys#ValueHash'
			  },
          'https://s/role':
          json{ '@id':'https://s/Capability/role/Set+Role',
			    '@type':'http://terminusdb.com/schema/sys#Set',
			    'http://terminusdb.com/schema/sys#class':
                json{ '@id':'https://s/Role',
					  '@type':"@id"
					}
			  },
          'https://s/scope':
          json{'@id':'https://s/Resource',
               '@type':"@id"}
        }.

test(schema_lexical_key_elaboration, []) :-
    Doc = json{ '@id' : "Person",
                '@type' : "Class",
                '@base' : "Person/",
                '@key' : json{ '@type' : "Lexical",
                               '@fields' : [ "name", "birthdate" ] },
                'name' : "xsd:string",
                'birthdate' : "xsd:date",
                'friends' : json{ '@type' : "Set",
                                  '@class' : "Person" } },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Doc, Context, Elaborate),

    Elaborate =
    json{ '@id':'https://s/Person',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"Person/"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Person/key/Lexical/name+birthdate',
				'@type':'http://terminusdb.com/schema/sys#Lexical',
                'http://terminusdb.com/schema/sys#fields':
                json{ '@container':"@list",
					  '@type':"@id",
					  '@value': [ json{ '@id':'https://s/name',
									    '@type':"@id"
									  },
								  json{ '@id':'https://s/birthdate',
									    '@type':"@id"
									  }
							    ]
					}
			  },
          'https://s/birthdate':
          json{ '@id':'http://www.w3.org/2001/XMLSchema#date',
				'@type':"@id"
			  },
          'https://s/friends':
          json{ '@id':'https://s/Person/friends/Set+Person',
				'@type':'http://terminusdb.com/schema/sys#Set',
				'http://terminusdb.com/schema/sys#class':
                json{ '@id':'https://s/Person',
					  '@type':"@id"
					}
			  },
          'https://s/name':
          json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
			    '@type':"@id"
			  }
        }.

test(idgen_lexical,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    JSON = json{'@type':'Person',
                birthdate:"1979-12-28",
                name:"jane"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{'@id':'http://i/Person/jane+1979-12-28',
         '@type':'http://s/Person',
         'http://s/birthdate':json{'@type':'http://www.w3.org/2001/XMLSchema#date',
                                   '@value':"1979-12-28"},
         'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                              '@value':"jane"}
        }.

test(idgen_hash,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Employee',
                birthdate:"1979-12-28",
                name:"jane",
                staff_number:"13",
                tasks : []
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{ '@id':'http://i/Employee/0eda88cddbf73a4c404cfc706100e97c20cadc8ffb71669474c503bff0972e41',
          '@type':'http://s/Employee',
          'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                     '@value':"1979-12-28"
			                       },
          'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                    '@value':"jane"
			                  },
          'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				                        '@value':"13"
				                      },
          'http://s/tasks':json{'@container':"@list",'@type':'http://s/Task','@value':[]}
        }.

test(idgen_value_hash,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Task',
                name:"Groceries"},

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON, Elaborated),
    Elaborated =
    json{ '@id':'http://i/Task/1c6097164e71960b32363b7901c6fa3c255632070355e7fdf9a6126f0e80a5af',
          '@type':'http://s/Task',
          'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Groceries"}}.

test(idgen_lexical_optional,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "Optional",
                                         '@class': "xsd:string"}})
                         ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: "foo"},
                                  Uri1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing"},
                                  Uri2)
                          )),

    Uri1 = 'http://somewhere.for.now/document/Thing/foo',
    Uri2 = 'http://somewhere.for.now/document/Thing/+none+'.

test(idgen_lexical_set,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "Set",
                                         '@class': "xsd:string"}})
                         ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["foo", "bar"]},
                                  Uri1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["bar", "foo"]},
                                  Uri2),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["quux"]},
                                  Uri3),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: []},
                                  Uri4),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing"},
                                  Uri5)
                          )),

    Uri1 = 'http://somewhere.for.now/document/Thing/bar++foo',
    Uri2 = 'http://somewhere.for.now/document/Thing/bar++foo',
    Uri3 = 'http://somewhere.for.now/document/Thing/quux',
    Uri4 = 'http://somewhere.for.now/document/Thing/+none+',
    Uri5 = 'http://somewhere.for.now/document/Thing/+none+'.

test(idgen_lexical_list,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "List",
                                         '@class': "xsd:string"}})
                         ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["foo", "bar"]},
                                  Uri1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["bar", "foo"]},
                                  Uri2),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["quux"]},
                                  Uri3),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: []},
                                  Uri4)
                          )),

    Uri1 = 'http://somewhere.for.now/document/Thing/foo++bar',
    Uri2 = 'http://somewhere.for.now/document/Thing/bar++foo',
    Uri3 = 'http://somewhere.for.now/document/Thing/quux',
    Uri4 = 'http://somewhere.for.now/document/Thing/+none+'.

test(idgen_lexical_array,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "Array",
                                         '@class': "xsd:string"}})
                         ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["foo", "bar"]},
                                  Uri1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["bar", "foo"]},
                                  Uri2),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["quux"]},
                                  Uri3),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: []},
                                  Uri4),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing"},
                                  Uri5)
                          )),

    Uri1 = 'http://somewhere.for.now/document/Thing/foo++bar',
    Uri2 = 'http://somewhere.for.now/document/Thing/bar++foo',
    Uri3 = 'http://somewhere.for.now/document/Thing/quux',
    Uri4 = 'http://somewhere.for.now/document/Thing/+none+',
    Uri5 = 'http://somewhere.for.now/document/Thing/+none+'.

test(idgen_find_collision,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "Array",
                                         '@class': "xsd:string"}})
                         ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["", "none", ""]},
                                  Uri1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: []},
                                  Uri2),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: ["none"]},
                                  Uri3)
                          )),
    Uri1 = 'http://somewhere.for.now/document/Thing/++none++',
    Uri2 = 'http://somewhere.for.now/document/Thing/+none+',
    Uri3 = 'http://somewhere.for.now/document/Thing/none'.

test(idgen_random,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type': 'Event',
                action: "click click",
                timestamp: "2021-05-20T20:33:00.000Z"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{ '@id':Id,
          '@type':'http://s/Event',
          'http://s/action':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                      '@value':"click click"
			                    },
          'http://s/timestamp':json{ '@type':'http://www.w3.org/2001/XMLSchema#dateTime',
				                     '@value':"2021-05-20T20:33:00.000Z"
			                       }
        },

    atom_concat('http://i/Event/',_,Id).

test(type_family_id, []) :-

    type_family_id(json{'@type':"Cardinality",
                        '@cardinality':3,
                        '@class':'Person'},
                   _{},
                   [property(friend_of), type('Person')],
                   'Person/friend_of/Cardinality+Person+3+3').

test(schema_elaborate, []) :-

    Schema = json{ '@type' : 'Class',
                   '@id' : 'Person',
                   'name' : 'xsd:string',
                   'age' : json{ '@type' : 'Optional',
                                 '@class' : 'xsd:decimal' },
                   'friend_of' : json{ '@type' : 'Cardinality',
                                       '@class' : 'Person',
                                       '@cardinality' : 3 }
                 },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s#')),

    json_schema_elaborate(Schema, Context, Elaborated),

    Elaborated =
    json{'@id':'https://s#Person',
         '@type':'http://terminusdb.com/schema/sys#Class',
         'https://s#age':
         json{'@id':'https://s#Person/age/Optional+xsd%3Adecimal',
              '@type':'http://terminusdb.com/schema/sys#Optional',
              'http://terminusdb.com/schema/sys#class':
              json{'@id':'http://www.w3.org/2001/XMLSchema#decimal',
                   '@type':"@id"}},
         'https://s#friend_of':
         json{'@id':'https://s#Person/friend_of/Cardinality+Person',
              '@type':'http://terminusdb.com/schema/sys#Cardinality',
              'http://terminusdb.com/schema/sys#max_cardinality':
              json{'@type':'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
                   '@value':3},
              'http://terminusdb.com/schema/sys#min_cardinality':
              json{'@type':'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
                   '@value':3},
              'http://terminusdb.com/schema/sys#class':
              json{'@id':'https://s#Person','@type':"@id"}},
         'https://s#name':json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                               '@type':"@id"}},

    findall(Triple,
            json_schema_triple(Schema, Context, Triple),
            Triples),

    sort(Triples,Sorted),

    Sorted = [
        t('https://s#Person',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Class'),
        t('https://s#Person',
          'https://s#age',
          'https://s#Person/age/Optional+xsd%3Adecimal'),
        t('https://s#Person',
          'https://s#friend_of',
          'https://s#Person/friend_of/Cardinality+Person'),
        t('https://s#Person',
          'https://s#name',
          'http://www.w3.org/2001/XMLSchema#string'),
        t('https://s#Person/age/Optional+xsd%3Adecimal',
          'http://terminusdb.com/schema/sys#class',
          'http://www.w3.org/2001/XMLSchema#decimal'),
        t('https://s#Person/age/Optional+xsd%3Adecimal',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Optional'),
        t('https://s#Person/friend_of/Cardinality+Person',
          'http://terminusdb.com/schema/sys#class',
          'https://s#Person'),
        t('https://s#Person/friend_of/Cardinality+Person',
          'http://terminusdb.com/schema/sys#max_cardinality',
          3^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t('https://s#Person/friend_of/Cardinality+Person',
          'http://terminusdb.com/schema/sys#min_cardinality',
          3^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t('https://s#Person/friend_of/Cardinality+Person',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Cardinality')
    ].

test(list_id_key_context_triple, []) :-
    findall(Triple,
            list_id_key_context_triple(
                [json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Get Groceries"}},
                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Take out rubbish"}}],
                _{},
                elt,
                p,
                _{'@base' : ''},
                Triple),
            Triples),

    Triples =
    [ t(elt,
		p,
		Cons0),
	  t(Cons0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t(Cons0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		"task_a4963868aa3ad8365a4b164a7f206ffc"),
	  t("task_a4963868aa3ad8365a4b164a7f206ffc",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		task),
	  t("task_a4963868aa3ad8365a4b164a7f206ffc",
		name,
		"Get Groceries" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(Cons0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		Cons1),
	  t(Cons1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t(Cons1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		"task_f9e4104c952e71025a1d68218d88bab1"),
	  t("task_f9e4104c952e71025a1d68218d88bab1",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		task),
	  t("task_f9e4104c952e71025a1d68218d88bab1",
		name,
		"Take out rubbish" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(Cons1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')
	].

test(array_id_key_triple, []) :-
    findall(Triple,
            array_id_key_context_triple(
                [json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Get Groceries"}},
                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Take out rubbish"}}],
                _{},
                1,
                elt,
                p,
                _{'@base' : ''},
                Triple),
            Triples),

    Triples =
    [ t(Array0,
		'http://terminusdb.com/schema/sys#index',
		0 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
	  t(Array0,
		'http://terminusdb.com/schema/sys#value',
		"task_a4963868aa3ad8365a4b164a7f206ffc"),
	  t("task_a4963868aa3ad8365a4b164a7f206ffc",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		task),
	  t("task_a4963868aa3ad8365a4b164a7f206ffc",
		name,
		"Get Groceries" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(elt,
		p,
		Array0),
	  t(Array0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#Array'),
	  t(Array1,
		'http://terminusdb.com/schema/sys#index',
		1 ^^ 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
	  t(Array1,
		'http://terminusdb.com/schema/sys#value',
		"task_f9e4104c952e71025a1d68218d88bab1"),
	  t("task_f9e4104c952e71025a1d68218d88bab1",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		task),
	  t("task_f9e4104c952e71025a1d68218d88bab1",
		name,
		"Take out rubbish" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(elt,
		p,
		Array1),
	  t(Array1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#Array')
	].

test(set_id_key_context_triple, []) :-
    findall(Triple,
            set_id_key_context_triple(
                [json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Get Groceries"}},
                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Take out rubbish"}}],
                _{},
                elt,
                p,
                _{},
                Triple),
            Triples),

    Triples =
    [ t(elt,p,"task_a4963868aa3ad8365a4b164a7f206ffc"),
	  t("task_a4963868aa3ad8365a4b164a7f206ffc",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		task),
	  t("task_a4963868aa3ad8365a4b164a7f206ffc",
		name,
		"Get Groceries" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(elt,p,"task_f9e4104c952e71025a1d68218d88bab1"),
	  t("task_f9e4104c952e71025a1d68218d88bab1",
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		task),
	  t("task_f9e4104c952e71025a1d68218d88bab1",
		name,
		"Take out rubbish" ^^ 'http://www.w3.org/2001/XMLSchema#string')
	].

test(list_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Employee',
                name: "Gavin",
                birthdate: "1977-05-24",
                staff_number: "12",
                tasks : [
                    json{ name : "Get Groceries" },
                    json{ name : "Take out rubbish" }
                ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),
    Elaborated =
    json{ '@id':'http://i/Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
          '@type':'http://s/Employee',
          'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
                                     '@value':"1977-05-24"
                                   },
          'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Gavin"
                              },
          'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
                                        '@value':"12"
                                      },
          'http://s/tasks':_{ '@container':"@list",
                              '@type':'http://s/Task',
                              '@value':[ json{ '@id':'http://i/Task/9cd6a4bf2f165cd4e5d9cd23a8490c200241490699e8846ace30b9990bc6151c',
                                               '@type':'http://s/Task',
                                               'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
                                                                     '@value':"Get Groceries"
                                                                   }
                                             },
                                         json{ '@id':'http://i/Task/153a66ced94d3aed26fb4c23e9302e2235bbb70d0cf3cf127bdd7bee3baf9cc0',
                                               '@type':'http://s/Task',
                                               'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
                                                                     '@value':"Take out rubbish"
                                                                   }
                                             }
                                       ]
                            }
        },

    json_triples(DB, JSON, Triples),

    Triples =
    [ t('http://i/Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Employee'),
	  t('http://i/Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
		'http://s/birthdate',
		date(1977,5,24,0)^^'http://www.w3.org/2001/XMLSchema#date'),
	  t('http://i/Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
		'http://s/name',
		"Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('http://i/Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
		'http://s/staff_number',
		"12"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('http://i/Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
		'http://s/tasks',
		Cons0),
	  t(Cons0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t(Cons0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		'http://i/Task/9cd6a4bf2f165cd4e5d9cd23a8490c200241490699e8846ace30b9990bc6151c'),
	  t('http://i/Task/9cd6a4bf2f165cd4e5d9cd23a8490c200241490699e8846ace30b9990bc6151c',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Task'),
	  t('http://i/Task/9cd6a4bf2f165cd4e5d9cd23a8490c200241490699e8846ace30b9990bc6151c',
		'http://s/name',
		"Get Groceries"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t(Cons0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		Cons1),
	  t(Cons1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
	  t(Cons1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
		'http://i/Task/153a66ced94d3aed26fb4c23e9302e2235bbb70d0cf3cf127bdd7bee3baf9cc0'),
	  t('http://i/Task/153a66ced94d3aed26fb4c23e9302e2235bbb70d0cf3cf127bdd7bee3baf9cc0',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Task'),
	  t('http://i/Task/153a66ced94d3aed26fb4c23e9302e2235bbb70d0cf3cf127bdd7bee3baf9cc0',
		'http://s/name',
		"Take out rubbish" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t(Cons1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')
	],

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Fresh_JSON),

    Fresh_JSON =
    json{ '@id':'Employee/2c125b82d49478456066b30ee0e1f78480f86ec2047572dfca2b07266c9d0eb3',
          '@type':'Employee',
          birthdate:"1977-05-24",
          name:"Gavin",
          staff_number:"12",
          tasks:[ 'Task/9cd6a4bf2f165cd4e5d9cd23a8490c200241490699e8846ace30b9990bc6151c',
                  'Task/153a66ced94d3aed26fb4c23e9302e2235bbb70d0cf3cf127bdd7bee3baf9cc0'
	            ]
        }.

test(array_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BookClub',
                name: "Marxist book club",
                book_list : [
                    json{ name : "Das Kapital" },
                    json{ name : "Der Ursprung des Christentums" }
                ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,Elaborated),

    Elaborated = json{ '@id':'http://i/BookClub/Marxist%20book%20club',
                       '@type':'http://s/BookClub',
                       'http://s/book_list':
                       _{ '@container':"@array",
                          '@dimensions':1,
			              '@type':'http://s/Book',
			              '@value':[ json{ '@id':'http://i/Book/Das%20Kapital',
					                       '@type':'http://s/Book',
					                       'http://s/name':
                                           json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								                 '@value':"Das Kapital"
								               }
					                     },
					                 json{ '@id':'http://i/Book/Der%20Ursprung%20des%20Christentums',
					                       '@type':'http://s/Book',
					                       'http://s/name':
                                           json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								                 '@value':"Der Ursprung des Christentums"
								               }
					                     }
				                   ]
			            },
                       'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                                 '@value':"Marxist book club"
			                               }
                     },

    json_triples(DB, JSON, Triples),

    Triples =
    [ t('http://i/BookClub/Marxist%20book%20club',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/BookClub'),
	  t(Array0,
		'http://terminusdb.com/schema/sys#index',
		0^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
	  t(Array0,
		'http://terminusdb.com/schema/sys#value',
		'http://i/Book/Das%20Kapital'),
	  t('http://i/Book/Das%20Kapital',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Book'),
	  t('http://i/Book/Das%20Kapital',
		'http://s/name',
		"Das Kapital"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('http://i/BookClub/Marxist%20book%20club',
		'http://s/book_list',
		Array0),
	  t(Array0,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#Array'),
	  t(Array1,
		'http://terminusdb.com/schema/sys#index',
		1^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
	  t(Array1,
		'http://terminusdb.com/schema/sys#value',
		'http://i/Book/Der%20Ursprung%20des%20Christentums'),
	  t('http://i/Book/Der%20Ursprung%20des%20Christentums',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Book'),
	  t('http://i/Book/Der%20Ursprung%20des%20Christentums',
		'http://s/name',
		"Der Ursprung des Christentums" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t('http://i/BookClub/Marxist%20book%20club',
		'http://s/book_list',
		Array1),
	  t(Array1,
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#Array'),
	  t('http://i/BookClub/Marxist%20book%20club',
		'http://s/name',
		"Marxist book club" ^^ 'http://www.w3.org/2001/XMLSchema#string')
	],

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Recovered),

    Recovered = json{ '@id':'BookClub/Marxist%20book%20club',
                      '@type':'BookClub',
                      book_list:[ 'Book/Das%20Kapital',
		                          'Book/Der%20Ursprung%20des%20Christentums'
		                        ],
                      name:"Marxist book club"
                    }.

test(set_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BookClub',
                name: "Marxist book club",
                people: [
                    json{'@type' : 'Person',
                         name : "jim",
                         birthdate: "1982-05-03"
                        },
                    json{'@type':'Person',
                         birthdate:"1979-12-28",
                         name:"jane"
                        }],
                book_list : []
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{ '@id':'http://i/BookClub/Marxist%20book%20club',
          '@type':'http://s/BookClub',
          'http://s/book_list':
          _{ '@container':"@array",
             '@dimensions':1,
			 '@type':'http://s/Book',
			 '@value':[]
		   },
          'http://s/name':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			    '@value':"Marxist book club"
			  },
          'http://s/people':
          _{ '@container':"@set",
			 '@type':'http://s/Person',
			 '@value':[ json{ '@id':'http://i/Person/jim+1982-05-03',
					          '@type':'http://s/Person',
					          'http://s/birthdate':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
								    '@value':"1982-05-03"
								  },
					          'http://s/name':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								    '@value':"jim"
								  }
					        },
				        json{ '@id':'http://i/Person/jane+1979-12-28',
					          '@type':'http://s/Person',
					          'http://s/birthdate':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
								    '@value':"1979-12-28"
								  },
					          'http://s/name':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								    '@value':"jane"
								  }
					        }
				      ]
		   }
        },

    json_triples(DB, JSON, Triples),

    Triples =
    [ t('http://i/BookClub/Marxist%20book%20club',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/BookClub'),
	  t('http://i/BookClub/Marxist%20book%20club',
		'http://s/name',
		"Marxist book club" ^^ 'http://www.w3.org/2001/XMLSchema#string'),
	  t('http://i/BookClub/Marxist%20book%20club',
		'http://s/people',
		'http://i/Person/jim+1982-05-03'),
	  t('http://i/Person/jim+1982-05-03',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Person'),
	  t('http://i/Person/jim+1982-05-03',
		'http://s/birthdate',
		date(1982,5,3,0) ^^ 'http://www.w3.org/2001/XMLSchema#date'),
	  t('http://i/Person/jim+1982-05-03',
		'http://s/name',
		"jim"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('http://i/BookClub/Marxist%20book%20club',
		'http://s/people',
		'http://i/Person/jane+1979-12-28'),
	  t('http://i/Person/jane+1979-12-28',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://s/Person'),
	  t('http://i/Person/jane+1979-12-28',
		'http://s/birthdate',
		date(1979,12,28,0) ^^ 'http://www.w3.org/2001/XMLSchema#date'),
	  t('http://i/Person/jane+1979-12-28',
		'http://s/name',
		"jane"^^'http://www.w3.org/2001/XMLSchema#string')
	],

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Book_Club),

    Book_Club = json{ '@id':'BookClub/Marxist%20book%20club',
                      '@type':'BookClub',
                      name:"Marxist book club",
                      people:['Person/jane+1979-12-28','Person/jim+1982-05-03']
                    }.

test(set_elaborate_id,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BookClub',
                name: "Marxist book club",
                people: [ "Person/jim/1982-05-03", "Person/jane/1979-12-28"],
                book_list : []
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),
    Elaborated =
    json{'@id':'http://i/BookClub/Marxist%20book%20club',
         '@type':'http://s/BookClub',
         'http://s/book_list':_1506{'@container':"@array",
                                    '@dimensions':1,
                                    '@type':'http://s/Book',
                                    '@value':[]},
         'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                              '@value':"Marxist book club"},
         'http://s/people':_1390{'@container':"@set",
                                 '@type':'http://s/Person',
                                 '@value':[json{'@id':'http://i/Person/jim/1982-05-03',
                                                '@type':"@id"},
                                           json{'@id':'http://i/Person/jane/1979-12-28',
                                                '@type':"@id"}]}}.

test(elaborate_id,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Human',
                '@id' : "Human/Cleatus",
                'mother' : "Human/MaryJane",
                'father' : "Human/BobbyJoe" },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),
    Elaborated =
    json{'@id':'http://i/Human/Cleatus',
         '@type':'http://s/Human',
         'http://s/father':json{'@id':'http://i/Human/BobbyJoe',
                                '@type':"@id"},
         'http://s/mother':json{'@id':'http://i/Human/MaryJane',
                                '@type':"@id"}}.

test(empty_list,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Employee',
                name: "Gavin",
                birthdate: "1977-05-24",
                staff_number: "12",
                tasks : []
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, DB),
    get_document(DB, Id, Employee_JSON),

    Employee_JSON = json{'@id':_,
                         '@type':'Employee',
                         birthdate:"1977-05-24",
                         name:"Gavin",
                         staff_number:"12",
                         tasks:[]}.

test(enum_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    database_prefixes(DB,Prefixes),
    type_context(DB,'Dog',Prefixes,TypeContext),

    TypeContext = json{ 'http://s/hair_colour':json{'@id':'http://s/hair_colour',
                                                    '@type':'http://s/Colour'},
                        'http://s/name':json{ '@id':'http://s/name',
                                              '@type':'http://www.w3.org/2001/XMLSchema#string'
                                            }
                      },

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    json_elaborate(DB, JSON, Elaborated),

    Elaborated = json{ '@id':'http://i/Dog/Ralph',
                       '@type':'http://s/Dog',
                       'http://s/hair_colour':json{'@id':'http://s/Colour/blue',
                                                   '@type':"@id"},
                       'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                                 '@value':"Ralph"
			                               }
                     },

    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB,Id, Dog_JSON),

    Dog_JSON = json{'@id':'Dog/Ralph',
                    '@type':'Dog',
                    hair_colour:blue,
                    name:"Ralph"}.


test(elaborate_tagged_union,[]) :-

    Binary_Tree = json{ '@type' : 'TaggedUnion',
                        '@id' : 'BinaryTree',
                        '@base' : "BinaryTree/",
                        '@key' : json{ '@type' : 'ValueHash' },
                        leaf : 'sys:Unit',
                        node : 'Node'
                      },

    Node = json{ '@type' : 'Class',
                 '@id' : 'Node',
                 '@base' : "Node/",
                 '@key' : json{ '@type' : 'ValueHash' },
                 value : 'xsd:integer',
                 left : "BinaryTree",
                 right : "BinaryTree"
               },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Binary_Tree, Context, BT_Elaborated),

    BT_Elaborated =
    json{ '@id':'https://s/BinaryTree',
          '@type':'http://terminusdb.com/schema/sys#TaggedUnion',
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"BinaryTree/"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/BinaryTree/key/ValueHash',
				'@type':'http://terminusdb.com/schema/sys#ValueHash'
			  },
          'https://s/leaf':json{ '@id':'http://terminusdb.com/schema/sys#Unit',
			                     '@type':"@id"
			                   },
          'https://s/node':json{'@id':'https://s/Node','@type':"@id"}
        },

    json_schema_elaborate(Node, Context, Node_Elaborated),

    Node_Elaborated =
    json{ '@id':'https://s/Node',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"Node/"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Node/key/ValueHash',
				'@type':'http://terminusdb.com/schema/sys#ValueHash'
			  },
          'https://s/left':json{'@id':'https://s/BinaryTree','@type':"@id"},
          'https://s/right':json{'@id':'https://s/BinaryTree','@type':"@id"},
          'https://s/value':json{ '@id':'http://www.w3.org/2001/XMLSchema#integer',
			                      '@type':"@id"
			                    }
        }.

test(binary_tree_context,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    database_prefixes(DB,Prefixes),
    type_context(DB,'BinaryTree', Prefixes, Binary_Context),

    Binary_Context = json{ 'http://s/leaf':json{'@id':'http://s/leaf',
                                                '@type':'http://terminusdb.com/schema/sys#Unit'},
                           'http://s/node':json{'@id':'http://s/node','@type':"@id"}
                         },
    type_context(DB,'Node', Prefixes, Node_Context),
    Node_Context = json{ 'http://s/left':json{'@id':'http://s/left','@type':"@id"},
                         'http://s/right':json{'@id':'http://s/right','@type':"@id"},
                         'http://s/value':json{ '@id':'http://s/value',
                                                '@type':'http://www.w3.org/2001/XMLSchema#integer'
                                              }
                       }.

test(binary_tree_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BinaryTree',
                node: json{'@type':'Node',
                           value: 1,
                           left: json{'@type':'BinaryTree',
                                      node: json{'@type':'Node',
                                                 value: 0,
                                                 left: json{'@type':'BinaryTree',
                                                            leaf : []},
                                                 right: json{'@type':'BinaryTree',
                                                             leaf : []}}},
                           right: json{'@type':'BinaryTree',
                                       node: json{'@type':'Node',
                                                  value: 2,
                                                  left: json{'@type':'BinaryTree',
                                                             leaf : []},
                                                  right: json{'@type':'BinaryTree',
                                                              leaf : []}}}}},

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,Elaborated),

    Elaborated =
    json{ '@id':'http://i/binary_tree/cc616e9ea0244e719943bf05ee7fe8a6aea23ab82a553346b458e86cf3bda278',
          '@type':'http://s/BinaryTree',
          'http://s/node':json{ '@id':'http://i/Node/106bb5d8f5b3cdbb0a6cadabee00105afc64772f3eeb81ac69c62ddfecac5f20',
                                '@type':'http://s/Node',
                                'http://s/left':json{ '@id':'http://i/binary_tree/aa16106a510bd7ba084e914989beff5bd14217b739ad838f8200b22d2e7adb18',
                                                      '@type':'http://s/BinaryTree',
                                                      'http://s/node':json{ '@id':'http://i/Node/7a5e35224cc6d6605abf93527586f21a92104b198531713cc88e473865b59610',
                                                                            '@type':'http://s/Node',
                                                                            'http://s/left':json{ '@id':'http://i/binary_tree/ef79cbaf3a78467a055cec4535bf9c9ca58b03508d52ef4611a057d93ae77310',
                                                                                                  '@type':'http://s/BinaryTree',
                                                                                                  'http://s/leaf':[]
                                                                                                },
                                                                            'http://s/right':json{ '@id':'http://i/binary_tree/ef79cbaf3a78467a055cec4535bf9c9ca58b03508d52ef4611a057d93ae77310',
                                                                                                   '@type':'http://s/BinaryTree',
                                                                                                   'http://s/leaf':[]
                                                                                                 },
                                                                            'http://s/value':json{ '@type':'http://www.w3.org/2001/XMLSchema#integer',
                                                                                                   '@value':0
                                                                                                 }
                                                                          }
                                                    },
                                'http://s/right':json{ '@id':'http://i/binary_tree/fbd75c683214c33ca6bb8a8bb9b86d283a6bfa24c9e890e3d37e972bdd4f1e2b',
                                                       '@type':'http://s/BinaryTree',
                                                       'http://s/node':json{ '@id':'http://i/Node/837198a5cf8d029974f490915dd0b2a22a6ee1d56875048c28171e024b39f40f',
                                                                             '@type':'http://s/Node',
                                                                             'http://s/left':json{ '@id':'http://i/binary_tree/ef79cbaf3a78467a055cec4535bf9c9ca58b03508d52ef4611a057d93ae77310',
                                                                                                   '@type':'http://s/BinaryTree',
                                                                                                   'http://s/leaf':[]
                                                                                                 },
                                                                             'http://s/right':json{ '@id':'http://i/binary_tree/ef79cbaf3a78467a055cec4535bf9c9ca58b03508d52ef4611a057d93ae77310',
                                                                                                    '@type':'http://s/BinaryTree',
                                                                                                    'http://s/leaf':[]
                                                                                                  },
                                                                             'http://s/value':json{ '@type':'http://www.w3.org/2001/XMLSchema#integer',
                                                                                                    '@value':2
                                                                                                  }
                                                                           }
						                             },
			                    'http://s/value':json{ '@type':'http://www.w3.org/2001/XMLSchema#integer',
						                               '@value':1
						                             }
			                  }
        },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Fresh_JSON),

    Fresh_JSON =
    json{ '@id':'binary_tree/cc616e9ea0244e719943bf05ee7fe8a6aea23ab82a553346b458e86cf3bda278',
          '@type':'BinaryTree',
          node:'Node/106bb5d8f5b3cdbb0a6cadabee00105afc64772f3eeb81ac69c62ddfecac5f20'
        }.

test(insert_get_delete,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    get_document(Desc, Id, _),

    run_delete_document(Desc, commit_object{ author : "me", message : "boo"}, Id),

    \+ get_document(Desc, Id, _).


test(document_update,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    New_JSON = json{'@type':'Dog',
                    '@id' : Id,
                    name: "Ralph",
                    hair_colour: "green"
                   },

    run_replace_document(Desc, commit_object{ author : "me", message : "boo"}, New_JSON, Id),

    get_document(Desc, Id, Updated_JSON),

    Updated_JSON = json{'@id':'Dog/Ralph',
                        '@type':'Dog',
                        hair_colour:green,
                        name:"Ralph"}.


test(auto_id_update,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, _Id),

    New_JSON = json{'@type':'Dog',
                    name: "Ralph",
                    hair_colour: "green"
                   },

    run_replace_document(Desc, commit_object{ author : "me", message : "boo"}, New_JSON, Same_Id),

    get_document(Desc, Same_Id, Updated_JSON),

    Updated_JSON = json{'@id':'Dog/Ralph',
                        '@type':'Dog',
                        hair_colour:green,
                        name:"Ralph"}.

test(partial_document_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id' : 'Dog/Henry',
                '@type':'Dog',
                name: "Henry",
                hair_colour: "blue"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_ID),

    JSON_ID = json{ '@id':'http://i/Dog/Henry',
                    '@type':'http://s/Dog',
                    'http://s/hair_colour':json{'@id':'http://s/Colour/blue',
                                                '@type':"@id"},
                    'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                         '@value':"Henry"}
                  }.


test(all_path_values,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id' : 'BookClub/Mystery%20Readers',
                '@type' : 'BookClub',
                name: "Mystery Readers",
                book_list: [ json{ name : "And Then There Were None" },
                             json{ name : "In Cold Blood" }
                           ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_El),

    get_all_path_values(JSON_El, Paths),

    Paths =
    [['@type']-'http://s/BookClub',
     ['http://s/book_list','@type']-'http://s/Book',
     ['http://s/book_list','@value',0,'@type']-'http://s/Book',
     ['http://s/book_list','@value',0,'http://s/name']-("And Then There Were None"^^'http://www.w3.org/2001/XMLSchema#string'),
     ['http://s/book_list','@value',1,'@type']-'http://s/Book',
     ['http://s/book_list','@value',1,'http://s/name']-("In Cold Blood"^^'http://www.w3.org/2001/XMLSchema#string'),
     ['http://s/name']-("Mystery Readers"^^'http://www.w3.org/2001/XMLSchema#string')].

test(partial_document_elaborate_list,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id' : 'BookClub/Murder%20Mysteries',
                '@type': 'BookClub',
                name : "Murder Mysteries",
                book_list: [ json{ name : "And Then There Were None" },
                             json{ name : "In Cold Blood" }
                           ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_ID),

    JSON_ID = json{ '@id':'http://i/BookClub/Murder%20Mysteries',
                    '@type':'http://s/BookClub',
                    'http://s/book_list':
                    _{ '@container':"@array",
                       '@dimensions':1,
			           '@type':'http://s/Book',
			           '@value':
                       [ json{ '@id':'http://i/Book/And%20Then%20There%20Were%20None',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"And Then There Were None"
								   }
					         },
					     json{ '@id':'http://i/Book/In%20Cold%20Blood',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"In Cold Blood"
								   }
					         }
				       ]
			         },
                    'http://s/name':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			              '@value':"Murder Mysteries"
			            }
                  }.

test(partial_document_elaborate_list_without_required,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [json{'@type':required_field_does_not_exist_in_document,
                       document:
                       json{'@id':'http://i/BookClub/Murder%20Mysteries',
                            '@type':'http://s/BookClub',
                            'http://s/book_list':
                            json{'@container':"@array",'@dimensions':1,
                                 '@type':'http://s/Book',
                                 '@value':[
                                     json{'@id':null,'@type':'http://s/Book',
                                          'http://s/name':
                                          json{'@type':
                                               'http://www.w3.org/2001/XMLSchema#string','@value':"And Then There Were None"}},
                                     json{'@id':null,
                                          '@type':'http://s/Book',
                                          'http://s/name':
                                          json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                               '@value':"In Cold Blood"}}]}},
                       field:'http://s/name'}]),_)
     ]) :-

    JSON = json{'@id' : 'BookClub/Murder%20Mysteries',
                '@type': 'BookClub',
                book_list: [ json{ name : "And Then There Were None" },
                             json{ name : "In Cold Blood" }
                           ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_ID),

    JSON_ID = json{ '@id':'http://i/BookClub/Murder%20Mysteries',
                    '@type':'http://s/BookClub',
                    'http://s/book_list':
                    _{ '@container':"@array",
			           '@type':'http://s/Book',
			           '@value':
                       [ json{ '@id':'http://i/Book/And%20Then%20There%20Were%20None',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"And Then There Were None"
								   }
					         },
					     json{ '@id':'http://i/Book/In%20Cold%20Blood',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"In Cold Blood"
								   }
					         }
				       ]
			         }
                  }.

test(optional_missing,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [json{'@type':required_field_does_not_exist_in_document,
                       document:json{'@type':'http://s/Event',
                                     'http://s/action':
                                     json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                          '@value':"test"}},
                       field:'http://s/timestamp'}]),
             _)
     ]) :-

    JSON = json{ '@type' : "Event",
                 action : "test" },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, _Id).


test(extract_schema_person,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Person', JSON),

    JSON = json{'@base':"Person/",
                '@id':'Person',
                '@key':json{'@fields':[name,birthdate],
                            '@type':"Lexical"},
                '@type':'Class',
                birthdate:'xsd:date',
                friends:json{'@class':'Person','@type':'Set'},
                name:'xsd:string'}.

test(extract_schema_employee,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Employee', JSON),
    JSON = json{'@base':"Employee/",
                '@id':'Employee',
                '@inherits':'Person',
                '@key':json{'@fields':[name,birthdate],
                            '@type':"Hash"},
                '@type':'Class',
                boss:json{'@class':'Employee','@type':'Optional'},
                staff_number:'xsd:string',
                tasks:json{'@class':'Task',
                           '@type':'List'}}.

test(extract_schema_colour,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Colour', JSON),
    JSON = json{'@id':'Colour',
                '@type':'Enum',
                '@value':[red,blue,green]}.

test(extract_schema_binary_tree,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'BinaryTree', JSON),

    JSON = json{'@base':"binary_tree/",
                '@id':'BinaryTree',
                '@key':json{'@type':"ValueHash"},
                '@type':'TaggedUnion',
                leaf:'sys:Unit',
                node:'Node'}.

test(insert_schema_object,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    get_schema_document(DB2, 'Squash', JSON),
    JSON = json{'@id':'Squash',
                '@key':json{'@fields':[genus,species],
                            '@type':"Lexical"},
                '@type':'Class',
                colour:'xsd:string',
                genus:'xsd:string',
                name:'xsd:string',
                shape:'xsd:string',
                species:'xsd:string'}.

test(delete_schema_document,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2,
                   _{ author : "me",
                      message : "Have you tried bitcoin?"
                    },
                   Context2),

    with_transaction(
        Context2,
        delete_schema_document(Context2, 'Squash'),
        _
    ),

    open_descriptor(Desc, DB3),
    \+ get_schema_document(DB3, 'Squash', _).

test(replace_schema_document,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2,
                   _{ author : "me",
                      message : "Have you tried bitcoin?"
                    },
                   Context2),

    New_Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Hash",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string",
       is_a_pumpkin : "xsd:boolean"
     },

    with_transaction(
        Context2,
        replace_schema_document(Context2, New_Document),
        _
    ),

    open_descriptor(Desc, DB3),
    get_schema_document(DB3, 'Squash', JSON),

    JSON =
    json{
        '@id':'Squash',
        '@key':json{'@fields':[genus,species],'@type':"Hash"},
        '@type':'Class',
        colour:'xsd:string',
        genus:'xsd:string',
        is_a_pumpkin:'xsd:boolean',
        name:'xsd:string',
        shape:'xsd:string',
        species:'xsd:string'}.

test(replace_schema_document_lexical_key,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus"] },
       genus : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2, _{ author : "me", message : "Have you tried bitcoin?" }, Context2),

    New_Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus"] },
       genus : "xsd:string"
     },

    with_transaction(
        Context2,
        replace_schema_document(Context2, New_Document),
        _
    ),

    open_descriptor(Desc, DB3),
    get_schema_document(DB3, 'Squash', JSON),

    JSON =
    json{
        '@id':'Squash',
        '@key':json{'@fields':[genus],'@type':"Lexical"},
        '@type':'Class',
        genus:'xsd:string'}.

test(double_insert_schema,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             can_not_insert_existing_object_with_id("Squash")
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2,
                   _{ author : "me",
                      message : "Have you tried bitcoin?"
                    },
                   Context2),

    New_Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Hash",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string",
       is_a_pumpkin : "xsd:boolean"
     },

    with_transaction(
        Context2,
        insert_schema_document(Context2, New_Document),
        _
    ).

test(comment_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       '@documentation' :
       _{ '@comment' : "Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica.",
          '@properties' : _{ genus : "The genus of the Cucurtiba is always Cucurtiba",
                             species : "There are between 13 and 30 species of Cucurtiba",
                             colour: "Red, Green, Brown, Yellow, lots of things here.",
                             shape: "Round, Silly, or very silly!" }
        },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Document, Context, Elaborated),

    Elaborated =
    json{ '@id':'https://s/Squash',
		  '@type':'http://terminusdb.com/schema/sys#Class',
		  'http://terminusdb.com/schema/sys#documentation':
          json{ '@container':"@set",
				'@type':'http://terminusdb.com/schema/sys#Documentation',
				'@value':[
                    json{ '@id':'https://s/Squash/0/documentation/Documentation',
						  '@type':'http://terminusdb.com/schema/sys#Documentation',
						  'http://terminusdb.com/schema/sys#comment':
                          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								'@value':"Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica."
							  },
						  'http://terminusdb.com/schema/sys#properties':
                          json{ '@id':'https://s/Squash/0/documentation/Documentation/properties/colour+genus+shape+species',
								'@type':'http://terminusdb.com/schema/sys#PropertyDocumentation',
								'https://s/colour':
                                json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
									  '@value':"Red, Green, Brown, Yellow, lots of things here."
									},
								'https://s/genus':
                                json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
									  '@value':"The genus of the Cucurtiba is always Cucurtiba"
									},
								'https://s/shape':
                                json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
									  '@value':"Round, Silly, or very silly!"
									},
								'https://s/species':
                                json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
									  '@value':"There are between 13 and 30 species of Cucurtiba"
									}
							  }
						}
				]
			  },
		  'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Squash/key/Lexical/genus+species',
				'@type':'http://terminusdb.com/schema/sys#Lexical',
				'http://terminusdb.com/schema/sys#fields':
                json{ '@container':"@list",
					  '@type':"@id",
					  '@value':[ json{ '@id':'https://s/genus',
									   '@type':"@id"
									 },
								 json{ '@id':'https://s/species',
									   '@type':"@id"
									 }
							   ]
					}
			  },
		  'https://s/colour':json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
								   '@type':"@id"
								 },
		  'https://s/genus':json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
								  '@type':"@id"
								},
		  'https://s/name':json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
								 '@type':"@id"
							   },
		  'https://s/shape':json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
								  '@type':"@id"
								},
		  'https://s/species':json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
									'@type':"@id"
								  }
		},

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context2),
    with_transaction(
        Context2,
        insert_schema_document(Context2, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    get_schema_document(DB2, 'Squash', New),

    New = json{'@documentation':
               json{'@comment':"Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica.",
                    '@properties':json{colour:"Red, Green, Brown, Yellow, lots of things here.",
                                       genus:"The genus of the Cucurtiba is always Cucurtiba",
                                       shape:"Round, Silly, or very silly!",
                                       species:"There are between 13 and 30 species of Cucurtiba"}
                   },
               '@id':'Squash',
               '@key':json{'@fields':[genus,species],
                           '@type':"Lexical"},
               '@type':'Class',
               colour:'xsd:string',
               genus:'xsd:string',
               name:'xsd:string',
               shape:'xsd:string',
               species:'xsd:string'}.

test(bad_documentation,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':invalid_property_in_property_documentation_object,
                          class:'http://s/Not_A_Squash',
                          predicate:'http://s/shape',
                          subject:'http://s/Not_A_Squash/0/documentation/Documentation/properties/genus+shape'}])
         )
     ]) :-

    Document =
    _{ '@id' : "Not_A_Squash",
       '@type' : "Class",
       '@documentation' :
       _{ '@comment' : "Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica.",
          '@properties' :
          _{ genus : "The genus of the Cucurtiba is always Cucurtiba",
             shape: "Round, Silly, or very silly!" }
        },
       genus : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context2),
    with_transaction(
        Context2,
        insert_schema_document(Context2, Document),
        _
    ).

test(bad_unfoldable,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure([witness{'@type':property_path_cycle_detected,
                                           class:_,
                                           path:_}]),
             _)
     ]) :-
     DocumentA =
     _{ '@id' : "A",
        '@type' : "Class",
        '@unfoldable' : [],
        p : "B" },

     DocumentB =
     _{ '@id' : "B",
        '@type' : "Class",
        '@unfoldable' : [],
        q : "A" },

     with_test_transaction(
         Desc,
         Context,
         (   insert_schema_document(Context, DocumentA),
             insert_schema_document(Context, DocumentB)
         ),
         _
     ).

test(good_unfoldable,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
     DocumentA =
     _{ '@id' : "A",
        '@type' : "Class",
        '@unfoldable' : [],
        p : "B" },

     DocumentB =
     _{ '@id' : "B",
        '@type' : "Class",
        q : "A" },

     with_test_transaction(
         Desc,
         Context,
         (   insert_schema_document(Context, DocumentA),
             insert_schema_document(Context, DocumentB)
         ),
         _
     ).

test(sub_unfoldable,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure([witness{'@type':property_path_cycle_detected,
                                           class:'http://s/A1',
                                           path:['http://s/q','http://s/A']}]),
             _)
     ]) :-
     DocumentA =
     _{ '@id' : "A",
        '@type' : "Class",
        '@unfoldable' : [],
        '@abstract' : [] },

     DocumentB =
     _{ '@id' : "A1",
        '@type' : "Class",
        '@inherits' : ["A"],
        q : "A" },

     with_test_transaction(
         Desc,
         Context,
         (   insert_schema_document(Context, DocumentA),
             insert_schema_document(Context, DocumentB)
         ),
         _
     ).

test(trans_unfoldable,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':property_path_cycle_detected,
                            class:_,
                            path:_}]),
               _)
     ]) :-
     DocumentA =
     _{ '@id' : "A",
        '@type' : "Class",
        '@unfoldable' : [],
        p : "B",
        '@abstract' : [] },

     DocumentB =
     _{ '@id' : "B",
        '@type' : "Class",
        '@unfoldable' : [],
        q : "C" },

     DocumentC =
     _{ '@id' : "C",
        '@type' : "Class",
        '@unfoldable' : [],
        r : "A" },

     with_test_transaction(
         Desc,
         Context,
         (   insert_schema_document(Context, DocumentA),
             insert_schema_document(Context, DocumentB),
             insert_schema_document(Context, DocumentC)
         ),
         _
     ).

test(oneof_unfoldable,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':property_path_cycle_detected,
                          class:'http://s/A',
                          path:['http://s/q','http://s/A']}]),
             _)
     ]) :-

     DocumentA =
     _{ '@id' : "A",
        '@type' : "Class",
        '@unfoldable' : [],
        '@oneOf' :
        _{
            p : "B",
            q : "A"
        },
        '@abstract' : [] },

     DocumentB =
     _{ '@id' : "B",
        '@type' : "Class",
        '@unfoldable' : [],
        q : "xsd:string" },

     with_test_transaction(
         Desc,
         Context,
         (   insert_schema_document(Context, DocumentA),
             insert_schema_document(Context, DocumentB)
         ),
         _
     ).

test(always_smaller_unfoldable,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

     DocumentA =
     _{ '@id' : "A",
        '@type' : "Class",
        '@unfoldable' : [],
        '@abstract' : []
      },

     DocumentB =
     _{ '@id' : "B",
        '@type' : "Class",
        '@unfoldable' : [],
        '@abstract' : []},

     DocumentA1 =
     _{ '@id' : "A1",
        '@type' : "Class",
        '@inherits' : ["A"],
        a1 : "B1"
      },

     DocumentB1 =
     _{ '@id' : "B1",
        '@type' : "Class",
        '@inherits' : ["B"],
        b1 : "A2"
      },

     DocumentA2 =
     _{ '@id' : "A2",
        '@type' : "Class",
        '@inherits' : ["A"],
        a2 : "xsd:string"
      },


     with_test_transaction(
         Desc,
         Context,
         (   insert_schema_document(Context, DocumentA),
             insert_schema_document(Context, DocumentB),
             insert_schema_document(Context, DocumentA1),
             insert_schema_document(Context, DocumentB1),
             insert_schema_document(Context, DocumentA2)
         ),
         _
     ).

test(subdocument_hash_key,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Has_Non_Squash =
    _{ '@id' : "Has_Non_Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Hash",
                   '@fields' : ["me"]},
       me : "xsd:string",
       non_squash : "Not_A_Squash"
     },

    Not_A_Squash =
    _{ '@id' : "Not_A_Squash",
       '@type' : "Class",
       '@subdocument' : [],
       '@key' : _{ '@type' : "Hash",
                   '@fields' : ["genus"]},
       genus : "xsd:string"
     },

    create_context(Desc, _{ author : "me", message : "Adding context" }, Context),
    with_transaction(
        Context,
        (   insert_schema_document(Context, Not_A_Squash),
            insert_schema_document(Context, Has_Non_Squash)
        ),
        _
    ),

    Document =
    _{ '@type' : "Has_Non_Squash",
       me : "It's me",
       non_squash : _{ '@type' : "Not_A_Squash",
                       genus : "Malus Mill" }},

    create_context(Desc, _{ author : "me", message : "Adding doc." }, Context2),
    with_transaction(
        Context2,
        insert_document(Context2, Document,Id),
        _
    ),

    get_document(Desc, Id, Assigned),

    Assigned =
    json{ '@id':'Has_Non_Squash/29c888f481e22801650eeafe95cfd4fc51d46a6b51fcd470f213dfa675deb8d9',
          '@type':'Has_Non_Squash',
          me:"It's me",
          non_squash:json{ '@id':'Has_Non_Squash/29c888f481e22801650eeafe95cfd4fc51d46a6b51fcd470f213dfa675deb8d9/non_squash/Not_A_Squash/7d9b9f4bfd817be17dc408732ceba768bd84c35f349932ff7fa1a716a94afbc9',
                           '@type':'Not_A_Squash',
                           genus:"Malus Mill"
		                 }
        }.

test(subdocument_lexical_key,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Has_Non_Squash =
    _{ '@id' : "Has_Non_Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["me"]},
       me : "xsd:string",
       non_squash : "Not_A_Squash"
     },

    Not_A_Squash =
    _{ '@id' : "Not_A_Squash",
       '@type' : "Class",
       '@subdocument' : [],
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus"]},
       genus : "xsd:string"
     },

    create_context(Desc, _{ author : "me", message : "Adding context" }, Context),
    with_transaction(
        Context,
        (   insert_schema_document(Context, Not_A_Squash),
            insert_schema_document(Context, Has_Non_Squash)
        ),
        _
    ),

    Document =
    _{ '@type' : "Has_Non_Squash",
       me : "It's me",
       non_squash : _{ '@type' : "Not_A_Squash",
                       genus : "Malus Mill" }},

    create_context(Desc, _{ author : "me", message : "Adding doc." }, Context2),
    with_transaction(
        Context2,
        insert_document(Context2, Document,Id),
        _
    ),

    get_document(Desc, Id, Assigned),
    !,

    Assigned =
    json{'@id':'Has_Non_Squash/It\'s%20me',
         '@type':'Has_Non_Squash',
         me:"It's me",
         non_squash:json{'@id': 'Has_Non_Squash/It\'s%20me/non_squash/Not_A_Squash/Malus%20Mill',
                         '@type':'Not_A_Squash',
                         genus:"Malus Mill"}}.

test(subdocument_lexical_key_with_odd_chars,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Has_Non_Squash =
    _{ '@id' : "Has_Non_Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["me"]},
       me : "xsd:string",
       non_squash : "Not_A_Squash"
     },

    Not_A_Squash =
    _{ '@id' : "Not_A_Squash",
       '@type' : "Class",
       '@subdocument' : [],
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus"]},
       genus : "xsd:string"
     },

    create_context(Desc, _{ author : "me", message : "Adding context" }, Context),
    with_transaction(
        Context,
        (   insert_schema_document(Context, Not_A_Squash),
            insert_schema_document(Context, Has_Non_Squash)
        ),
        _
    ),

    Document =
    _{ '@type' : "Has_Non_Squash",
       me : "Its / me",
       non_squash : _{ '@type' : "Not_A_Squash",
                       genus : "Malus / Mill" }},


    create_context(Desc, _{ author : "me", message : "Adding doc." }, Context2),
    with_transaction(
        Context2,
        insert_document(Context2, Document,Id),
        _
    ),

    get_document(Desc, Id, Assigned),
    !,

    Assigned =
    json{'@id':'Has_Non_Squash/Its%20%2F%20me',
         '@type':'Has_Non_Squash',
         me:"Its / me",
         non_squash:json{'@id': 'Has_Non_Squash/Its%20%2F%20me/non_squash/Not_A_Squash/Malus%20%2F%20Mill',
                         '@type':'Not_A_Squash',
                         genus:"Malus / Mill"}}.


test(document_with_no_required_field,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [json{'@type':required_field_does_not_exist_in_document,
                       document:
                       json{'@id':'http://i/Moo/doug',
                            '@type':'http://s/Moo'},
                       field:'http://s/name'}]),
             _)
     ]) :-

    Document = _{ '@id' : "Moo/doug",
                  '@type' : "Moo"},

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_document(Context, Document, _Id),
        _
    ).

test(add_a_double_field,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':instance_not_cardinality_one,
                            class:'http://www.w3.org/2001/XMLSchema#string',
                            instance:'http://i/Moo/doug',
                            predicate:'http://s/name'}
                   ]),
               _)
     ]) :-
    Document = _{ '@id' : "Moo/doug",
                  '@type' : "Moo",
                  name : "moo?"},

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_document(Context, Document, Id),
        _
    ),

    create_context(Desc, _{ author : "me", message : "Jaws Part 2" }, Context2),

    with_transaction(
        Context2,
        ask(Context2, insert(Id, name, "moo!"^^xsd:string)),
        _
    ).

schema2_0('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }


{ "@id" : "Task",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "name" : "xsd:string" }

{ "@id" : "TaskList",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "task_list" : { "@type" : "List",
                  "@class" : "Task" }}

{ "@id" : "TaskArray",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "task_list" : { "@type" : "Array",
                  "@class" : "Task" }}

{ "@id" : "Thing1",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "thing2" : "Thing2",
  "name" : "xsd:string" }

{ "@id" : "Thing2",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "name" : "xsd:string" }

').

test(delete_list_element,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc),
                 write_schema(schema2_0,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [
                       _{'@type':deleted_object_still_referenced,
                         object:"http://i/Task/task3",
                         predicate:'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',subject:_}]),
               _)
     ]) :-
    Document = _{ '@id' : "TaskList/my_task_list",
                  '@type' : "TaskList",
                  task_list : [
                      _{ '@id' : "Task/task1",
                         '@type' : "Task",
                         name : "Laundry"},
                      _{ '@id' : "Task/task2",
                         '@type' : "Task",
                         name : "Rubbish"},
                      _{ '@id' : "Task/task3",
                         '@type' : "Task",
                         name : "Dishes"}
                  ]},

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_document(Context, Document, _Id),
        _
    ),

    create_context(Desc, _{ author : "me", message : "Jaws Part 2" }, Context2),
    with_transaction(
        Context2,
        delete_document(Context2, 'Task/task3'),
        _
    ).

test(delete_referenced_object,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc),
                 write_schema(schema2_0,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':instance_not_cardinality_one,
                            class:'http://s/Thing2',
                            instance:_,
                            predicate:'http://s/thing2'
                           }]),
               _)
     ]) :-

    Document0 =
    _{ '@type' : "Thing1",
       name : "Joe",
       thing2 : _{ '@ref' :  "Jim" }
     },

    Document1 =
    _{ '@type' : "Thing2",
       '@capture' : "Jim",
       name : "Jim"
     },

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    empty_assoc(Captures_In),
    with_transaction(
        Context,
        (   insert_document(Context, Document0, false, Captures_In, _Id1, _, Captures_Out1),
            insert_document(Context, Document1, false, Captures_Out1, Id2, _, _)
        ),
        _
    ),
    create_context(Desc, _{ author : "me", message : "Jaws Part 2" }, Context2),
    with_transaction(
        Context2,
        delete_document(Context2, Id2),
        _
    ).

test(alter_documentation,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Doc1 = _{ '@type': "Class",
              '@id': "Doc001",
              '@key': _{
                          '@type': "Random"
                      },
              '@documentation': _{
                                    '@comment': "comment 01"
                                }
            },

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Doc1),
        _
    ),

    Doc2 = _{ '@type': "Class",
              '@id': "Doc001",
              '@key': _{
                          '@type': "Random"
                      },
              '@documentation': _{
                                    '@comment': "comment 02"
                                }
            },

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, Context2),
    with_transaction(
        Context2,
        replace_schema_document(Context2, Doc2),
        _
    ),

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Doc001', JSON),

    "comment 02" = (JSON.'@documentation'.'@comment').

test(elaborate_null,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Class = _{ '@type': "Class",
               '@id': "Doc",
               s : _{ '@type' : "Optional",
                      '@class' : "xsd:string"}
             },

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Class),
        _
    ),

    Document = _{ '@type' : "Doc",
                  s : null },


    open_descriptor(Desc, DB),
    json_elaborate(DB, Document, Elaborated),

    database_prefixes(DB,Prefixes),
    findall(Triple, json_triple_(Elaborated,Prefixes,Triple), Triples),

    Triples = [
        t(_,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://somewhere.for.now/schema#Doc')
    ].

schema10('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }
{ "@type" : "Class",
  "@id" : "Boolean",
  "b" : "xsd:boolean" }
').

test(boolean_in_boolean_field,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc),
                 write_schema(schema10, Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Document = _{ '@type': "Boolean", b: true },
    create_context(Desc, _{ author: "a", message: "m" }, Context),
    with_transaction(
        Context,
        insert_document(Context, Document, _Id),
        _
    ).

test(round_trip_float,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Geo_Schema_Atom = '{ "@type" : "Class",
      "@id" : "GeoCoordinate",
      "@key" : { "@type" : "Lexical",
                 "@fields" : ["latitude", "longitude"] },
      "latitude" : "xsd:decimal",
      "longitude" : "xsd:decimal"
    }',

    atom_json_dict(Geo_Schema_Atom, Geo_Schema, []),

    Geo_Atom = '{
        "@type": "GeoCoordinate",
        "latitude": "41.2008",
        "longitude": "0.5679"
    }',

    atom_json_dict(Geo_Atom, Geo, []),


    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              Geo_Schema)
                         ),

    with_test_transaction(Desc,
                          C2,
                          insert_document(
                              C2,
                              Geo,
                              Uri)),
    get_document(Desc, Uri, Doc),

    Doc = json{'@id':'GeoCoordinate/41.2008+0.5679',
               '@type':'GeoCoordinate',
               latitude:41.2008,
                           longitude:0.5679}.

:- use_module(core(query)).
test(status_update,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [
                       witness{'@type':instance_not_of_class,
                               class:'http://somewhere.for.now/schema#Status',
                               instance:'http://somewhere.for.now/document/Status/inactive'
                              }
                   ]),
               _)
     ]) :-

    Enum_Atom = '
      { "@type" : "Enum",
        "@id" : "Status",
        "@value" : [ "active", "inactive" ]
      }',
    Object_Atom = '
      { "@type" : "Class",
        "@id" : "Object",
        "@key" : { "@type" : "Lexical",
                   "@fields" : ["name"] },
        "name" : "xsd:string",
        "status" : "Status"
      }',

    atom_json_dict(Enum_Atom, Enum, []),
    atom_json_dict(Object_Atom, Object, []),

    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(
                                  C1,
                                  Enum),
                              insert_schema_document(
                                  C1,
                                  Object)
                          )
                         ),

    Doc_Atom = '{
        "@type": "Object",
        "name": "foo",
        "status": "active"
    }',

    atom_json_dict(Doc_Atom, Doc, []),


    with_test_transaction(Desc,
                          C2,
                          insert_document(
                              C2,
                              Doc,
                              Uri)),

    with_test_transaction(
        Desc,
        C3,
        ask(C3,
            (   t(Uri, name, "foo"^^xsd:string),
                t(Uri, status, Status),
                delete(Uri, status, Status),
                % Note: Instance prefix is used by default and not @schema
                insert(Uri, status, 'Status/inactive')
            )
           )
    ).

test(status_update2,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-


    Schema_Atom = '[
      { "@type" :"Class",
        "@id" : "APIKey" },
      {
        "@id": "User",
        "@inherits": "Entity",
        "@key": {
            "@fields": [
                "user_id"
            ],
            "@type": "Lexical"
        },
        "@type": "Class",
        "api_key": {
            "@class": "APIKey",
            "@type": "Set"
        },
        "company": "xsd:string",
        "email": "xsd:string",
        "first_name": "xsd:string",
        "last_name": "xsd:string",
        "picture": "xsd:string",
        "registration_date": {
            "@class": "xsd:dateTime",
            "@type": "Optional"
        },
        "user_id": "xsd:string"
      },
    {
        "@abstract": [],
        "@id": "Entity",
        "@type": "Class",
        "status": "Status"
    },
    {
        "@id": "Status",
        "@type": "Enum",
        "@value": [
            "pending",
            "inactive",
            "active",
            "needs_invite",
            "invite_sent",
            "accepted",
            "rejected"
        ]
    },
    {
        "@id": "Invitation",
        "@inherits": "Entity",
        "@key": {
            "@fields": [
                "invited_by",
                "email_to",
                "creation_date"],
            "@type": "Hash"
        },
        "@subdocument": [],
        "@type": "Class",
        "email_to": "xsd:string",
        "invited_by": "User",
        "note": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "role": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "creation_date":"xsd:dateTime"
    }]',

    atom_json_dict(Schema_Atom, Docs, []),

    with_test_transaction(
        Desc,
        C1,
        forall(member(Doc, Docs),
               insert_schema_document(
                   C1,
                   Doc))
    ),

    User_Atom = '{
        "@type": "User",
        "company": "orgTest",
        "email": "collaborator@gmail.com",
        "first_name": "collaborator",
        "last_name": "collaborator",
        "picture": "https://s.gravatar.com/avatar/5d4d9906d3b46bdcaad9221ce335b754?s=480&r=pg&d=https%3A%2F%2Fcdn.auth0.com%2Favatars%2Fco.png",
        "status": "active",
        "user_id": "auth0|615462f8ab33f4006a6bee0c"
    }',
    atom_json_dict(User_Atom, User, []),

    with_test_transaction(
        Desc,
        C2,
        insert_document(
            C2,
            User,
            _User_Uri)
    ),

    Invitation_Atom = '{
        "@id": "Invitation",
        "@inherits": "Entity",
        "@key": {
            "@fields": [
                "invited_by",
                "email_to",
                "creation_date"],
            "@type": "Hash"
        },
        "@subdocument": [],
        "@type": "Class",
        "email_to": "xsd:string",
        "invited_by": "User",
        "note": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "role": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "creation_date":"xsd:dateTime"
    }',
    atom_json_dict(Invitation_Atom, Invitation, []),

    with_test_transaction(
        Desc,
        C3,
        delete_schema_document(C3, "Invitation")
    ),

    with_test_transaction(
        Desc,
        C4,
        insert_schema_document(C4, Invitation)
    ),

    with_test_transaction(
        Desc,
        C5,
        replace_schema_document(C5, Invitation)
    ).

test(property_documentation_mismatch,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':invalid_property_in_property_documentation_object,
                            class:'http://somewhere.for.now/schema#User',
                            predicate:'http://somewhere.for.now/schema#times',
                            subject:'http://somewhere.for.now/schema#User/0/documentation/Documentation/properties/times'}]), _)
     ]) :-

    Schema_Atom = '{
        "@id": "User",
        "@documentation" : {
        "@comment" : "A user",
        "@properties" : { "times" : "Wrong documentation." }
  },
        "@key": {
            "@fields": [
                "user_id"
            ],
            "@type": "Lexical"
        },
        "@type": "Class",
        "company": "xsd:string",
        "email": "xsd:string",
        "first_name": "xsd:string",
        "last_name": "xsd:string",
        "picture": "xsd:string",
        "registration_date": {
            "@class": "xsd:dateTime",
            "@type": "Optional"
        },
        "user_id": "xsd:string"
    }',

    atom_json_dict(Schema_Atom, Doc, []),

    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            Doc)
    ).

test(inherits_documentation_multi,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Schema_Atom1 = '{
        "@abstract": [],
        "@documentation": [
            {
                "@comment": "An abstract Subject Class",
                "@label": "Subject",
                "@language": "en",
                "@properties": {
                    "Number_of_classes_attended": {
                        "@comment": "Number of Classes Attended",
                        "@label": "Classes Attended"
                    },
                    "course_start_date": {
                        "@comment": "Course Start Date",
                        "@label": "Start Date"
                    }
                }
            },
            {
                "@comment": "აბსტრაქტული საგნობრივი კლასი",
                "@label": "საგანი",
                "@language": "ka",
                "@properties": {
                    "Number_of_classes_attended": {
                        "@comment": "კლასების რაოდენობა",
                        "@label": "კლასები დაესწრო"
                    },
                    "course_start_date": {
                        "@comment": "კურსის დაწყების თარიღი",
                        "@label": "Დაწყების თარიღი"
                    }
                }
            }
        ],
        "@id": "Subject",
        "@subdocument": [],
        "@type": "Class",
        "Number_of_classes_attended": {
            "@class": "xsd:integer",
            "@type": "Optional"
        },
        "course_start_date": {
            "@class": "xsd:dateTime",
            "@type": "Optional"
        }
    }',
    Schema_Atom2 = '{
        "@id": "Maths",
        "@documentation": [
            {
                "@comment": "A Maths Subject class",
                "@label": "Maths",
                "@language": "en",
                "@properties": {
                    "level": {
                        "@comment": "Math level",
                        "@label": "Level"
                    },
                    "love_maths": {
                        "@comment": "a choice to love maths",
                        "@label": "Do you like Maths?"
                    }
                }
            },
            {
                "@comment": "მათემატიკის გაკვეთილი",
                "@label": "Მათემატიკა",
                "@language": "ka",
                "@properties": {
                    "level": {
                        "@comment": "მათემატიკის დონე",
                        "@label": "დონე"
                    },
                    "love_maths": {
                        "@comment": "არჩევანი გიყვარდეს მათემატიკა",
                        "@label": "მოგწონთ მათემატიკა?"
                    }
                }
            }
        ],
        "@inherits": "Subject",
        "@key": {
            "@type": "Random"
        },
        "@subdocument": [],
        "@type": "Class",
        "level": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "love_maths": {
            "@class": "xsd:boolean",
            "@type": "Optional"
        }
    }',

    atom_json_dict(Schema_Atom1, Schema1, []),
    atom_json_dict(Schema_Atom2, Schema2, []),

    with_test_transaction(
        Desc,
        C1,
        (   insert_schema_document(
                C1,
                Schema1),
            insert_schema_document(
                C1,
                Schema2)
        )
    ),

    class_frame(Desc, 'Maths', Maths, [compress_ids(true)]),

    Maths =
    json{ '@documentation':
          [ json{ '@language':"en",
                  '@comment': "A Maths Subject class",
                  '@label': "Maths",
				  '@properties':
                  json{
                      'Number_of_classes_attended':
                      json{ '@comment':"Number of Classes Attended",
							'@label':"Classes Attended"
						  },
					  course_start_date:json{ '@comment':"Course Start Date",
											  '@label':"Start Date"
											},
					  level:json{ '@comment':"Math level",
								  '@label':"Level"
								},
					  love_maths:json{ '@comment':"a choice to love maths",
									   '@label':"Do you like Maths?"
									 }
				  }
				},
			json{ '@language':"ka",
                  '@comment': "მათემატიკის გაკვეთილი",
                  '@label': "Მათემატიკა",
				  '@properties':json{ 'Number_of_classes_attended':
                                      json{ '@comment':"კლასების რაოდენობა",
											'@label':"კლასები დაესწრო"
										  },
									  course_start_date:
                                      json{
                                          '@comment':"კურსის დაწყების თარიღი",
										  '@label':"\u1C93აწყების თარიღი"
									  },
									  level:json{ '@comment':"მათემატიკის დონე",
												  '@label':"დონე"
												},
									  love_maths:json{ '@comment':"არჩევანი გიყვარდეს მათემატიკა",
													   '@label':"მოგწონთ მათემატიკა?"
													 }
									}
				}
		  ],
		  '@key':json{'@type':"Random"},
		  '@subdocument':[],
		  '@type':'Class',
		  'Number_of_classes_attended':json{ '@class':'xsd:integer',
										     '@type':'Optional'
										   },
		  course_start_date:json{ '@class':'xsd:dateTime',
								  '@type':'Optional'
								},
		  level:json{ '@class':'xsd:string',
					  '@type':'Optional'
					},
		  love_maths:json{ '@class':'xsd:boolean',
						   '@type':'Optional'
						 }
		}.

test(inherits_documentation,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Schema_Parent =
    _{
        '@id' : "Agent",
        '@type' : "Class",
        '@documentation' :
        [_{ '@language' : "en",
            '@label' : "Agent",
            '@comment' : "An agent",
            '@properties' : _{ name : "Super Docs",
                               id : "Id"}},
        _{ '@language' : "de",
           '@label' : "Agent*in",
           '@comment' : "Ein*e Agent*in",
           '@properties' : _{ name : _{ '@label' : "Name",
                                        '@comment' : "Uberdokument" },
                              id : _{ '@label' : "ID",
                                      '@comment' : "ID des Agent*in"}}}],
        id : "xsd:integer",
        name : "xsd:string"},

    Schema_Child =
    _{
        '@id' : "User",
        '@type' : "Class",
        '@inherits' : ["Agent"],
        '@documentation' :
        [_{ '@language' : "en",
            '@label' : "User",
            '@comment' : "A user",
            '@properties' : _{ name : "Sub Docs",
                               age : "Age"}},
         _{ '@language' : "de",
            '@label' : "Benutzer",
            '@properties' : _{ name : "Unterdokument",
                               age : "Das Alter"}}],
        age : "xsd:integer"
    },

    with_test_transaction(
        Desc,
        C1,
        (   insert_schema_document(
                C1,
                Schema_Parent),
            insert_schema_document(
                C1,
                Schema_Child)
        )
    ),

    class_frame(Desc, 'User', User, [compress_ids(true)]),
    User =
    json{ '@documentation':
          [ json{ '@comment':"A user",
				  '@label':"User",
				  '@language':"en",
			      '@properties':json{age:"Age",id:"Id",name:"Sub Docs"}
			    },
			json{ '@language':"de",
			      '@label':"Benutzer",
				  '@properties':json{ age:"Das Alter",
						              id:json{ '@comment':"ID des Agent*in",
							                   '@label':"ID"
							                 },
						              name:"Unterdokument"
						            }
			    }
		  ],
          '@type':'Class',
          age:'xsd:integer',
          id:'xsd:integer',
          name:'xsd:string'
        }.

test(empty_test_for_optional,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':instance_not_cardinality_one,
                          class:'http://www.w3.org/2001/XMLSchema#string',
                          instance:_,
                          predicate:'http://somewhere.for.now/schema#name'}
                 ]),
             _)
     ]) :-

    Schema = _{ '@id': 'Foo', '@type': 'Class' },
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            Schema)
    ),
    Doc = _{ '@type': 'Foo' },
    with_test_transaction(Desc,
                          C2,
                          insert_document(
                              C2,
                              Doc,
                              Uri)),
    Schema2 = _{ '@id' : 'Foo', '@type': 'Class', name : 'xsd:string' },
    with_test_transaction(Desc,
                          C3,
                          replace_schema_document(
                              C3,
                              Schema2)),

    get_document(Desc, Uri, _).

test(elaborate_one_of,
     []) :-
    Doc = _{ '@id' : "Choice2",
             '@type' : "Class",
             '@oneOf' : _{ a : "xsd:string",
                           b : "xsd:integer" }},
    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),
    json_schema_elaborate(Doc, Context, Elaborated),

    Elaborated =
    json{'@id':'https://s/Choice2',
         '@type':'http://terminusdb.com/schema/sys#Class',
         'http://terminusdb.com/schema/sys#oneOf':
         json{'@container':"@set",
              '@type':'http://terminusdb.com/schema/sys#Choice',
              '@value':[json{'@id':'https://s/Choice2/oneOf/a+b',
                             '@type':'http://terminusdb.com/schema/sys#Choice',
                             'https://s/a':
                             json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                  '@type':"@id"},
                             'https://s/b':
                             json{'@id':'http://www.w3.org/2001/XMLSchema#integer',
                                  '@type':"@id"}}]}}.

test(extract_one_of,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Choice2', JSON),

    JSON = json{'@id':'Choice2',
                '@oneOf':json{a:'xsd:string',
                              b:'xsd:integer'},
                '@type':'Class'}.

test(two_oneof_an_error,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         error(
             schema_check_failure(
                 [ json{ '@type':choice_has_too_many_answers,
				         choice:json{ 'http://s/a':'http://www.w3.org/2001/XMLSchema#string',
							          'http://s/b':'http://www.w3.org/2001/XMLSchema#integer'
						            },
				         document:json{ '@type':'http://s/Choice2',
						                'http://s/a':"asdf",
						                'http://s/b':1
						              }
				       }
			     ]),
             _),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{'@type' : 'Choice2',
                    a : "asdf",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _Id).

test(no_oneof_an_error,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [json{ '@type':no_choice_is_cardinality_one,
				        choice:
                        json{ 'http://s/a':'http://www.w3.org/2001/XMLSchema#string',
							  'http://s/b':'http://www.w3.org/2001/XMLSchema#integer'
						    },
				        document:json{'@type':'http://s/Choice2'}
				      }
			     ]),
             _)
     ]) :-

    Document = json{'@type' : 'Choice2'},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _Id).

test(inheritence_of_tagged_union,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{'@type' : 'InheritsChoice',
                    a : "asdf",
                    c : "boo"},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(inheritence_of_tagged_union_fails,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [json{ '@type':choice_has_too_many_answers,
				          choice:json{ 'http://s/a':'http://www.w3.org/2001/XMLSchema#string',
							           'http://s/b':'http://www.w3.org/2001/XMLSchema#integer'
						             },
				          document:json{ '@type':'http://s/InheritsChoice',
						                 'http://s/a':"asdf",
						                 'http://s/b':1,
						                 'http://s/c':"fdsa"
						               }
				        }
			       ]),
               _)
     ]) :-

    Document = json{'@type' : 'InheritsChoice',
                    a : "asdf",
                    c : "fdsa",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(mixed_anyof,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{'@type' : 'Choice3',
                    c : "some_string",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(inherits_oneof,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{'@type' : 'InheritsChoice2',
                    c : "some_string",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(inherits_two_oneofs,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{'@type' : 'InheritsChoices',
                    c : "some_string",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(inherits_two_oneofs_error,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [json{'@type':choice_has_too_many_answers,
                       choice:
                       json{'http://s/a':'http://www.w3.org/2001/XMLSchema#string',
                            'http://s/b':'http://www.w3.org/2001/XMLSchema#integer'},
                       document:
                       json{'@type':'http://s/InheritsChoices',
                            'http://s/a':"beep",
                            'http://s/b':1,
                            'http://s/c':"some_string"}}]),
             _)
     ]) :-

    Document = json{'@type' : 'InheritsChoices',
                    a : "beep",
                    c : "some_string",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(double_choice_round_trip,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'DoubleChoice', JSON),
    JSON = json{'@id':'DoubleChoice',
                '@oneOf':[json{a:'xsd:string',
                               b:'xsd:integer'},
                          json{c:'xsd:string',
                               d:'xsd:integer'}],
                '@type':'Class'}.

test(double_choice_error,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [json{'@type':choice_has_too_many_answers,
                         choice:
                         json{'http://s/a':
                              'http://www.w3.org/2001/XMLSchema#string',
                              'http://s/b':
                              'http://www.w3.org/2001/XMLSchema#integer'},
                         document:json{'@type':'http://s/DoubleChoice',
                                       'http://s/a':"beep",
                                       'http://s/b':1,
                                       'http://s/c':"some_string"}}]),_)
     ]) :-

    Document = json{'@type' : 'DoubleChoice',
                    a : "beep",
                    c : "some_string",
                    b : 1},
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _).

test(double_choice_triples,[]) :-
    Document = json{'@id':'http://s/DoubleChoice',
                    '@type':'http://terminusdb.com/schema/sys#Class',
                    'http://terminusdb.com/schema/sys#oneOf':
                    json{'@container':"@set",
                         '@type':"@id",
                         '@value':[
                             json{'@id':'http://s/oneOf/DoubleChoice/a+b',
                                  'http://s/a':
                                  json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                       '@type':"@id"},
                                  'http://s/b':
                                  json{'@id':'http://www.w3.org/2001/XMLSchema#integer',
                                       '@type':"@id"}},
                             json{'@id':'http://s/oneOf/DoubleChoice/c+d',
                                  'http://s/c':
                                  json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                       '@type':"@id"},
                                  'http://s/d':
                                  json{'@id':'http://www.w3.org/2001/XMLSchema#integer',
                                       '@type':"@id"}}]}},
    Context = _{'@base':"http://i/",
                '@schema':"http://s/",
                '@type':"@context",
                api:'http://terminusdb.com/schema/api#',
                owl:'http://www.w3.org/2002/07/owl#',
                rdf:'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
                rdfs:'http://www.w3.org/2000/01/rdf-schema#',
                sys:'http://terminusdb.com/schema/sys#',
                vio:'http://terminusdb.com/schema/vio#',
                woql:'http://terminusdb.com/schema/woql#',
                xdd:'http://terminusdb.com/schema/xdd#',
                xsd:'http://www.w3.org/2001/XMLSchema#'},
    findall(
        Triple,
        json_triple_(Document, Context, Triple),
        Triples),

    Triples = [
        t('http://s/DoubleChoice',
		  'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		  'http://terminusdb.com/schema/sys#Class'),
		t('http://s/DoubleChoice',
		  'http://terminusdb.com/schema/sys#oneOf',
		  'http://s/oneOf/DoubleChoice/a+b'),
		t('http://s/oneOf/DoubleChoice/a+b',
		  'http://s/a',
		  'http://www.w3.org/2001/XMLSchema#string'),
		t('http://s/oneOf/DoubleChoice/a+b',
		  'http://s/b',
		  'http://www.w3.org/2001/XMLSchema#integer'),
		t('http://s/DoubleChoice',
		  'http://terminusdb.com/schema/sys#oneOf',
		  'http://s/oneOf/DoubleChoice/c+d'),
		t('http://s/oneOf/DoubleChoice/c+d',
		  'http://s/c',
		  'http://www.w3.org/2001/XMLSchema#string'),
		t('http://s/oneOf/DoubleChoice/c+d',
		  'http://s/d',
		  'http://www.w3.org/2001/XMLSchema#integer')
	].

test(double_choice_frame,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc,'DoubleChoice',Frame),

    Frame = json{'@oneOf':[json{a:'xsd:string',b:'xsd:integer'},
                           json{c:'xsd:string',d:'xsd:integer'}],
                 '@type':'Class'}.

test(double_choice_frame_uncompressed,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc,'DoubleChoice',Frame,[compress_ids(false)]),
    Frame = json{'@oneOf':[json{'http://s/a':'http://www.w3.org/2001/XMLSchema#string',
                                'http://s/b':'http://www.w3.org/2001/XMLSchema#integer'},
                           json{'http://s/c':'http://www.w3.org/2001/XMLSchema#string',
                                'http://s/d':'http://www.w3.org/2001/XMLSchema#integer'}],
                 '@type':'http://terminusdb.com/schema/sys#Class'}.

test(mixed_frame,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc,'Choice3',Frame),
    Frame = json{'@oneOf':[json{a:'xsd:string',b:'xsd:integer'}],
                 c:'xsd:string',
                 '@type':'Class'}.

test(oneof_unit,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema2,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc,'EnumChoice',Frame),

    Frame = json{'@oneOf':[json{a:'sys:Unit',b:'sys:Unit',c:'sys:Unit',d:'sys:Unit'}],
                 '@type':'Class'}.

test(enum_documentation,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Schema = _{ '@id': 'Pet',
                '@type': 'Enum',
                '@value' : ["dog","cat"],
                '@documentation' : _{ '@comment' : "What kind of pet?",
                                      '@values' : _{dog : "A doggie",
                                                    cat : "A kitty" }
                                    }
              },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),
    json_schema_elaborate(Schema, Context, Elaborate),

    Elaborate =
    json{'@id':'https://s/Pet',
         '@type':'http://terminusdb.com/schema/sys#Enum',
         'http://terminusdb.com/schema/sys#documentation':
         json{'@container':"@set",
              '@type':'http://terminusdb.com/schema/sys#Documentation',
              '@value':[
                  json{'@id':'https://s/Pet/0/documentation/Documentation',
                       '@type':'http://terminusdb.com/schema/sys#Documentation',
                       'http://terminusdb.com/schema/sys#comment':
                       json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                            '@value':"What kind of pet?"},
                       'http://terminusdb.com/schema/sys#values':
                       json{'@id':'https://s/Pet/0/documentation/Documentation/values/cat+dog',
                            '@type':'http://terminusdb.com/schema/sys#EnumDocumentation',
                            'https://s/Pet/cat':
                            json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                 '@value':"A kitty"},
                            'https://s/Pet/dog':
                            json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                 '@value':"A doggie"}}}]},
         'http://terminusdb.com/schema/sys#value':
         json{'@container':"@list",
              '@type':"@id",
              '@value':[json{'@id':'https://s/Pet/dog',
                             '@type':"@id"},
                        json{'@id':'https://s/Pet/cat',
                             '@type':"@id"}]}},

    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            Schema)
    ),

    open_descriptor(Desc, Trans),
    get_schema_document(Trans, 'Pet', Result),
    Result = json{'@documentation':
                  json{'@comment':"What kind of pet?",
                       '@values':json{cat:"A kitty",
                                      dog:"A doggie"}},
                  '@id':'Pet',
                  '@type':'Enum',
                  '@value':[dog,cat]}.

:- end_tests(json).

:- begin_tests(schema_checker, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(unknown_property,
     [
         setup(
             (
                 setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(teardown_temp_store(State)),
         error(
             schema_check_failure(
                 [json{'@type':unknown_property_for_type,
                       document:json{'@type':'http://somewhere.for.now/schema#Test',
                                     'http://somewhere.for.now/schema#unknownProperty':abc},
                       property:['http://somewhere.for.now/schema#unknownProperty'],
                       type:'http://somewhere.for.now/schema#Test'}]),
               _)
     ]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{ '@type': "Class",
                                 '@id': "Test" }
                          )),

    with_test_transaction(Desc,
                          C2,
                          insert_document(
                              C2,
                              _{ '@type': "Test",
                                 unknownProperty: 'abc'
                               },
                              _ID
                          )).

test(context_missing,
     [
         setup((setup_temp_store(State), test_document_label_descriptor(Desc))),
         cleanup(teardown_temp_store(State)),
         error(no_context_found_in_schema,_)
     ]) :-
    write_schema_string('{"@base": "http://b/", "@schema": "http://s/"}', Desc).

test(context_missing_base_prefix,
     [
         setup((setup_temp_store(State), test_document_label_descriptor(Desc))),
         cleanup(teardown_temp_store(State)),
         error(
             schema_check_failure(
                 [witness{
                      '@type': context_missing_system_prefix,
                      prefix_name: 'http://terminusdb.com/schema/sys#base'
                  }]))
     ]) :-
    write_schema_string('{"@type": "@context", "@schema": "http://s/"}', Desc).

test(context_missing_schema_prefix,
     [
         setup((setup_temp_store(State), test_document_label_descriptor(Desc))),
         cleanup(teardown_temp_store(State)),
         error(
             schema_check_failure(
                 [witness{
                      '@type': context_missing_system_prefix,
                      prefix_name: 'http://terminusdb.com/schema/sys#schema'
                  }]))
     ]) :-
    write_schema_string('{"@type": "@context", "@base": "http://b/"}', Desc).

test(context_base_prefix_not_uri,
     [
         setup((setup_temp_store(State), test_document_label_descriptor(Desc))),
         cleanup(teardown_temp_store(State)),
         error(
             schema_check_failure(
                 [witness{
                      '@type': context_system_prefix_is_not_a_uri,
                      prefix_name: 'http://terminusdb.com/schema/sys#base',
                      prefix_value: "b/"
                  }]))
     ]) :-
    write_schema_string('{"@type": "@context", "@base": "b/", "@schema": "http://s/"}', Desc).

test(context_schema_prefix_not_uri,
     [
         setup((setup_temp_store(State), test_document_label_descriptor(Desc))),
         cleanup(teardown_temp_store(State)),
         error(
             schema_check_failure(
                 [witness{
                      '@type': context_system_prefix_is_not_a_uri,
                      prefix_name: 'http://terminusdb.com/schema/sys#schema',
                      prefix_value: "s/"
                  }]))
     ]) :-
    write_schema_string('{"@type": "@context", "@base": "http://b/", "@schema": "s/"}', Desc).

schema3('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@inherits" : "Engineer" }

{ "@id" : "Employee",
  "@type" : "Class",
  "@inherits" : "Person" }

{ "@id" : "Engineer",
  "@type" : "Class",
  "@inherits" : "Employee" }
').

test(check_for_cycles_bad,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [
                     witness{'@type':cycle_in_class,
                             from_class:_,
                             path:[_,
                                   _,
                                   _,
                                   _],
                             to_class:_}]),
             _)
     ]) :-

    write_schema(schema3,Desc).

schema4('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Top",
  "@type" : "Class",
  "bottom_face" : { "@type" : "Optional",
                    "@class" : "Bottom" } }

{ "@id" : "Left",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:string",
  "right_face" : { "@type" : "List",
                   "@class" : "Right" } }

{ "@id" : "Right",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:string",
  "left_face" : { "@type" : "Set",
                  "@class" : "Left" } }

{ "@id" : "Bottom",
  "@type" : "Class",
  "@inherits" : [ "Right", "Left"],
  "top_face" : { "@type" : "Array",
                 "@class" : "Top" }  }
').

test(elaborate_multiple_inheritance, []) :-
    Doc = json{'@id':"Bottom",
               '@inherits':["Right", "Left"],
               '@type':"Class"},
    Ctxt = json{'@base':"http://i/", '@schema':"http://s/"},
    json_schema_elaborate(
        Doc,
        Ctxt,
        Result),

    Result = json{'@id':'http://s/Bottom',
                  '@inherits':
                  json{'@container':"@set",
                       '@type':"@id",
                       '@value':['http://s/Right','http://s/Left']},
                  '@type':'http://terminusdb.com/schema/sys#Class'},

    findall(t(S,P,O),
            json_triple_(Result,Ctxt,t(S,P,O)),
            Triples),
    Triples = [t('http://s/Bottom',
                 'http://terminusdb.com/schema/sys#inherits',
                 'http://s/Right'),
               t('http://s/Bottom',
                 'http://terminusdb.com/schema/sys#inherits',
                 'http://s/Left'),
               t('http://s/Bottom',
                 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                 'http://terminusdb.com/schema/sys#Class')].

test(diamond_ok,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    write_schema(schema4,Desc),

    open_descriptor(Desc, Transaction),
    class_frame(Transaction, "Bottom", Frame),

    Frame = json{ '@type':'Class',
				  bottom_face:json{ '@class':'Bottom',
							        '@type':'Optional'
							      },
				  left_face:json{'@class':'Left','@type':'Set'},
				  right_face:json{'@class':'Right','@type':'List'},
				  thing:'xsd:string',
				  top_face:json{ '@class':'Top',
						         '@dimensions':1,
						         '@type':'Array'
						       }
				}.

test(extract_bottom,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema4,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Bottom', JSON),
    JSON = json{'@id':'Bottom',
                '@inherits':['Left','Right'],
                '@type':'Class',
                top_face:json{'@class':'Top',
                              '@type':'Array',
                              '@dimensions':1}}.

% NOTE: We need to check diamond properties at schema creation time
schema5('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Top",
  "@type" : "Class",
  "bottom_face" : { "@type" : "Optional",
                    "@class" : "Bottom" } }

{ "@id" : "Left",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:string",
  "right_face" : { "@type" : "List",
                   "@class" : "Right" } }

{ "@id" : "Right",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:dateTime",
  "left_face" : { "@type" : "Set",
                  "@class" : "Left" } }

{ "@id" : "Bottom",
  "@type" : "Class",
  "@inherits" : [ "Right", "Left"],
  "top_face" : { "@type" : "Array",
                 "@class" : "Top" } }
').

test(diamond_bad,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':violation_of_diamond_property,
                          class:'http://s/Bottom',
                          predicate:thing}]),_)
     ]) :-

    write_schema(schema5,Desc).

test(incompatible_key_change,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure([json{'@type':lexical_key_changed,predicate:'http://somewhere.for.now/schema#f2',subject:'http://somewhere.for.now/document/Thing/foo'}]))
     ]) :-

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema = _{ '@type' : "Class",
                '@id' : "Thing",
                '@key' : _{ '@type': "Lexical",
                            '@fields': ["f1"] },
                'f1' : "xsd:string",
                'f2' : "xsd:string"},

    with_transaction(Context1,
                     insert_schema_document(Context1, Schema),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    with_transaction(Context2,
                     insert_document(Context2, _{'@type': "Thing",
                                                 'f1' : "foo",
                                                 'f2' : "bar"},
                                     _),
                     _),


    New_Schema = (Schema.put('@key', _{ '@type': "Lexical",
                                        '@fields': ["f2"]})),
    create_context(Desc, commit_info{author: "test", message: "test"}, Context3),

    with_transaction(Context3,
                     replace_schema_document(Context3, New_Schema),
                     _).

test(compatible_key_change_same_value,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure([json{'@type':lexical_key_changed,predicate:'http://somewhere.for.now/schema#f2',subject:'http://somewhere.for.now/document/Thing/foo'}]))
     ]) :-

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema = _{ '@type' : "Class",
                '@id' : "Thing",
                '@key' : _{ '@type': "Lexical",
                            '@fields': ["f1"] },
                'f1' : "xsd:string",
                'f2' : "xsd:string"},

    with_transaction(Context1,
                     insert_schema_document(Context1, Schema),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    with_transaction(Context2,
                     insert_document(Context2, _{'@type': "Thing",
                                                 'f1' : "foo",
                                                 'f2' : "foo"},
                                     _),
                     _),


    New_Schema = (Schema.put('@key', _{ '@type': "Lexical",
                                        '@fields': ["f2"]})),
    create_context(Desc, commit_info{author: "test", message: "test"}, Context3),

    with_transaction(Context3,
                     replace_schema_document(Context3, New_Schema),
                     _).

test(compatible_key_change_to_random,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema = _{ '@type' : "Class",
                '@id' : "Thing",
                '@key' : _{ '@type': "Lexical",
                            '@fields': ["f1"] },
                'f1' : "xsd:string",
                'f2' : "xsd:string"},

    with_transaction(Context1,
                     insert_schema_document(Context1, Schema),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    with_transaction(Context2,
                     insert_document(Context2, _{'@type': "Thing",
                                                 'f1' : "foo",
                                                 'f2' : "bar"},
                                     _),
                     _),


    New_Schema = (Schema.put('@key', _{ '@type': "Random"})),
    create_context(Desc, commit_info{author: "test", message: "test"}, Context3),

    with_transaction(Context3,
                     replace_schema_document(Context3, New_Schema),
                     _).

test(insert_with_empty_list,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Thing",
                 'f' : _{'@type': "List",
                         '@class': "OtherThing"}},
    Schema2 = _{ '@type' : "Class",
                 '@id' : "OtherThing"},
    with_transaction(Context1,
                     (   insert_schema_document(Context1, Schema1),
                         insert_schema_document(Context1, Schema2)),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    Document = _{ '@type': "Thing",
                  '@id': "Thing/a_thing",
                  'f': []},
    with_transaction(Context2,
                     insert_document(Context2, Document, _),
                     _),

    once(ask(Desc, t('Thing/a_thing', f, rdf:nil))).

test(insert_with_empty_array,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Thing",
                 'f' : _{'@type': "Array",
                         '@class': "OtherThing"}},
    Schema2 = _{ '@type' : "Class",
                 '@id' : "OtherThing"},
    with_transaction(Context1,
                     (   insert_schema_document(Context1, Schema1),
                         insert_schema_document(Context1, Schema2)),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    Document = _{ '@type': "Thing",
                  '@id': "Thing/a_thing",
                  'f': []},
    with_transaction(Context2,
                     insert_document(Context2, Document, _),
                     _),

    \+ ask(Desc, t('Thing/a_thing', f, _)).


setup_db_with_list(Desc) :-
    create_db_with_empty_schema("admin", "foo"),
    resolve_absolute_string_descriptor("admin/foo", Desc),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema = _{ '@type' : "Class",
                '@id' : "Thing",
                'f' : _{'@type': "List",
                        '@class': "xsd:string"}},
    with_transaction(Context1,
                     insert_schema_document(Context1, Schema),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    Document = _{ '@type': "Thing",
                  '@id': "Thing/a_thing",
                  'f': ["hello"]},
    with_transaction(Context2,
                     insert_document(Context2, Document, _),
                     _).


test(delete_cell_first,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_list(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':list_predicate_not_cardinality_one,
                        instance:_,predicate:'http://www.w3.org/1999/02/22-rdf-syntax-ns#first'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Cons),
                                  t(Cons, rdf:first, Val),
                                  delete(Cons, rdf:first, Val)))),
                     _).

test(add_extra_cell_first,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_list(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':list_predicate_not_cardinality_one,
                        instance:_,predicate:'http://www.w3.org/1999/02/22-rdf-syntax-ns#first'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Cons),
                                  insert(Cons, rdf:first, "second value"^^xsd:string)))),
                     _).

test(delete_cell_rest,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_list(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':list_predicate_not_cardinality_one,
                        instance:_,predicate:'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Cons),
                                  delete(Cons, rdf:rest, rdf:nil)))),
                     _).

test(add_extra_cell_rest,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':list_predicate_not_cardinality_one,
                        instance:_,predicate:'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema = _{ '@type' : "Class",
                '@id' : "Thing",
                'f' : _{'@type': "List",
                        '@class': "xsd:string"}},
    with_transaction(Context1,
                     insert_schema_document(Context1, Schema),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    Document = _{ '@type': "Thing",
                  '@id': "Thing/a_thing",
                  'f': ["hello", "hi"]},
    with_transaction(Context2,
                     insert_document(Context2, Document, _),
                     _),


    create_context(Desc, commit_info{author: "test", message: "test"}, Context3),
    with_transaction(Context3,
                     once(ask(Context3,
                              (   t('Thing/a_thing', f, Cons),
                                  insert(Cons, rdf:rest, rdf:nil)))),
                     _).

setup_db_with_array(Desc) :-
    create_db_with_empty_schema("admin", "foo"),
    resolve_absolute_string_descriptor("admin/foo", Desc),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context1),
    Schema = _{ '@type' : "Class",
                '@id' : "Thing",
                'f' : _{'@type': "Array",
                        '@class': "xsd:string"}},
    with_transaction(Context1,
                     insert_schema_document(Context1, Schema),
                     _),

    create_context(Desc, commit_info{author: "test", message: "test"}, Context2),
    Document = _{ '@type': "Thing",
                  '@id': "Thing/a_thing",
                  'f': ["hello"]},
    with_transaction(Context2,
                     insert_document(Context2, Document, _),
                     _).

test(delete_array_index,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_array(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':array_predicate_not_cardinality_one,
                        instance:_,predicate:'http://terminusdb.com/schema/sys#index'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Array),
                                  t(Array, sys:index, Index),
                                  delete(Array, sys:index, Index)))),
                     _).

test(delete_array_value,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_array(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':array_predicate_not_cardinality_one,
                        instance:_,predicate:'http://terminusdb.com/schema/sys#value'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Array),
                                  t(Array, sys:value, Value),
                                  delete(Array, sys:value, Value)))),
                     _).

test(add_extra_array_index,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_array(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':array_predicate_not_cardinality_one,
                        instance:_,predicate:'http://terminusdb.com/schema/sys#index'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Array),
                                  insert(Array, sys:index, 1^^xsd:nonNegativeInteger)))),
                     _).

test(insert_extra_array_value,
     [
         setup(
             (   setup_temp_store(State),
                 setup_db_with_array(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{
                        '@type':array_predicate_not_cardinality_one,
                        instance:_,predicate:'http://terminusdb.com/schema/sys#value'}]))
     ]) :-
    create_context(Desc, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     once(ask(Context,
                              (   t('Thing/a_thing', f, Array),
                                  insert(Array, sys:value, "extra entry"^^xsd:string)))),
                     _).

test(add_enum_array,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Schema1 = _{ '@type' : "Enum",
                 '@id' : "Number",
                 '@value' : [ "one", "two", "three" ] },

    Schema2 = _{ '@type' : "Class",
                 '@id' : "Sequence",
                 'sequence' : _{'@type': "Array",
                                '@class': "Number"}},
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                     _),

    Document = _{ 'sequence': ["three", "two", "three", "one"]},
    with_test_transaction(Desc,
                          C2,
                          insert_document(C2, Document, _),
                          _).

test(bad_enum_array,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [json{'@type':no_unique_type_for_document,
                         document:json{sequence:["three","two","asdf","one"]},
                         reason:json{'@type':not_a_valid_enum,
                                     enum:'http://somewhere.for.now/schema#Number',
                                     value:"asdf"}}]))
     ]) :-
    Schema1 = _{ '@type' : "Enum",
                 '@id' : "Number",
                 '@value' : [ "one", "two", "three" ] },

    Schema2 = _{ '@type' : "Class",
                 '@id' : "Sequence",
                 'sequence' : _{'@type': "Array",
                                '@class': "Number"}},
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                     _),

    Document = _{ 'sequence': ["three", "two", "asdf", "one"]},
    with_test_transaction(Desc,
                          C2,
                          insert_document(C2, Document, _),
                          _).

test(untyped_object_array,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':references_untyped_array_range,
                            object:_,
                            predicate:"http://somewhere.for.now/schema#sequence",
                            subject:_Array_Cell}]),
               _)
     ]) :-
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Thing",
                 '@key' : _{ '@type' : "Lexical",
                             '@fields' : ["name"]},
                 name : "xsd:string" },

    Schema2 = _{ '@type' : "Class",
                 '@id' : "Sequence",
                 'sequence' : _{'@type': "Array",
                                '@class': "Thing"}},
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                     _),

    Document = _{ 'sequence': ["Thing/foo", "Thing/bar"]},
    with_test_transaction(Desc,
                          C2,
                          insert_document(C2, Document, _),
                          _).

test(add_enum_list,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Schema1 = _{ '@type' : "Enum",
                 '@id' : "Number",
                 '@value' : [ "one", "two", "three" ] },

    Schema2 = _{ '@type' : "Class",
                 '@id' : "Sequence",
                 'sequence' : _{'@type': "List",
                                '@class': "Number"}},
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                     _),

    Document = _{ 'sequence': ["three", "two", "three", "one"]},
    with_test_transaction(Desc,
                          C2,
                          insert_document(C2, Document, _),
                          _).

test(untyped_elt_in_list,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':references_untyped_list_range,
                            index:0,
                            object:"http://somewhere.for.now/document/Thing/one",
                            predicate:"http://somewhere.for.now/schema#sequence",
                            subject:_}]),
               _)
     ]) :-
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Thing",
                 name : "xsd:string" },

    Schema2 = _{ '@type' : "Class",
                 '@id' : "Sequence",
                 'sequence' : _{'@type': "List",
                                '@class': "Thing"}},
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                     _),

    Document = _{ 'sequence': ["Thing/one"]},
    with_test_transaction(Desc,
                          C2,
                          insert_document(C2, Document, _),
                          _).

test(untyped_elt_deep_in_list,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':references_untyped_list_range,
                            index:1,
                            object:"http://somewhere.for.now/document/Thing/one",
                            predicate:"http://somewhere.for.now/schema#sequence",
                            subject:_}]),
               _)
     ]) :-
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Thing",
                 '@key' : _{ '@type' : "Lexical", '@fields' : ["name"]},
                 name : "xsd:string" },

    Schema2 = _{ '@type' : "Class",
                 '@id' : "Sequence",
                 'sequence' : _{'@type': "List",
                                '@class': "Thing"}},
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                     _),

    Document = _{ 'sequence': [_{ name : "foo" },"Thing/one"]},
    with_test_transaction(Desc,
                          C2,
                          insert_document(C2, Document, _),
                          _).

test(non_existing_class_reference,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':not_a_class_or_base_type,
                            class:'http://somewhere.for.now/schema#star'}]),
               _)
     ]) :-
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Repository",
                 repository_star : _{ '@class' : "star", '@type': "Set" }},
    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(C1, Schema1),
                          _).

test(inherit_missing_class,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':inherits_from_non_existent_class,
                            class:'http://somewhere.for.now/schema#Thing',
                            super:'http://somewhere.for.now/schema#Something'}]),_)
     ]) :-
    Schema1 = _{ '@type' : "Class",
                 '@id' : "Thing",
                 '@inherits' : ["Something"],
                 name : "xsd:string" },
    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(C1, Schema1),
                          _).

test(inherit_tagged_union,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Schema1 =
    _{
        '@type': "TaggedUnion",
        '@id': "EitherAorB",
        '@abstract': [],
        a : "xsd:string",
        b : "xsd:integer"
    },
    Schema2 =
    _{
        '@type': "Class",
        '@id': "EitherAorBandC",
        '@inherits': "EitherAorB",
        c : "xsd:float"
    },
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                          _).


test(inherit_enum,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(schema_check_failure(
                   [witness{'@type':inherits_from_invalid_super_class,
                            class:'http://somewhere.for.now/schema#EnumBaby',
                            super:'http://somewhere.for.now/schema#Number'}]), _)
     ]) :-
    Schema1 =
    _{ '@type' : "Enum",
       '@id' : "Number",
       '@value' : [ "one", "two", "three" ] },
    Schema2 =
    _{
        '@type': "Class",
        '@id': "EnumBaby",
        '@inherits': "Number",
        c : "xsd:float"
    },
    with_test_transaction(Desc,
                          C1,
                          (   insert_schema_document(C1, Schema1),
                              insert_schema_document(C1, Schema2)
                          ),
                          _).

test(json_document_range,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema("admin", "foo"),
                 resolve_absolute_string_descriptor("admin/foo", Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(json_document_as_range(json_document,"sys:JSONDocument"),
               _)
     ]) :-

    Schema =
    _{ '@id': "HasJSONDocument",
       '@type': "Class",
       'json_document': "sys:JSONDocument"
     },
    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(C1, Schema),
                          _).

:- end_tests(schema_checker).


:- begin_tests(woql_document, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(subsumption_insert,
     [
         setup(
             (   setup_temp_store(State),
                 test_woql_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = _{'@type': "NamedQuery",
             name: "TestQuery",
             query: _{'@type' : "Subsumption",
                      'child' : _{ '@type' : "NodeValue",
                                   'node' : "system:Organization"},
                      'parent' : _{'@type' : "NodeValue",
                                   'variable' : "Parent"}}},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),

    get_document(Desc, Id, JSON2),

    JSON2 = json{'@id':'NamedQuery/TestQuery',
                 '@type':'NamedQuery',
                 name: "TestQuery",
                 query: json{ '@id':'Subsumption/df269c56e3b263424bb3f9c9e4316a10ceb21e43c14e51bebd705c89068e7ddf',
                              '@type':'Subsumption',
                              child:json{ '@id':'NodeValue/ce25df5164a1d812aa1e79d8aa0e5cbd61f3f1a4b69d53add7115b267989117b',
                                          '@type':'NodeValue',
                                          node:"system:Organization"
                                        },
                              parent:json{ '@id':'NodeValue/9f77cd3ff5da0240e21bacceec8c31f137cb92a15426d6b21e7c55ae26cf8390',
                                           '@type':'NodeValue',
                                           variable:"Parent"
		                                 }
                            }}.

test(substring_insert, [
         setup(
             (   setup_temp_store(State),
                 test_woql_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = _{'@type': "NamedQuery",
             name: "TestQuery",
             query: _{'@type' : "Substring",
                      string : _{ '@type' : "DataValue",
                                  data : _{'@type' : "xsd:string",
                                           '@value' : "Test"}},
                      before : _{ '@type' : "DataValue",
                                  data : _{'@type' : "xsd:integer",
                                           '@value' : 1}},
                      length : _{ '@type' : "DataValue",
                                  variable : "Length"},
                      after : _{ '@type' : "DataValue",
                                 data : _{'@type' : "xsd:integer",
                                          '@value' : 1}},
                      substring : _{ '@type' : "DataValue",
                                     variable : "Substring" }}},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),

    get_document(Desc, Id, JSON2),

    JSON2 = json{'@id':'NamedQuery/TestQuery',
                 '@type':'NamedQuery',
                 name: "TestQuery",
                 query: json{ '@id':'Substring/58966c0b377f0bef26c5e70c9e1dfcc755a0d0283dfd5c1bd24ab00088dfcf74',
                              '@type':'Substring',
                              after:json{ '@id':'DataValue/7754f109f1156e98967aab291d4297d84e880a1791bd668cfd92843806006953',
                                          '@type':'DataValue',
                                          data:json{'@type':'xsd:integer','@value':1}
                                        },
                              before:json{ '@id':'DataValue/7754f109f1156e98967aab291d4297d84e880a1791bd668cfd92843806006953',
                                           '@type':'DataValue',
                                           data:json{'@type':'xsd:integer','@value':1}
                                         },
                              length:json{ '@id':'DataValue/345e2ac5aa9fdb2497f535fa0b8a3bfbb32ba34bc6b248b804fba60c2259fd6e',
                                           '@type':'DataValue',
                                           variable:"Length"
                                         },
                              string:json{ '@id':'DataValue/361af56aa6241921e995130e61dd0aaf6d4d307e5e7a5bfce77e188d1ebeacb2',
                                           '@type':'DataValue',
                                           data:json{'@type':'xsd:string','@value':"Test"}
                                         },
                              substring:json{ '@id':'DataValue/c53baa036cc4e3af24ed85c0e339c592262c6ebccea830850dba0be4bbfd0b66',
		                                      '@type':'DataValue',
		                                      variable:"Substring"
		                                    }
                            }
                }.

test(named_parametric_query, [
         setup(
             (   setup_temp_store(State),
                 test_woql_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    Query = _{ '@type': "NamedParametricQuery",
               name: "user_name_uri",
               parameters: ["User_Name", "URI"],
               query: _{ '@type': "And",
                         and: [_{'@type' : "Data",
                                 subject: _{'@type' : "NodeValue",
                                            variable: "URI"},
                                 predicate: _{'@type' : "NodeValue",
                                              node: name},
                                 object: _{'@type' : "DataValue",
                                           variable: "User_Name"}},
                               _{'@type': "IsA",
                                 element: _{'@type': "NodeValue",
                                            variable: "URI"},
                                 type: _{'@type': "NodeValue",
                                         node : "User"}}]}},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, Query, Id),

    get_document(Desc, Id, JSON2),
    JSON2 =
    json{ '@id':'NamedParametricQuery/user_name_uri',
          '@type':'NamedParametricQuery',
          name:"user_name_uri",
          parameters:["User_Name","URI"],
          query:json{ '@id':'And/01ee9d3e300ede2b8247ce20665a2d97dcdc9d62a5f4feb21b94a01668369682',
                      '@type':'And',
                      and:[ json{ '@id':'Data/4cb7e7296113897e4c6d25d1af22715ce2d98d6349c36aed61be0ffcc7a532f2',
                                  '@type':'Data',
                                  object:json{ '@id':'DataValue/a49c922485adc3477cbcd489c81111954fa57fb62d5dfcc700a4e7b304cdb965',
                                               '@type':'DataValue',
                                               variable:"User_Name"
                                             },
                                  predicate:json{ '@id':'NodeValue/9e6b6c91ec7f10179a5c07fce64f4aa8a4ef00f714f9a3d49d96688559af18da',
                                                  '@type':'NodeValue',
                                                  node:"name"
                                                },
                                  subject:json{ '@id':'NodeValue/8d2b6dc02bece9b03cb90064b43637438833a5d69df842cecaf5006302741f5c',
                                                '@type':'NodeValue',
                                                variable:"URI"
                                              }
                                },
                            json{ '@id':'IsA/868517cf306fc73ee346426b7328a416d0639e6efb1f08e452e9787ef5ba0b8a',
                                  '@type':'IsA',
                                  element:json{ '@id':'NodeValue/8d2b6dc02bece9b03cb90064b43637438833a5d69df842cecaf5006302741f5c',
                                                '@type':'NodeValue',
                                                variable:"URI"
                                              },
                                  type:json{ '@id':'NodeValue/0b576982430d0e608a643d6e118c198ef9407c964c1d3353d07964e909cd8bd2',
					                         '@type':'NodeValue',
					                         node:"User"
				                           }
			                    }
		                  ]
		            }
        }.

:- end_tests(woql_document).

:- begin_tests(arithmetic_document, []).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema5('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "NamedExpression",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string",
  "expression" : "ArithmeticExpression" }

{ "@id" : "ArithmeticExpression",
  "@type" : "Class",
  "@subdocument" : [],
  "@abstract" : [] }

{ "@id": "Plus",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression",
  "@key" : { "@type" : "ValueHash" },
  "left" : "ArithmeticExpression",
  "right" : "ArithmeticExpression" }

{ "@id" : "Value",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression",
  "@key" : { "@type" : "ValueHash" },
  "number" : "xsd:integer" }

{ "@id" : "NamedExpression2",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "name" : "xsd:string",
  "expression" : "ArithmeticExpression2" }

{ "@id" : "ArithmeticExpression2",
  "@type" : "Class",
  "@subdocument" : [],
  "@abstract" : [] }

{ "@id": "Plus2",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression2",
  "@key" : { "@type" : "Random" },
  "left" : "ArithmeticExpression2",
  "right" : "ArithmeticExpression2" }

{ "@id" : "Value2",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression2",
  "@key" : { "@type" : "Random" },
  "number" : "xsd:integer" }

{ "@id" : "Outer",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields": ["name"] },
  "name" : "xsd:string",
  "inner" : "Inner",
  "number" : "xsd:integer" }

{ "@id" : "Things",
  "@type" : "Enum",
  "@value" : ["thing1","thing2"] }

{ "@id" : "Inner",
  "@type" : "Class",
  "@subdocument" : [],
  "@key" : { "@type" : "Random" },
  "things" : { "@type" : "List",
               "@class" : "xsd:integer" },
  "name" : "xsd:string",
  "number" : "xsd:integer" }

{ "@id" : "NewOuter",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string",
  "inners" : { "@type" : "Set",
               "@class" : "Inner" },
  "inner" : "Inner",
  "things" : "Things",
  "number" : "xsd:integer" }

{ "@id" : "Abstract",
  "@type" : "Class",
  "@abstract" : [] }

{ "@id" : "A",
  "@type" : "Class",
  "@inherits" : "Abstract",
  "has_something" : "B" }

{ "@id" : "B",
  "@type" : "Class",
  "@inherits" : "Abstract",
  "has_something" : "A" }

{ "@id" : "Points_To_Abstract",
  "@type" : "Class",
  "points" : "Abstract" }

').

test(get_value_schema, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, Trans),
    get_schema_document(Trans, 'Value', Document),

    Document = json{'@id':'Value',
                    '@inherits':'ArithmeticExpression',
                    '@key':json{'@type':"ValueHash"},
                    '@type':'Class',
                    number:'xsd:integer'}.

test(plus_doc_extract, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON =
    json{'@type': "NamedExpression",
         name: "3+(2+1)",
         expression:
         json{'@type' : "Plus",
              left : json{'@type' : "Value",
                          number : 3},
              right : json{'@type' : "Plus",
                           left : json{'@type' : "Value",
                                       number : 2},
                           right : json{'@type' : "Value",
                                        number : 1}}
             }
        },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),

    get_document(Desc, Id, JSON2),

    JSON2 =
    json{ '@id':'NamedExpression/3%2B(2%2B1)',
          '@type':'NamedExpression',
          expression:json{ '@id':'Plus/6b5016852fddd861bdd842be23dcc38777058e851b01d27ba33419a66ff459a1',
                           '@type':'Plus',
                           left:json{ '@id':'Value/693d7edbc69308a1a4599dc315c11c69e6bee584b70358b09dec1ca5d52053dd',
                                      '@type':'Value',
                                      number:3
                                    },
                           right:json{ '@id':'Plus/07da26dfc92e81aecc5245147fd17b0f14c07feddef26f5ec5d48b6bff471bc1',
                                       '@type':'Plus',
                                       left:json{ '@id':'Value/7090d4636c9e37659754ae4ca1b93326f9d5a711a04d64f2045d820896ed0269',
                                                  '@type':'Value',
                                                  number:2
                                                },
                                       right:json{ '@id':'Value/9341cb64549b5460321d002663fd0a2f7bfe62a804d22c8a65b4f00f46723c3f',
                                                   '@type':'Value',
                                                   number:1
					                             }
				                     }
		                 },
          name:"3+(2+1)"
        }.

test(plus_doc_delete, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON =
    json{'@type': "NamedExpression2",
         name: "3+(2+1)",
         expression:
         json{'@type' : "Plus2",
              left : json{'@type' : "Value2",
                          number : 3},
              right : json{'@type' : "Plus2",
                           left : json{'@type' : "Value2",
                                       number : 2},
                           right : json{'@type' : "Value2",
                                        number : 1}}
             }
        },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),



    get_document(Desc, Id, Document),

    Document =
    json{'@id':_,
         '@type':'NamedExpression2',
         expression:json{'@id':_,
                         '@type':'Plus2',
                         left:json{'@id':_,
                                   '@type':'Value2',
                                   number:3},
                         right:json{'@id':_,
                                    '@type':'Plus2',
                                    left:json{'@id':_,
                                              '@type':'Value2',
                                              number:2},
                                    right:json{'@id':_,
                                               '@type':'Value2',
                                               number:1}}},
         name:"3+(2+1)"},


    run_delete_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, Id),

    \+ get_document_uri(Desc, true, Id).

test(subdocument_deletes_lists, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-


    JSON =
    json{'@type': "Outer",
         name: "Outer",
         inner:
         json{'@type' : "Inner",
              name: "Inner",
              number: 10,
              things: [1,2,3]},
         number: 1},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),
    get_document(Desc, Id, JSON2),

    JSON2 = json{'@id':'Outer/Outer',
                 '@type':'Outer',
                 inner:json{'@id':_,
                            '@type':'Inner',
                            name:"Inner",
                            number:10,
                            things:[1,2,3]},
                 name:"Outer",
                 number:1},


    run_delete_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, Id),

    open_descriptor(Desc, Trans),
    database_instance(Trans, Inst),
    \+ xrdf(Inst, _, _, _).

test(arithmetic_frame, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc, 'Plus2', JSON),

    JSON = json{'@type':'Class',
                '@key':json{'@type':"Random"},
                '@subdocument':[],
                left:[json{'@class':'Plus2','@subdocument':[]},
                      json{'@class':'Value2','@subdocument':[]}],
                right:[json{'@class':'Plus2','@subdocument':[]},
                       json{'@class':'Value2','@subdocument':[]}]}.


test(outer_frame, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc, 'NewOuter', JSON),

    JSON = json{'@type':'Class',
                '@key':json{'@fields':[name],'@type':"Lexical"},
                inner:json{'@class':'Inner',
                           '@subdocument':[]},
                inners:json{'@class':json{'@class':'Inner',
                                          '@subdocument':[]},
                            '@type':'Set'},
                name:'xsd:string',
                number:'xsd:integer',
                things:json{'@id':'Things',
                            '@type':'Enum',
                            '@values':[thing1,thing2]}}.


test(points_to_abstract, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema5,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    class_frame(Desc, 'Points_To_Abstract', JSON),
    JSON = json{'@type' : 'Class', points:['A','B']}.

:- end_tests(arithmetic_document).

:- begin_tests(employee_documents, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema6('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@documentation": {"@comment": "This is address", "@properties": {}},
  "@id": "Address",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "country": "Country",
  "postal_code": "xsd:string",
  "street": "xsd:string"}

{ "@id": "Country",
  "@key": {"@type": "ValueHash"},
  "@type": "Class",
  "name": "xsd:string",
  "perimeter": {"@class": "Coordinate",
                "@type": "List"}}

{ "@id": "Coordinate",
  "@key": {"@type": "Random"},
  "@type": "Class",
  "x": "xsd:decimal",
  "y": "xsd:decimal"}

{ "@id": "Team", "@type": "Enum",
  "@value": ["IT", "Marketing"]}

{ "@id": "Employee",
  "@inherits": "Person",
  "@key": {"@type": "Random"},
  "@type": "Class",
  "address_of": "Address",
  "contact_number": {"@class": "xsd:string",
                     "@type": "Optional"},
  "managed_by": "Employee"}

{ "@documentation": { "@comment": "This is a person",
                      "@properties": { "age": "Age of the person.",
                                       "name": "Name of the person." }},
  "@id": "Person",
  "@key": {"@type": "Random"},
  "@type": "Class",
  "age": "xsd:integer",
  "friend_of": {"@class": "Person",
                "@type": "Set"},
  "name": "xsd:string" }
').

test(all_class_frames, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema6,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )]) :-

    open_descriptor(Desc, DB),
    all_class_frames(DB,  Frames),

    Frames = json{ '@context':_{ '@base':"http://i/",
						         '@schema':"http://s/",
						         '@type':'Context'
						       },
				   'Address':json{ '@documentation':
                                   json{ '@comment':"This is address"
									   },
							       '@key':json{'@type':"Random"},
							       '@subdocument':[],
							       '@type':'Class',
							       country:'Country',
							       postal_code:'xsd:string',
							       street:'xsd:string'
							     },
				   'Coordinate':json{ '@key':json{ '@type':"Random"
									             },
								      '@type':'Class',
								      x:'xsd:decimal',
								      y:'xsd:decimal'
							        },
				   'Country':json{ '@key':json{ '@type':"ValueHash"
									          },
							       '@type':'Class',
							       name:'xsd:string',
							       perimeter:json{ '@class':'Coordinate',
									               '@type':'List'
									             }
							     },
				   'Employee':json{ '@key':json{ '@type':"Random"
									           },
							        '@type':'Class',
                                    '@documentation' :
                                    json{'@properties':json{age:"Age of the person.",
                                                            name:"Name of the person."}},
							        address_of:json{ '@class':'Address',
										             '@subdocument':[]
									               },
							        age:'xsd:integer',
							        contact_number:json{ '@class':'xsd:string',
										                 '@type':'Optional'
										               },
							        friend_of:json{ '@class':'Person',
									                '@type':'Set'
									              },
							        managed_by:'Employee',
							        name:'xsd:string'
							      },
				   'Person':json{ '@documentation':
                                  json{ '@comment':"This is a person",
										'@properties':json{ age:"Age of the person.",
													        name:"Name of the person."
												          }
									  },
							      '@key':json{'@type':"Random"},
							      '@type':'Class',
							      age:'xsd:integer',
							      friend_of:json{ '@class':'Person',
									              '@type':'Set'
									            },
							      name:'xsd:string'
							    },
				   'Team':json{ '@type':'Enum',
							    '@values':['IT','Marketing']
							  }
				 }.

test(doc_frame, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema6,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )]) :-
    open_descriptor(Desc, DB),
    class_frame(DB, 'Address', Frame),
    Frame = json{'@type':'Class',
                 '@documentation':json{'@comment':"This is address"},
	             '@key':json{'@type':"Random"},
	             '@subdocument':[],
                 country:'Country',
                 postal_code:'xsd:string',
	             street:'xsd:string'}.

test(insert_employee, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema6,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [json{'http://s/address_of':
                       json{'http://s/country':
                            json{'@type':required_field_does_not_exist_in_document,
                                 document:
                                 json{'@type':'http://s/Country',
                                      'http://s/name':
                                      json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                           '@value':"United Kingdom"}},
                                 field:'http://s/perimeter'}}}]),
             _)

     ]) :-

    D1 = _{'@type': "Country",
           name: "United Kingdom"
          },
    D2 = _{'@type': "Address",
           country: D1,
           postal_code: "A12 345",
           street: "123 Abc Street"},
    D3 = _{'@id': "Employee/def2f711f95943378d8b9712b2820a8a",
           '@type': "Employee",
           name: "Bob",
           age: 22,
           address_of: D2,
           contact_number: "07777123456",
           managed_by: _{'@id': "Employee/def2f711f95943378d8b9712b2820a8a",
                         '@type': "@id"}},

    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        (
            insert_document(Context, D3, ID3)
        ),
        _),

    get_document(Desc, ID3, JSON3),
    ID1 = (JSON3.address_of.country),
    get_document(Desc, ID1, _JSON1).

test(update_enum,[
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema6,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    New_Team =  _{'@id': "Team",
                  '@type': "Enum",
                  '@value': ["Information Technology", "Amazing Marketing"]},

    database_prefixes(Desc,Prefixes),
    json_schema_elaborate(New_Team, Prefixes, Elaborated),

    Elaborated =
    json{'@id':'http://s/Team',
         '@type':'http://terminusdb.com/schema/sys#Enum',
         'http://terminusdb.com/schema/sys#value':
         json{'@container':"@list",
              '@type':"@id",
              '@value':[json{'@id':'http://s/Team/Information%20Technology',
                             '@type':"@id"},
                        json{'@id':'http://s/Team/Amazing%20Marketing',
                             '@type':"@id"}]}},
    %nl,
    %writeq('------------------------------------------'),nl,

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        replace_schema_document(Context, New_Team, Id),
        _
    ),

    open_descriptor(Desc, DB2),
    get_schema_document(DB2, Id, New_Document),
    New_Document =
    json{'@id':'Team',
         '@type':'Enum',
         '@value':['Information Technology','Amazing Marketing']}.


:- end_tests(employee_documents).

:- begin_tests(polity_documents, []).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema7('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id": "GeneralVariables",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "alternative_name": {"@class": "xsd:string", "@type": "Set"},
  "language": "xsd:string"}

{ "@id": "WarfareVariables",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "military_technologies": "MilitaryTechnologies"}

{ "@id": "MilitaryTechnologies",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "atlatl": {"@class": "EpistemicState", "@type": "Optional"},
  "battle_axes": {"@class": "EpistemicState", "@type": "Optional"},
  "breastplates": {"@class": "EpistemicState", "@type": "Optional"}}

{ "@id": "SocialComplexityVariables",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "hierarchical_complexity": "HierarchicalComplexity",
  "information": "Information"}

{ "@id": "Information",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "articles": {"@class": "EpistemicState", "@type": "Optional"}}

{ "@id": "Confidence", "@type": "Enum", "@value": ["inferred", "suspected"]}

{ "@id": "HierarchicalComplexity",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "@type": "Class",
  "admin_levels": "AdministrativeLevels"}

{ "@id": "EpistemicState",
  "@type": "Enum",
  "@value": ["absent",
             "present",
             "unknown",
             "inferred_absent",
             "inferred_present"]}

{ "@id": "AdministrativeLevels",
  "@type": "Enum",
  "@value": ["five", "four", "three", "two", "one"]}

{ "@id": "Polity",
  "@key": {"@type": "Random"},
  "@type": "Class",
  "general_variables": "GeneralVariables",
  "name": "xsd:string",
  "social_complexity_variables": "SocialComplexityVariables",
  "warfare_variables": "WarfareVariables"}').

test(insert_polity,
     [setup(
          (   setup_temp_store(State),
              test_document_label_descriptor(Desc),
              write_schema(schema7,Desc)
          )),
      cleanup(
          teardown_temp_store(State)
      )
     ]
    ) :-

    Polity =
    _{'@type': 'Polity',
      'general_variables': _{'@type': 'GeneralVariables',
                             'language' : 'latin',
                             'alternative_name': ['Sadozai Kingdom',
                                                  'Last Afghan Empire']},
      'name': 'AfDurrn',
      'social_complexity_variables':
      _{'@type': 'SocialComplexityVariables',
        'hierarchical_complexity': _{'@type': 'HierarchicalComplexity',
                                     'admin_levels': 'five'},
        'information': _{'@type': 'Information',
                         'articles': "present"}},
      'warfare_variables': _{'@type': 'WarfareVariables',
                             'military_technologies': _{'@type': 'MilitaryTechnologies',
                                                        'atlatl': "present",
                                                        'battle_axes': "present",
                                                        'breastplates': "present"}}},

    open_descriptor(Desc, DB),
    json_elaborate(DB, Polity, Elaborated),

    Elaborated =
    json{'@id':_,
         '@type':'http://s/Polity',
         'http://s/general_variables':
         json{'@id':_,
              '@type':'http://s/GeneralVariables',
              'http://s/alternative_name':
              _{'@container':"@set",
                '@type':'http://www.w3.org/2001/XMLSchema#string',
                '@value':[
                    json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                         '@value':'Sadozai Kingdom'},
                    json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                         '@value':'Last Afghan Empire'}]},
              'http://s/language':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                       '@value':latin}},
         'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                              '@value':'AfDurrn'},
         'http://s/social_complexity_variables':
         json{'@id':_,
              '@type':'http://s/SocialComplexityVariables',
              'http://s/hierarchical_complexity':
              json{'@id':_,
                   '@type':'http://s/HierarchicalComplexity',
                   'http://s/admin_levels':json{'@id':'http://s/AdministrativeLevels/five',
                                                '@type':"@id"}},
              'http://s/information':
              json{'@id':_,
                   '@type':'http://s/Information',
                   'http://s/articles':
                   json{'@id':'http://s/EpistemicState/present',
                        '@type':"@id"}}},
         'http://s/warfare_variables':
         json{'@id':_,
              '@type':'http://s/WarfareVariables',
              'http://s/military_technologies':
              json{'@id':_,
                   '@type':'http://s/MilitaryTechnologies',
                   'http://s/atlatl':json{'@id':'http://s/EpistemicState/present',
                                          '@type':"@id"},
                   'http://s/battle_axes':json{'@id':'http://s/EpistemicState/present',
                                               '@type':"@id"},
                   'http://s/breastplates':json{'@id':'http://s/EpistemicState/present',
                                                '@type':"@id"}}}},

    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        (
            insert_document(Context, Polity, ID)
        ),
        _
    ),

    get_document(Desc, ID, JSON),

    JSON = json{'@id':_,
                '@type':'Polity',
                general_variables:
                json{'@id':_,
                     '@type':'GeneralVariables',
                     alternative_name:["Last Afghan Empire",
                                       "Sadozai Kingdom"],
                     language:"latin"},
                name:"AfDurrn",
                social_complexity_variables:
                json{'@id':_,
                     '@type':'SocialComplexityVariables',
                     hierarchical_complexity:
                     json{'@id':_,
                          '@type':'HierarchicalComplexity',
                          admin_levels:five},
                     information:json{'@id':_,
                                      '@type':'Information',
                                      articles:present}},
                warfare_variables:
                json{'@id':_,
                     '@type':'WarfareVariables',
                     military_technologies:
                     json{'@id':_,
                          '@type':'MilitaryTechnologies',
                          atlatl:present,
                          battle_axes:present,
                          breastplates:present}}}.

:- end_tests(polity_documents).

:- begin_tests(system_documents, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(database_expansion,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Database = json{'@type':'UserDatabase',
                    comment:"a test db",
                    creation_date:"2021-07-22T10:05:20.000Z",
                    label:"test",
                    name:"foo",
                    state:"creating"},
    open_descriptor(system_descriptor{}, Transaction),
    json_elaborate(Transaction,Database,Elaborated),

    Elaborated =
    json{'@id':_,
         '@type':'http://terminusdb.com/schema/system#UserDatabase',
         'http://terminusdb.com/schema/system#comment':
         json{'@type':'http://www.w3.org/2001/XMLSchema#string',
              '@value':"a test db"},
         'http://terminusdb.com/schema/system#creation_date':
         json{'@type':'http://www.w3.org/2001/XMLSchema#dateTime',
              '@value':"2021-07-22T10:05:20.000Z"},
         'http://terminusdb.com/schema/system#label':
         json{'@type':'http://www.w3.org/2001/XMLSchema#string',
              '@value':"test"},
         'http://terminusdb.com/schema/system#name':
         json{'@type':'http://www.w3.org/2001/XMLSchema#string',
              '@value':"foo"},
         'http://terminusdb.com/schema/system#state':
         json{'@id':'http://terminusdb.com/schema/system#DatabaseState/creating',
              '@type':"@id"}}.

:- end_tests(system_documents).


:- begin_tests(python_client_bugs, []).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema8('
{"@base": "terminusdb:///data/", "@schema": "terminusdb:///schema#", "@type": "@context"}
{"@documentation": {"@comment": "This is address"},
 "@id": "Address",
 "@key": {"@type": "Random"},
 "@subdocument": [],
 "@type": "Class",
 "country": "Country",
 "postal_code": "xsd:string", "street": "xsd:string"}
{"@id": "Coordinate",
 "@key": {"@type": "Random"},
 "@type": "Class",
 "x": "xsd:decimal",
 "y": "xsd:decimal"}
{"@id": "Country",
 "@key": {"@type": "ValueHash"},
 "@type": "Class",
 "name": "xsd:string",
 "perimeter": {"@class": "Coordinate", "@type": "List"}}
{"@id": "Employee",
 "@inherits": "Person",
 "@key": {"@type": "Random"},
 "@type": "Class",
 "address_of": "Address",
 "age": "xsd:integer",
 "contact_number": {"@class": "xsd:string", "@type": "Optional"},
 "friend_of": {"@class": "Person", "@type": "Set"},
 "managed_by": "Employee",
 "member_of": "Team",
 "name": "xsd:string"}
{"@documentation": {"@comment": "This is a person", "@properties": {"age": "Age of the person.", "name": "Name of the person."}},
 "@id": "Person",
 "@key": {"@type": "Random"},
 "@type": "Class",
 "age": "xsd:integer",
 "friend_of": {"@class": "Person", "@type": "Set"},
 "name": "xsd:string"}
{"@id": "Team",
 "@type": "Enum",
 "@value": ["Information Technology", "Marketing"]}').

schema8_1('
{"@base": "terminusdb:///data/", "@schema": "terminusdb:///schema#", "@type": "@context"}
{ "@type": "Class",
  "@id": "my_ip",
  "@key": { "@type": "Lexical",
            "@fields": ["timestamp"]},
  "ip": "xsd:string",
  "timestamp": "xsd:dateTime"}').

test(type_not_found,
     [setup(
          (   setup_temp_store(State),
              test_document_label_descriptor(Desc),
              write_schema(schema8,Desc)
          )),
      cleanup(
          teardown_temp_store(State)
      )
     ]
    ) :-

    UK = _{ '@type' : "Country",
            name : "United Kingdom",
            perimeter : []
          },

    Home = _{ '@type' : "Address",
              street : "123 Abc Street",
              country : UK,
              postal_code : "A12 345"
            },

    Cheuk = _{ '@type' : "Employee",
               '@id' : 'Employee/Cheuk2342343',
               address_of : Home,
               contact_number : "07777123456",
               age : 21,
               name : "Cheuk",
               managed_by : 'Employee/Cheuk2342343',
               member_of : "Information Technology"
             },

    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        (
            insert_document(Context, Cheuk, 'terminusdb:///data/Employee/Cheuk2342343')
        ),
        _
    ).

test(lexical_timestamp,
     [setup(
          (   setup_temp_store(State),
              test_document_label_descriptor(Desc)
          )),
      cleanup(
          teardown_temp_store(State)
      )
     ]
    ) :-

    write_schema(schema8_1,Desc).

test(key_exchange_problem,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema8_1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Query = '{"@type": "Class", "@id": "Grades", "last_name": "xsd:string", "first_name": "xsd:string", "ssn": "xsd:string", "test1": "xsd:decimal", "test2": "xsd:decimal", "test3": "xsd:decimal", "test4": "xsd:decimal", "final": "xsd:decimal", "grade": "xsd:string", "@key": {"@type": "Hash", "@fields": ["last_name", "first_name", "ssn", "test1", "test2", "test3", "test4", "final", "grade"]}}',
    atom_json_dict(Query, Document, []),

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    get_schema_document(DB2, 'Grades', Grades),
    "Hash" = (Grades.'@key'.'@type').

:- end_tests(python_client_bugs).


:- begin_tests(javascript_client_bugs, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema9('
{ "@base": "terminusdb:///data/", "@schema": "terminusdb:///schema#", "@type": "@context"}
{ "@abstract": [], "@id": "Entity", "@type": "Class", "status": "Status" }
{ "@id": "Invitation", "@inherits": "Entity", "@key": { "@type": "ValueHash" },
  "@subdocument": [], "@type": "Class", "email_to": "xsd:string", "invited_by": "User", "note": { "@class": "xsd:string", "@type": "Optional" }, "sent_date": { "@class": "xsd:dateTime", "@type": "Optional" } }
{ "@id": "Organization", "@inherits": "Entity", "@key": { "@fields": [ "organization_name" ], "@type": "Lexical" }, "@type": "Class", "child": { "@class": "Organization", "@type": "Set" }, "collaborators": { "@class": "User", "@type": "Set" }, "creation_date": "xsd:dateTime", "expiration_data": { "@class": "xsd:dateTime", "@type": "Optional" }, "invitations": { "@class": "Invitation", "@type": "Set" }, "organization_name": "xsd:string", "owned_by": "User", "stripe_subscription": "StripeSubscription" }
{ "@id": "Personal", "@inherits": "Organization", "@type": "Class" }
{ "@id": "Status", "@type": "Enum", "@value": [ "pending", "inactive", "active", "needs_invite", "invite_sent", "accepted", "rejected" ] }
{ "@id": "StripeSubscription", "@inherits": "Entity", "@key": {"@type": "Random"}, "@subdocument": [], "@type": "Class", "billing_email": "xsd:string", "stripe_id": "xsd:string", "stripe_quantity": "xsd:decimal", "stripe_user": "User", "subscription_id": "xsd:string" }
{ "@id": "Team", "@inherits": "Organization", "@type": "Class" }
{ "@id": "User", "@inherits": "Entity", "@key": { "@fields": [ "user_id" ], "@type": "Lexical" }, "@type": "Class", "company": "xsd:string", "email": "xsd:string", "first_name": "xsd:string", "last_name": "xsd:string", "picture": "xsd:string", "registration_date": { "@class": "xsd:dateTime", "@type": "Optional" }, "user_id": "xsd:string" }
').

test(js_type_not_found,
     [setup(
          (   setup_temp_store(State),
              test_document_label_descriptor(Desc),
              write_schema(schema9,Desc)
          )),
      cleanup(
          teardown_temp_store(State)
      )
     ]
    ) :-
    Bonzai = _{ '@type' : "User",
                company: "Yoyodyne",
                email: "Bonzai@yoyodyne.com",
                first_name: "Buckaroo",
                last_name: "Bonzai",
                picture: "My pic",
                status: "active",
                registration_date: "2009-07-01T10:11:12Z",
                user_id: "bonzai"
              },
    Hikita = _{ '@type' : "User",
                company: "Yoyodyne",
                email: "Tohichi@yoyodyne.com",
                first_name: "Tohichi",
                last_name: "Hikita",
                picture: "My pic of me",
                status: "active",
                registration_date: "2009-07-01T10:11:12Z",
                user_id: "hikita"
              },
    Organization = _{ '@type':"Organization",
                      child: [],
                      collaborators:[ "User/hikita" ],
                      invitations:[ _{ email_to:"hello",
                                       invited_by:"User/bonzai",
                                       status:"needs_invite" },
                                    _{ email_to:"monkey",
                                       invited_by:"User/bonzai",
                                       status:"needs_invite" } ],
                      organization_name:"withsubscription",
                      owned_by: "User/bonzai",
                      status: "invite_sent",
                      stripe_subscription: _{ '@type' : "StripeSubscription",
                                               billing_email:"somewkjf",
                                               status: "active",
                                               stripe_id:"KItty",
                                               stripe_quantity:"32",
                                               stripe_user:"User/hikita",
                                               subscription_id:"932438238429384ASBJDA" },
                      creation_date:"2011-01-01T01:00:37Z" },
    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        (
            insert_document(Context, Bonzai, _),
            insert_document(Context, Hikita, _),
            insert_document(Context, Organization, _)
        ),
        _
    ).

test(subdocument_update,
     [setup(
          (   setup_temp_store(State),
              test_document_label_descriptor(Desc),
              write_schema(schema9,Desc)
          )),
      cleanup(
          teardown_temp_store(State)
      )
     ]
    ) :-

    Bonzai = _{ '@type' : "User",
                company: "Yoyodyne",
                email: "Bonzai@yoyodyne.com",
                first_name: "Buckaroo",
                last_name: "Bonzai",
                picture: "My pic",
                status: "active",
                registration_date: "2009-07-01T10:11:12Z",
                user_id: "bonzai"
              },
    Hikita = _{ '@type' : "User",
                company: "Yoyodyne",
                email: "Tohichi@yoyodyne.com",
                first_name: "Tohichi",
                last_name: "Hikita",
                picture: "My pic of me",
                status: "active",
                registration_date: "2009-07-01T10:11:12Z",
                user_id: "hikita"
              },
    Document = _{'@id':"Organization/somewhere",
                 '@type':"Organization",
                 invitations:[
                     _{'@type':"Invitation",
                       invited_by: 'User/bonzai',
                       email_to:"something",
                       note:"whjgasdj",
                       status:"pending"}
                 ],
                 stripe_subscription: _{ '@type' : "StripeSubscription",
                                          billing_email:"somewkjf",
                                          status: "active",
                                          stripe_id:"KItty",
                                          stripe_quantity:"32",
                                          stripe_user:"User/bonzai",
                                          subscription_id:"932438238429384ASBJDA" } ,
                 owned_by: 'User/bonzai',
                 creation_date: "2021-05-01T12:10:10Z",
                 organization_name:"somewhere",
                 status:"inactive"},

    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        (
            insert_document(Context, Bonzai, _),
            insert_document(Context, Hikita, _),
            insert_document(Context, Document, _)
        ),
        _
    ),

    Document2 = _{'@id':"Organization/somewhere",
                  '@type':"Organization",
                  invitations:[
                      _{'@type':"Invitation",
                        email_to:"something",
                        invited_by: 'User/hikita',
                        note:"whjgasdj",
                        status:"pending"}
                  ],
                  stripe_subscription: _{ '@type' : "StripeSubscription",
                                           billing_email:"somewkjf",
                                           status: "active",
                                           stripe_id:"KItty",
                                           stripe_quantity:"32",
                                           stripe_user:"User/bonzai",
                                           subscription_id:"932438238429384ASBJDA" },
                  owned_by: 'User/bonzai',
                  creation_date: "2021-05-01T12:10:10Z",
                  organization_name:"somewhere",
                  status:"inactive"},

    create_context(Desc, commit{author: "me", message: "something"}, Context2),
    with_transaction(
        Context2,
        (
            replace_document(Context2, Document2, _)
        ),
        _
    ),

    open_descriptor(Desc, DB),
    get_document(DB, 'Organization/somewhere', Organization),
    Organization = json{'@id':'Organization/somewhere',
                        '@type':'Organization',
                        creation_date:"2021-05-01T12:10:10Z",
                        invitations:[json{'@id':_,
                                          '@type':'Invitation',
                                          email_to:"something",
                                          invited_by:'User/hikita',
                                          note:"whjgasdj",
                                          status:pending}],
                        organization_name:"somewhere",
                        owned_by:'User/bonzai',
                        status:inactive,
                        stripe_subscription:json{'@id':_,
                                                 '@type':'StripeSubscription',
                                                 billing_email:"somewkjf",
                                                 status:active,
                                                 stripe_id:"KItty",
                                                 stripe_quantity:32,
                                                 stripe_user:'User/bonzai',
                                                 subscription_id:"932438238429384ASBJDA"}}.

:- end_tests(javascript_client_bugs).

:- begin_tests(referential_integrity, []).
:- use_module(core(util/test_utils)).

:- use_module(core(query)).
person_schema('
    {
        "@type"     : "@context",
        "@schema"   : "http://xyz#",
        "@base"     : "base://path/",
        "xsd"       : "http://www.w3.org/2001/XMLSchema#"
    }
    {
        "@id"           : "Person",
        "@type"         : "Class",
        "name"          : "xsd:string",
        "@key"          : {"@type": "Lexical", "@fields": ["name"]},
    }
    {
        "@id"           : "MyDoc",
        "@type"         : "Class",
        "title"         : "xsd:string",
        "@key"          : {"@type": "Lexical", "@fields": ["title"]},
        "owner"         : "Person",
    }
').

test(subdocument_update,
     [setup(
          (   setup_temp_store(State),
              test_document_label_descriptor(Desc),
              write_schema(person_schema,Desc)
          )),
      cleanup(
          teardown_temp_store(State)
      ),
      error(
          schema_check_failure(
              [_{'@type':references_untyped_object,
                 object:"base://path/Fred",
                 predicate:"http://xyz#owner",
                 subject:"base://path/MyDoc/Some%20title"}])
      )
     ]
    ) :-
    Document =
    _{
        '@type' : "MyDoc",
        title : "Some title",
        owner : "Fred"
    },
    Commit_Info = commit_info{ author : me, message: yes},
    create_context(Desc, Commit_Info, Context),
    with_transaction(Context,
                     insert_document(
                         Context,
                         Document,
                         Id),
                     _),
    writeq(Id),

    get_document(Desc, Id, New_Doc),
    writeq(New_Doc).


:- end_tests(referential_integrity).


:- begin_tests(document_id_generation, []).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test_generated_document_id(Desc, Schema, Instance, ID) :-
    Commit_Info = commit_info{author:"test",message:"test"},
    create_context(Desc, Commit_Info, Context1),
    with_transaction(Context1,
                     insert_schema_document(
                         Context1,
                         Schema),
                     _),

    create_context(Desc, Commit_Info, Context2),
    with_transaction(Context2,
                     insert_document(
                         Context2,
                         Instance,
                         ID_Ex),
                     _),

    database_prefixes(Desc, Prefixes),
    compress_dict_uri(ID_Ex, Prefixes, Found_ID),

    do_or_die(ID = Found_ID,
              error(comparison_error(expected(ID), found(Found_ID)))).

test(document_lexical,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "Lexical",
                     '@fields': ["foo","bar"]},
           foo: "xsd:string",
           bar: "xsd:decimal",
           baz: "xsd:integer"},

        _{ '@type': "Thing",
           foo: "hi",
           bar: (0.5),
           baz: 42},

        'Thing/hi+0.5').

test(document_hash,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "Hash",
                     '@fields': ["foo","bar"]},
           foo: "xsd:string",
           bar: "xsd:decimal",
           baz: "xsd:integer"},

        _{ '@type': "Thing",
           foo: "hi",
           bar: (0.5),
           baz: 42},
        'Thing/3fd2bf062f0655d2c37084467c96cb4144de9078cb9034fb0c945a9d6a563fe7').

test(document_random,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "Random"},
           foo: "xsd:string",
           bar: "xsd:decimal",
           baz: "xsd:integer"},

        _{ '@type': "Thing",
           foo: "hi",
           bar: (0.5),
           baz: 42},

        ID),

    atom_concat('Thing/', _, ID).

test(document_valuehash,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "ValueHash"},
           foo: "xsd:string",
           bar: "xsd:decimal",
           baz: "xsd:integer"},

        _{ '@type': "Thing",
           foo: "hi",
           bar: (0.5),
           baz: 42},

        'Thing/78b07792a224ec58ac4b7688707482a1f42a7a695a907f5780d11dc634739aae').

test(document_invalid_id_submitted,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(submitted_id_does_not_match_generated_id('http://somewhere.for.now/document/ThisIsWrong','http://somewhere.for.now/document/Thing/hi+0.5'),_)
     ]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "Lexical",
                     '@fields': ["foo", "bar"]},
           foo: "xsd:string",
           bar: "xsd:decimal",
           baz: "xsd:integer"},

        _{ '@type': "Thing",
           '@id' : 'ThisIsWrong',
           foo: "hi",
           bar: (0.5),
           baz: 42},

        _ID).

test(underscore_space_slash_in_id,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "Lexical",
                     '@fields': ["foo", "bar", "baz"]},
           foo: "xsd:string",
           bar: "xsd:decimal",
           baz: "xsd:string"},

        _{ '@type': "Thing",
           foo: "hi_there buddy",
           bar: (0.5),
           baz: "lo_there/buddy"},

        'Thing/hi_there%20buddy+0.5+lo_there%2Fbuddy').

test(normalizable_float,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"),
             resolve_absolute_string_descriptor("admin/foo", Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    test_generated_document_id(
        Desc,

        _{ '@type': "Class",
           '@id': "Thing",
           '@key': _{'@type': "Lexical",
                     '@fields': ["foo"]},
           foo: "xsd:float"},

        _{ '@type': "Thing",
           foo: "0.5000000"},
        'Thing/0.5').

:- end_tests(document_id_generation).


:- begin_tests(foreign_types, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(elaborate_foreign_type,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foreign"),
             resolve_absolute_string_descriptor("admin/foreign", Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    From = _{ '@type' : "Class",
              '@id' : "From",
              '@key' : _{ '@type' : "Lexical", '@fields' : ["to"]},
              to : "To"},
    To= _{ '@type' : "Foreign",
           '@id' : "To" },
    create_context(Desc, commit_info{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        (   insert_schema_document(Context,From),
            insert_schema_document(Context,To)
        ),
        _
    ),

    From_Doc = _{ '@type' : "From", to : "To/george" },
    open_descriptor(Desc, DB),

    json_elaborate(DB, From_Doc, Elaborated),

    Elaborated =
    json{'@id':'http://somewhere.for.now/document/From/http%3A%2F%2Fsomewhere.for.now%2Fdocument%2FTo%2Fgeorge',
         '@type':'http://somewhere.for.now/schema#From',
         'http://somewhere.for.now/schema#to':
         json{'@id':'http://somewhere.for.now/document/To/george',
              '@type':"@id",
              '@foreign' : 'http://somewhere.for.now/schema#To'}},

    json_triples(DB,From_Doc, Triples),

    Triples =[
        t('http://somewhere.for.now/document/From/http%3A%2F%2Fsomewhere.for.now%2Fdocument%2FTo%2Fgeorge',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://somewhere.for.now/schema#From'),
        t('http://somewhere.for.now/document/To/george',
          'http://terminusdb.com/schema/sys#foreign_type',
          'http://somewhere.for.now/schema#To'),
        t('http://somewhere.for.now/document/From/http%3A%2F%2Fsomewhere.for.now%2Fdocument%2FTo%2Fgeorge',
          'http://somewhere.for.now/schema#to',
          'http://somewhere.for.now/document/To/george')].

test(foreign_type,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","hr"),
             create_db_with_empty_schema("admin","finance"),
             resolve_absolute_string_descriptor("admin/hr", HR_Desc),
             resolve_absolute_string_descriptor("admin/finance", Finance_Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Employee =
    _{ '@type' : "Class",
       '@id' : "Employee",
       '@key' : _{ '@type' : "Lexical", '@fields' : ["name", "start_date"] },
       name : "xsd:string",
       start_date : "xsd:date",
       termination_date : _{ '@type' : "Optional", '@class' : "xsd:date"},
       title : "xsd:string"
     },

    create_context(HR_Desc, commit_info{author: "me", message: "something"}, HR_Context),
    with_transaction(
        HR_Context,
        insert_schema_document(HR_Context,Employee),
        _
    ),

    Payroll =
    _{ '@type' : "Class",
       '@id' : "Payroll",
       payroll : _{ '@type' : "Set",
                    '@class' : "PayRecord"}},
    PayRecord =
    _{ '@type' : "Class",
       '@id' : "PayRecord",
       '@subdocument' : [],
       '@key' : _{ '@type' : "Lexical", '@fields' : ["employee"] },
       pay_period : "xsd:duration",
       pay : "xsd:decimal",
       employee : "Employee"
     },
    Employee_Stub =
    _{ '@type' : "Foreign",
       '@id' : "Employee" },

    create_context(Finance_Desc, commit_info{author: "me", message: "something"}, Finance_Context),
    with_transaction(
        Finance_Context,
        (   insert_schema_document(Finance_Context,Payroll),
            insert_schema_document(Finance_Context,PayRecord),
            insert_schema_document(Finance_Context,Employee_Stub)
        ),
        _
    ),

    Joe = _{ '@type' : "Employee",
             name : "joe",
             start_date : "2012-05-03",
             title : "Senior Engineer" },
    Jane = _{ '@type' : "Employee",
              name : "jane",
              start_date : "1995-05-03",
              title : "Senior Senior Engineer" },

    create_context(HR_Desc, commit_info{author: "me", message: "something"}, HR_Doc_Context),
    with_transaction(
        HR_Doc_Context,
        (   insert_document(HR_Doc_Context,Joe,Joe_Id),
            insert_document(HR_Doc_Context,Jane,Jane_Id)
        ),
        _
    ),

    Payroll_Doc =
    _{ '@type' : "Payroll",
       '@id' : "Payroll/standard",
       payroll : [
           _{ '@type' : "PayRecord",
              pay_period : "P1M",
              pay : "12.30",
              employee : Joe_Id },
           _{ '@type' : "PayRecord",
              pay_period : "P1M",
              pay : "32.85",
              employee : Jane_Id }
       ]
     },

    create_context(Finance_Desc, commit_info{author: "me", message: "something"}, Finance_Doc_Context),
    with_transaction(
        Finance_Doc_Context,
        (   insert_document(Finance_Doc_Context,Payroll_Doc,Payroll_Doc_Id)
        ),
        _
    ),

    get_document(Finance_Desc, Payroll_Doc_Id, New_Payroll),
    New_Payroll =
    json{'@id':'Payroll/standard',
         '@type':'Payroll',
         payroll:[json{'@id':'Payroll/standard/payroll/PayRecord/http%3A%2F%2Fsomewhere.for.now%2Fdocument%2FEmployee%2Fjane%2B1995-05-03',
                       '@type':'PayRecord',
                       employee:'Employee/jane+1995-05-03',
                       pay:32.85,
                              pay_period:"P1M"},
                  json{'@id':'Payroll/standard/payroll/PayRecord/http%3A%2F%2Fsomewhere.for.now%2Fdocument%2FEmployee%2Fjoe%2B2012-05-03',
                       '@type':'PayRecord',
                       employee:'Employee/joe+2012-05-03',
                       pay:12.3,
                              pay_period:"P1M"}]}.

:- end_tests(foreign_types).

:- begin_tests(id_capture, []).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

cross_reference_required_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{
  "@type": "Class",
  "@id": "Person",
  "@key": {"@type":"Lexical","@fields":["name"]},
  "name": "xsd:string",
  "friend": "Person"
}
').
cross_reference_optional_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{
  "@type": "Class",
  "@id": "Person",
  "@key": {"@type":"Lexical","@fields":["name"]},
  "name": "xsd:string",
  "friend": {"@type":"Optional","@class":"Person"}
}
').

cross_reference_set_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{
  "@type": "Class",
  "@id": "Person",
  "@key": {"@type":"Lexical","@fields":["name"]},
  "name": "xsd:string",
  "friends": {"@type":"Set","@class":"Person"}
}
').

cross_reference_list_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{
  "@type": "Class",
  "@id": "Person",
  "@key": {"@type":"Lexical","@fields":["name"]},
  "name": "xsd:string",
  "friends": {"@type":"List","@class":"Person"}
}
').

cross_reference_array_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{
  "@type": "Class",
  "@id": "Person",
  "@key": {"@type":"Lexical","@fields":["name"]},
  "name": "xsd:string",
  "friends": {"@type":"Array","@class":"Person"}
}
').

test(cross_reference_required,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_required_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Bert",
                     name: "Bert",
                     friend: _{'@ref': "Capture_Ernie"}},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Ernie",
                     name: "Ernie",
                     friend: _{'@ref': "Capture_Bert"}},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out),

    ground(Out),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    Bert_Id == (Ernie_Elaborated.'terminusdb:///schema#friend'.'@id'),
    Ernie_Id == (Bert_Elaborated.'terminusdb:///schema#friend'.'@id').

test(cross_reference_optional,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_optional_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Bert",
                     name: "Bert",
                     friend: _{'@ref': "Capture_Ernie"}},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Ernie",
                     name: "Ernie",
                     friend: _{'@ref': "Capture_Bert"}},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out),

    ground(Out),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    Bert_Id == (Ernie_Elaborated.'terminusdb:///schema#friend'.'@id'),
    Ernie_Id == (Bert_Elaborated.'terminusdb:///schema#friend'.'@id').

test(cross_reference_set_singleton,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_set_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Bert",
                     name: "Bert",
                     friends: _{'@ref': "Capture_Ernie"}},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Ernie",
                     name: "Ernie",
                     friends: _{'@ref': "Capture_Bert"}},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out),

    ground(Out),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    [Bert_Cross] = (Ernie_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Ernie_Cross] = (Bert_Elaborated.'terminusdb:///schema#friends'.'@value'),
    Bert_Id == (Bert_Cross.'@id'),
    Ernie_Id == (Ernie_Cross.'@id').

test(cross_reference_set_multi,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_set_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Bert",
                     name: "Bert",
                     friends: [_{'@ref': "Capture_Ernie"},
                               _{'@ref': "Capture_Elmo"}]},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Ernie",
                     name: "Ernie",
                     friends: [_{'@ref': "Capture_Bert"},
                               _{'@ref': "Capture_Elmo"}]},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out_2),
    \+ ground(Out_2),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Elmo",
                     name: "Elmo",
                     friends: [_{'@ref': "Capture_Bert"},
                               _{'@ref': "Capture_Ernie"}]},
                   Context,
                   Out_2,
                   Elmo_Elaborated,
                   _Dependencies_3,
                   Out),

    ground(Out),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    Elmo_Id = (Elmo_Elaborated.'@id'),
    [Bert_Cross_1, Elmo_Cross_1] = (Ernie_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Ernie_Cross_1, Elmo_Cross_2] = (Bert_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Bert_Cross_2, Ernie_Cross_2] = (Elmo_Elaborated.'terminusdb:///schema#friends'.'@value'),
    Bert_Id == (Bert_Cross_1.'@id'),
    Bert_Id == (Bert_Cross_2.'@id'),
    Ernie_Id == (Ernie_Cross_1.'@id'),
    Ernie_Id == (Ernie_Cross_2.'@id'),
    Elmo_Id == (Elmo_Cross_1.'@id'),
    Elmo_Id == (Elmo_Cross_2.'@id').

test(cross_reference_list_multi,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_list_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Bert",
                     name: "Bert",
                     friends: [_{'@ref': "Capture_Ernie"},
                               _{'@ref': "Capture_Elmo"}]},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Ernie",
                     name: "Ernie",
                     friends: [_{'@ref': "Capture_Bert"},
                               _{'@ref': "Capture_Elmo"}]},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out_2),
    \+ ground(Out_2),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Elmo",
                     name: "Elmo",
                     friends: [_{'@ref': "Capture_Bert"},
                               _{'@ref': "Capture_Ernie"}]},
                   Context,
                   Out_2,
                   Elmo_Elaborated,
                   _Dependencies_3,
                   Out),

    ground(Out),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    Elmo_Id = (Elmo_Elaborated.'@id'),
    [Bert_Cross_1, Elmo_Cross_1] = (Ernie_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Ernie_Cross_1, Elmo_Cross_2] = (Bert_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Bert_Cross_2, Ernie_Cross_2] = (Elmo_Elaborated.'terminusdb:///schema#friends'.'@value'),
    Bert_Id == (Bert_Cross_1.'@id'),
    Bert_Id == (Bert_Cross_2.'@id'),
    Ernie_Id == (Ernie_Cross_1.'@id'),
    Ernie_Id == (Ernie_Cross_2.'@id'),
    Elmo_Id == (Elmo_Cross_1.'@id'),
    Elmo_Id == (Elmo_Cross_2.'@id').

test(cross_reference_array_multi,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_array_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Bert",
                     name: "Bert",
                     friends: [_{'@ref': "Capture_Ernie"},
                               _{'@ref': "Capture_Elmo"}]},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Ernie",
                     name: "Ernie",
                     friends: [_{'@ref': "Capture_Bert"},
                               _{'@ref': "Capture_Elmo"}]},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out_2),
    \+ ground(Out_2),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Elmo",
                     name: "Elmo",
                     friends: [_{'@ref': "Capture_Bert"},
                               _{'@ref': "Capture_Ernie"}]},
                   Context,
                   Out_2,
                   Elmo_Elaborated,
                   _Dependencies_3,
                   Out),

    ground(Out),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    Elmo_Id = (Elmo_Elaborated.'@id'),
    [Bert_Cross_1, Elmo_Cross_1] = (Ernie_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Ernie_Cross_1, Elmo_Cross_2] = (Bert_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Bert_Cross_2, Ernie_Cross_2] = (Elmo_Elaborated.'terminusdb:///schema#friends'.'@value'),
    Bert_Id == (Bert_Cross_1.'@id'),
    Bert_Id == (Bert_Cross_2.'@id'),
    Ernie_Id == (Ernie_Cross_1.'@id'),
    Ernie_Id == (Ernie_Cross_2.'@id'),
    Elmo_Id == (Elmo_Cross_1.'@id'),
    Elmo_Id == (Elmo_Cross_2.'@id').

test(self_reference_required,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_required_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),
    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture_Elmo",
                     name: "Elmo",
                     friend: _{'@ref': "Capture_Elmo"}},
                   Context,
                   In,
                   Elmo_Elaborated,
                   _Dependencies_1,
                   Out),

    ground(Out),

    Elmo_Id = (Elmo_Elaborated.'@id'),
    Elmo_Id == (Elmo_Elaborated.'terminusdb:///schema#friend'.'@id').

test(deep_reference,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_set_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(Desc, DB),

    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "C_Bert",
                     name: "Bert",
                     friends: [_{'@ref': "C_Ernie"},
                               _{'@type': "Person",
                                 '@capture': "C_Elmo",
                                 name: "Elmo",
                                 friends: [_{'@ref': "C_Bert"},
                                           _{'@ref': "C_Ernie"}]
                                }]},
                   Context,
                   In,
                   Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    \+ ground(Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "C_Ernie",
                     name: "Ernie",
                     friends: [_{'@ref': "C_Bert"},
                               _{'@ref': "C_Elmo"}]},
                   Context,
                   Out_1,
                   Ernie_Elaborated,
                   _Dependencies_2,
                   Out_2),

    ground(Out_2),

    [_,Elmo_Elaborated] = (Bert_Elaborated.'terminusdb:///schema#friends'.'@value'),

    Bert_Id = (Bert_Elaborated.'@id'),
    Ernie_Id = (Ernie_Elaborated.'@id'),
    Elmo_Id = (Elmo_Elaborated.'@id'),
    [Bert_Cross_1, Elmo_Cross_1] = (Ernie_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Ernie_Cross_1, Elmo_Cross_2] = (Bert_Elaborated.'terminusdb:///schema#friends'.'@value'),
    [Bert_Cross_2, Ernie_Cross_2] = (Elmo_Elaborated.'terminusdb:///schema#friends'.'@value'),
    Bert_Id == (Bert_Cross_1.'@id'),
    Bert_Id == (Bert_Cross_2.'@id'),
    Ernie_Id == (Ernie_Cross_1.'@id'),
    Ernie_Id == (Ernie_Cross_2.'@id'),
    Elmo_Id == (Elmo_Cross_1.'@id'),
    Elmo_Id == (Elmo_Cross_2.'@id').

test(double_capture,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(cross_reference_set_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(capture_already_bound("Capture"))
     ]) :-
    open_descriptor(Desc, DB),

    database_prefixes(DB,Context),
    empty_assoc(In),
    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture",
                     name: "Bert",
                     friends: []},
                   Context,
                   In,
                   _Bert_Elaborated,
                   _Dependencies_1,
                   Out_1),

    json_elaborate(DB,
                   _{'@type': "Person",
                     '@capture': "Capture",
                     name: "Ernie",
                     friends: []},
                   Context,
                   Out_1,
                   _Ernie_Elaborated,
                   _Dependencies_2,
                   _Out_2).

:- end_tests(id_capture).

:- begin_tests(json_tables, [concurrent(true)]).
:- use_module(core(util/test_utils)).

geojson_point_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type" : "Enum",
  "@id" : "Point_Type",
  "@value" : [ "Point" ] }

{ "@type": "Class",
  "@id": "Point",
  "type": "Point_Type",
  "coordinates" : {"@type":"Array",
                   "@dimensions" : 1,
                   "@class": "xsd:decimal"}}

{ "@type" : "Enum",
  "@id" : "MultiPoint_Type",
  "@value" : [ "MultiPoint" ] }

{ "@type": "Class",
  "@id": "MultiPoint",
  "type": "MultiPoint_Type",
  "coordinates" : {"@type":"Array",
                   "@dimensions" : 2,
                   "@class": "xsd:decimal"}}
').

test(just_a_table,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{ '@type': "Class",
                                 '@id': "MultiPoint",
                                 coordinates : _{ '@type' : "Table",
                                                  '@class' : "xsd:decimal"}}
                          )).

test(geojson_point,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(geojson_point_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    %    tspy('document/json':insert_document),
    with_test_transaction(Desc,
                          C1,
                          insert_document(
                              C1,
                              _{ '@type': "MultiPoint",
                                 '@id': "MultiPoint/MyMultiPoint",
                                 type: "MultiPoint",
                                 coordinates : [
                                     [100.0, 0.0],
                                     [101.0, 1.0]
                                 ]},
                              ID
                          )
                         ),
    open_descriptor(Desc, New_DB),
    get_document(New_DB, ID, Fresh_JSON),
    Fresh_JSON = json{'@id':'MultiPoint/MyMultiPoint',
                      '@type':'MultiPoint',
                      coordinates:[[100.0,0.0],[101.0,1.0]],
                      type:'MultiPoint'}.

spreadsheet_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "Class",
  "@id": "Spreadsheet",
  "sheets" : {"@type":"Set",
              "@class": "Sheet"}}

{ "@type": "Class",
  "@id": "Sheet",
  "@key": { "@fields": [ "index" ], "@type": "Lexical" },
  "@subdocument" : [],
  "index" : "xsd:string",
  "cells" : {"@type":"Array",
             "@dimensions":2,
             "@class":"Cell"}}

{ "@type": "Class",
  "@id": "Cell",
  "@key": { "@type" : "ValueHash" },
  "@subdocument" : [],
  "value" : "xsd:string" }
').

test(spreadsheet,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(spreadsheet_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    % tspy('document/json':insert_document),
    with_test_transaction(Desc,
                          C1,
                          insert_document(
                              C1,
                              _{ '@type': "Spreadsheet",
                                 '@id': "Spreadsheet/My_Spreadsheet",
                                 sheets: [
                                     _{ '@type' : "Sheet",
                                        index : "1",
                                        cells : [
                                            [
                                                _{ '@type' : "Cell",
                                                   value : "A"
                                                 },
                                                _{ '@type' : "Cell",
                                                   value : "B"
                                                 }

                                            ]
                                        ]}

                                 ]},
                              ID
                          )
                         ),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, ID, Fresh_JSON),
    Fresh_JSON =
    json{'@id':'Spreadsheet/My_Spreadsheet',
         '@type':'Spreadsheet',
         sheets:
         [json{'@id':'Spreadsheet/My_Spreadsheet/sheets/Sheet/1',
               '@type':'Sheet',
               cells:
               [[json{'@id':'Cell/d3b1acf9b81f8781fb2c42e59f9eb482746a444c7793e3591fc526d14f850ab0',
                      '@type':'Cell',value:"A"},
                 json{'@id':'Cell/f77864fbb7e9824adc461725159bd4b318bbcd98c064a0057f673f3411c81810',
                      '@type':'Cell',value:"B"}]],
               index:"1"}]}.

test(wrong_dim_error,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(geojson_point_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':invalid_array_dimensions,
                      array:[[100.0,0.0],[101.0,1.0]],
                      dimensions:1}]),
            _)
     ]) :-
    with_test_transaction(Desc,
                          C1,
                          insert_document(
                              C1,
                              _{ '@type': "Point",
                                 '@id': "Point/MyPoint",
                                 type: "Point",
                                 coordinates : [
                                     [100.0, 0.0],
                                     [101.0, 1.0]
                                 ]
                               },
                              _ID
                          )
                         ).

:- end_tests(json_tables).

:- begin_tests(json_unit_type, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema_class_with_unit_property('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "Class",
  "@id": "Foo",
  "field": "sys:Unit"
}
').

test(class_with_unit_property,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    write_schema(schema_class_with_unit_property,Desc),

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo",
                                            'field': []},
                                          Id)
                         ),

    get_document(Desc, Id, Document),


    _{'@type': 'Foo',
      'field': []} :< Document.

test(class_with_unit_property_but_nonnil_data,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_class_with_unit_property,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':not_a_sys_unit,
                      document:json{'@type':'terminusdb:///schema#Foo',
                                    'terminusdb:///schema#field':42},
                      field:'terminusdb:///schema#field'}]),
            _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo",
                                            'field': 42},
                                          _Id)
                         ).

test(class_with_unit_property_missing_field,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_class_with_unit_property,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure([json{'@type':not_a_sys_unit,
                                       document:json{'@type':'terminusdb:///schema#Foo'},
                                       field:'terminusdb:///schema#field'}]),_)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo"},
                                          _Id)
                         ).

schema_class_with_oneof_unit_property('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "Class",
  "@id": "Foo",
  "@oneOf": {"a": "sys:Unit",
             "b": "sys:Unit"}
}
').

test(class_with_oneof_unit_property,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    write_schema(schema_class_with_oneof_unit_property,Desc),

    with_test_transaction(Desc,
                          C,
                          (   insert_document(C,
                                              _{'@type': "Foo",
                                                'a': []},
                                              Id1),
                              insert_document(C,
                                              _{'@type': "Foo",
                                                'b': []},
                                              Id2)
                          )),

    get_document(Desc, Id1, Document1),
    get_document(Desc, Id2, Document2),


    _{'@type': 'Foo',
      'a': []} :< Document1,
    _{'@type': 'Foo',
      'b': []} :< Document2.

test(class_with_oneof_unit_property_but_nonnil_data,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_class_with_oneof_unit_property,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':not_a_sys_unit,
                      document:json{'@type':'terminusdb:///schema#Foo',
                                    'terminusdb:///schema#a':42},
                      field:'terminusdb:///schema#a'}]),
            _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo",
                                            'a': 42},
                                          _Id)
                         ).

test(class_with_oneof_unit_property_missing_field,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_class_with_oneof_unit_property,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':no_choice_is_cardinality_one,
                      choice:json{'terminusdb:///schema#a':'http://terminusdb.com/schema/sys#Unit',
                                  'terminusdb:///schema#b':'http://terminusdb.com/schema/sys#Unit'},
                      document:json{'@type':'terminusdb:///schema#Foo'}}]),
            _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo"},
                                          _Id)
                         ).

schema_taggedunion_with_unit_property('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "TaggedUnion",
  "@id": "Foo",
  "a": "sys:Unit",
  "b": "sys:Unit"
}
').

test(taggedunion_with_unit_property,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    write_schema(schema_taggedunion_with_unit_property,Desc),

    with_test_transaction(Desc,
                          C,
                          (   insert_document(C,
                                              _{'@type': "Foo",
                                                'a': []},
                                              Id1),
                              insert_document(C,
                                              _{'@type': "Foo",
                                                'b': []},
                                              Id2)
                          )),

    get_document(Desc, Id1, Document1),
    get_document(Desc, Id2, Document2),


    _{'@type': 'Foo',
      'a': []} :< Document1,
    _{'@type': 'Foo',
      'b': []} :< Document2.

test(taggedunion_with_unit_property_but_nonnil_data,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_taggedunion_with_unit_property,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':not_a_sys_unit,
                      document:json{'@type':'terminusdb:///schema#Foo',
                                    'terminusdb:///schema#a':42},
                      field:'terminusdb:///schema#a'}]),_)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo",
                                            'a': 42},
                                          _Id)
                         ).

test(taggedunion_with_unit_property_missing_field,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_taggedunion_with_unit_property,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':no_choice_is_cardinality_one,
                      choice:
                      json{'terminusdb:///schema#a':'http://terminusdb.com/schema/sys#Unit',
                           'terminusdb:///schema#b':'http://terminusdb.com/schema/sys#Unit'},
                      document:json{'@type':'terminusdb:///schema#Foo'}}]),
            _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Foo"},
                                          _Id)
                         ).

:- end_tests(json_unit_type).

:- begin_tests(json_cardinality, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema_cardinality('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "Class",
  "@id": "Card",
  "a": { "@type" : "Cardinality",
         "@class" : "xsd:integer",
         "@cardinality" : 1}
}

{ "@type": "Class",
  "@id": "Min",
  "a": { "@type" : "Cardinality",
         "@class" : "xsd:integer",
         "@min_cardinality" : 1}
}

{ "@type": "Class",
  "@id": "Max",
  "a": { "@type" : "Cardinality",
         "@class" : "xsd:integer",
         "@max_cardinality" : 1}
}

{ "@type": "Class",
  "@id": "MinMax",
  "a": { "@type" : "Cardinality",
         "@class" : "xsd:integer",
         "@min_cardinality" : 1,
         "@max_cardinality" : 2}
}
').

test(insert_card,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Card",
                                            'a': 42},
                                          _Id)
                         ).

test(fail_card,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(
          schema_check_failure(
              [json{'@type':field_has_wrong_cardinality,
                    actual:2,
                    document:json{'@type':'terminusdb:///schema#Card',
                                  'terminusdb:///schema#a':[42,23]},
                    field:'terminusdb:///schema#a',
                    max:1,min:1}]),
          _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Card",
                                            'a': [42,23]},
                                          _Id)
                         ).

test(insert_card_min,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Min",
                                            'a': 42},
                                          _Id)
                         ).

test(fail_card_min_under,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':required_field_does_not_exist_in_document,
                      document:json{'@type':'terminusdb:///schema#Min'},
                      field:'terminusdb:///schema#a'}]),
            _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Min"},
                                          _Id)
                         ).

test(card_max_zero,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Max"},
                                          _Id)
                         ).

test(card_max_one,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Max",
                                            'a' : 42},
                                          _Id)
                         ).

test(fail_card_max_over,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_cardinality,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure([json{'@type':field_has_wrong_cardinality,
                                       actual:2,
                                       document:
                                       json{'@type':'terminusdb:///schema#Max',
                                            'terminusdb:///schema#a':[42,43]},
                                       field:'terminusdb:///schema#a',
                                       max:1,
                                       min:0}]),
           _)
     ]) :-

    with_test_transaction(Desc,
                          C,
                          insert_document(C,
                                          _{'@type': "Max",
                                            'a': [42,43]},
                                          _Id)
                         ).


:- end_tests(json_cardinality).

:- begin_tests(big).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema_big('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type": "Class",
  "@id": "Big",
  "big": { "@type" : "Set",
           "@class" : "Big" }
}
').

gen_big(0,_,Big) :-
    !,
    Big = _{ '@type' :  "Big",
             big : []}.
gen_big(Depth,Width,Big) :-
    findall(Inner_Big,
            (Depth_Next is Depth - 1,
             between(0,Width,_),
             gen_big(Depth_Next,Width,Inner_Big)),
            List),
    Big = _{ '@type' :  "Big",
             'big' : List}.

test(big,
     [blocked('too slow'),
      setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(schema_big,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    gen_big(5,7,Big),
    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Big,_)
    ).

:- end_tests(big).

:- begin_tests(json_datatype, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

json_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type" : "Class",
  "@id" : "HasSomeMetaData",
  "name" : "xsd:string",
  "json" : "sys:JSON"
}

{ "@type" : "Class",
  "@id" : "Thing",
  "json_set" : { "@type" : "Set",
                 "@class" : "sys:JSON" }
}

{ "@type" : "Class",
  "@id" : "Thing2",
  "json_array" : { "@type" : "Array",
                   "@class" : "sys:JSON" }
}

{ "@type" : "Class",
  "@id" : "Thing3",
  "json_list" : { "@type" : "List",
                  "@class" : "sys:JSON" }
}

{ "@type" : "Class",
  "@id" : "Thing4",
  "json_optional" : { "@type" : "Optional",
                      "@class" : "sys:JSON" }
}

').

test(json_triples,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@type' : "HasSomeMetaData",
        '@id' : 'HasSomeMetaData/i_exist',
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    open_descriptor(Desc, DB),
    json_triples(DB, Document, Triples),
    Triples =
    [ t('terminusdb:///data/HasSomeMetaData/i_exist',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'terminusdb:///schema#HasSomeMetaData'),
	  t('terminusdb:///json/JSON/SHA1/9c375c0d2cd0bd7949e387cc28dafbc1c5be8de3',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/SHA1/9c375c0d2cd0bd7949e387cc28dafbc1c5be8de3',
		'http://terminusdb.com/schema/json#that',
		2.0^^'http://www.w3.org/2001/XMLSchema#decimal'),
	  t('terminusdb:///json/JSON/SHA1/9c375c0d2cd0bd7949e387cc28dafbc1c5be8de3',
		'http://terminusdb.com/schema/json#random',
		"stuff"^^'http://www.w3.org/2001/XMLSchema#string'),
	  t('terminusdb:///json/JSON/SHA1/5f6f1ff2d0ceea4cc50351637eeaf3bc469f15df',
		'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
		'http://terminusdb.com/schema/sys#JSON'),
	  t('terminusdb:///json/JSON/SHA1/5f6f1ff2d0ceea4cc50351637eeaf3bc469f15df',
		'http://terminusdb.com/schema/json#some',
		'terminusdb:///json/JSON/SHA1/9c375c0d2cd0bd7949e387cc28dafbc1c5be8de3'),
	  t('terminusdb:///data/HasSomeMetaData/i_exist',
		'terminusdb:///schema#json',
		'terminusdb:///json/JSON/SHA1/5f6f1ff2d0ceea4cc50351637eeaf3bc469f15df'),
	  t('terminusdb:///data/HasSomeMetaData/i_exist',
		'terminusdb:///schema#name',
		"testing"^^'http://www.w3.org/2001/XMLSchema#string')
	].

test(json_subobject,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@type' : "HasSomeMetaData",
        '@id' : 'HasSomeMetaData/reproducible',
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,Id)
    ),
    get_document(Desc,Id,JSON),

    JSON = json{ '@id':'HasSomeMetaData/reproducible',
                 '@type':'HasSomeMetaData',
                 json:json{some:json{random:"stuff", that:2.0}},
                 name:"testing"
               }.

test(top_level_json_with_id,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@id' : 'JSONDocument/my_json',
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,Id)
    ),
    get_document(Desc,Id,JSON),
    JSON =
    json{'@id' : Id,
         json:json{some:json{random:"stuff",that:2.0}},
         name:"testing"}.

test(top_level_json_without_id,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,Id)
    ),
    get_document(Desc,Id,JSON),
    JSON =
    json{'@id' : Id,
         json:json{some:json{random:"stuff",that:2.0}},
         name:"testing"}.

test(replace_document,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@id' : 'JSONDocument/my_json',
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,Id)
    ),

    Document2 =
    json{
        '@id' : 'JSONDocument/my_json',
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : false
                         }}
    },

    with_test_transaction(
        Desc,
        C2,
        replace_document(C2,Document2,false,true,_)
    ),

    get_document(Desc,Id,JSON),

    JSON =
    json{'@id':'terminusdb:///data/JSONDocument/my_json',
         json:json{some:json{random:"stuff",that:false}},
         name:"testing"}.

test(insert_num_list,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@id' : 'JSONDocument/my_json',
        numlist : [1,2,3]
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,Id)
    ),

    get_document(Desc,Id,JSON),

    JSON = json{'@id':'terminusdb:///data/JSONDocument/my_json',
                numlist:[1,2,3]}.

test(replace_hash_document,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,Id)
    ),

    Document2 =
    json{
        '@id' : Id,
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : false
                         }}
    },

    with_test_transaction(
        Desc,
        C2,
        replace_document(C2,Document2,false,true,_)
    ),
    atom_concat('terminusdb:///data/JSONDocument/', _, Id).

test(replace_named_document,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(
          not_a_valid_json_object_id('terminusdb:///data/named'),
          _)
     ]) :-

    Document =
    json{
        '@id' : named,
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,_)
    ).

test(insert_json_set,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    _{  json_set : [ json{ some :
                           json{ random : "stuff",
                                 that : 2.0
                               }},
                     json{ some :
                           json{ random : "other stuff",
                                 that : 3.0
                               }},
                     json{ three : 3 }]
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,Id)
    ),

    with_test_transaction(
        Desc,
        C2,
        get_document(C2,Id, Doc_Out)
    ),
    json{ '@type':'Thing',
          json_set:[ json{some:json{random:"stuff",that:2.0}},
		             json{some:json{random:"other stuff",that:3.0}},
		             json{three:3}
	               ]
        } :< Doc_Out.

test(insert_json_array,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    _{  json_array : [ json{ some :
                             json{ random : "stuff",
                                   that : 2.0
                                 }},
                       json{ some :
                             json{ random : "other stuff",
                                   that : 3.0
                                 }},
                       json{ three : 3 }]
     },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,Id)
    ),

    with_test_transaction(
        Desc,
        C2,
        get_document(C2,Id, Doc_Out)
    ),

    json{ '@type':'Thing2',
          json_array:[ json{some:json{random:"stuff",that:2.0}},
		               json{some:json{random:"other stuff",that:3.0}},
		               json{three:3}
	                 ]
        } :< Doc_Out.

test(insert_json_list,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    _{  json_list : [ json{ some :
                            json{ random : "stuff",
                                  that : 2.0
                                }},
                      json{ some :
                            json{ random : "other stuff",
                                  that : 3.0
                                }},
                      json{ three : 3 }]
     },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,Id)
    ),

    with_test_transaction(
        Desc,
        C2,
        get_document(C2,Id, Doc_Out)
    ),
    json{ '@type':'Thing3',
          json_list:[ json{some:json{random:"stuff",that:2.0}},
		             json{some:json{random:"other stuff",that:3.0}},
		             json{three:3}
	               ]
        } :< Doc_Out.

test(insert_json_optional,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    _{  json_optional : json{ some :
                              json{ random : "stuff",
                                    that : 2.0
                                  }}},

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,Id)
    ),

    with_test_transaction(
        Desc,
        C2,
        get_document(C2,Id,Doc_Out)
    ),

    json{ '@type':'Thing4',
          json_optional: json{some:json{random:"stuff",that:2.0}}
        }:< Doc_Out.

test(can_not_insert_json_class,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(can_not_insert_class_with_reserve_name('JSONDocument'),_)
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(C1,json{ '@id' : 'JSONDocument'})
    ).

test(inserts_as_json_when_typed,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@type' : "sys:JSONDocument",
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        % Note that we are not in raw insertion mode!
        insert_document(C1,Document,false,Id)
    ),
    get_document(Desc,Id,JSON),
    JSON =
    json{'@id' : Id,
         json:json{some:json{random:"stuff",that:2.0}},
         name:"testing"}.

test(instance_schema_check,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(json_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Document =
    json{
        '@id' : 'JSONDocument/named',
        name : "testing",
        json : json{ some :
                     json{ random : "stuff",
                           that : 2.0
                         }}
    },

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1,Document,true,_)
    ),

    with_test_transaction(
        Desc,
        C2,
        insert_schema_document(
            C2,
            _{ '@type': "Class",
               '@id': "Point",
               coordinates : _{ '@type' : "Array",
                                '@dimensions':2,
                                '@class' : "xsd:decimal"}}
        )).

geojson_schema('[
    { "@type" : "Enum",
      "@id" : "FeatureCollection_Type",
      "@value" : [ "FeatureCollection" ]
    },

    { "@type" : "Class",
      "@id" : "FeatureCollection",
      "@key" : { "@type" : "Random" },
      "name" : { "@type" : "Optional",
                 "@class" : "xsd:string" },
      "type": "FeatureCollection_Type",
      "crs" : { "@type" : "Optional",
                "@class" : "name" },
      "properties" : { "@type" : "Optional",
                       "@class" : "sys:JSON" },
      "features" : { "@type" : "Set",
                     "@class": "Feature"}
    },

    { "@type" : "Enum",
      "@id" : "Feature_Type",
      "@value" : [ "Feature" ]
    },

    { "@type" : "Class",
      "@id" : "Feature",
      "type" : "Feature_Type",
      "@key" : { "@type" : "Random" },
      "id" : { "@type" : "Optional",
               "@class" : "xsd:string" },
      "title" : { "@type" : "Optional",
                  "@class" : "xsd:string" },
      "geometry" : "Geometry",
      "properties" : { "@type" : "Optional",
                       "@class" : "sys:JSON"},
      "centerline" : { "@type" : "Optional",
                       "@class" : "Geometry" }
    },

    { "@type" : "Class",
      "@id" : "Geometry",
      "@abstract" : [],
      "@unfoldable" : []
    },

    { "@type" : "Enum",
      "@id" : "Point_Type",
      "@value" : [ "Point" ]
    },

    { "@type" : "Class",
      "@id" : "Point",
      "@inherits" : "Geometry",
      "@key" : { "@type" : "Random" },
      "type" : "Point_Type",
      "coordinates" : { "@type" : "Array",
                        "@dimensions" : 1,
                        "@class" : "xsd:decimal" }
    },

    { "@type" : "Enum",
      "@id" : "LineString_Type",
      "@value" : [ "LineString" ]
    },

    { "@type" : "Class",
      "@id" : "LineString",
      "@inherits" : "Geometry",
      "@key" : { "@type" : "Random" },
      "type" : "LineString_Type",
      "coordinates" : { "@type" : "Array",
                        "@dimensions" : 2,
                        "@class" : "xsd:decimal" }
    },

    { "@type" : "Enum",
      "@id" : "Polygon_Type",
      "@value" : [ "Polygon" ]
    },

    { "@type" : "Class",
      "@id" : "Polygon",
      "@key" : { "@type" : "Random" },
      "@inherits" : "Geometry",
      "type" : "Polygon_Type",
      "coordinates" : { "@type" : "Array",
                        "@dimensions" : 3,
                        "@class" : "xsd:decimal" }
    },

    { "@type" : "Enum",
      "@id" : "MultiPolygon_Type",
      "@value" : [ "MultiPolygon" ]
    },

    { "@type" : "Class",
      "@id" : "MultiPolygon",
      "@key" : { "@type" : "Random" },
      "@inherits" : "Geometry",
      "type" : "MultiPolygon_Type",
      "coordinates" : { "@type" : "Array",
                        "@dimensions" : 3,
                        "@class" : "xsd:decimal" }
    },

    { "@type" : "Enum",
      "@id" : "GeometryCollection_Type",
      "@value" : [ "GeometryCollection" ]
    },

    { "@type" : "Class",
      "@id" : "GeometryCollection",
      "@key" : { "@type" : "Random" },
      "@inherits" : "Geometry",
      "type" : "GeometryCollection_Type",
      "geometries" : { "@type" : "Set",
                       "@class" : "Geometry"}
    },

    { "@type" : "Enum",
      "@id" : "Name_Type",
      "@value" : [ "name" ]
    },

    { "@type" : "Class",
      "@id" : "name",
      "type": "Name_Type",
      "properties": "sys:JSON"
    }
]').

test(geojson_unfoldable,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-

     geojson_schema(Schema_Atom),
     atom_json_dict(Schema_Atom, Schema_Documents, []),

     with_test_transaction(
         Desc,
         C1,
         forall(member(Schema_Doc, Schema_Documents),
                insert_schema_document(C1, Schema_Doc))
     ),

     with_test_transaction(
         Desc,
         C2,
         get_schema_document(C2, 'Geometry', Geometry)
     ),

     Geometry = json{'@abstract':[],'@id':'Geometry','@type':'Class','@unfoldable':[]}.

test(geojson_example,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-

    geojson_schema(Schema_Atom),
    atom_json_dict(Schema_Atom, Schema_Documents, []),

    with_test_transaction(
        Desc,
        C1,
        forall(member(Schema_Doc, Schema_Documents),
               insert_schema_document(C1, Schema_Doc))
    ),

    with_test_transaction(
        Desc,
        C2,
        insert_document(C2,
                        _{ 'geometry':
                           _{ 'coordinates':
                              [[[-122.42407516836617,
                                 37.782909438426415,
                                 0.0]]],
                              'type': "Polygon"
                            },
                           'properties':
                           _{
                               'BLKLOT': "VACSTWIL",
                               'BLOCK_NUM': "VACST",
                               'FROM_ST': null,
                               'LOT_NUM': "WIL",
                               'MAPBLKLOT': "VACSTWIL",
                               'ODD_EVEN': null,
                               'STREET': null,
                               'ST_TYPE': null,
                               'TO_ST': null
                           },
                           'type': "Feature"
                         },
                        false,
                        _Id)
    ).

:- use_module(core(api/api_patch)).
test(json_diff,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc)
            )),
      cleanup(teardown_temp_store(State))]) :-

    Doc1 =
    _{
        prop : "value"
    },

    create_context(Desc, _{ author : "me", message : "Have you tried bitcoin?" }, C1),
    with_transaction(
        C1,
        insert_document(C1, Doc1, true, Id1),
        Meta1
    ),
    get_dict(data_versions, Meta1, DV1),
    memberchk(Desc - data_version(branch,Commit1), DV1),

    Doc2 =
    _{
        '@id' : Id1,
        prop : "value2"
    },

    create_context(Desc, _{ author : "me", message : "cures what ails you!" }, C2),
    Insert = false,
    Raw_Json = true,
    with_transaction(
        C2,
        replace_document(C2, Doc2, Insert, Raw_Json, _Id2),
        Meta2
    ),

    get_dict(data_versions, Meta2, DV2),
    memberchk(Desc - data_version(branch,Commit2), DV2),
    super_user_authority(Auth),

    api_diff_all_documents(
        system_descriptor{},
        Auth,
        "admin/testdb",
        Commit1,
        Commit2,
        Patch,
        _{ keep : _{ '@id' : true, '_id' : true }}),

    Patch = [json{'@id':_,
                  prop:json{'@after':"value2",
                            '@before':"value",
                            '@op':"SwapValue"}}].

:- end_tests(json_datatype).

:- begin_tests(multilingual).
:- use_module(core(util/test_utils)).

multilingual_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "@documentation" : [{
      "@language" : "en",
      "@title" : "Example Schema",
      "@description" : "This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
      "@authors" : ["Gavin Mendel-Gleason"]
   },
   {  "@language" : "ka",
      "@title" : "მაგალითი სქემა",
      "@description" : "ეს არის მაგალითის სქემა. ჩვენ ვიყენებთ მას, რათა ვაჩვენოთ ინფორმაციის მრავალ ენაზე ჩვენების შესაძლებლობა ერთი და იმავე სემანტიკური შინაარსის შესახებ.",
      "@authors" : ["გავინ მენდელ-გლისონი"]
   }
  ],
  "xsd" : "http://www.w3.org/2001/XMLSchema#"
}

{ "@id" : "Example",
  "@type" : "Class",
  "@documentation" : [
     {
       "@language" : "en",
       "@label" : "Example",
       "@comment" : "An example class",
       "@properties" : { "name" : { "@label" : "name",
                                    "@comment" : "The name of the example object" },
                         "choice" : { "@label" : "choice",
                                      "@comment" : "A thing to choose" }}
     },
     {
        "@language" : "ka",
        "@label" : "მაგალითი",
        "@comment" : "მაგალითი კლასი",
        "@properties" : { "name" : { "@label" : "სახელი",
                                     "@comment" : "მაგალითის ობიექტის სახელი" },
                          "choice" : { "@label" : "არჩევანი",
                                       "@comment" : "რაც უნდა აირჩიოთ" }}
     }
  ],
  "name" : "xsd:string",
  "choice" : "Choice"
}

{ "@id" : "Choice",
  "@type" : "Enum",
  "@documentation" : [
     {
       "@language" : "en",
       "@label" : "Choice",
       "@comment" : "A Choice of a thing",
       "@values" : {
         "yes" : { "@label" : "yes",
                   "@comment" : "Is it a yes?" },
         "no" : { "@label" : "no",
                  "@comment" : "Or is it a no?" }
       }
     },
     {
       "@language" : "ka",
       "@label" : "არჩევანი",
       "@values" : {
          "yes" : { "@label" : "დიახ",
                    "@comment" : "კია?" },
          "no" : { "@label" : "არა",
                   "@comment" : "ან არის არა?" }
       }
     }
  ],
  "@value" : ["yes", "no"]
}
').

bogus_context_multilingual_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "@documentation" : {
      "@language" : "bogus",
      "@title" : "Example Schema",
      "@description" : "This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
      "@authors" : ["Gavin Mendel-Gleason"]
   },
  "xsd" : "http://www.w3.org/2001/XMLSchema#"
}').

bogus_multilingual_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "@documentation" : [{
      "@title" : "Example Schema",
      "@description" : "This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
      "@authors" : ["Gavin Mendel-Gleason"]
   },
   {  "@language" : "ka",
      "@title" : "მაგალითი სქემა",
      "@description" : "ეს არის მაგალითის სქემა. ჩვენ ვიყენებთ მას, რათა ვაჩვენოთ ინფორმაციის მრავალ ენაზე ჩვენების შესაძლებლობა ერთი და იმავე სემანტიკური შინაარსის შესახებ.",
      "@authors" : ["გავინ მენდელ-გლისონი"]
   }
  ],
  "xsd" : "http://www.w3.org/2001/XMLSchema#"
}').

bogus_multilingual_class('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
}

{ "@id" : "Example",
  "@type" : "Class",
  "@documentation" : [
     {
       "@label" : "Example",
       "@comment" : "An example class",
       "@properties" : { "name" : { "@label" : "name",
                                    "@comment" : "The name of the example object" },
                         "choice" : { "@label" : "choice",
                                      "@comment" : "A thing to choose" }}
     },
     {
        "@language" : "ka",
        "@label" : "მაგალითი",
        "@comment" : "მაგალითი კლასი",
        "@properties" : { "name" : { "@label" : "სახელი",
                                     "@comment" : "მაგალითის ობიექტის სახელი" },
                          "choice" : { "@label" : "არჩევანი",
                                       "@comment" : "რაც უნდა აირჩიოთ" }}
     }
  ],
  "name" : "xsd:string"
}
').

repeating_multilingual_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "@documentation" : [{
      "@language" : "ka",
      "@title" : "Example Schema",
      "@description" : "This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
      "@authors" : ["Gavin Mendel-Gleason"]
   },
   {  "@language" : "ka",
      "@title" : "მაგალითი სქემა",
      "@description" : "ეს არის მაგალითის სქემა. ჩვენ ვიყენებთ მას, რათა ვაჩვენოთ ინფორმაციის მრავალ ენაზე ჩვენების შესაძლებლობა ერთი და იმავე სემანტიკური შინაარსის შესახებ.",
      "@authors" : ["გავინ მენდელ-გლისონი"]
   }
  ],
  "xsd" : "http://www.w3.org/2001/XMLSchema#"
}').

test(schema_write,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    write_schema(multilingual_schema,Desc).

test(schema_read_class,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multilingual_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    get_schema_document(Desc, 'Example', Example),
    Example =
    json{'@documentation':
         [json{'@comment':"An example class",
               '@label':"Example",
               '@language':"en",
               '@properties':
               json{choice:json{'@comment':"A thing to choose",
                                '@label':"choice"},
                    name:json{'@comment':"The name of the example object",
                              '@label':"name"}}},
          json{'@comment':"მაგალითი კლასი",
               '@label':"მაგალითი",
               '@language':"ka",
               '@properties':
               json{choice:json{'@comment':"რაც უნდა აირჩიოთ",
                                '@label':"არჩევანი"},
                    name:json{'@comment':"მაგალითის ობიექტის სახელი",
                              '@label':"სახელი"}}}],
         '@id':'Example',
         '@type':'Class',
         choice:'Choice',
         name:'xsd:string'}.

test(schema_read_enum,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multilingual_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    get_schema_document(Desc, 'Choice', Choice),
    Choice =
    json{'@documentation':
         [json{'@comment':"A Choice of a thing",
               '@label':"Choice",
               '@language':"en",
               '@values':json{no:json{'@comment':"Or is it a no?",'@label':"no"},
                              yes:json{'@comment':"Is it a yes?",'@label':"yes"}}},
          json{'@label':"არჩევანი",
               '@language':"ka",
               '@values':json{no:json{'@comment':"ან არის არა?",'@label':"არა"},
                              yes:json{'@comment':"კია?",'@label':"დიახ"}}}],
         '@id':'Choice',
         '@type':'Enum',
         '@value':[yes,no]}.

test(schema_read_context,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multilingual_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    database_context_object(Desc, Context),
    Context =
    _5642{'@base':"terminusdb:///data/",
          '@documentation':[
              json{'@authors':["Gavin Mendel-Gleason"],
                   '@description':"This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
                   '@language':"en",
                   '@title':"Example Schema"},
              json{'@authors':["გავინ მენდელ-გლისონი"],
                   '@description':"ეს არის მაგალითის სქემა. ჩვენ ვიყენებთ მას, რათა ვაჩვენოთ ინფორმაციის მრავალ ენაზე ჩვენების შესაძლებლობა ერთი და იმავე სემანტიკური შინაარსის შესახებ.",
                   '@language':"ka",
                   '@title':"მაგალითი სქემა"}],
          '@schema':"terminusdb:///schema#",
          '@type':'Context',
          xsd:"http://www.w3.org/2001/XMLSchema#"}.

test(class_frame,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(multilingual_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    class_frame(Desc, 'Example', Frame),

    Frame =
    json{'@documentation':[
             json{'@comment':"An example class",
                  '@label':"Example",
                  '@language':"en",
                  '@properties':json{choice:json{'@comment':"A thing to choose",
                                                 '@label':"choice"},
                                     name:json{'@comment':"The name of the example object",
                                               '@label':"name"}}},
             json{'@comment':"მაგალითი კლასი",
                  '@label':"მაგალითი",
                  '@language':"ka",
                  '@properties':json{choice:json{'@comment':"რაც უნდა აირჩიოთ",
                                                 '@label':"არჩევანი"},
                                     name:json{'@comment':"მაგალითის ობიექტის სახელი",
                                               '@label':"სახელი"}}}],
         '@type':'Class',
         choice:json{'@id':'Choice',
                     '@type':'Enum',
                     '@values':[yes,no]},
         name:'xsd:string'}.

test(bogus_schema_write,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(casting_error("bogus",'http://www.w3.org/2001/XMLSchema#language'),_)
     ]) :-
    write_schema(bogus_context_multilingual_schema,Desc).

test(bogus_multilingual_write,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(no_language_tag_for_multilingual,_)
     ]) :-
    write_schema(bogus_multilingual_schema,Desc).

test(repeating_multilingual_schema,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(language_tags_repeated(["ka"]),_)
     ]) :-
    write_schema(repeating_multilingual_schema,Desc).

test(bogus_multilingual_class,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(no_language_tag_for_multilingual,_)
     ]) :-
    write_schema(bogus_multilingual_class,Desc).

:- end_tests(multilingual).

:- begin_tests(json_metadata).
:- use_module(core(util/test_utils)).

metadata_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "@metadata" : { "some" : [1,2,3], "things" : null, "remain" : { "value" : true }},
  "@documentation" : {
      "@title" : "Example Schema",
      "@description" : "This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
      "@authors" : ["Gavin Mendel-Gleason"]
   }
}').

test(elaborate_schema_metadata,
     []) :-
    metadata_schema(Schema),
    atom_json_dict(Schema, Context, []),
    context_elaborate(
        Context,
        Elaborated),
    Elaborated =
    json{'@id':"terminusdb://context",
         '@type':'sys:Context',
         'sys:base':json{'@type':"xsd:string",'@value':"terminusdb:///data/"},
         'sys:documentation':
         json{'@container':"@set",
              '@type':'sys:SchemaDocumentation',
              '@value':
              [json{'@id':'terminusdb://context/SchemaDocumentation/0',
                    '@type':"sys:SchemaDocumentation",
                    'sys:authors':
                    json{'@container':"@list",
                         '@type':"xsd:string",
                         '@value':[json{'@type':"xsd:string",
                                        '@value':"Gavin Mendel-Gleason"}]},
                    'sys:description':json{'@type':"xsd:string",
                                           '@value':"This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content."},
                    'sys:title':
                    json{'@type':"xsd:string",'@value':"Example Schema"}}]},
         'sys:metadata':_{'@type':"sys:JSON",
                          remain:_{value:true},some:[1,2,3],things:null},
         'sys:prefix_pair':json{'@container':"@set",'@type':"sys:Prefix",'@value':[]},
         'sys:schema':json{'@type':"xsd:string",'@value':"terminusdb:///schema#"}}.

test(schema_metadata,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    write_schema(metadata_schema,Desc),

    database_context_object(Desc,Context),
    Context =
    _{'@base':"terminusdb:///data/",
      '@documentation':
      json{
          '@authors':["Gavin Mendel-Gleason"],
          '@description':"This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
          '@title':"Example Schema"},
      '@metadata':json{remain:json{value:true},some:[1,2,3],things:null},
      '@schema':"terminusdb:///schema#",
      '@type':'Context'}.

metadata_class('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"
}

{ "@id" : "Example",
  "@type" : "Class",
  "@metadata" : { "some" : 3, "where" : [null,true], "over" : { "the" : ["r","a"]}},
  "name" : "xsd:string"
}

{ "@id" : "Choice",
  "@type" : "Enum",
  "@metadata" : { "blah" : "blah"},
  "@value" : ["yes", "no"]
}
').

garbage_metadata_enum('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"
}

{ "@id" : "Garbage",
  "@type" : "Enum",
  "@metadata" : { "blah" : "blah"}
}
').

test(elaborate_class_metadata,
     []) :-
    Class = json{'@id':"Example",
                 '@metadata':json{over: { the : ["r", "a"]},
                                  some:3,
                                  where: [null,true]},
                 '@type':"Class",
                 name:"xsd:string"},
    Prefixes = json{'@base':"terminusdb:///data/",
                    '@schema':"terminusdb:///schema#",
                    '@type':"@context",
                    api:'http://terminusdb.com/schema/api#',
                    json:'http://terminusdb.com/schema/json#',
                    owl:'http://www.w3.org/2002/07/owl#',
                    rdf:'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
                    rdfs:'http://www.w3.org/2000/01/rdf-schema#',
                    sys:'http://terminusdb.com/schema/sys#',
                    vio:'http://terminusdb.com/schema/vio#',
                    woql:'http://terminusdb.com/schema/woql#',
                    xdd:'http://terminusdb.com/schema/xdd#',
                    xsd:'http://www.w3.org/2001/XMLSchema#'},
    json_schema_elaborate(Class,Prefixes, Elaborated),
    Elaborated =
    json{'@id':'terminusdb:///schema#Example',
         '@type':'http://terminusdb.com/schema/sys#Class',
         'http://terminusdb.com/schema/sys#metadata':
         json{'@type':'http://terminusdb.com/schema/sys#JSON',
              over:{the:["r","a"]},some:3,where:[null,true]},
         'terminusdb:///schema#name':
         json{'@id':'http://www.w3.org/2001/XMLSchema#string','@type':"@id"}}.

test(class_metadata,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(metadata_class,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    get_schema_document(Desc, 'Example', Example),
    Example =
    json{'@id':'Example',
         '@metadata':json{over:json{the:["r","a"]},some:3,where:[null,true]},
         '@type':'Class',
         name:'xsd:string'}.

test(elaborate_enum_metadata,
     []) :-
    Class = json{'@id':"Choice",
                 '@metadata':json{blah : "blah"},
                 '@type':"Enum",
                 '@value' : ["yes", "no"]},
    Prefixes = json{'@base':"terminusdb:///data/",
                    '@schema':"terminusdb:///schema#",
                    '@type':"@context",
                    api:'http://terminusdb.com/schema/api#',
                    json:'http://terminusdb.com/schema/json#',
                    owl:'http://www.w3.org/2002/07/owl#',
                    rdf:'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
                    rdfs:'http://www.w3.org/2000/01/rdf-schema#',
                    sys:'http://terminusdb.com/schema/sys#',
                    vio:'http://terminusdb.com/schema/vio#',
                    woql:'http://terminusdb.com/schema/woql#',
                    xdd:'http://terminusdb.com/schema/xdd#',
                    xsd:'http://www.w3.org/2001/XMLSchema#'},
    json_schema_elaborate(Class,Prefixes, Elaborated),
    Elaborated =
    json{'@id':'terminusdb:///schema#Choice',
         '@type':'http://terminusdb.com/schema/sys#Enum',
         'http://terminusdb.com/schema/sys#metadata':
         json{'@type':'http://terminusdb.com/schema/sys#JSON',
              blah:"blah"},
         'http://terminusdb.com/schema/sys#value':
         json{'@container':"@list",
              '@type':"@id",
              '@value':[json{'@id':'terminusdb:///schema#Choice/yes',
                             '@type':"@id"},
                        json{'@id':'terminusdb:///schema#Choice/no',
                             '@type':"@id"}]}}.

test(enum_metadata,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    write_schema(metadata_class,Desc),
    get_schema_document(Desc, 'Choice', Choice),
    Choice =
    json{'@id':'Choice',
         '@metadata':json{blah:"blah"},
         '@type':'Enum',
         '@value':[yes,no]}.

test(garbage_metadata_enum,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(invalid_enum_values(
                json{'@id':"Garbage",
                     '@metadata':json{blah:"blah"},
                     '@type':"Enum"}), _)
     ]) :-
    write_schema(garbage_metadata_enum,Desc).


:- end_tests(json_metadata).

:- begin_tests(language_codes).
:- use_module(core(util/test_utils)).

language_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "rdfs" : "http://www.w3.org/2000/01/rdf-schema#",
  "rdf" : "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "@type": "@context"}

{ "@type" : "Class",
  "@id" : "Language",
  "language" : "xsd:language" }

{ "@type" : "Class",
  "@id" : "LangString",
  "rdfs:label" : "rdf:langString" }
').

test(various_lang_combos,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(language_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        (   insert_document(C1, _{ 'language': "en-GB"}, _),
            insert_document(C1, _{ 'language': "egy-Egyd"}, _),
            insert_document(C1, _{ 'language' : "en-Latn-US"}, _),
            insert_document(C1, _{ 'language' : "cy-GB-Latn"}, _),
            insert_document(C1, _{ 'language' : "az-Latn-IR"}, _)
        )
    ).

test(duplicate,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(language_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(
          schema_check_failure(
              [json{'@type':no_unique_type_for_document,
                    document:json{language:"en-en"},
                    reason:json{'terminusdb:///schema#language':
                                json{'@type':could_not_interpret_as_type,
                                     type:'http://www.w3.org/2001/XMLSchema#language',
                                     value:"en-en"}}}]),
          _)
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1, _{ 'language': "en-en"}, _)
    ).

test(nonsense_1,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(language_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':no_unique_type_for_document,
                      document:json{language:"foo-Bar"},
                      reason:json{'terminusdb:///schema#language':
                                  json{'@type':could_not_interpret_as_type,
                                       type:'http://www.w3.org/2001/XMLSchema#language',
                                       value:"foo-Bar"}}}]), _)

     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1, _{ 'language': "foo-Bar"}, _)
    ).

test(nonsense_2,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(language_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_check_failure(
                [json{'@type':no_unique_type_for_document,
                      document:json{language:"en-GBR"},
                      reason:json{'terminusdb:///schema#language':
                                  json{'@type':could_not_interpret_as_type,
                                       type:'http://www.w3.org/2001/XMLSchema#language',
                                       value:"en-GBR"}}}]), _)
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1, _{ 'language': "en-GBR"}, _)
    ).

test(rdf_language,[]) :-
    findall(t(S,P,V),
            json_triple_(
                json{'@id':'terminusdb:///data/This/thing',
                     '@type':'terminusdb:///schema#This',
                     'http://www.w3.org/2000/01/rdf-schema#label':
                     json{'@lang':"en",
                          '@type':'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',
                          '@value':"Test"}},
                context{'@base':"terminusdb:///data/",
                        '@schema':"terminusdb:///schema#",
                        '@type':'Context',
                        rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                        rdfs:"http://www.w3.org/2000/01/rdf-schema#"},
                t(S, P, V)),
            Triples),
    Triples =
    [ t('terminusdb:///data/This/thing',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'terminusdb:///schema#This'),
      t('terminusdb:///data/This/thing',
        'http://www.w3.org/2000/01/rdf-schema#label',
        "Test"@"en")
    ].

test(rdf_language_doc_insert_get,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(language_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1, _{ 'rdfs:label' : _{ '@lang' : "en", '@value' : "Me"}}, Id)
    ),

    get_document(Desc, Id, Doc),

    Doc = json{ '@id':_Id,
                '@type':'LangString',
                'rdfs:label':json{'@lang':"en",'@value':"Me"}
              }.


test(rdf_language_nonsense,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(language_schema,Desc)
            )),
      cleanup(teardown_temp_store(State)),
      error(unknown_language_tag("foobar"), _)
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_document(C1, _{ 'rdfs:label' : _{ '@lang' : "foobar", '@value' : "Me"}}, _Id)
    ).

:- end_tests(language_codes).

:- begin_tests(class_frames).

:- use_module(core(util/test_utils)).

abstract_choice_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type" : "Class",
  "@id" : "Abstract",
  "name" : "xsd:string",
  "@abstract" : [] }

{ "@type" : "Class",
  "@id" : "Concrete",
  "@inherits" : "Abstract",
  "concrete" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "Alternative",
  "alternative" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "Choice",
  "@oneOf" : { "a" : "Abstract",
               "b" : "Alternative" }}

').

test(choice_with_oneof_abstract,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Desc),
             write_schema(abstract_choice_schema,Desc)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    class_frame(Desc, "Choice", Frame),
    Frame = json{'@oneOf':[json{a:['Concrete'],b:'Alternative'}],'@type':'Class'}.


:- end_tests(class_frames).
