:- module(database_utils,[
              db_name_uri/2,
              db_exists_in_layer/2,
              db_finalized_in_layer/2,
              database_exists/1,
              terminus_graph_layer/2,
              database_instance/2,
              database_inference/2,
              database_schema/2
          ]).

/** <module> Database Utilities
 *
 * Various database level utilities. This is a layer above the triple store
 * in terms of logic, and placed here as we want to be able to make use
 * of WOQL and other libraries without circularity.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(file_utils).
:- use_module(triplestore).
:- use_module(utils).
:- use_module(database).
:- use_module(expansions).
:- use_module(terminus_bootstrap).
:- use_module(literals, [object_storage/2]).
:- use_module(casting, [idgen/3]).
:- reexport(syntax).

/*
 * db_name_uri(+Name,-Uri) is det.
 * db_name_uri(-Name,+Uri) is det.
 *
 * Make a fully qualified Uri from Name or vice-versa
 */
db_name_uri(Name, Uri) :-
    idgen('terminus:///terminus/document/Database', [Name], Uri).

db_exists_in_layer(Layer, Name) :-
    database_name_property_uri(Database_Name_Property_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(Name^^Xsd_String_Type_Uri, Name_Literal),
    db_name_uri(Name, Db_Uri),
    % TODO: We can use high level querying as well
    triple(Layer,
           Db_Uri,
           Database_Name_Property_Uri,
           Name_Literal).

db_finalized_in_layer(Layer,Name) :-
    finalized_element_uri(Finalized),
    database_state_prop_uri(State_Prop),

    db_name_uri(Name, Db_Uri),
    triple(Layer,Db_Uri,State_Prop,node(Finalized)).

/**
 * terminus_graph_layer(-Graph,-Layer) is det.
 *
 * Get the document graph for Terminus as a Layer
 */
terminus_graph_layer(Graph,Layer) :-
    storage(Store),
    terminus_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer).

/*
 * database_exists(DB_URI) is semidet.
 *
 */
database_exists(Name) :-
    terminus_graph_layer(_Graph,Layer),
    db_exists_in_layer(Layer,Name).

create_schema(DB_URI,Schema,DB) :-
    interpolate([DB_URI],Label),
    interpolate([Schema,' ontology for ',DB_URI],Comment),
    % goal expansion doesn't work here..
    global_prefix_expand(rdf:type, Rdf_type),
    global_prefix_expand(rdfs:label, Rdfs_label),
    global_prefix_expand(rdfs:comment, Rdfs_comment),
    global_prefix_expand(owl:'Ontology', OWL_ontology),
    insert(Schema, DB_URI, Rdf_type, OWL_ontology),
    insert(Schema, DB_URI, Rdfs_label, Label@en),
    insert(Schema, DB_URI, Rdfs_comment, Comment@en).

query_default_write_descriptor(Query_Object, Write_Descriptor) :-
    convlist([Obj,Graph_Desc]>>(
                 write_obj{ descriptor : Graph_Desc }:< Obj.descriptor,
                 Graph_Desc.name = "main"
             ),
             Query_Object.instance_write_objs,
             [Write_Descriptor]).

/*
 * database_instance(Transaction_Object, Instance_Read_Write_Objects) is det.
 *
 * DEPRECATED!
 * This is a compatibility predicate that returns the list of instances associated with a transaction_object
 */
database_instance(Transaction_Object, Instances) :-
    Instances = Transaction_Object.instance_objects.

/*
 * database_inference(Transaction_Object, Inference_Read_Write_Objects) is det.
 *
 * DEPRECATED!
 * This is a compatibility predicate that returns the list of inferences associated with a transaction_object
 */
database_inference(Transaction_Object, Inferences) :-
    Inferences = Transaction_Object.inference_objects.

/*
 * database_schema(Transaction_Object, Schema_Read_Write_Objects) is det.
 *
 * DEPRECATED!
 * This is a compatibility predicate that returns the list of schemas associated with a transaction_object
 */
database_schema(Transaction_Object, Schemas) :-
    Schemas = Transaction_Object.schema_objects.

error_on_pipe(Name) :-
    (   re_match('\\|', Name)
    ->  throw(error(syntax_error('This name has a pipe!',
                                 Name)))
    ;   true).

/**
 * user_database_name(User,DB,Name) is det.
 *
 */
user_database_name(User,DB,Name) :-
    freeze(User,error_on_pipe(User)),
    freeze(DB,error_on_pipe(DB)),
    merge_separator_split(Name,'|',[User,DB]).
