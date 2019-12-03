:- module(database_utils,[
              create_db/2,
              post_create_db/1,
              delete_db/1,
              database_exists/1,
              extend_database_defaults/3
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

/*
 * database_exists(DB_URI) is semidet.
 */
database_exists(DB_URI) :-
    database_name_list(Databases),
    memberchk(DB_URI,Databases).

/**
 * create_db(+DB:atom) is semidet.
 *
 * Create a new empty graph
 */
create_db(Terminus_DB,DB_URI) :-

    initialise_prefix_db(DB_URI),
    storage(Store),

    % Set up schemata
    forall(
        (
            % If none, we succeed... (headless)
            database_record_schema_list(Terminus_DB, DB_URI,Schemata),
            member(Schema,Schemata)
        ),
        safe_create_named_graph(Store,Schema,_)
    ),

    % Set up instance graphs
    forall(
        (
            % If none, we succeed... (legless)
            database_record_instance_list(Terminus_DB, DB_URI,Instances),
            member(Instance,Instances)
        ),
        safe_create_named_graph(Store,Instance,_)
    ).

create_schema(DB_URI,Schema,DB) :-
    interpolate([DB_URI],Label),
    interpolate([Schema,' ontology for ',DB_URI],Comment),
    % goal expansion doesn't work here..
    global_prefix_expand(rdf:type, Rdf_type),
    global_prefix_expand(rdfs:label, Rdfs_label),
    global_prefix_expand(rdfs:comment, Rdfs_comment),
    global_prefix_expand(owl:'Ontology', OWL_ontology),
    insert(DB, Schema, DB_URI, Rdf_type, OWL_ontology),
    insert(DB, Schema, DB_URI, Rdfs_label, literal(lang(en,Label))),
    insert(DB, Schema, DB_URI, Rdfs_comment, literal(lang(en,Comment))).

post_create_db(DB_URI) :-
    make_database_from_database_name(DB_URI, Database),
    Schemata = Database.schema,
    with_transaction(
        [transaction_record{
             pre_database: Database,
             write_graphs: Schemata,
             update_database: Update_DB,
             post_database: _Post_DB},
         witnesses(Witnesses)],
        forall(member(Schema,Schemata),
               create_schema(DB_URI,Schema,Update_DB)),
        % always an ok update...
        true
    ),
    % Succeed only if the witnesses are empty.
    Witnesses = [].

% evil much? Unlikely to be safe.
delete_db(DB) :-
    db_path(Path),
    www_form_encode(DB,DBID),
    interpolate(['^(?<name>',DBID,'.*).label$'],Pattern),
    files(Path,Files),
    forall(
        (   member(File,Files),
            re_matchsub(Pattern,File,Sub,[])
        ),
        (
            interpolate([Path,File],QFile),
            get_time(Time),
            interpolate([Path,Sub.name,'-',Time,'.deleted'],Deleted_File),
            rename_file(QFile,Deleted_File)
        )
    ).

/*
 * should probably go in JSON-LD
 */
add_dictionary_default(Doc, Key, Default, New_Doc) :-
    % hairy logic - can this be simplified?
    (   get_dict(Key, Doc, Result)
    ->  (   is_list(Result)
        ->  (   member(Res, Result),
                select_dict(Res, Default, _Rest)
            ->  Doc = New_Doc
            ;   put_dict(Key, Doc, [Default|Result], New_Doc)
            )
        ;   (   select_dict(Result, Default, _Rest)
            ->  Doc = New_Doc
            ;   put_dict(Key, Doc, [Default,Result], New_Doc)))
    ;   put_dict(Key, Doc, Default, New_Doc)
    ).

extend_database_defaults(URI,Doc,Ext) :-
    format(string(Document),'~s~s',([URI,'/document'])),
    add_dictionary_default(Doc, 'http://terminusdb.com/schema/terminus#instance',
                           _{'@value':Document, '@type':'http://www.w3.org/2001/XMLSchema#string'},
                           Doc1),
    format(string(Schema),'~s~s',([URI,'/schema'])),
    add_dictionary_default(Doc1, 'http://terminusdb.com/schema/terminus#schema',
                           _{'@value':Schema, '@type':'http://www.w3.org/2001/XMLSchema#string'},
                           Ext).
