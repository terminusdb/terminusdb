:- module(system_entity, [
              database_exists/2,
              database_exists/3,
              db_uri_organization/3,
              organization_database_name_uri/4,
              organization_name_uri/3,
              organization_name_exists/2,
              database_finalized/3,
              user_name_uri/3,
              agent_name_uri/3,
              agent_name_exists/2,
              insert_db_object/6
          ]).

:- use_module(library(terminus_store)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(document)).

:- use_module(descriptor).

database_exists(Organization,DB) :-
    database_exists(system_descriptor{}, Organization, DB).

database_exists(Askable, Organization, DB) :-
    organization_database_name_uri(Askable, Organization, DB, _).

db_uri_organization(Askable, Db_Uri, Organization) :-
    once(ask(Askable,
             (
                 t(Organization_Uri, database, Db_Uri),
                 t(Organization_Uri, rdf:type, '@schema':'Organization'),
                 t(Organization_Uri, name, Organization^^xsd:string)
             ))).

organization_database_name_uri(Askable, Organization, DB, Db_Uri) :-
    once(ask(Askable,
             (
                 t(Organization_Uri, name, Organization^^xsd:string),
                 t(Organization_Uri, rdf:type, '@schema':'Organization'),
                 t(Organization_Uri, database, Db_Uri),
                 t(Db_Uri, name, DB^^xsd:string),
                 t(Db_Uri, rdf:type, '@schema':'UserDatabase')
             ))).


organization_name_uri(Askable,Organization, Uri) :-
    once(ask(Askable,
             (   t(Uri, name, Organization^^xsd:string),
                 t(Uri, rdf:type, '@schema':'Organization')
             ),
             [compress_prefixes(false)])).

organization_name_exists(Askable, Name) :-
    organization_name_uri(Askable, Name, _).

database_finalized(Askable,Organization,Database) :-
    organization_database_name_uri(Askable,Organization,Database,Db_Uri),
    once(ask(Askable,
             t(Db_Uri, state, '@schema':'DatabaseState_finalized'))).

user_name_uri(Askable, User_Name, Uri) :-
    once(ask(Askable,
             (   t(Uri, name, User_Name^^xsd:string),
                 t(Uri, rdf:type, '@schema':'User')))).

/*
 * agent_name_uri(Askable, Name, User_URI) is semidet.
 */
agent_name_uri(Askable, Name, User_URI) :-
    once(ask(Askable,
             (   t(User_URI, name, Name^^xsd:string),
                 t(User_URI, rdf:type, '@schema':'User')
             ),
             [compress_prefixes(false)]
            )).

agent_name_exists(Askable, Name) :-
    agent_name_uri(Askable, Name, _).

insert_db_object(System_Transaction, Organization_Name, Database_Name, Label, Comment, DB_Uri) :-
    organization_name_uri(System_Transaction, Organization_Name, Organization_Uri),
    current_xsd_date_time(Date),
    insert_document(
        System_Transaction,
        _{ '@type' : 'UserDatabase',
           'name' : Database_Name,
           'label' : Label,
           'state' : "creating",
           'creation_date' : Date,
           'comment' : Comment},
        DB_Uri),

    ask(System_Transaction,
        insert(Organization_Uri, database, DB_Uri)
       ).
