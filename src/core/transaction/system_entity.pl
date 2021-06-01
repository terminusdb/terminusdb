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

:- use_module(descriptor).

database_exists(Organization,DB) :-
    database_exists(system_descriptor{}, Organization, DB).

database_exists(Askable, Organization, DB) :-
    organization_database_name_uri(Askable, Organization, DB, _).

db_uri_organization(Askable, Db_Uri, Organization) :-
    once(ask(Askable,
             (
                 t(Organization_Uri, system:resource_includes, Db_Uri),
                 t(Organization_Uri, rdf:type, system:'Organization'),
                 t(Organization_Uri, system:resource_name, Organization^^xsd:string)
             ))).

organization_database_name_uri(Askable, Organization, DB, Db_Uri) :-
    once(ask(Askable,
             (
                 t(Organization_Uri, system:resource_name, Organization^^xsd:string),
                 t(Organization_Uri, rdf:type, system:'Organization'),
                 t(Organization_Uri, system:resource_includes, Db_Uri),
                 t(Db_Uri, system:resource_name, DB^^xsd:string),
                 t(Db_Uri, rdf:type, system:'Database')
             ))).


organization_name_uri(Askable,Organization, Uri) :-
    once(ask(Askable,
             (   t(Uri, '@schema':name, Organization^^xsd:string),
                 t(Uri, rdf:type, '@schema':'Organization')
             ))).

organization_name_exists(Askable, Name) :-
    organization_name_uri(Askable, Name, _).

database_finalized(Askable,Organization,Database) :-
    organization_database_name_uri(Askable,Organization,Database,Db_Uri),
    once(ask(Askable,
             t(Db_Uri, system:database_state, system:finalized))).

user_name_uri(Askable, User_Name, Uri) :-
    once(ask(Askable,
             (   t(Uri, system:agent_name, User_Name^^xsd:string),
                 t(Uri, rdf:type, system:'User')))).

/*
 * agent_name_uri(Askable, Name, User_URI) is semidet.
 */
agent_name_uri(Askable, Name, User_URI) :-
    once(ask(Askable,
             t(User_URI, system:agent_name, Name^^xsd:string),
             [compress_prefixes(false)]
            )).

agent_name_exists(Askable, Name) :-
    agent_name_uri(Askable, Name, _).

insert_db_object(System_Transaction, Organization_Name, Database_Name, Label, Comment, DB_Uri) :-
    ask(System_Transaction,
        (
            t(Organization_Uri, system:organization_name, Organization_Name^^xsd:string),
            random_idgen(doc:'Database', [Organization_Name^^xsd:string, Database_Name^^xsd:string], DB_Uri),
            insert(DB_Uri, rdf:type, system:'Database'),
            insert(DB_Uri, system:resource_name, Database_Name^^xsd:string),
            insert(DB_Uri, rdfs:label, Label@en),
            insert(DB_Uri, rdfs:comment, Comment@en),

            insert(Organization_Uri, system:organization_database, DB_Uri)
        )).
