:- module(system_entity, [
              database_exists/2,
              database_exists/3,
              organization_database_name_uri/4,
              organization_name_uri/3,
              organization_name_exists/2,
              database_finalized/3
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

organization_database_name_uri(Askable, Organization, DB, Db_Uri) :-
    once(ask(Askable,
             (   t(Organization_Uri, system:resource_name, Organization^^xsd:string),
                 t(Organization_Uri, rdf:type, system:'Organization'),
                 t(Organization_Uri, system:resource_includes, Db_Uri),
                 t(Db_Uri, system:resource_name, DB^^xsd:string),
                 t(Db_Uri, rdf:type, system:'Database')
             ))).

organization_name_uri(Askable,Organization, Uri) :-
    once(ask(Askable,
             (   t(Uri, system:organization_name, Organization^^xsd:string),
                 t(Uri, rdf:type, system:'Organization')
             ))).

organization_name_exists(Askable, Name) :-
    organization_name_uri(Askable, Name, _).

database_finalized(Askable,Organization,Database) :-
    organization_database_name_uri(Askable,Organization,Database,Db_Uri),
    once(ask(Askable,
             t(Db_Uri, system:database_state, system:finalized))).

