:- module(api_user_organizations, [
              user_organizations/3
          ]).

:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(document)).

user_organizations(System_DB, Auth, Result) :-
    % TODO, need auth check.
    findall(
        _{ '@type' : 'Organization',
           '@id' : Organization,
           name : Organization_Name,
           databases : Databases},
        (   ask(System_DB,
                (
                    t(Auth, capability, Capability),
                    t(Capability, scope, Organization),
                    t(Organization, rdf:type, '@schema':'Organization'),
                    t(Organization, name, Organization_Name_Type)
                )
               ),
            Organization_Name_Type = Organization_Name^^_,
            findall(
                Database,
                (
                    ask(System_DB,
                        t(Organization, database, Database_URI)),
                    get_document(System_DB, Database_URI, Database)

                ),
                Databases
            )
        ),
        Result
    ).

:- begin_tests(user_organization).
:- use_module(core(util/test_utils)).
:- use_module(db_branch).
:- use_module(library(http/json)).

test(context_repository_head_pack,
     [setup((setup_temp_store(State),
             add_user("TERMINUSQA",some('password'),Auth),
             create_db_without_schema("TERMINUSQA",foo))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    open_descriptor(system_descriptor{}, System_DB),
    user_organizations(System_DB, Auth, Results),

    with_output_to(atom(Atom),
                   json_write_dict(current_output, Results, [width(0)])),
    Atom = '[ {"@id":"Organization/TERMINUSQA", "@type":"Organization", "databases": [ {"@id":"UserDatabase/6ccca3d196c28464816aeec04f21264ecbe2c24debc4340d782a7e196c325a93", "@type":"UserDatabase", "comment":"a test db", "creation_date":"2021-10-05T13:23:10.226Z", "label":"test", "name":"foo", "state":"finalized"} ], "name":"TERMINUSQA"} ]'.

:- end_tests(user_organization).
