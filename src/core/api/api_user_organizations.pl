:- module(api_user_organizations, [
              user_organizations/3
          ]).

:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(document)).

:- use_module(library(plunit)).

user_organizations(System_DB, Auth, Result) :-
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
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_branch).
:- use_module(library(http/json)).

test(retrieve_organization_databases,
     [setup((setup_temp_store(State),
             add_user("TERMINUSQA",some('password'),Auth),
             create_db_without_schema("TERMINUSQA",foo))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    open_descriptor(system_descriptor{}, System_DB),
    user_organizations(System_DB, Auth, Results),

    Results = [ _{ '@id':'Organization/TERMINUSQA',
                   '@type':'Organization',
                   databases:[ json{ '@id':_,
                                     '@type':'UserDatabase',
                                     comment:"a test db",
                                     creation_date:_,
                                     label:"test",
		                     name:"foo",
		                     state:finalized
		                   }
	                     ],
                   name:"TERMINUSQA"
                 }
              ].
:- end_tests(user_organization).
