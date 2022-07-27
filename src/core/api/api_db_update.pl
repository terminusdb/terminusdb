:- module(api_db_update, [
              api_db_update/6
          ]).

/** <module> Module for updating fields and metadata about databases
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).

:- use_module(core(api/db_create)).

:- use_module(library(lists)).
:- use_module(library(plunit)).


api_db_update(System_DB, Organization, Database, Auth, Commit_Info, Updates) :-
    create_context(System_DB, System_Context),

    do_or_die(
        organization_name_uri(System_Context, Organization, _),
        error(unknown_organization(Organization),_)),

    do_or_die(
        organization_database_name_uri(System_Context,Organization,Database,Db_Uri),
        error(unknown_database(Organization,Database), _)),

    (   get_dict(prefixes, Updates, Prefixes)
    ->  validate_prefixes(Prefixes)
    ;   Prefixes = none
    ),

    format(atom(Path), "~s/~s", [Organization, Database]),
    resolve_absolute_string_descriptor(Path, Descriptor),
    create_context(Descriptor, Commit_Info, DB_Context),

    assert_auth_action_scope(System_Context, Auth, '@schema':'Action/manage_capabilities', Db_Uri),

    with_transaction(
        System_Context,
        (
            ask(System_Context,
                get_document(Db_Uri, DB_Record)),

            dict_field_verifier(
                Updates,
                _{ '@id' : (*),
                   name : atom,
                   label : atom,
                   comment : atom
                 },
                Clean_Updates),

            put_dict(Clean_Updates, DB_Record, Updated_DB_Record),

            replace_document(
                System_Context,
                Updated_DB_Record),

            dict_field_verifier(
                Updates,
                _{ public : [X]>>( memberchk(X, [true,false])) },
                Public
            ),

            (   get_dict(public, Public, true)
            ->  make_db_public(System_Context, Db_Uri)
            ;   get_dict(public, Public, false)
            ->  make_db_private(System_Context, Db_Uri)
            ;   true
            )
        ),
        _),

    % Two phase non-transactional is a bit dubious
    % I wonder if we should set the db status flag.
    % failure half way seems farily low cost though...
    with_transaction(
        DB_Context,
        (   (   get_dict(schema, Updates, false)
            ->  ask(DB_Context,
                    insert('terminusdb://data/Schema', rdf:type, rdf:nil, schema))
            ;   get_dict(schema, Updates, true)
            ->  ask(DB_Context,
                    delete('terminusdb://data/Schema', rdf:type, rdf:nil, schema))
            ;   true
            ),

            (   Prefixes = none
            ->  true
            ;   put_dict(_{'@type' : "@context"}, Prefixes, Prefix_Obj),
                insert_context_document(DB_Context, Prefix_Obj)
            )
        ),
        _
    ).

