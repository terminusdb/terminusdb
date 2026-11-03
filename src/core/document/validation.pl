:- module('document/validation', [
              refute_validation_objects/2
          ]).

:- use_module(core(util), [exists/2]).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core(document/migration), [operations_are_weakening/1]).
:- use_module(core(document/migration_dict), [migration_list_to_ast_list/2]).
:- use_module(config(terminus_config), [ignore_ref_and_repo_schema/0]).

:- use_module(instance).
:- use_module(schema).

:- use_module(library(lists)).
:- use_module(library(json)).

validation_triple_update(Object) :-
    get_dict(triple_update, Object, true).

/*
 * needs_schema_validation(Validation_Object) is det.
 *
 */
needs_schema_validation(Validation_Object) :-
    validation_object{
        schema_objects: Schema_Objects
    } :< Validation_Object,
    exists(validation_object_changed, Schema_Objects).

/*
 * needs_schema_instance_validation(Validation_Object) is det.
 *
 * Check to see if we need to do a blast-radius calculation
 * on the schema and renew instance checking on possibly impacted
 * triples.
 *
 * Currently just assumes we do if the schema changed.
 *
 */
needs_schema_instance_validation(Validation_Object) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects
    } :< Validation_Object,
    exists(validation_object_has_layer, Instance_Objects),
    exists(validation_object_changed, Schema_Objects),
    \+ is_schemaless(Validation_Object),
    \+ migration_is_weakening(Validation_Object),
    \+ (   config:trust_migrations,
           get_dict(commit_info, Validation_Object, CI),
           ground(CI),
           % existance of a migration should be proof
           % that we don't need to check
           get_dict(migration, CI, _Migration)
       ).

/*
 * migration_is_weakening(+Validation_Object) is semidet.
 *
 * True when the validation object has an inferred migration whose
 * operations are all weakening. Weakening operations (such as
 * replace_class_metadata and replace_class_documentation) have no
 * impact on existing instances, so instance validation can be
 * safely skipped.
 */
migration_is_weakening(Validation_Object) :-
    get_dict(commit_info, Validation_Object, CI),
    ground(CI),
    get_dict(migration, CI, Migration_Atom),
    atom_json_dict(Migration_Atom, Operations, [default_tag(json)]),
    is_list(Operations),
    migration_list_to_ast_list(Operations, Ast_Operations),
    operations_are_weakening(Ast_Operations).

/*
 * needs_local_instance_validation(Validation_Object) is det.
 *
 * Checks to see if we need to do some local instance validation.
 *
 */
needs_local_instance_validation(Validation_Object) :-
    validation_object{
        schema_objects : Schema_Objects,
        instance_objects : Instance_Objects
    } :< Validation_Object,
    Schema_Objects \= [],
    exists(validation_object_changed, Instance_Objects),
    \+ is_schemaless(Validation_Object).

/*
 * needs_referential_integrity_validation(Validation_Object) is det.
 *
 * Checks to see if we only need to check integrity of links
 *
 */
only_needs_referential_integrity_validation(Validation_Object) :-
    validation_object{
        schema_objects : Schema_Objects,
        instance_objects : Instance_Objects
    } :< Validation_Object,
    Schema_Objects \= [],
    exists(validation_object_changed, Instance_Objects),
    \+ exists(validation_triple_update, Instance_Objects),
    \+ is_schemaless(Validation_Object).

/*
 * refute(+Validation:validation_obj, -Witness) is nondet.
 *
 * We are for the moment going to do validation on all objects
 * regardless of level in the tier of our hierarchy. Since higher level objects
 * are written only by us, and the schemata are never written we can presumably
 * get away with dispensing with schema and instance checking. However in
 * early phases, it's probably best if we leave it in so we can be confident
 * we are not writing nonsense!
 *
 * As a feature, the environment variable TERMINUSDB_IGNORE_REF_AND_REPO_SCHEMA
 * can be set to true to skip validation of the ref and repo graph.
 *
 * There is no current way to do inference validation. We are not allowing updates
 * so hopefully this is ok!
 */
refute(Validation_Object, _Witness) :-
    ignore_ref_and_repo_schema,
    is_dict((Validation_Object.descriptor), Type),
    memberchk(Type, [repository_descriptor, database_descriptor]),
    !,
    fail.
refute(Validation_Object, Witness) :-
    needs_schema_validation(Validation_Object),
    refute_schema(Validation_Object, Witness),
    % Do not proceed if we have a broken schema
    !.
refute(Validation_Object, Witness) :-
    needs_schema_instance_validation(Validation_Object),
    !,
    refute_instance_schema(Validation_Object, Witness).
refute(Validation_Object, Witness) :-
    only_needs_referential_integrity_validation(Validation_Object),
    !,
    refute_referential_integrity(Validation_Object,Witness).
refute(Validation_Object, Witness) :-
    needs_local_instance_validation(Validation_Object),
    refute_instance(Validation_Object,Witness).

/*
 * refute_validation_objects(Validation_Objects, Witness) is nondet.
 *
 * Find all refutations of the given validation object.
 */
refute_validation_objects(Validation_Objects, Witness) :-
    member(Validation_Object, Validation_Objects),
    refute(Validation_Object, Witness).

:- begin_tests(metadata_only_schema_change).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core(account)).
:- use_module(core(document)).
:- use_module(core(triple)).
:- use_module(core(document/migration), [infer_weakening_migration/3]).

metadata_test_schema('
{ "@base": "http://somewhere.for.now/document/",
  "@schema": "http://somewhere.for.now/schema#",
  "@type": "@context"
}

{ "@id" : "Thing",
  "@type" : "Class",
  "@metadata" : { "embedding" : { "graphql" : "query { Thing { name } }" } },
  "@documentation" : { "@title" : "Thing", "@description" : "A thing" },
  "name" : "xsd:string"
}
').

structural_test_schema('
{ "@base": "http://somewhere.for.now/document/",
  "@schema": "http://somewhere.for.now/schema#",
  "@type": "@context"
}

{ "@id" : "Thing",
  "@type" : "Class",
  "name" : "xsd:string",
  "age" : "xsd:integer"
}
').

test(metadata_only_change_skips_instance_validation,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "test"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/test", Desc),

    % Write initial schema with metadata
    write_schema(metadata_test_schema, Desc),

    % Insert an instance document
    with_test_transaction(Desc, C1,
        insert_document(C1, _{'@type': "Thing", name: "foo"}, _Id),
        _),

    % Get the current schema document
    get_schema_document(Desc, 'Thing', ThingDoc),

    % Modify only metadata
    UpdatedDoc = (ThingDoc.put('@metadata',
                               _{embedding: _{graphql: "query { Thing { name id } }"}})),

    % Create a new transaction context and replace schema document
    create_context(Desc, commit_info{author: "test", message: "update metadata"}, C2),
    replace_schema_document(C2, UpdatedDoc),

    % Build validation objects from the uncommitted transaction
    query_context_transaction_objects(C2, Transactions),
    transaction_objects_to_validation_objects(Transactions, Validation_Objects0),
    % Run migration inference as run_transactions would.
    % If weakening inference fails, no migration is set and instance
    % validation proceeds normally.
    (   infer_weakening_migration(Validation_Objects0, Validation_Objects, _)
    ->  true
    ;   Validation_Objects = Validation_Objects0
    ),
    member(Validation_Object, Validation_Objects),
    needs_schema_validation(Validation_Object),
    !,
    \+ needs_schema_instance_validation(Validation_Object).

test(structural_change_requires_instance_validation,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "test"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/test", Desc),

    % Write initial schema
    write_schema(metadata_test_schema, Desc),

    % Insert an instance document
    with_test_transaction(Desc, C1,
        insert_document(C1, _{'@type': "Thing", name: "foo"}, _Id),
        _),

    % Get the current schema document and add a structural field
    get_schema_document(Desc, 'Thing', ThingDoc),
    UpdatedDoc = (ThingDoc.put(age, "xsd:integer")),

    % Create a new transaction context and replace schema document
    create_context(Desc, commit_info{author: "test", message: "add field"}, C2),
    replace_schema_document(C2, UpdatedDoc),

    % Build validation objects from the uncommitted transaction
    query_context_transaction_objects(C2, Transactions),
    transaction_objects_to_validation_objects(Transactions, Validation_Objects0),
    % Run migration inference as run_transactions would.
    % Adding a required (non-optional) property is not a weakening,
    % so inference fails and no migration is set.
    (   infer_weakening_migration(Validation_Objects0, Validation_Objects, _)
    ->  true
    ;   Validation_Objects = Validation_Objects0
    ),
    member(Validation_Object, Validation_Objects),
    needs_schema_validation(Validation_Object),
    !,
    needs_schema_instance_validation(Validation_Object).

test(context_metadata_only_change_skips_instance_validation,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "test"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/test", Desc),

    write_schema(metadata_test_schema, Desc),

    with_test_transaction(Desc, C1,
        insert_document(C1, _{'@type': "Thing", name: "foo"}, _Id),
        _),

    get_schema_document(Desc, '@context', ContextDoc),
    UpdatedDoc = (ContextDoc.put('@metadata',
                                 _{terminusdb: _{options: ["store_clustering"]}})),

    create_context(Desc, commit_info{author: "test", message: "update context metadata"}, C2),
    replace_schema_document(C2, UpdatedDoc),

    query_context_transaction_objects(C2, Transactions),
    transaction_objects_to_validation_objects(Transactions, Validation_Objects0),
    (   infer_weakening_migration(Validation_Objects0, Validation_Objects, _)
    ->  true
    ;   Validation_Objects = Validation_Objects0
    ),
    member(Validation_Object, Validation_Objects),
    needs_schema_validation(Validation_Object),
    !,
    \+ needs_schema_instance_validation(Validation_Object).

test(context_base_change_requires_instance_validation,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "test"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/test", Desc),

    write_schema(metadata_test_schema, Desc),

    with_test_transaction(Desc, C1,
        insert_document(C1, _{'@type': "Thing", name: "foo"}, _Id),
        _),

    get_schema_document(Desc, '@context', ContextDoc),
    UpdatedDoc = (ContextDoc.put('@base', "http://changed/")),

    create_context(Desc, commit_info{author: "test", message: "change base"}, C2),
    replace_schema_document(C2, UpdatedDoc),

    query_context_transaction_objects(C2, Transactions),
    transaction_objects_to_validation_objects(Transactions, Validation_Objects0),
    (   infer_weakening_migration(Validation_Objects0, Validation_Objects, _)
    ->  true
    ;   Validation_Objects = Validation_Objects0
    ),
    member(Validation_Object, Validation_Objects),
    needs_schema_validation(Validation_Object),
    !,
    needs_schema_instance_validation(Validation_Object).

:- end_tests(metadata_only_schema_change).
