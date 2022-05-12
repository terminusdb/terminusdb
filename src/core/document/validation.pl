:- module('document/validation', [
              refute_validation_objects/2
          ]).

:- use_module(core(util), [exists/2]).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(config(terminus_config), [ignore_ref_and_repo_schema/0]).
:- use_module(instance).
:- use_module(schema).

:- use_module(library(lists)).

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
    \+ is_schemaless(Validation_Object).

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
