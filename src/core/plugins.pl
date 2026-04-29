:- module(plugins, [
              pre_commit_hook/2,
              post_commit_hook/2,
              enrich_commit_info/3,
              fast_document_history/6,
              fast_document_history_entries/5,
              pre_server_startup_hook/1,
              post_server_startup_hook/1,
              enrich_history/5,
              enrich_history/6,
              load_plugins/0
          ]).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- use_module(library(assoc)).
:- use_module(library(terminus_store)).
:- use_module(config(terminus_config)).

:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(document/schema), [
      schema_is_shared/2
  ]).
:- use_module(core(document/instance), [
      instance_of/3
  ]).
:- use_module(core(document/json), [
      delete_document/2
  ]).
:- use_module(core(util)).

:- multifile pre_commit_hook/2.
:- multifile post_commit_hook/2.
:- multifile enrich_commit_info/3.
:- multifile fast_document_history/6.
:- multifile fast_document_history_entries/5.
:- multifile enrich_history/5.
:- multifile enrich_history/6.
:- multifile pre_server_startup_hook/1.
:- multifile post_server_startup_hook/1.

load_plugins :-
    plugin_path(Path),
    exists_directory(Path),
    !,
    directory_files(Path, Files),
    forall((member(File, Files),
            file_name_extension(_, '.pl', File)),

           (   directory_file_path(Path, File, Full_Path),
               load_files(Full_Path, [if(not_loaded), must_be_module(false)]))).
load_plugins.

%%% ====================================================================
%%% @shared cascade delete hook
%%%
%%% Fires before commit. For each removed triple whose object is a
%%% @shared target, checks whether the target has zero remaining
%%% references (from ANY source, ANY predicate). If so, cascade-deletes
%%% it. Recurses: the cascade-deleted document may orphan further
%%% @shared targets.
%%%
%%% The hook mutates validation objects in-place (via nb_set_dict) when
%%% cascade deletes are needed. nb_set_dict modifications are
%%% non-backtrackable and persist even when the hook fails (which is
%%% the success case — no witness means success).
%%%
%%% Termination guarantee:
%%%   - cascade_shared_deletes uses a Visited set to avoid re-deleting
%%%     the same document within one iteration
%%%   - is_shared_target_live uses a Visited set to detect circular
%%%     references during liveness checks
%%%   - Between iterations, the layer is mutated so deleted documents
%%%     no longer appear in xrdf queries — has_shared_orphans will not
%%%     rediscover them
%%% ====================================================================

%% pre_commit_hook(+Validations, -Witness)
%
%  Entry point for @shared cascade delete.
%  On success (cascade completed), performs mutations and FAILS (no witness).
%  On error, succeeds with a witness.
pre_commit_hook(Validations, Witness) :-
    member(Validation, Validations),
    has_shared_orphans(Validation),
    !,
    perform_shared_cascade(Validation, Result),
    Result = error(Witness).

%% has_shared_orphans(+Validation)
%
%  True if the validation has any removed triples pointing to @shared
%  instances that are no longer reachable from non-@shared documents.
%  Uses the full reachability check for correctness (handles circular refs).
has_shared_orphans(Validation) :-
    empty_assoc(Empty),
    find_orphaned_shared_target(Validation, Empty, _Target).

%% perform_shared_cascade(+Validation, -Result)
%
%  Performs the cascade delete loop using Rust fast path.
%  The Rust predicate '$doc':cascade_shared/3 finds all orphaned @shared
%  instances in a single pass and batch-deletes them.
%  Iterates until no more orphans are found (Count = 0).
perform_shared_cascade(Validation, Result) :-
    % Convert validation to transaction (gets a fresh builder)
    validation_objects_to_transaction_objects([Validation], [Transaction]),
    % Get document context from the transaction (reflects current committed layer)
    '$doc':get_document_context(Transaction, Context),
    % Ensure the transaction has an instance builder
    ensure_transaction_has_builder(instance, Transaction),
    % Call Rust fast path: finds and deletes all orphans in one pass
    (   catch(
            '$doc':cascade_shared(Context, Transaction, Count),
            Error,
            (   json_log_warning_formatted(
                    'Rust cascade_shared failed: ~q', [Error]),
                fail
            )
        )
    ->  (   Count > 0
        ->  % Commit the transaction builder to get new layers
            transaction_objects_to_validation_objects([Transaction], [New_Validation]),
            % Update the original validation object's instance layer in place
            update_validation_instance_layer(Validation, New_Validation),
            % Iterate: deleting @shared docs may orphan further @shared targets
            perform_shared_cascade(Validation, Result)
        ;   % Count = 0: no orphans found, cascade complete
            Result = ok
        )
    ;   % Rust call failed — fall back to Prolog cascade
        perform_shared_cascade_prolog(Validation, Result)
    ).

%% perform_shared_cascade_prolog(+Validation, -Result)
%
%  Fallback: pure Prolog cascade (used if Rust predicate is unavailable).
perform_shared_cascade_prolog(Validation, Result) :-
    validation_objects_to_transaction_objects([Validation], [Transaction]),
    empty_assoc(Empty_Visited),
    cascade_shared_deletes(Transaction, Validation, Empty_Visited, Cascade_Result),
    (   Cascade_Result = done
    ->  transaction_objects_to_validation_objects([Transaction], [New_Validation]),
        update_validation_instance_layer(Validation, New_Validation),
        (   has_shared_orphans(Validation)
        ->  perform_shared_cascade_prolog(Validation, Result)
        ;   Result = ok
        )
    ;   Result = error(Cascade_Result)
    ).

%% cascade_shared_deletes(+Transaction, +Validation, +Visited, -Result)
%
%  Find all orphaned @shared targets in this iteration and delete them.
%  Uses the Validation for reading (schema lookups, liveness checks)
%  and Transaction for writing (delete_document).
%  Visited is an assoc map for O(log n) membership checks.
%  Result is 'done' on success, or a witness dict on error.
cascade_shared_deletes(Transaction, Validation, Visited, Result) :-
    (   find_orphaned_shared_target(Validation, Visited, Target)
    ->  (   catch(
                delete_document(Transaction, Target),
                Error,
                (   json_log_warning_formatted(
                        'Shared cascade delete failed for ~q: ~q', [Target, Error]),
                    fail
                )
            )
        ->  put_assoc(Target, Visited, true, New_Visited),
            cascade_shared_deletes(Transaction, Validation, New_Visited, Result)
        ;   Result = witness{
                '@type': shared_cascade_delete_failed,
                target: Target
            }
        )
    ;   Result = done
    ).

%% find_orphaned_shared_target(+Validation, +Visited, -Target)
%
%  Find a @shared target that has lost references and is no longer
%  reachable from any non-@shared document.
%  Deterministic — returns the first match.
%  Visited is an assoc map for O(log n) membership checks.
find_orphaned_shared_target(Validation, Visited, Target) :-
    database_instance(Validation, Instance),
    database_schema(Validation, Schema),
    % Enumerate removed triples whose object is an atom (node IRI)
    xrdf_deleted(Instance, _S, _P, Target),
    atom(Target),
    % Exclude already-visited (already deleted) targets
    \+ get_assoc(Target, Visited, _),
    % Check the target still has a type triple (not already deleted)
    xrdf(Instance, Target, rdf:type, Target_Class),
    % Check the target is a @shared class instance
    schema_is_shared(Schema, Target_Class),
    % Liveness check: target is NOT reachable from any non-@shared document
    empty_assoc(Empty_Live_Visited),
    \+ is_shared_target_live(Validation, Target, Empty_Live_Visited),
    !.

%% is_shared_target_live(+Validation, +Target, +Visited)
%
%  True if Target (a @shared instance) is reachable from at least one
%  non-@shared document. Handles circular references via Visited assoc.
%  A @shared target is live if:
%    - Any non-@shared document directly references it, OR
%    - A @shared document that is itself live references it.
is_shared_target_live(Validation, Target, Visited) :-
    database_instance(Validation, Instance),
    database_schema(Validation, Schema),
    % Find a triple pointing to Target
    xrdf(Instance, Source, _P, Target),
    % Source must not be Target itself and not already visited
    Source \= Target,
    \+ get_assoc(Source, Visited, _),
    % Check: is Source a @shared instance?
    (   xrdf(Instance, Source, rdf:type, Source_Class),
        schema_is_shared(Schema, Source_Class)
    ->  % Source is @shared — recurse: is Source itself reachable from non-@shared?
        put_assoc(Target, Visited, true, New_Visited),
        is_shared_target_live(Validation, Source, New_Visited)
    ;   % Source is NOT @shared (or has no type = was deleted) — Target is live!
        true
    ).

%% update_validation_instance_layer(+OldValidation, +NewValidation)
%
%  Mutably updates the instance layer in the old validation object
%  with the new committed layer from the new validation object.
update_validation_instance_layer(OldValidation, NewValidation) :-
    OldValidation.instance_objects = [Old_Instance_Obj|_],
    NewValidation.instance_objects = [New_Instance_Obj|_],
    New_Layer = New_Instance_Obj.read,
    nb_set_dict(read, Old_Instance_Obj, New_Layer),
    nb_set_dict(changed, Old_Instance_Obj, true).
