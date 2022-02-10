:- module(database_utils,[
              system_graph_layer/2,
              database_instance/2,
              database_inference/2,
              database_schema/2,
              organization_database_name/3,
              excluded_organization/1,
              excluded_database/1,
              error_on_excluded_organization/1,
              error_on_excluded_database/1
          ]).

/** <module> Database Utilities
 *
 * Various database level utilities. This is a layer above the triple store
 * in terms of logic, and placed here as we want to be able to make use
 * of WOQL and other libraries without circularity.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
:- reexport(core(util/syntax)).

:- use_module(triplestore).
:- use_module(constants).
:- use_module(literals, [object_storage/2]).

:- use_module(core(util)).

:- use_module(core(query/expansions)).
:- use_module(core(query/ask)).

:- use_module(core(transaction/database)).

:- use_module(library(apply)).
:- use_module(library(pcre)).
:- use_module(library(plunit)).
:- use_module(library(yall)).
:- use_module(library(lists)).

/**
 * system_graph_layer(-Graph,-Layer) is det.
 *
 * Get the document graph for Terminus as a Layer
 */
system_graph_layer(Graph,Layer) :-
    storage(Store),
    system_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer).

query_default_write_descriptor(Query_Object, Write_Descriptor) :-
    convlist([Obj,Graph_Desc]>>(
                 write_obj{ descriptor : Graph_Desc }:< Obj.descriptor,
                 Graph_Desc.name = "main"
             ),
             Query_Object.instance_write_objs,
             [Write_Descriptor]).

/*
 * database_instance(Transaction_Object, Instance_Read_Write_Objects) is det.
 *
 * DEPRECATED!
 * This is a compatibility predicate that returns the list of instances associated with a transaction_object
 */
database_instance(Transaction_Object, Instances) :-
    Instances = Transaction_Object.instance_objects.

/*
 * database_inference(Transaction_Object, Inference_Read_Write_Objects) is det.
 *
 * DEPRECATED!
 * This is a compatibility predicate that returns the list of inferences associated with a transaction_object
 */
database_inference(Transaction_Object, Inferences) :-
    Inferences = Transaction_Object.inference_objects.

/*
 * database_schema(Transaction_Object, Schema_Read_Write_Objects) is det.
 *
 * DEPRECATED!
 * This is a compatibility predicate that returns the list of schemas associated with a transaction_object
 */
database_schema(Transaction_Object, Schemas) :-
    Schemas = Transaction_Object.schema_objects.

excluded_organization(Organization) :-
    Organization == ''.

error_on_excluded_organization(Organization) :-
    die_if(excluded_organization(Organization),
           error(invalid_organization_name(Organization),_)).

excluded_database(DB) :-
    (   re_match('\\|', DB)
    ;   DB = '').

error_on_excluded_database(DB) :-
    die_if(excluded_database(DB),
           error(invalid_database_name(DB), _)).

/**
 * organization_database_name(User,DB,Name) is det.
 *
 */
organization_database_name(Organization, DB, Name) :-
    ground(Organization),
    !,
    error_on_excluded_organization(Organization),
    atom_concat(Organization, '|', Organization_Pipe),
    atom_concat(Organization_Pipe, DB, Name),
    error_on_excluded_database(DB).
organization_database_name(Organization, DB, Name) :-
    ground(DB),
    !,
    error_on_excluded_database(DB),
    atom_concat('|', DB, DB_Pipe),
    atom_concat(Organization, DB_Pipe, Name),
    error_on_excluded_organization(Organization).
organization_database_name(Organization, DB, Name) :-
    ground(Name),
    merge_separator_split(Name,'|',Elts),
    last(Elts, DB),
    atom_concat('|', DB, DB_Pipe),
    atom_concat(Organization, DB_Pipe, Name),
    error_on_excluded_organization(Organization),
    error_on_excluded_database(DB).

:- begin_tests(database_utils).
test(excluded_organization_pipe_names) :-
    \+ excluded_organization("john|smith"),
    \+ excluded_organization("|"),
    \+ excluded_organization("smith|"),
    \+ excluded_organization("|amy").

test(excluded_organization_normal_names) :-
    \+ excluded_organization("patrick"),
    \+ excluded_organization("john"),
    \+ excluded_organization("megacorp"),
    \+ excluded_organization("littlecorp"),
    \+ excluded_organization("jane").

test(merge_organization_db_name_properly) :-
    organization_database_name(foo, bar, 'foo|bar'),
    organization_database_name('foo|', bar, 'foo||bar'),
    organization_database_name('|f|o|o|', bar, '|f|o|o||bar').
test(split_organization_db_name_properly_normal) :-
    organization_database_name(O,D, 'foo|bar'),
    O = 'foo',
    D = 'bar'.
test(split_organization_db_name_properly_pipe) :-
    organization_database_name(O,D, '|foo||bar'),
    O = '|foo|',
    D = 'bar'.
test(split_organization_db_name_properly_db_ground) :-
    organization_database_name(O,'bar', '|foo||bar'),
    O = '|foo|'.
test(split_organization_db_name_properly_org_ground) :-
    organization_database_name('|foo|',D, '|foo||bar'),
    D = 'bar'.
test(db_name_pipe_error_merge,
    [error(invalid_database_name('|bar'))]) :-
    organization_database_name(foo, '|bar', _).
test(db_name_pipe_error_split,
    [error(invalid_database_name('|bar'))]) :-
    organization_database_name('foo', _, 'foo||bar').
test(db_name_pipe_error_split_db_ground,
    [error(invalid_database_name('|bar'))]) :-
    organization_database_name(_, '|bar', 'foo||bar').
test(db_name_empty_error_merge,
     [error(invalid_database_name(''))]) :-
    organization_database_name(foo, '', _).
test(db_name_empty_error_split,
     [error(invalid_database_name(''))]) :-
    organization_database_name(_, _, 'foo|').
test(db_name_empty_error_split_org_ground,
     [error(invalid_database_name(''))]) :-
    organization_database_name(foo, _, 'foo|').
test(org_name_empty_org_merge_error,
     [error(invalid_organization_name(''))]) :-
    organization_database_name('', 'bar', _).
test(org_name_excluded_org_split_error,
     [error(invalid_organization_name(''))]) :-
    organization_database_name(_,_, '|bar').
test(org_name_excluded_org_split_error_db_ground,
     [error(invalid_organization_name(''))]) :-
    organization_database_name(_,bar, '|bar').

:- end_tests(database_utils).
