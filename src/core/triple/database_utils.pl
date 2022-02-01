:- module(database_utils,[
              system_graph_layer/2,
              database_instance/2,
              database_inference/2,
              database_schema/2,
              organization_database_name/3
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
    re_match('^(console|api|db|home|profile)$', Organization).

error_on_excluded_organization(Organization) :-
    do_or_die(\+ excluded_organization(Organization),
              error(invalid_organization_name(Organization),_)).

error_on_pipe(Name) :-
    do_or_die(\+ re_match('\\|', Name),
              error(invalid_database_name(Name), _)).

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
    error_on_pipe(DB).
organization_database_name(Organization, DB, Name) :-
    ground(DB),
    !,
    error_on_pipe(DB),
    atom_concat('|', DB, DB_Pipe),
    atom_concat(Organization, DB_Pipe, Name),
    error_on_excluded_organization(Organization),
    error_on_pipe(DB).
organization_database_name(Organization, DB, Name) :-
    ground(Name),
    merge_separator_split(Name,'|',Elts),
    last(Elts, DB),
    atom_concat('|', DB, DB_Pipe),
    atom_concat(Organization, DB_Pipe, Name),
    error_on_excluded_organization(Organization),
    error_on_pipe(DB).

:- begin_tests(database_utils).

test(excluded_organization_bad_names) :-
    excluded_organization("console"),
    excluded_organization("api"),
    excluded_organization("db"),
    excluded_organization("home"),
    excluded_organization("profile").

test(excluded_organization_pipe_names) :-
    \+ excluded_organization("john|smith"),
    \+ excluded_organization("|"),
    \+ excluded_organization("smith|"),
    \+ excluded_organization("|amy").


test(excluded_organization_good_names) :-
    \+ excluded_organization("consolez"),
    \+ excluded_organization("zconsole"),
    \+ excluded_organization("adba"),
    \+ excluded_organization("dba"),
    \+ excluded_organization("adb").


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
test(org_name_excluded_org_merge_error,
     [error(invalid_organization_name('profile'))]) :-
    organization_database_name(profile, 'bar', _).
test(org_name_excluded_org_split_error,
     [error(invalid_organization_name('profile'))]) :-
    organization_database_name(_,_, 'profile|bar').
test(org_name_excluded_org_split_error_db_ground,
     [error(invalid_organization_name('profile'))]) :-
    organization_database_name(_,bar, 'profile|bar').

:- end_tests(database_utils).
