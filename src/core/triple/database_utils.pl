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
    re_match('\\||^(console|api|db|home|profile)$', Organization).

error_on_excluded_organization(Organization) :-
    do_or_die(\+ excluded_organization(Organization),
              error(invalid_organization_name(Organization))).

error_on_pipe(Name) :-
    do_or_die(\+ re_match('\\|', Name),
              error(ceci_n_est_pas_une_pipe(Name),_)).

/**
 * organization_database_name(User,DB,Name) is det.
 *
 */
organization_database_name(Organization,DB,Name) :-
    freeze(Organization,error_on_excluded_organization(Organization)),
    freeze(DB,error_on_pipe(DB)),
    merge_separator_split(Name,'|',[Organization,DB]).

:- begin_tests(database_utils).

test(excluded_organization_bad_names) :-
    excluded_organization("console"),
    excluded_organization("api"),
    excluded_organization("db"),
    excluded_organization("home"),
    excluded_organization("profile"),
    excluded_organization("john|smith"),
    excluded_organization("|"),
    excluded_organization("smith|"),
    excluded_organization("|amy").


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

:- end_tests(database_utils).
