:- module(ask,[
              ask/2,
              ask/3,
              ask_ast/3,
              askable_prefixes/2,
              is_query_context/1,
              create_context/2,
              create_context/3,
              askable_context/4,
              askable_context/5,
              askable_settings_context/3,
              context_to_parent_transaction/2,
              context_extend_prefixes/3,
              context_default_prefixes/2,
              query_default_collection/2,
              query_default_write_graph/2,
              query_default_schema_write_graph/2
          ]).

/** <module> Ask
 *
 * Prolog interface to ask queries
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(woql_compile).
:- use_module(global_prefixes).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

:- use_module(core(document), [database_prefixes/2]).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(gensym)).

prefix_preterm(Ctx, Woql_Var, Pre_Term) :-
    freeze(Woql_Var,
           (   is_dict(Woql_Var) % Document
           ->  Woql_Var = Pre_Term
           ;   Woql_Var = [_|_]
           ->  maplist(prefix_preterm(Ctx),Woql_Var,Pre_Term)
           ;   number(Woql_Var)
           ->  Woql_Var = Pre_Term
           ;   Woql_Var =.. [date|_]
           ->  Woql_Var = Pre_Term
           ;   Woql_Var = _@_
           ->  Pre_Term = Woql_Var
           ;   Woql_Var = Elt^^Type
           ->  freeze(Type,
                      (   uri_to_prefixed(Type,Ctx,Prefixed_Type),
                          Pre_Term = Elt^^Prefixed_Type))
           ;   uri_to_prefixed(Woql_Var,Ctx,Pre_Term))).

term_var_to_binding(object, Ctx, Options, Pre_Term, Term, Bindings_In, Bindings_Out) :-
    % A special case where we're querying for an object knowing either the type or the object but maybe not both.
    nonvar(Pre_Term),
    Pre_Term = Pre_Value_Term^^Pre_Type_Term,
    !,
    (   var(Pre_Value_Term)
    ->  (   lookup_backwards(Pre_Value_Term,Value_V,Bindings_In)
        ->  Bindings_In = Bindings_Out_1,
            Value_Term = v(Value_V)
        ;   gensym('Var',G_Value),
            Value_Term = v(G_Value),
            Bindings_Out_1 = [var_binding{ var_name : G_Value,
                                           prolog_var: Pre_Value_Term,
                                           woql_var : Pre_Value_Term}|Bindings_In])
    ;   Value_Term = Pre_Value_Term,
        Bindings_Out_1 = Bindings_In),

    (   var(Pre_Type_Term)
    ->  (   lookup_backwards(Pre_Type_Term,Type_V,Bindings_Out_1)
        ->  Bindings_Out_1 = Bindings_Out,
            Type_Term = v(Type_V)
        ;   gensym('Var',G_Type),
            Type_Term = v(G_Type),
            Bindings_Out = [var_binding{ var_name : G_Type,
                                         prolog_var: Pre_Type_Term,
                                         woql_var : Woql_Type_Term}|Bindings_In],
            freeze(Woql_Type_Term,
                   (   memberchk(compress_prefixes(true), Options)
                   ->  uri_to_prefixed(Woql_Type_Term, Ctx, Pre_Type_Term)
                   ;   Woql_Type_Term = Pre_Type_Term)))
    ;   Type_Term = Pre_Type_Term,
        Bindings_Out = Bindings_Out_1),

    Term = Value_Term^^Type_Term.

term_var_to_binding(Var_Type, Ctx, Options, Pre_Term, Term, Bindings_In, Bindings_Out) :-
    (   var(Pre_Term)
    ->  (   lookup_backwards(Pre_Term,V,Bindings_In)
        ->  Bindings_In = Bindings_Out,
            Term = v(V)
        ;   gensym('Var',G),
            Bindings_Out = [var_binding{ var_name : G,
                                         prolog_var: Pre_Term,
                                         woql_var : Woql_Var}|Bindings_In],
            (   memberchk(compress_prefixes(true), Options)
            ->  (   Var_Type = subject
                ->  freeze(Woql_Var,
                       instance_uri_to_prefixed(Woql_Var,Ctx,Pre_Term))
                ;   Var_Type = predicate
                ->  freeze(Woql_Var,
                       schema_uri_to_prefixed(Woql_Var,Ctx,Pre_Term))
                ;   Var_Type = object
                ->  freeze(Woql_Var,
                           (   Woql_Var = _@_
                           ->  Pre_Term = Woql_Var
                           ;   Woql_Var = Elt^^Type
                           ->  freeze(Type,
                                      (   uri_to_prefixed(Type,Ctx,Prefixed_Type),
                                          Pre_Term = Elt^^Prefixed_Type))
                           ;   instance_uri_to_prefixed(Woql_Var,Ctx,Pre_Term)))
                ;   throw(error(unknown_term_var_type, _)))
            ;   Pre_Term = Woql_Var),
            Term = v(G)
        )
    ;   Term = Pre_Term,
        Bindings_Out = Bindings_In).

/**
 * pre_term_to_term_and_bindings(Pre_Term, Woql_Term, Bindings_In, Bindings_Out) is det.
 *
 * Pre term has free variables that need to be changed into woql variables witha binding
 */
pre_term_to_term_and_bindings(Ctx,Options,Pre_Term,Term,Bindings_In,Bindings_Out) :-
    (   var(Pre_Term)
    ->  (   lookup_backwards(Pre_Term,V,Bindings_In)
        ->  Bindings_In = Bindings_Out,
            Term = v(V)
        ;   gensym('Var',G),
            Bindings_Out = [var_binding{ var_name : G,
                                         prolog_var: Pre_Term,
                                         woql_var : Woql_Var}|Bindings_In],
            (   member(compress_prefixes(true), Options)
            ->  prefix_preterm(Ctx,Woql_Var,Pre_Term)
            ;   Pre_Term = Woql_Var),
            Term = v(G)
        )
    ;   is_dict(Pre_Term)
    ->  Term = Pre_Term,
        Bindings_In=Bindings_Out
    ;   Pre_Term = isa(O,T)
    ->  term_var_to_binding(subject, Ctx, Options, O, O_Post, Bindings_In, Bindings_Out_1),
        term_var_to_binding(predicate, Ctx, Options, T, T_Post, Bindings_Out_1, Bindings_Out),
        Term = isa(O_Post,T_Post)
    ;   Pre_Term = t(S,P,O)
    ->  term_var_to_binding(subject, Ctx, Options, S, S_Post, Bindings_In, Bindings_Out_1),
        term_var_to_binding(predicate, Ctx, Options, P, P_Post, Bindings_Out_1, Bindings_Out_2),
        term_var_to_binding(object, Ctx, Options, O, O_Post, Bindings_Out_2, Bindings_Out),
        Term = t(S_Post, P_Post, O_Post)
    ;   Pre_Term = t(S,P,O, Filter)
    ->  term_var_to_binding(subject, Ctx, Options, S, S_Post, Bindings_In, Bindings_Out_1),
        term_var_to_binding(predicate, Ctx, Options, P, P_Post, Bindings_Out_1, Bindings_Out_2),
        term_var_to_binding(object, Ctx, Options, O, O_Post, Bindings_Out_2, Bindings_Out),
        Term = t(S_Post, P_Post, O_Post, Filter)
    ;   Pre_Term =.. [F|Args],
        mapm(pre_term_to_term_and_bindings(Ctx,Options),Args,New_Args,Bindings_In,Bindings_Out),
        Term =.. [F|New_Args]
    ).

askable_prefixes(Context,Prefixes) :-
    query_context{} :< Context,
    !,
    Prefixes = (Context.prefixes).
askable_prefixes(Transaction_Object,Prefixes) :-
    transaction_object{ descriptor : Descriptor } :< Transaction_Object,
    !,
    collection_descriptor_prefixes(Descriptor, Prefixes).
askable_prefixes(Descriptor,Prefixes) :-
    collection_descriptor_prefixes(Descriptor, Prefixes).

/**
 * create_context(+Askable, -Context) is semidet.
 *
 * Create a context. Fail if Askable does not exist.
 */
create_context(Askable, Context) :-
    do_or_die(
       nonvar(Askable),
       error(unexpected_argument_instantiation(create_context, 1, Askable), _)),
    do_or_die(
       var(Context),
       error(unexpected_argument_instantiation(create_context, 2, Context), _)),
    create_context_(Askable, Context).

create_context_(Layer, Context) :-
    blob(Layer, layer),
    !,
    open_descriptor(Layer, Transaction),
    create_context_(Transaction, Context).
create_context_(Context, Context) :-
    query_context{} :< Context,
    !.
create_context_(Validation,Context) :-
    validation_object{ instance_objects : Inst,
                       schema_objects : Schema,
                       descriptor : Desc } :< Validation,
    !,
    Transaction_Object = transaction_object{
                             instance_objects : Inst,
                             schema_objects : Schema,
                             descriptor : Desc },
    create_context_(Transaction_Object,Context).
create_context_(Transaction_Object, Context) :-
    transaction_object{ descriptor : Descriptor } :< Transaction_Object,
    !,
    database_prefixes(Transaction_Object, Database_Prefixes),
    collection_descriptor_prefixes(Descriptor, Default_Prefixes),
    put_dict(Database_Prefixes, Default_Prefixes, Prefixes),
    collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor),

    % Note: should we be using system_descriptor{} below? or open it?
    super_user_authority(Super_User),
    Context = query_context{
        authorization : Super_User,
        transaction_objects : [Transaction_Object],
        default_collection : Descriptor,
        filter : type_filter{ types : [instance] },
        prefixes : Prefixes,
        write_graph : Graph_Descriptor,
        system : system_descriptor{},
        update_guard : _Guard,
        all_witnesses : false,
        files : [],
        bindings : [],
        selected : []
    }.
create_context_(Descriptor, Context) :-
    open_descriptor(Descriptor, Transaction_Object),
    create_context_(Transaction_Object, Context).

is_query_context(Context) :-
    is_dict(Context),
    query_context{} :< Context.

/**
 * create_context(+Askable, +Commit_Info, -Context) is semidet.
 *
 * Create a context and add commit info to it. Fail if Askable does not exist.
 */
create_context(Askable, Commit_Info, Context) :-
    create_context(Askable, Context_Without_Commit),
    Context = (Context_Without_Commit.put(commit_info, Commit_Info)).

/*
 * askable_context/N
 *
 * Create a context from an askable with system/auth and optional commit_info
 */
askable_context(Askable, System_DB, Auth, Context) :-
    create_context(Askable, Context_Without_Commit),
    Context = (Context_Without_Commit.put(_{ authorization : Auth,
                                             system: System_DB })).

askable_context(Askable, System_DB, Auth, Commit_Info, Context) :-
    create_context(Askable, Context_Without_Commit),
    Context = (Context_Without_Commit.put(_{ authorization: Auth,
                                             system: System_DB,
                                             commit_info: Commit_Info})).

askable_settings_context(Askable, Settings, Context) :-
    create_context(Askable, Context_Without_Settings),
    Context = (Context_Without_Settings.put(Settings)).

/**
 * descriptor_capabilities_context(Askable, Capabilities, Context) is det.
 *
 * Create a context with capabilities included
 */
descriptor_capabilities_context(Askable, Capabilities, Context) :-
    create_context(Askable, Context),
    Context.put(capabilities, Capabilities).

/*
 * context_extend_prefixes(+Context:query_context, +Prefixes:prefixes,
 *                             -New_Context:query_context) is det.
 *
 * Override the current query context with these prefixes when
 * there are collisions.
 */
context_extend_prefixes(Context, Prefixes, New_Context) :-
    Query_Prefixes = Context.prefixes,
    merge_dictionaries(Prefixes,Query_Prefixes, New_Prefixes),
    New_Context = Context.put(prefixes, New_Prefixes).

/*
 * context_default_prefixes(+Context:query_context, -New_Context:query_context) is det.
 *
 * Transform the given context, stripping out all prefixes, and
 * replacing them with the default global prefix set + the collection prefixes.
 */
context_default_prefixes(Context, New_Context) :-
    default_prefixes(Default),
    database_prefixes(Context, New_Prefixes),
    Total = (Default.put(New_Prefixes)),
    New_Context = Context.put(prefixes, Total).

context_to_parent_transaction(Context,Parent_Transaction) :-
    do_or_die(
        (   get_dict(transaction_objects,Context,[Transaction]),
            get_dict(parent,Transaction,Parent_Transaction)
        ),
        error(context_has_multiple_parents(Context))).

/*
 * ask(+Transaction_Object, Pre_Term:Goal) is nondet.
 *
 * Ask a woql query.
 *
 * Presumably this should commit the context since
 * it can't give anything back?
 */
ask(Askable, Pre_Term) :-
    ask(Askable, Pre_Term, [compress_prefixes(true)]).

/*
 * ask(+Transaction_Object, ?Pre_Term:Goal, +Options) is nondet.
 *
 * Ask with options
 *
 * Options include:
 * - compress_prefixes(Bool): Compress prefixes for results.
 */
ask(Askable, Pre_Term, Options) :-
    create_context(Askable, Query_Context),

    pre_term_to_term_and_bindings(Query_Context.prefixes,Options,
                                  Pre_Term,Term,
                                  [],Bindings_Out),
    New_Query_Ctx = Query_Context.put(bindings,Bindings_Out),

    ask_ast(New_Query_Ctx, Term, _).


/*
 * ask(+Transaction_Object, Pre_Term:Goal) is nondet.
 *
 * Ask a woql query and get back the resulting context.
 */
ask_ast(Context, Ast, Output_Context) :-
    compile_query(Ast,Prog, Context, Output_Context),
    debug(terminus(sdk),'Program: ~q~n', [Prog]),

    woql_compile:Prog.

/*
 * query_default_collection(Query_Context, Collection) is semidet.
 *
 * Finds the transaction object for the default collection if it exists.
 */
query_default_collection(Query_Context, Collection) :-
    collection_descriptor_transaction_object(Query_Context.default_collection,
                                             Query_Context.transaction_objects,
                                             Collection).

/*
 * query_default_write_graph(Query_Context, Graph) is det.
 *
 * Finds the transaction object for the default collection if it exists
 * and errors otherwise
 */
query_default_write_graph(Query_Context, Write_Graph) :-
    Graph_Descriptor = (Query_Context.write_graph),
    Transaction_Objects = (Query_Context.transaction_objects),
    do_or_die(
        graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor,
                                                               Transaction_Objects,
                                                               Write_Graph),
        error(unknown_graph(Graph_Descriptor))).

/*
 * query_default_schema_write_graph(Query_Context, Write_Graph) is semidet.
 *
 * fails if no default schema write graph is found
 */
query_default_schema_write_graph(Query_Context, Write_Graph) :-
    Default_Collection = (Query_Context.default_collection),
    do_or_die(
        branch_descriptor{
            branch_name : Branch_Name,
            repository_descriptor : Repository_Descriptor
        } :< Default_Collection,
        error(not_a_branch_descriptor(Default_Collection))),
    Repository_Descriptor = (Default_Collection.repository_descriptor),
    Repository_Name = (Repository_Descriptor.repository_name),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    Database_Name = (Database_Descriptor.database_name),
    Organization_Name = (Database_Descriptor.organization_name),
    Graph_Descriptor = branch_graph{
                           organization_name : Organization_Name,
                           database_name : Database_Name,
                           repository_name : Repository_Name,
                           branch_name : Branch_Name,
                           type : schema
                       },
    Transaction_Objects = (Query_Context.transaction_objects),
    graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor,
                                                           Transaction_Objects,
                                                           Write_Graph).
