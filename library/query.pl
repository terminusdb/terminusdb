:- module(query,[
              connect/2,
              ask/2
          ]).

/** <module> Query
 *
 * Prolog interface to queries
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(utils).
:- use_module(database).
:- use_module(woql_compile).

:- reexport(woql_term).

/*
 * connect(+Collection_Resource_Name:uri -Ctx:query_context) is det.
 *
 * Resolves a query resource uri to a query_context which includes the queryable objects.
 */
connect(Collection_Resource_Name,New_Ctx) :-
    empty_ctx(Ctx),
    % TODO: This needs to actually work
    resolve_query_resource(Collection_Resource_Name,Descriptor),

    get_collection_resource_prefix_list(Descriptor, Prefixes),

    Ctx1 = Ctx.put(prefixes, Prefixes),

    descriptor_query(Descriptor,all,[],Query_Object),

    query_default_instance_write_descriptor(Query_Object,Main),

    Ctx2 = Ctx1.put(current_collection,Query_Object),
    Ctx3 = Ctx2.put(write_graph,Main),
    New_Ctx = Ctx3.put(query_objects,[Query_Object]).

/* TODO: This needs to be updated to new var representation
 */
pre_term_to_term_and_bindings(Pre_Term,Term,Bindings_In,Bindings_Out) :-
    (   var(Pre_Term)
    ->  (   lookup(V,X,Bindings_In),
            Pre_Term == X
        ->  Bindings_In = Bindings_Out,
            Term = v(V)
        ;   gensym('Var',G),
            Bindings_Out = [var_binding{ var_name : G,
                                         woql_var : Pre_Term}|Bindings_In],
            Term = v(G)
        )
    ;   is_dict(Pre_Term)
    ->  Term = Pre_Term,
        Bindings_In=Bindings_Out
    ;   Pre_Term =.. [F|Args],
        mapm(sdk:pre_term_to_term_and_bindings,Args,New_Args,Bindings_In,Bindings_Out),
        Term =.. [F|New_Args]
    ).

/* Ask a woql query */
ask(Resource_Desc,Pre_Term) :-
    pre_term_to_term_and_bindings(Pre_Term,Term,[],Bindings_Out),
    empty_ctx(Ctx),
    compile_query(Term,Prog,Ctx,_),
    debug(terminus(sdk),'Program: ~q~n', [Prog]),
    woql_compile:Prog.

ask(Pre_Term) :-
    empty_ctx(Ctx),
    ask(Ctx,Pre_Term).

