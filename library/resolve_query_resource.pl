:- module(resolve_query_resource,[
              connect/2,
              resolve_query_resource/2
          ]).

/** <module> Resolve Query Resource
 *
 * Resolves resource URIs to the appropiate associated graph_descriptor.
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

/*****************************************
 * URI Resource Resolution:
 *
 * We need to resolve URIs to the appropriate object - i.e. a desriptor which
 * can be interpreted by the prolog term output by WOQL. This involves two specific
 * scenarios: read and write.
 *
 * WOQL should *compile* to the resolving descriptor, which can then be used in the transaction to
 * find the right query_object, allowing the transaction logic to be simplified, and eventually
 * supporting nested transactions.
 *
 ** Read: Querying a specific object.
 *
 * Here we need to be able to read the union of the instance graphs of the appropriate query_object,
 * against the union of schema graphs (where relevant - i.e. with subsumption).
 *
 * As long as the resolution leaves us with a query_object, we can treat it irrespective of the
 * "layer of the union".
 *
 ** Write: Writing to a specific graph
 *
 * Here we need to be more directed. If we are unable to resolve to a specific graph we need to
 * throw an error, describing the graph set which might be intended, with their specific URIs.
 *
 * We can default the write graph to Server://DB_Name/local/document/main
 * Server can be defaulted to terminusHub
 */

/**
 * resolve_query_resource(Uri, Descriptor) is semidet.
 *
 * We need to be able to resolve arbitrary resource URI's
 * to the appropriate portion of the query_object.
 *
 * TODO: Make this exist
 */
resolve_query_resource('terminus:///terminus/',terminus_descriptor).
resolve_query_resource(URI, X) :-
    % The generic full path for a resource is:
    %
    %
    % Queryable resource:
    % protocol://server/database => protocol://server/database/local/master
    %
    % Graph resource:
    % protocol://server/database => protocol://server/database/local/master/instance/main
    % protocol://server/database/repository/ref/{instance,schema,inference}/graph_name
    % protocol://server/database/ is short for protocol://server/database/master/instance
    %
    % Repo metadata?:
    %
    % protocol://server/repository/ref
    %
    true.

/*
 * connect(+DB_Name:uri -Ctx:context) is det.
 *
 * Resolves a query resource uri to a context which includes the queryable objects.
 */
connect(DB_Mame,New_Ctx) :-
    empty_ctx(Ctx),

    get_database_prefix_list(DB_Name, Prefixes),

    Ctx1 = Ctx.prefixes = Prefixes,
    % TODO: This needs to actually work
    resolve_
    make_database_from_database_name(DB_Name,DB_Obj),
    maybe_open_read_transaction(DB_Obj,DBR),

    database_default_write_instance(DB_Obj,I),
    Ctx2 = Ctx1.database = DBR,
    Ctx3 = Ctx2.write_graph=I,
    New_Ctx.collection=DB.

