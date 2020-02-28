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

/*
 * We need to be able to resolve arbitrary resource URI's
 * to the appropriate portion of the query_object.
 *
 * TODO: Make this exist
 */
resolve_query_resource(_,_).

/*
 * connect(+DB_Name:uri -Ctx:context) is det.
 *
 * Resolves a query resource uri to a context which includes the queryable objects.
 */
connect(DB_Mame,New_Ctx) :-
    empty_ctx(Ctx),

    get_database_prefix_list(DB_Name, Prefixes),

    Ctx1 = Ctx.prefixes = Prefixes,
    make_database_from_database_name(DB_Name,DB_Obj),
    maybe_open_read_transaction(DB_Obj,DBR),

    database_default_write_instance(DB_Obj,I),
    Ctx2 = Ctx1.database = DBR,
    Ctx3 = Ctx2.write_graph=I,
    New_Ctx.collection=DB.

