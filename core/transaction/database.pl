:- module(database,[
              query_context_transaction_objects/2,
              run_transaction/1,
              run_transactions/1
          ]).

/** <module> Implementation of database graph management
 *
 * This module helps other modules with the representation of databases and
 * their associated graphs by bundling them as objects with some convenience
 * operators and accessors.
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

:- reexport(core(util/syntax)).
:- use_module(core(transaction/descriptor)).
:- use_module(core(transaction/validate)).

:- use_module(library(prolog_stack)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- use_module(library(terminus_store)).


/*
 * run_transaction(Transaction) is det.
 *
 * Run transaction and throw errors with witnesses.
 *
 */
run_transaction(Transaction) :-
    transaction_objects_to_validation_objects([Transaction], Validations),
    commit_validation_objects(Validations).

/*
 * run_transactions(Transaction) is det.
 *
 * Run all transactions and throw errors with witnesses.
 *
 */
run_transactions(Transactions) :-
    transaction_objects_to_validation_objects(Transactions, Validations),
    commit_validation_objects(Validations).


/*
 * query_context_transaction_objects(+Query_Object,Transaction_Objects) is det.
 *
 * Marshall commit info into the transaction object and run it.
 */
query_context_transaction_objects(Query_Context,Transaction_Objects) :-
    maplist({Query_Context}/[Transaction_Object,New_Transaction_Object]>>(
                get_dict(commit_info,Query_Context,Commit_Info),
                put_dict(_{commit_info : Commit_Info},
                         Transaction_Object,
                         New_Transaction_Object)
            ),
            Query_Context.transaction_objects,
            Transaction_Objects).
