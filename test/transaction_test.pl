:- module(transaction_test,[
          ]).

/** <module> Transaction testing
 *
 * Run tests to avoid regression on transactions.
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

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core(api)).

:- begin_tests(transaction_test).

test(terminus_descriptor_read_query_test) :-
    Descriptor = terminus_descriptor{},

    open_descriptor(Descriptor, Terminus),

    once(ask(Terminus,
             (   t('http://terminusdb.com/schema/terminus', rdf:type, owl:'Ontology', 'schema/*'),
                 t(doc:admin, rdf:type, terminus:'User'),
                 t(terminus:authority_scope, owl:propertyChainAxiom, _, 'inference/*')
             ))).

test(create_db_test, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_db('admin/Database', 'http://terminushub.com/document').


test(delete_db_test, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_db('admin/Database', 'http://terminushub.com/document'),
    delete_db('admin/Database').


test(empty_db_test, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    Name = 'admin/Database',
    create_db(Name, 'http://terminushub.com/document'),
    Descriptor = database_descriptor{ database_name : Name },
    findall(t(X,P,Y),
            ask(Descriptor,
                (   t(X, P, Y) )),
            Triples),
    write(Triples).

:- end_tests(transaction_test).
