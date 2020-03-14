:- module(transaction_testing,[
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

:- use_module(core(util/test_utils)).

:- use_module(core(triple/database_utils)).

:- use_module(core(transaction/database)).

:- use_module(core(api/db_init)).
:- use_module(core(api/db_delete)).

:- begin_tests(transaction_test).

test(terminus_descriptor_read_query_test) :-
    Descriptor = terminus_descriptor,
    Terminus_Schema = 'terminus:///terminus/schema',
    Terminus_Instance = 'terminus:///terminus/document',
    Terminus_Inference = 'terminus:///terminus/inference',
    Read_Graph_Descriptors = [named_graph{ name : Terminus_Schema},
                              named_graph{ name : Terminus_Instance},
                              named_graph{ name : Terminus_Inference}],
    Write_Graph_Descriptors = [],
    database:descriptor_query(Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, [], New_Map),
    New_Map = [terminus_descriptor=Query_Object],
    Query_Object.schema_read_objects = [Schema_Read_Obj],

    Schema_Read_Obj.read = Schema_Layer,
    terminus_store:triple(Schema_Layer,
                          'http://terminusdb.com/schema/terminus',
                          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                          node('http://www.w3.org/2002/07/owl#Ontology')),
    Query_Object.instance_read_objects = [Instance_Read_Obj],
    Instance_Read_Obj.read = Instance_Layer,
    terminus_store:triple(Instance_Layer,
                          'terminus:///terminus/document/admin',
                          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                          node('http://terminusdb.com/schema/terminus#User')),

    Query_Object.inference_read_objects = [Inference_Read_Obj],
    Inference_Read_Obj.read = Inference_Layer,
    once(terminus_store:triple(Inference_Layer,
                          'http://terminusdb.com/schema/terminus#authority_scope',
                          'http://www.w3.org/2002/07/owl#propertyChainAxiom',
                          _)).

test_db_name('Terminus_Testing_Database').
test_base('http://localhost/').

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
    delete_db('admin/Database', 'http://terminushub.com/document').


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
