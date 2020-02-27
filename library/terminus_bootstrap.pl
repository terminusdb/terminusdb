:- module(terminus_bootstrap,[
              layer_ontology/1,
              repository_ontology/1,
              ref_ontology/1,
              terminus_instance_name/1,
              terminus_inference_name/1,
              terminus_schema_name/1
          ]).

/** <module> Terminus Bootstrap
 *
 * Terminus bootstrap implements hard coded constants
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

terminus_instance_name('terminus:///terminus/document').

terminus_schema_name('terminus:///terminus/schema').

terminus_inference_name('terminus:///terminus/inference').

layer_ontology('http://terminusdb.com/schema/layer').

repository_ontology('http://terminusdb.com/schema/repository').

ref_ontology('http://terminusdb.com/schema/ref').

