:- module(terminus_bootstrap,[
              layer_ontology/1,
              repository_ontology/1,
              ref_ontology/1,
              terminus_instance_name/1,
              terminus_inference_name/1,
              terminus_schema_name/1,
              rdf_type_uri/1,
              xsd_string_type_uri/1,
              database_class_uri/1,
              database_name_property_uri/1,
              finalized_element_uri/1
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

%%% various URIs that we use often
rdf_type_uri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type').
xsd_string_type_uri('http://www.w3.org/2001/XMLSchema#string').

%%% URIs from terminus schema
database_class_uri('http://terminusdb.com/schema/terminus#Database').
database_name_property_uri('http://terminusdb.com/schema/terminus#database_name').
finalized_element_uri('http://terminusdb.com/schema/terminus#finalized').

%%% URIs from repository schema
respository_class_uri('http://terminusdb.com/schema/repository#Local').
