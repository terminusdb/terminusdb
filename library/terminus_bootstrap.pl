:- module(terminus_bootstrap,[
              terminus_instance_name/1,
              terminus_schema_name/1,
              terminus_inference_name/1,
              layer_ontology/1,
              repository_ontology/1,
              ref_ontology/1,

              rdf_type_uri/1,
              xsd_string_type_uri/1,
              xsd_any_uri_type_uri/1,
              label_prop_uri/1,

              database_class_uri/1,
              database_name_property_uri/1,
              database_state_prop_uri/1,
              finalized_element_uri/1,
              deleting_element_uri/1,

              layer_class_uri/1,
              layer_id_prop_uri/1,

              branch_class_uri/1,
              ref_commit_prop_uri/1,
              ref_settings_class_uri/1,
              ref_settings_base_uri_prop_uri/1,
              ref_branch_base_uri_prop_uri/1,
              ref_branch_name_prop_uri/1,

              local_repository_class_uri/1,
              remote_repository_class_uri/1,

              repository_head_prop_uri/1,
              repository_name_prop_uri/1,
              repository_remote_url_prop_uri/1,
              repo_type_document_prefix/2,
              repo_type_document_prefix/2,

              repo_name_uri/4
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
xsd_any_uri_type_uri('http://www.w3.org/2001/XMLSchema#anyURI').
label_prop_uri('http://www.w3.org/2000/01/rdf-schema#label').

%%% URIs from terminus schema
database_class_uri('http://terminusdb.com/schema/terminus#Database').
database_name_property_uri('http://terminusdb.com/schema/terminus#database_name').
database_state_prop_uri('http://terminusdb.com/schema/terminus#database_state').
finalized_element_uri('http://terminusdb.com/schema/terminus#finalized').
deleting_element_uri('http://terminusdb.com/schema/terminus#deleting').



%%% URIs from layer schema
layer_class_uri('http://terminusdb.com/schema/layer#Layer').
layer_id_prop_uri('http://terminusdb.com/schema/layer#layer_id').

%%% URIs from repo schema
local_repository_class_uri('http://terminusdb.com/schema/repository#Local').
remote_repository_class_uri('http://terminusdb.com/schema/repository#Remote').

repository_head_prop_uri('http://terminusdb.com/schema/repository#repository_head').
repository_name_prop_uri('http://terminusdb.com/schema/repository#repository_name').
repository_remote_url_prop_uri('http://terminusdb.com/schema/repository#remote_url').
repo_type_document_prefix(local, 'Local_').
repo_type_document_prefix(remote, 'Remote_').

repo_name_uri(Db_Name, Repo_Name, Repo_Type, Uri) :-
    repo_type_document_prefix(Repo_Type, Repo_Type_Prefix),
    atomic_list_concat(['terminus:///', Db_Name, '/document/', Repo_Type_Prefix, Repo_Name], Uri).

%%% URIs from ref schema
branch_class_uri('http://terminusdb.com/schema/ref#Branch').
ref_commit_prop_uri('http://terminusdb.com/schema/ref#ref_commit').
ref_settings_class_uri('http://terminusdb.com/schema/ref#Settings').
ref_settings_base_uri_prop_uri('http://terminusdb.com/schema/ref#settings_base_uri').
ref_branch_base_uri_prop_uri('http://terminusdb.com/schema/ref#branch_base_uri').
ref_branch_name_prop_uri('http://terminusdb.com/schema/ref#branch_name').
