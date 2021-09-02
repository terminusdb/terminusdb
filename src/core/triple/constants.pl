:- module(constants,[
              system_instance_name/1,
              system_schema_name/1,
              system_inference_name/1,
              layer_ontology/1,
              repository_ontology/1,
              woql_ontology/1,
              ref_ontology/1,

              rdf_type_uri/1,
              xsd_string_type_uri/1,
              xsd_any_uri_type_uri/1,
              label_prop_uri/1,
              comment_prop_uri/1,

              database_class_uri/1,
              resource_name_property_uri/1,
              database_state_prop_uri/1,
              finalized_element_uri/1,
              deleting_element_uri/1,
              resource_includes_prop_uri/1,
              organization_database_prop_uri/1,
              allow_origin_prop_uri/1,

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

              admin_organization_uri/1,
              super_user_authority/1
          ]).

/** <module> Constants
 *
 * Terminus bootstrap implements hard coded constants
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

system_instance_name('terminusdb:///system/data').

system_schema_name('terminusdb:///system/schema').

system_inference_name('terminusdb:///system/inference').

layer_ontology('http://terminusdb.com/schema/layer').

repository_ontology('http://terminusdb.com/schema/repository').

ref_ontology('http://terminusdb.com/schema/ref').

woql_ontology('http://terminusdb.com/schema/woql').

%%% various URIs that we use often
rdf_type_uri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type').
xsd_string_type_uri('http://www.w3.org/2001/XMLSchema#string').
xsd_any_uri_type_uri('http://www.w3.org/2001/XMLSchema#anyURI').
label_prop_uri('http://www.w3.org/2000/01/rdf-schema#label').
comment_prop_uri('http://www.w3.org/2000/01/rdf-schema#comment').

%%% URIs from system schema
database_class_uri('http://terminusdb.com/schema/system#Database').
resource_name_property_uri('http://terminusdb.com/schema/system#resource_name').
database_state_prop_uri('http://terminusdb.com/schema/system#database_state').
finalized_element_uri('http://terminusdb.com/schema/system#finalized').
deleting_element_uri('http://terminusdb.com/schema/system#deleting').
resource_includes_prop_uri('http://terminusdb.com/schema/system#resource_includes').
organization_database_prop_uri('http://terminusdb.com/schema/system#organization_database').
allow_origin_prop_uri('http://terminusdb.com/schema/system#allow_origin').

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

%%% URIs from ref schema
branch_class_uri('http://terminusdb.com/schema/ref#Branch').
ref_commit_prop_uri('http://terminusdb.com/schema/ref#ref_commit').
ref_settings_class_uri('http://terminusdb.com/schema/ref#Settings').
ref_settings_base_uri_prop_uri('http://terminusdb.com/schema/ref#settings_base_uri').
ref_branch_base_uri_prop_uri('http://terminusdb.com/schema/ref#branch_base_uri').
ref_branch_name_prop_uri('http://terminusdb.com/schema/ref#branch_name').

%%% URIs from terminus instance
admin_organization_uri('terminusdb://system/data/Organization/admin').
super_user_authority('terminusdb://system/data/User/admin').
