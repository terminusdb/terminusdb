:- module(validation, [
              % schema_definitions.pl
              schema_predicate/1,
              rdfs_properties/1,
              owl_basic_properties/1,
              owl_list_properties/1,
              rdfs_types/1,
              owl_types/1,
              metadata_predicates/1,
              util_predicates/1,

              % validate_instance.pl
              instance_class/3,
              most_specific_type/3,
              refute_insertion/5,
              refute_deletion/5,
              refute_basetype_elt/3,

              % validate_schema.pl
              class/2,
              immediate_class/2,
              restriction/2,
              class_or_restriction/2,
              sub_class_of/3,
              union_of/3,
              intersection_of/3,
              sub_class_strict/3,
              disjoint_union_of/3,
              label/3,
              comment/3,
              subsumption_of/3,
              strict_subsumption_of/3,
              complement_of/3,
              terminus_tag/3,
              document/2,

              union_of_list/3,
              intersection_of_list/3,
              disjoint_union_of_list/3,
              one_of_list/3,

              datatype_property/2,
              object_property/2,
              annotation_property/2,
              property/2,
              sub_property_of/3,
              subsumption_properties_of/3,
              range/3,
              domain/3,
              any_range/3,
              any_domain/3,
              most_specific_domain/3,
              most_specific_range/3,
              collect/4,
              functional_property/2,
              inverse_functional_property/2,
              restriction_on_property/3,
              datatype_subsumption_of/3,
              basetype_subsumption_of/2,
              custom_datatype/2,
              datatype/2,
              strict_subsumption_property_of/3,
              orphan_property/4,
              rdf_meta_property/1,
              rdfs_property/1,
              % SC == Schema Constraints
              % constraints must be pred/2

              % REQUIRED Best Practice
              class_cycle_SC/2,               % Best Practice
              property_cycle_SC/2,            % Best Practice

              % Best practice
              no_immediate_domain_SC/2,
              no_immediate_range_SC/2,         % Best Practice
              % schema_blank_node_SC/2,
              not_unique_class_label_SC/2,      % Best Practice
              not_unique_class_SC/2,
              not_unique_property_SC/2,        % Best Practice
              no_immediate_class_SC/2,
              annotation_overload_SC/2,
              property_type_overload_SC/2,

              % OWL DL (Constraint)
              orphan_class_SC/2,              % OWL
              orphan_property_SC/2,           % OWL
              invalid_domain_SC/2,
              invalid_range_SC/2,             % OWL
              domain_not_subsumed_SC/2,
              range_not_subsumed_SC/2,         % OWL
              invalid_RDFS_property_SC/2    % OWL
          ]).

:- use_module(validation/schema_definitions).
:- use_module(validation/validate_instance).
:- use_module(validation/validate_schema).
