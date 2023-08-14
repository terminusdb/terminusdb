:- module(migration_dict, [migration_list_to_ast_list/2,
                           migration_dict_to_ast/2]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(core(util)).

migration_list_to_ast_list(List,AST) :-
    maplist(migration_dict_to_ast_ex, List, AST).

migration_dict_to_ast_ex(Dict, AST) :-
    do_or_die(
        migration_dict_to_ast(Dict,AST),
        error(unknown_schema_migration_operation(Dict), _)).

default_or_error(json{ '@type' : "Error"}, error).
default_or_error(json{ '@type' : "Default",
                       value: Value}, value(Value)).

migration_dict_to_ast(json{ '@type' : "DeleteClass",
                            class : Class },
                      delete_class(Class)).
migration_dict_to_ast(json{ '@type' : "CreateClass",
                            class_document : Class_Document },
                      create_class(Class_Document)).
migration_dict_to_ast(json{ '@type' : "MoveClass", from : From, to: To },
                      move_class(From, To)).
migration_dict_to_ast(json{ '@type' : "ReplaceClassMetadata", class : Class, metadata: Metadata },
                      replace_class_metadata(Class, Metadata)).
migration_dict_to_ast(json{ '@type' : "NotAbstract",
                            class: Class},
                      not_abstract(Class)).
migration_dict_to_ast(json{ '@type' : "Abstract",
                            class: Class},
                      abstract(Class)).
migration_dict_to_ast(json{ '@type' : "Unfoldable",
                            class: Class},
                      unfoldable(Class)).
migration_dict_to_ast(json{ '@type' : "NotUnfoldable",
                            class: Class},
                      not_unfoldable(Class)).
gration_dict_to_ast(json{ '@type' : "ReplaceClassDocumentation",
                            class: Class,
                            documentation: Documentation},
                      replace_class_documentation(Class, Documentation)).
migration_dict_to_ast(json{ '@type' : "ExpandEnum",
                            enum: Enum,
                            values: Values},
                      expand_enum(Enum, Values)).
migration_dict_to_ast(json{ '@type' : "ReplaceContext",
                            context : Context },
                      replace_context(Context)).
migration_dict_to_ast(json{ '@type' : "DeleteClassProperty",
                            class: Class,
                            property: Property},
                      delete_class_property(Class, Property)).
migration_dict_to_ast(json{ '@type' : "CreateClassProperty",
                            class: Class,
                            property: Property,
                            type: Type,
                            default: Default
                          },
                      create_class_property(Class, Property, Type, Default)).
migration_dict_to_ast(json{ '@type' : "CreateClassProperty",
                            class: Class,
                            property: Property,
                            type: Type
                          },
                      create_class_property(Class, Property, Type)).
migration_dict_to_ast(json{'@type' : "MoveClassProperty",
                           class: Class,
                           from: From,
                           to: To},
                      move_class_property(Class, From, To)).
migration_dict_to_ast(json{'@type' : "UpcastClassProperty",
                           class: Class,
                           property: Property,
                           type: Type},
                      upcast_class_property(Class, Property, Type)).
migration_dict_to_ast(json{'@type' : "CastClassProperty",
                           class: Class,
                           property: Property,
                           type: Type,
                           default: Default_Or_Error_Dict},
                      cast_class_property(Class, Property, Type, Default_Or_Error)) :-
    default_or_error(Default_Or_Error_Dict, Default_Or_Error).
migration_dict_to_ast(json{ '@type' : "ChangeKey",
                            class: Class,
                            key: KeyType},
                      change_key(Class, KeyType, [])) :-
    !.
migration_dict_to_ast(json{ '@type' : "ChangeKey",
                            class: Class,
                            key: KeyType,
                            fields: Fields},
                      change_key(Class, KeyType, Fields)).
migration_dict_to_ast(json{ '@type' : "ChangeParents",
                            class: Class,
                            parents: Parents},
                      change_parents(Class, Parents, [])) :-
    !.
migration_dict_to_ast(json{ '@type' : "ChangeParents",
                            class: Class,
                            parents: Parents,
                            properties: Defaults},
                      change_parents(Class, Parents, Property_Defaults)) :-
    property_defaults(Defaults, Property_Defaults).

property_defaults(List, TermList) :-
    maplist([json{property: Property,
                  default: Default},
             property_default(Property, Default)]>>true,
            List,
            TermList).
