:- module(migration_dict, [migration_list_to_ast_list/2]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(core(util)).

migration_list_to_ast_list(List,AST) :-
    maplist(migration_dict_to_ast, List, AST).

migration_dict_to_ast(Dict,AST) :-
    do_or_die(
        get_dict('@type', Dict, Type),
        error(unknown_schema_migration_operation(Dict), _)),
    atom_string(Type_Atom, Type),
    migration_dict_to_ast_ex(Type_Atom,Dict,AST).

migration_dict_to_ast_ex(Atom, Dict, AST) :-
    do_or_die(
        migration_dict_to_ast(Atom,Dict,AST),
        error(unknown_schema_migration_operation(Dict), _)).

default_or_error(Dict, AST) :-
    do_or_die(
        get_dict('@type', Dict, String),
        error(unknown_schema_migration_operation(Dict), _)),
    atom_string(Atom,String),
    default_or_error_ex(Atom,Dict,AST).

default_or_error_ex(Atom,Dict,AST) :-
    do_or_die(
        default_or_error(Atom,Dict,AST),
        error(unknown_schema_migration_default(Dict), _)),

default_or_error('Error', _Dict, error).
default_or_error('Default', Dict, value(Value)) :-
    get_dict('value', Dict, Value).

migration_dict_to_ast('DeleteClass', Dict, delete_class(Class)) :-
    get_dict(class, Dict, Class).
migration_dict_to_ast('CreateClass', Dict, create_class(Class_Document)) :-
    get_dict(class_document, Dict, Class_Document).
migration_dict_to_ast('MoveClass', Dict, move_class(From, To)) :-
    get_dict(from, Dict, From),
    get_dict(to, Dict, To).
migration_dict_to_ast('ReplaceClassMetadata', Dict, replace_class_metadata(Class, Metadata)) :-
    get_dict(class, Dict, Class),
    get_dict(metadata, Dict, Metadata).
migration_dict_to_ast('ReplaceClassDocumentation', Dict, replace_class_documentation(Class, Documentation)) :-
    get_dict(class, Dict, Class),
    get_dict(documentation, Dict, Documentation).
migration_dict_to_ast('DeleteClassProperty', Dict, delete_class_property(Class, Property)) :-
    get_dict(class, Dict, Class),
    get_dict(property, Dict, Property).
migration_dict_to_ast('CreateClassProperty', Dict, Operation) :-
    get_dict(class, Dict, Class),
    get_dict(property, Dict, Property),
    get_dict(type, Dict, Type),
    (   get_dict(default, Dict, Default)
    ->  Operation = create_class_property(Class, Property, Type, Default)
    ;   Operation = create_class_property(Class, Property, Type)
    ).
migration_dict_to_ast('MoveClassProperty', Dict, move_class_property(Class, Old_Property, New_Property)) :-
    get_dict(class, Dict, Class),
    get_dict(from, Dict, Old_Property),
    get_dict(to, Dict, New_Property).
migration_dict_to_ast('UpcastClassProperty', Dict, upcast_class_property(Class, Old_Property, New_Type)) :-
    get_dict(class, Dict, Class),
    get_dict(property, Dict, Old_Property),
    get_dict(type, Dict, New_Type).
migration_dict_to_ast('CastClassProperty', Dict, cast_class_property(Class, Old_Property, New_Type, Default_Or_Error)) :-
    get_dict(class, Dict, Class),
    get_dict(property, Dict, Old_Property),
    get_dict(type, Dict, New_Type),
    (   get_dict(default, Dict, Default)
    ->  Default_Or_Error = default(Default)
    ;   Default_Or_Error = error
    ).
migration_dict_to_ast('ChangeKey', Dict, change_key(Class, KeyType, PropertyList)) :-
    get_dict(class, Dict, Class),
    get_dict(key, Dict, KeyType),
    (   get_dict(fields, Dict, PropertyList)
    ->  true
    ;   PropertyList = []
    ).
migration_dict_to_ast('ChangeParents', Dict, change_parents(Class, ParentList, Property_Defaults)) :-
    get_dict(class, Dict, Class),
    get_dict(parents, Dict, ParentList),
    (   get_dict(properties, Dict, Defaults)
    ->  property_defaults(Defaults, Property_Defaults)
    ;   Property_Defaults = []
    ).

property_defaults(List, TermList) :-
    maplist([Dict, property_default(Property, Default)]>>(
                get_dict(property, Dict, Property),
                get_dict(default, Dict, Default)
            ), List, TermList).
