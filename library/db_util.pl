:- module(db_util, [triple_val/4,
                    triple_string/4,
                    nb_add_triple_val/4,
                    nb_add_triple_string/4,
                    nb_add_instance/4]).
:- use_module(terminus_bootstrap).
:- use_module(library(terminus_store)).
:- use_module(literals).
:- use_module(syntax).

triple_val(Layer, Subject, Predicate, Object@Lang) :-
    var(Object),
    !,
    Object_Literal = value(_),
    triple(Layer, Subject, Predicate, Object_Literal),
    storage_object(Object_Literal, Object@Lang).
triple_val(Layer, Subject, Predicate, Object^^Type) :-
    var(Object),
    !,
    Object_Literal = value(_),
    triple(Layer, Subject, Predicate, Object_Literal),
    storage_object(Object_Literal, Object^^Type).
triple_val(Layer, Subject, Predicate, Object@Lang) :-
    !,
    object_storage(Object@Lang, Object_Literal),
    triple(Layer, Subject, Predicate, Object_Literal).
triple_val(Layer, Subject, Predicate, Object^^Type) :-
    object_storage(Object^^Type, Object_Literal),
    triple(Layer, Subject, Predicate, Object_Literal).

triple_string(Layer, Subject, Predicate, Object) :-
    xsd_string_type_uri(String_Type_Uri),
    triple_val(Layer, Subject, Predicate, Object^^String_Type_Uri).

nb_add_triple_val(Builder, Subject, Predicate, Object@Lang) :-
    !,
    object_storage(Object@Lang, Object_Literal),
    nb_add_triple(Builder, Subject, Predicate, Object_Literal).
nb_add_triple_val(Builder, Subject, Predicate, Object^^Type) :-
    object_storage(Object^^Type, Object_Literal),
    nb_add_triple(Builder, Subject, Predicate, Object_Literal).

nb_add_triple_string(Builder, Subject, Predicate, Object) :-
    xsd_string_type_uri(String_Type_Uri),
    nb_add_triple_val(Builder, Subject, Predicate, Object^^String_Type_Uri).

nb_add_triple_en(Builder, Subject, Predicate, Object) :-
    xsd_string_type_uri(String_Type_Uri),
    nb_add_triple_val(Builder, Subject, Predicate, Object@"en").

nb_add_instance(Builder, Instance_Uri, Type_Uri, Label) :-
    rdf_type_uri(Rdf_Type_Uri),
    label_prop_uri(Label_Prop_Uri),
    nb_add_triple(Builder, Instance_Uri, Rdf_Type_Uri, node(Type_Uri)),
    (   nonvar(Label)
    ->  nb_add_triple_val(Builder, Instance_Uri, Label_Prop_Uri, Label@en)
    ;   true).
