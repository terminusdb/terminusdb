:- module(types, []).

/** <module> Types

This module implements pervasive types which can be used for type checking.

*/

/** 
 * is_uri(+X) is semidet.
 * 
 * Mavis type checking.
 */ 
is_uri(X) :-
    atom(X).
is_uri(X) :-
    is_prefixed_uri(X).

error:has_type(uri,X) :-
    is_uri(X).

/**
 * is_prefixed_uri(+X) is semidet.
 * 
 **/
is_prefixed_uri(X):-
    nonvar(X),
    functor(X,(:),2).

error:has_type(prefixed_uri,X) :-
    is_prefixed_uri(X).

/** 
 * is_id(+X) is semidet.
 * 
 **/ 
is_id(id(Y)) :-
    (   atom(Y)
    ->  true
    ;   number(Y)).

error:has_type(id,X) :-
    is_id(X).



/** 
 * is_uri_or_id(+X) is semidet.
 * 
 **/
is_uri_or_id(X) :-
    is_uri(X).
is_uri_or_id(id(Y)) :-
    (   atom(Y)
    ->  true
    ;   number(Y)).

error:has_type(uri_or_id,X) :-
    is_uri_or_id(X).

/** 
 * is_object(+X) is semidet.
 * 
 **/ 
is_object(X) :-
    (   is_prefixed_uri(X)
    ->  true        
    ;   rdf_is_literal(X)
    ->  true
    ;   is_uri(X)).


/** 
 * is_object_or_id(+X) is semidet.
 * 
 **/ 
is_object_or_id(X) :-
    (   is_object(X)
    ->  true
    ;   is_id(X)).


/** 
 * is_number_compat(X) is det.
 * 
 * Compatibility with number type. Needs to not supply bindings as it 
 * is not a constraint but a test of current groundness.
 *
 * Name borrowed from Ciao.
 **/
is_number_compat(X) :-
    (   var(X)
    ->  true
    ;   number(X)).

