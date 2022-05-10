:- module(types, [
              is_literal/1,
              is_uri/1,
              is_id/1,
              is_bnode/1,
              is_prefixed_uri/1,
              is_uri_or_id/1,
              is_object/1,
              is_object_or_id/1,
              is_graph_identifier/1,
              is_prefix_db/1,
              is_database_identifier/1,
              is_empty_graph_name/1,
              is_database/1,
              is_read_obj/1,
              is_write_obj/1,
              % prolog xsd type representations
              is_date_time/1,
              is_date/1,
              is_point/1,
              is_coordinate_polygon/1,
              is_date_range/1,
              is_integer_range/1,
              is_decimal_range/1,
              is_gyear/1,
              is_gmonth/1,
              is_gday/1,
              is_gyear_month/1,
              is_gmonth_day/1,
              is_gyear_range/1,
              is_time/1,
              is_boolean/1,
              is_duration/1,
              is_byte/1,
              is_short/1,
              is_int/1,
              is_long/1,
              is_unsigned_byte/1,
              is_unsigned_short/1,
              is_unsigned_int/1,
              is_unsigned_long/1,
              is_positive_integer/1,
              is_negative_integer/1,
              is_nonpositive_integer/1,
              is_nonnegative_integer/1
          ]).

/** <module> Types
 *
 * This module implements pervasive types which can be used for type
 * checking. Works particularly well in conjunction with Mavis.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*************************************************************************

This file deliberately has no dependencies - please do not introduce them.

**************************************************************************/
:- use_module(syntax).
:- reexport(syntax).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(lists)).

/**
 * is_literal(+X) is semidet.
 *
 */
is_literal(_Data@Lang) :-
    atom(Lang).
is_literal(_Data^^_Type).

:- multifile error:has_type/2.
error:has_type(literal,X) :-
    is_literal(X).

/**
 * is_uri(+X) is semidet.
 *
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
    (   is_prefixed_uri(X) % DDD maybe not anymore...
    ->  true
    ;   is_literal(X)
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
 * is_bnode(+X) is semidet.
 */
is_bnode(X) :-
    atom_prefix(X,'_:').

/**
 * is_number_compat(X) is semidet.
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


/*
 * is_graph_identifier(X) is semidet.
 *
 * Type of a graph identifier object
 */
is_graph_identifier(GID) :-
    atom(GID).

error:has_type(graph_identifier, X) :-
    is_graph_identifier(X).

/*
 * is_prefix_db(X) is semidet.
 *
 * Tests to see if we have a valid prefix database.
 */
is_prefix_db([]).
is_prefix_db([A=B|Tail]) :-
    atom(A),
    atom(B),
    is_prefix_db(Tail).

error:has_type(prefix_db, X) :-
    is_prefix_db(X).


/*
 * is_database_identifier(X) is semidet.
 *
 * Type of a database identifier
 */
is_database_identifier(X) :-
    atom(X).

error:has_type(database_identifier, X) :-
    is_database_identifier(X).

/**
 * is_empty_graph_name(+Database_Id:graph_identifier) is semidet.
 *
 * Sometimes we want to designate that there is no graph
 * we can do this with none or false. JSON converts to @(false)
 * for a null object.
 **/
is_empty_graph_name(Database_Id):-
    member(Database_Id, [false, @(false), none]),
    !.

/*
 * is_database(Database) is semidet.
 *
 */
is_database(DB) :-
    is_dict(DB),
    DB.get(name) = Name,
    DB.get(instance) = Instance,
    (   DB.get(inference) = Inference
    ->  true
    ;   Inference = []),
    DB.get(schema) = Schema,
    (   DB.get(error_instance) = Error_Instance
    ->  true
    ;   Error_Instance = []),
    (   DB.get(error_schema) = Error_Schema
    ->  true
    ;   Error_Schema = []),
    DB.get(write_transaction) =  _WT,
    DB.get(read_transaction) = _RT,

    atom(Name),
    maplist(is_graph_identifier,Instance),
    maplist(is_graph_identifier,Inference),
    maplist(is_graph_identifier,Schema),
    maplist(is_graph_identifier,Error_Instance),
    maplist(is_graph_identifier,Error_Schema),
    maplist(is_write_obj,RT),
    maplist(is_read_obj,RT).

error:has_type(database, X) :-
    is_database(X).

is_write_obj(write_obj{ dbid: N,
                        graphid: G,
                        builder: _B }) :-
    atom(N),
    atom(G).

error:has_type(write_obj, X) :-
    is_write_obj(X).

is_read_obj(read_obj{ dbid: N,
                      graphid: G,
                      layer: _B }) :-
    atom(N),
    atom(G).

error:has_type(read_obj, X) :-
    is_read_obj(X).


days_in_month(_,1,31).
days_in_month(Y,2,D) :- (Y mod 4 =:= 0 -> D = 29 ; D = 28).
days_in_month(_,3,31).
days_in_month(_,4,30).
days_in_month(_,5,31).
days_in_month(_,6,30).
days_in_month(_,7,31).
days_in_month(_,8,31).
days_in_month(_,9,30).
days_in_month(_,10,31).
days_in_month(_,11,30).
days_in_month(_,12,31).

/*
 * is_date_time(+DateTime) is semidet.
 *
 * Determines if `DateTime` is a valid date time object.
 * We assume no DST, no zone, and offset of zero for normalised dates.
 */
is_date_time(date_time(Y,M,D,HH,MM,SS,NS)) :-
    integer(Y),
    integer(M), M >= 1, M =< 12,
    days_in_month(Y,M,Days), D >= 1, D =< Days,
    integer(HH), HH >= 0, HH =< 24,
    integer(MM), MM >= 0, MM =< 60,
    integer(SS), SS >= 0, HH =< 60,
    integer(NS), NS >= 0, NS =< 1 000 000 000.

is_offset(Offset) :-
    number(Offset),
    Offset =< 43200,
    Offset >= -43200.

is_date(date(Y,M,D,Offset)) :-
    integer(Y),
    integer(M), M >= 1, M =< 12,
    days_in_month(Y,M,Days), D >= 1, D =< Days,
    is_offset(Offset).

is_point(point(X,Y)) :-
    number(X),
    number(Y).

is_coordinate_polygon(coordinate_polygon(L)) :-
    maplist([[X,Y]]>>(number(X),number(Y)),L).

is_date_range(date_range(Date1,Date2)) :-
    is_date(Date1),
    is_date(Date2).

is_integer_range(integer_range(Integer1,Integer2)) :-
    integer(Integer1),
    integer(Integer2).

is_decimal_range(decimal_range(Float1,Float2)) :-
    number(Float1),
    number(Float2).

is_gyear(gyear(Year,Offset)) :-
    integer(Year),
    is_offset(Offset).

is_gyear_range(gyear_range(Year1,Year2)) :-
    is_gyear(Year1),
    is_gyear(Year2).

is_gyear_month(gyear_month(Year,Month,Offset)) :-
    integer(Year),
    integer(Month),
    Month >= 1,
    Month =< 12,
    is_offset(Offset).

is_gmonth(gmonth(Month,Offset)) :-
    integer(Month),
    Month >= 1,
    Month =< 12,
    is_offset(Offset).

is_gmonth_day(gmonth_day(Month,Day,Offset)) :-
    integer(Month),
    integer(Day),
    Month >= 1,
    Month =< 12,
    days_in_month(0,Month,Days),
    Day >= 0,
    Day =< Days,
    is_offset(Offset).

is_gday(gday(Day,Offset)) :-
    integer(Day),
    Day >= 1,
    Day =< 31,
    is_offset(Offset).

is_time(time(HH,MM,SS)) :-
    integer(HH),
    HH >= 0, HH < 24,
    integer(MM),
    MM >= 0, MM < 60,
    number(SS),
    SS >= 0, SS < 60.

is_boolean(true).
is_boolean(false).

is_duration(duration(Sign,Y,M,D,HH,MM,SS)) :-
    member(Sign, [1,-1]),
    number(Y),
    number(M),
    number(D),
    number(HH),
    number(MM),
    number(SS).

is_byte(S) :-
    integer(S),
    S >= -128,
    S =< 127.

is_short(S) :-
    integer(S),
    S >= -32768,
    S =< 32767.

is_int(S) :-
    integer(S),
    S >= -2147483648,
    S =< 2147483647.

is_long(S) :-
    integer(S),
    S >= -9223372036854775808,
    S =< 9223372036854775807.

is_unsigned_byte(S) :-
    integer(S),
    S >= 0,
    S =< 255.

is_unsigned_short(S) :-
    integer(S),
    S >= 0,
    S =< 65535.

is_unsigned_int(S) :-
    integer(S),
    S >= 0,
    S =< 4294967295.

is_unsigned_long(S) :-
    integer(S),
    S >= 0,
    S =< 18446744073709551615.

is_positive_integer(S) :-
    integer(S),
    S > 0.

is_nonnegative_integer(S) :-
    integer(S),
    S >= 0.

is_negative_integer(S) :-
    integer(S),
    S < 0.

is_nonpositive_integer(S) :-
    integer(S),
    S =< 0.
