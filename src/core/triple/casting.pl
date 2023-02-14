:- module(casting,[
              typecast/4
          ]).

/** <module> Casting
 *
 * Utilities for casting between types for I/O.
 *
 */

:- use_module(literals).

:- use_module(core(util)).
:- use_module(core(triple/base_type)).

:- reexport(core(util/syntax)).

:- use_module(core(query/jsonld)). % dubious. we should not be importing query stuff here.

:- use_module(library(lists)).

:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(url)).
:- use_module(library(http/json), [atom_json_dict/3]).

/*
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Hint, Cast) :-
    (   var(Val)
    ->  format(atom(M), 'Variable unbound in typecast to ~q', [Type]),
        throw(error(M))
    ;   (   \+ (   base_type(Type)
               ;   Type = 'http://www.w3.org/2002/07/owl#Thing'
               ;   Type = 'http://terminusdb.com/schema/sys#Top')
        ->  throw(error(unknown_type_casting_error(Val, Type), _))
        ;   Val = Bare_Literal^^Source_Type,
            (   basetype_subsumption_of(Source_Type,'http://www.w3.org/2001/XMLSchema#string')
                % Upcast to xsd:string for downcast
            ->  typecast_switch(Type,'http://www.w3.org/2001/XMLSchema#string',Bare_Literal,Hint,Cast)
            ;   basetype_subsumption_of(Source_Type,'http://www.w3.org/2001/XMLSchema#decimal')
                % Upcast to xsd:decimal for downcast
            ->  typecast_switch(Type,'http://www.w3.org/2001/XMLSchema#decimal',Bare_Literal,Hint,Cast)
            ;   typecast_switch(Type,Source_Type,Bare_Literal,Hint,Cast)
            )
        ->  true
        ;   Val = Bare_Literal@_, % upcast to string
            typecast_switch(Type,'http://www.w3.org/2001/XMLSchema#string',Bare_Literal,Hint,Cast)
        ->  true
        ;   atom(Val)
        ->  typecast_switch(Type,'http://www.w3.org/2002/07/owl#Thing',Val,Hint,Cast)
        ;   throw(error(casting_error(Val, Type), _))
        )
    ).

/*
 * typecast_switch(+Target_Type, +Source_Type, +Bare_Literal, +Hint, -Casted_Literal) is det.
 *
 * Casts from source type to target type.
 */
%%% xsd:string Self Cast
%%% NOTE: Should not be necessary as we shouldn't be able to have an atom in the first place
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val_String^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(Val),
    !,
    atom_string(Val,Val_String).
%%% Self Cast
typecast_switch(Type, Type, Val, _, Val^^Type) :-
    !.
%%% owl:Thing => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2002/07/owl#Thing', Val, _, String^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(Val),
    !,
    atom_string(Val,String).
%%% xsd:string => owl:Thing
typecast_switch('http://www.w3.org/2002/07/owl#Thing', 'http://www.w3.org/2001/XMLSchema#string', Val, Hints, Atom) :-
    /* Note: It might be wise to check URI validity */
    !,
    (   option(prefixes(Prefixes), Hints)
    ->  prefix_expand(Val, Prefixes, Atom)
    ;   format(atom(Atom), '~w', [Val])
    ).
%%% sys:Top => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/sys#Top', Val, _, String^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(Val),
    !,
    atom_string(Val,String).
%%% xsd:string => sys:Top
typecast_switch('http://terminusdb.com/schema/sys#Top', 'http://www.w3.org/2001/XMLSchema#string', Val, Hints, Atom) :-
    /* Note: It might be wise to check URI validity */
    !,
    (   option(prefixes(Prefixes), Hints)
    ->  prefix_expand(Val, Prefixes, Atom)
    ;   format(atom(Atom), '~w', [Val])
    ).
%%% xsd:string => xdd:coordinatePolygon
typecast_switch('http://terminusdb.com/schema/xdd#coordinatePolygon', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#coordinatePolygon') :-
    !,
    (   atom_codes(Val, Codes), phrase(coordinatePolygon(S),Codes)
    ->  Cast = coordinate_polygon(S)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#coordinatePolygon'),_))).
%%% xdd:coordinatePolygon => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#coordinatePolygon', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_coordinate_polygon(Val),
        Val = coordinate_polygon(L)
    ->  format(string(S), '~w', [L])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#coordinatePolygon'),_))).
%%% xsd:string => xdd:coordinatePolyline
typecast_switch('http://terminusdb.com/schema/xdd#coordinatePolyline', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#coordinatePolyline') :-
    !,
    (   atom_codes(Val, Codes), phrase(coordinatePolygon(S),Codes)
    ->  Cast = coordinate_polygon(S)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#coordinatePolyline'),_))).
%%% xdd:coordinatePolyline => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#coordinatePolyline', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_coordinate_polygon(Val),
        Val = coordinate_polygon(L)
    ->  format(string(S), '~w', [L])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#coordinatePolyline'),_))).
%%% xsd:string => xdd:coordinate
typecast_switch('http://terminusdb.com/schema/xdd#coordinate', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#coordinatePolyline') :-
    !,
    (   atom_codes(Val, Codes), phrase(point(X,Y),Codes)
    ->  Cast = point(X,Y)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#coordinate'),_))).
%%% xdd:coordinate => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#coordinate', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_point(Val),
        Val = point(X,Y)
    ->  format(string(S), '[~w,~w]', [X,Y])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#coordinate'),_))).
%%% xsd:string => xdd:dateRange
typecast_switch('http://terminusdb.com/schema/xdd#dateRange', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateRange') :-
    !,
    (   atom_codes(Val, Codes), phrase(dateRange(X,Y),Codes)
    ->  Cast = date_range(X,Y)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateRange'),_))).
%%% xdd:dateRange => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#dateRange', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_date_range(Val),
        Val = date_range(D1,D2),
        date_string(D1,Val1),
        date_string(D2,Val2)
    ->  format(string(S), '[~w,~w]', [Val1,Val2])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateRange'),_))).
%%% xsd:string => xdd:integerRange
typecast_switch('http://terminusdb.com/schema/xdd#integerRange', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#integerRange') :-
    !,
    (   atom_codes(Val, Codes), phrase(integerRange(X,Y),Codes)
    ->  Cast = integer_range(X,Y)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#integerRange'),_))).
%%% xdd:integerRange => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#integerRange', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_integer_range(Val),
        Val = integer_range(Int1,Int2)
    ->  format(string(S), '[~w,~w]', [Int1,Int2])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#integerRange'),_))).
%%% xsd:string => xdd:decimalRange
typecast_switch('http://terminusdb.com/schema/xdd#decimalRange', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#decimalRange') :-
    !,
    (   atom_codes(Val, Codes), phrase(decimalRange(X,Y),Codes)
    ->  Cast = decimal_range(X,Y)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#decimalRange'),_))).
%%% xdd:decimalRange => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#decimalRange', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_decimal_range(Val),
        Val = decimal_range(D1,D2)
    ->  format(string(S), '[~w,~w]', [D1,D2])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#decimalRange'),_))).
%%% xsd:string => xdd:gYearRange
typecast_switch('http://terminusdb.com/schema/xdd#gYearRange', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#gYearRange') :-
    !,
    (   atom_codes(Val, Codes), phrase(gYearRange(X,Y),Codes)
    ->  Cast = gyear_range(X,Y)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#gYearRange'),_))).
%%% xdd:gYearRange => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#gYearRange', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_gyear_range(Val),
        Val = gyear_range(Val1,Val2),
        gyear_string(Val1,S1),
        gyear_string(Val2,S2)
    ->  format(string(S), '[~w,~w]', [S1,S2])
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#string'),_))).
%%% xsd:string => xdd:url
typecast_switch('http://terminusdb.com/schema/xdd#url', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://terminusdb.com/schema/xdd#url') :-
    !,
    (   is_absolute_url(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#url'),_))).
%%% xdd:url => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#url', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xdd:email
typecast_switch('http://terminusdb.com/schema/xdd#email', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://terminusdb.com/schema/xdd#email') :-
    !,
    (   (atom_codes(Val,C), phrase(email,C,[]))
    ->  true
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#email'),_))).
%%% xdd:email => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#email', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xdd:json
typecast_switch('http://terminusdb.com/schema/xdd#json', 'http://www.w3.org/2001/XMLSchema#string', Val, _, JSON^^'http://terminusdb.com/schema/xdd#json') :-
    !,
    (   (catch(atom_json_dict(Val,JSON,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#json'),_))).
%%% xdd:json => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#json', JSON, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   (catch(atom_json_dict(Val,JSON,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:boolean
typecast_switch('http://www.w3.org/2001/XMLSchema#boolean', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#boolean') :-
    !,
    (   member(Val, ["0","false",false,0])
    ->  Casted = false
    ;   member(Val, ["1","true",true,1])
    ->  Casted = true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#boolean'),_))).
%%% xsd:boolean => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#boolean', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   member(Val, ["0","false",false,0])
    ->  Casted = "false"
    ;   member(Val, ["1","true",true,1])
    ->  Casted = "true"
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#boolean'),_))).
%%% xsd:string => xsd:decimal
typecast_switch('http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    (   number_string(Casted, Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#decimal'),_))).
%%% xsd:decimal => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   number(Val)
    ->  format(string(S), "~w", [Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#decimal'),_))).
%%% xsd:string => xsd:integer
typecast_switch('http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#integer') :-
    !,
    (   atom_codes(Val,C),
        phrase(integer(Casted),C,[])
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#integer'),_))).
%%% xsd:integer => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#integer', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   integer(Val)
    ->  format(string(S), "~w", [Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#integer'),_))).
%%% xsd:string => xsd:double
typecast_switch('http://www.w3.org/2001/XMLSchema#double', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#double') :-
    !,
    (   number_string(Casted,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))).
%%% xsd:double => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#double', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   number(Val)
    ->  format(string(S), "~w", [Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))).
%%% xsd:string => xsd:float
typecast_switch('http://www.w3.org/2001/XMLSchema#float', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#float') :-
    !,
    (   number_string(Casted,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#float'),_))).
%%% xsd:float => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#float', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   number(Val)
    ->  format(string(S), "~w", [Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#float'),_))).
%%% xsd:string => xsd:time
typecast_switch('http://www.w3.org/2001/XMLSchema#time', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#time') :-
    !,
    (   time_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#time'),_))).
%%% xsd:time => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#time', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_time(Val)
    ->  time_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:date
typecast_switch('http://www.w3.org/2001/XMLSchema#date', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#date') :-
    !,
    (   date_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#date'),_))).
%%% xsd:date => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#date', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_date(Val)
    ->  date_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:dateTime
typecast_switch('http://www.w3.org/2001/XMLSchema#dateTime', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    !,
    (   date_time_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#dateTime'),_))).
%%% xsd:dateTime => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#dateTime', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_date_time(Val)
    ->  date_time_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:dateTimeStamp
typecast_switch('http://www.w3.org/2001/XMLSchema#dateTimeStamp', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#dateTimeStamp') :-
    !,
    (   date_time_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#dateTimeStamp'),_))).
%%% xsd:dateTimeStamp => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#dateTimeStamp', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_date_time(Val)
    ->  date_time_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:gYear
typecast_switch('http://www.w3.org/2001/XMLSchema#gYear', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#gYear') :-
    !,
    (   gyear_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#gYear'),_))).
%%% xsd:decimal => xsd:gYear
typecast_switch('http://www.w3.org/2001/XMLSchema#gYear', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#gYear') :-
    !,
    (   integer(Val)
    ->  Cast = gyear(Val,0)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#gYear'),_))).
%%% xsd:gYear => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#gYear', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_gyear(Val)
    ->  gyear_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:gMonth
typecast_switch('http://www.w3.org/2001/XMLSchema#gMonth', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#gMonth') :-
    !,
    (   gmonth_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#gMonth'),_))).
%%% xsd:gMonth => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#gMonth', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_gmonth(Val)
    ->  gmonth_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:gDay
typecast_switch('http://www.w3.org/2001/XMLSchema#gDay', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#gDay') :-
    !,
    (   gday_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#gDay'),_))).
%%% xsd:gDay => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#gDay', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_gday(Val)
    ->  gday_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:gYearMonth
typecast_switch('http://www.w3.org/2001/XMLSchema#gYearMonth', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#gYearMonth') :-
    !,
    (   gyear_month_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#gYearMonth'),_))).
%%% xsd:gYearMonth => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#gYearMonth', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_gyear_month(Val)
    ->  gyear_month_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:gMonthDay
typecast_switch('http://www.w3.org/2001/XMLSchema#gMonthDay', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#gMonthDay') :-
    !,
    (   gmonth_day_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#gMonthDay'),_))).
%%% xsd:gMonthDay => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#gMonthDay', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_gmonth_day(Val)
    ->  gmonth_day_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:duration
typecast_switch('http://www.w3.org/2001/XMLSchema#duration', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#duration') :-
    !,
    (   duration_string(Cast,Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#duration'),_))).
%%% xsd:string => xsd:yearMonthDuration
typecast_switch('http://www.w3.org/2001/XMLSchema#yearMonthDuration', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#yearMonthDuration') :-
    !,
    (   duration_string(Cast,Val),
        Cast = duration(_Sign,_Y,_M,_D,0,0,0)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#yearMonthduration'),_))).
%%% xsd:string => xsd:dayTimeDuration
typecast_switch('http://www.w3.org/2001/XMLSchema#dayTimeDuration', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#dayTimeDuration') :-
    !,
    (   duration_string(Cast,Val),
        Cast = duration(_Sign,0,0,_D,_H,_M,_S)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#dayTimeDuration'),_))).
%%% xsd:duration => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#duration', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_duration(Val)
    ->  duration_string(Val,S)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:byte
typecast_switch('http://www.w3.org/2001/XMLSchema#byte', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#byte') :-
    !,
    (   number_string(Cast,Val),
        is_byte(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#byte'),_))).
%%% xsd:byte => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#byte', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_byte(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:short
typecast_switch('http://www.w3.org/2001/XMLSchema#short', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#short') :-
    !,
    (   number_string(Cast,Val),
        is_short(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#short'),_))).
%%% xsd:short => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#short', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_short(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:int
typecast_switch('http://www.w3.org/2001/XMLSchema#int', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#int') :-
    !,
    (   number_string(Cast,Val),
        is_int(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#int'),_))).
%%% xsd:int => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#int', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_int(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:long
typecast_switch('http://www.w3.org/2001/XMLSchema#long', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#long') :-
    !,
    (   number_string(Cast,Val),
        is_long(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#long'),_))).
%%% xsd:long => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#long', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_long(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:unsignedByte
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedByte', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#unsignedByte') :-
    !,
    (   number_string(Cast,Val),
        is_unsigned_byte(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedByte'),_))).
%%% xsd:unsignedByte => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#unsignedByte', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_unsigned_byte(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:unsignedShort
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedShort', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#unsignedShort') :-
    !,
    (   number_string(Cast,Val),
        is_unsigned_short(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedShort'),_))).
%%% xsd:unsignedShort => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#unsignedShort', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_unsigned_short(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:unsignedInt
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedInt', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#unsignedInt') :-
    !,
    (   number_string(Cast,Val),
        is_unsigned_int(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedInt'),_))).
%%% xsd:unsignedInt => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#unsignedInt', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_unsigned_int(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:unsignedLong
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedLong', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#unsignedLong') :-
    !,
    (   number_string(Cast,Val),
        is_unsigned_long(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedLong'),_))).
%%% xsd:unsignedLong => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#unsignedLong', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_unsigned_long(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:positiveInteger
typecast_switch('http://www.w3.org/2001/XMLSchema#positiveInteger', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#positiveInteger') :-
    !,
    (   number_string(Cast,Val),
        is_positive_integer(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#positiveInteger'),_))).
%%% xsd:positiveInteger => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#positiveInteger', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_positive_integer(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:nonNegativeInteger
typecast_switch('http://www.w3.org/2001/XMLSchema#nonNegativeInteger', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger') :-
    !,
    (   number_string(Cast,Val),
        is_nonnegative_integer(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),_))).
%%% xsd:nonNegativeInteger => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_nonnegative_integer(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:negativeInteger
typecast_switch('http://www.w3.org/2001/XMLSchema#negativeInteger', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#negativeInteger') :-
    !,
    (   number_string(Cast,Val),
        is_negative_integer(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#negativeInteger'),_))).
%%% xsd:negativeInteger => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#negativeInteger', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_negative_integer(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:nonPositiveInteger
typecast_switch('http://www.w3.org/2001/XMLSchema#nonPositiveInteger', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#nonPositiveInteger') :-
    !,
    (   number_string(Cast,Val),
        is_nonpositive_integer(Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#nonPositiveInteger'),_))).
%%% xsd:nonPositiveInteger => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#nonPositiveInteger', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   is_nonpositive_integer(Val)
    ->  format(string(S),'~w',[Val])
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string => xsd:base64Binary
typecast_switch('http://www.w3.org/2001/XMLSchema#base64Binary', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#base64Binary') :-
    !,
    (   (atom_codes(Val,C), phrase(base64Binary,C,[]))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#base64Binary'),_))).
%%% xsd:base64Binary => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#base64Binary', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:hexBinary
typecast_switch('http://www.w3.org/2001/XMLSchema#hexBinary', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#hexBinary') :-
    !,
    (   (atom_codes(Val,C), phrase(hexBinary,C,[]))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#hexBinary'),_))).
%%% xsd:hexBinary => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#hexBinary', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:anyURI
typecast_switch('http://www.w3.org/2001/XMLSchema#anyURI', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#anyURI') :-
    !,
    (   is_absolute_url(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#anyURI'),_))).
%%% xsd:anyURI => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#anyURI', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:language
typecast_switch('http://www.w3.org/2001/XMLSchema#language', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#language') :-
    !,
    (   (atom_codes(Val,C), phrase(language, C))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#language'),_))).
%%% xsd:language => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#language', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:normalizedString
typecast_switch('http://www.w3.org/2001/XMLSchema#normalizedString', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#normalizedString') :-
    !,
    (   (atom_codes(Val,C), phrase(normalizedString,C))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#normalizedString'),_))).
%%% xsd:normalizedString => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#normalizedString', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:token
typecast_switch('http://www.w3.org/2001/XMLSchema#token', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#token') :-
    !,
    (   (atom_codes(Val,C), phrase(normalizedString,C))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#token'),_))).
%%% xsd:token => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#token', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:NMTOKEN
typecast_switch('http://www.w3.org/2001/XMLSchema#NMTOKEN', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#NMTOKEN') :-
    !,
    (   (atom_codes(Val,C), phrase(nmtoken,C))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#NMTOKEN'),_))).
%%% xsd:NMTOKEN => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#NMTOKEN', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:Name
typecast_switch('http://www.w3.org/2001/XMLSchema#Name', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#Name') :-
    !,
    (   (atom_codes(Val,C), phrase(name,C))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#Name'),_))).
%%% xsd:Name => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#Name', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => xsd:NCName
typecast_switch('http://www.w3.org/2001/XMLSchema#NCName', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#NCName') :-
    !,
    (   (atom_codes(Val,C), phrase(ncname,C))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#NCName'),_))).
%%% xsd:NCName => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#NCName', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:string => rdf:PlainLiteral
typecast_switch('http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val@'') :-
    !,
    (   string(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral'),_))).
%%% rdf:PlainLiteral => xsd:string (downcast)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#NCName', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% xsd:dateTime => timestamp (xsd:decimal)
typecast_switch('http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#dateTime', date_time(Y,M,D,HH,MM,SS,0,Offset), _,
                Num^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    date_time_stamp(date(Y,M,D,HH,MM,SS,Offset,-,-), Num).
%%% xsd:integer => xdd:integerRange
typecast_switch('http://terminusdb.com/schema/xdd#integerRange', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Cast) :-
    integer(Val),
    !,
    Cast = Val^^'http://terminusdb.com/schema/xdd#integerRange'.
%%% xsd:decimal => xdd:decimalRange
typecast_switch('http://terminusdb.com/schema/xdd#decimalRange', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Cast) :-
    number(Val),
    !,
    Cast = Val^^'http://terminusdb.com/schema/xdd#decimalRange'.
%%%
%%% Numeric Downcasting
%%%
typecast_switch('http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#integer') :-
    !,
    (   integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#integer'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#double', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#double') :-
    !,
    (   number(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#float', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#float') :-
    !,
    (   number(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#float'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#byte', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#byte') :-
    !,
    (   is_byte(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#byte'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#short', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#short') :-
    !,
    (   is_short(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#short'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#int', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#int') :-
    !,
    (   is_int(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#int'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#long', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#long') :-
    !,
    (   is_long(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#long'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedByte', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#unsignedByte') :-
    !,
    (   is_unsigned_byte(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedByte'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedShort', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#unsignedShort') :-
    !,
    (   is_unsigned_short(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedShort'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedInt', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#unsignedInt') :-
    !,
    (   is_unsigned_int(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedInt'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#unsignedLong', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#unsignedLong') :-
    !,
    (   is_unsigned_long(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#unsignedLong'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#positiveInteger', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#positiveInteger') :-
    !,
    (   is_positive_integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#positiveInteger'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#nonNegativeInteger', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger') :-
    !,
    (   is_nonnegative_integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#negativeInteger', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#negativeInteger') :-
    !,
    (   is_negative_integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#negativeInteger'),_))
    ).
typecast_switch('http://www.w3.org/2001/XMLSchema#nonPositiveInteger', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#nonPositiveInteger') :-
    !,
    (   is_nonpositive_integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#nonPositiveInteger'),_))
    ).
%%% xsd:anyURI => xdd:url --- Special cast as technically xsd:anyURI is not a string
typecast_switch('http://terminusdb.com/schema/xdd#url','http://www.w3.org/2001/XMLSchema#anyURI', Val, _, Val^^'http://terminusdb.com/schema/xdd#url') :-
    !.
%%% xsd:dateTimeStamp => xsd:dateTime --- indistinguishable
typecast_switch('http://www.w3.org/2001/XMLSchema#dateTimeStamp','http://www.w3.org/2001/XMLSchema#anyURI', Val, _, Val^^'http://terminusdb.com/schema/xdd#dateTimeStamp') :-
    !.
%%% xsd:dateTime => xsd:dateTimeStamp --- indistinguishable
typecast_switch('http://www.w3.org/2001/XMLSchema#dateTime','http://www.w3.org/2001/XMLSchema#dateTimeStamp', Val, _, Val^^'http://terminusdb.com/schema/xdd#dateTime') :-
    !.
%%% xsd:string => xsd:anySimpleType
typecast_switch('http://www.w3.org/2001/XMLSchema#anySimpleType','http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#anySimpleType') :-
    !.
%%% xsd:anySimpleType => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string','http://www.w3.org/2001/XMLSchema#anySimpleType', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    !.
%%% Anything => xsd:anySimpleType (via cast to string first...)
typecast_switch('http://www.w3.org/2001/XMLSchema#anySimpleType',Type, Val, _, Res^^'http://www.w3.org/2001/XMLSchema#anySimpleType') :-
    !,
    typecast_switch('http://www.w3.org/2001/XMLSchema#string',Type, Val, _, Res^^_).
%%% xsd:anySimpleType => Anything (via cast to string first...)
typecast_switch(Type, 'http://www.w3.org/2001/XMLSchema#anySimpleType',Val, _, Cast) :-
    !,
    typecast_switch(Type,'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast).


:- begin_tests(typecast).

:- use_module(core(util/test_utils)).

test(anyURI_url,[]) :-
    typecast("terminusdb:///schema#gitHub_user_html_url"^^'http://www.w3.org/2001/XMLSchema#anyURI', 'http://terminusdb.com/schema/xdd#url', [], "terminusdb:///schema#gitHub_user_html_url"^^'http://terminusdb.com/schema/xdd#url').

test(decimal_long,[]) :-
    typecast(12^^'http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#long', [], 12^^'http://www.w3.org/2001/XMLSchema#long').

test(string_long,[]) :-
    typecast("12"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#long', [], 12^^'http://www.w3.org/2001/XMLSchema#long').

test(string_dateTime,[]) :-
    typecast("2012-10-09T00:00:00Z"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#dateTime', [], date_time(2012, 10, 9, 0, 0, 0, 0)^^'http://www.w3.org/2001/XMLSchema#dateTime').

test(dateTime_string,[]) :-
    typecast(date_time(2012, 10, 9, 0, 0, 0, 0)^^'http://www.w3.org/2001/XMLSchema#dateTime', 'http://www.w3.org/2001/XMLSchema#string', [], "2012-10-09T00:00:00Z"^^'http://www.w3.org/2001/XMLSchema#string').

test(boolean_string, []) :-
    typecast(true^^'http://www.w3.org/2001/XMLSchema#boolean', 'http://www.w3.org/2001/XMLSchema#string', [], "true"^^'http://www.w3.org/2001/XMLSchema#string'),
    typecast(false^^'http://www.w3.org/2001/XMLSchema#boolean', 'http://www.w3.org/2001/XMLSchema#string', [], "false"^^'http://www.w3.org/2001/XMLSchema#string').

test(string_boolean, []) :-
    typecast("true"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#boolean', [], true^^'http://www.w3.org/2001/XMLSchema#boolean'),
    typecast("1"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#boolean', [], true^^'http://www.w3.org/2001/XMLSchema#boolean'),
    typecast("false"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#boolean', [], false^^'http://www.w3.org/2001/XMLSchema#boolean'),
    typecast("0"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#boolean', [], false^^'http://www.w3.org/2001/XMLSchema#boolean').

test(node_string, []) :-
    typecast('https://bar/foo', 'http://www.w3.org/2001/XMLSchema#string', [], Cast),
    Cast = "https://bar/foo"^^'http://www.w3.org/2001/XMLSchema#string'.

test(string_node, []) :-
    /* This is clearly terrifying.  Should we not restrict to plausible castables?
       ... or some restriction on the form of URIs? */
    typecast("http://something"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2002/07/owl#Thing', [], Cast),
    Cast = 'http://something'.

test(cast_to_unknown, [error(unknown_type_casting_error(1^^'http://www.w3.org/2001/XMLSchema#boolean', unknown))]) :-
    typecast(1^^'http://www.w3.org/2001/XMLSchema#boolean',
             'unknown', [], _).

test(bad_upcast_to_nonnegative, [error(casting_error(_,_),_)]) :-
    typecast(-1^^'http://www.w3.org/2001/XMLSchema#integer',
             'http://www.w3.org/2001/XMLSchema#nonNegativeInteger', [], _).

test(negative_decimal_round_trip, []) :-
    findall(
        Term,
        (   basetype_subsumption_of(Source_Type,'http://www.w3.org/2001/XMLSchema#decimal'),
            catch(
                (   Number = (-1^^Source_Type),
                    typecast(Number,'http://www.w3.org/2001/XMLSchema#string', [], String),
                    typecast(String,Source_Type, [], Number),
                    throw(Source_Type)
                ),
                Term,
                true)
        ),
        Terms),
    sort(Terms, Sorted),
    Sorted = [
        'http://www.w3.org/2001/XMLSchema#byte',
        'http://www.w3.org/2001/XMLSchema#decimal',
        'http://www.w3.org/2001/XMLSchema#int',
        'http://www.w3.org/2001/XMLSchema#integer',
        'http://www.w3.org/2001/XMLSchema#long',
        'http://www.w3.org/2001/XMLSchema#negativeInteger',
        'http://www.w3.org/2001/XMLSchema#nonPositiveInteger',
        'http://www.w3.org/2001/XMLSchema#short',
        error(casting_error("-1",'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),_),
        error(casting_error("-1",'http://www.w3.org/2001/XMLSchema#positiveInteger'),_),
        error(casting_error("-1",'http://www.w3.org/2001/XMLSchema#unsignedByte'),_),
        error(casting_error("-1",'http://www.w3.org/2001/XMLSchema#unsignedInt'),_),
        error(casting_error("-1",'http://www.w3.org/2001/XMLSchema#unsignedLong'),_),
        error(casting_error("-1",'http://www.w3.org/2001/XMLSchema#unsignedShort'),_)].


test(positive_decimal_round_trip, []) :-
    findall(
        Term,
        (   basetype_subsumption_of(Source_Type,'http://www.w3.org/2001/XMLSchema#decimal'),
            catch(
                (   Number = (1^^Source_Type),
                    typecast(Number,'http://www.w3.org/2001/XMLSchema#string', [], String),
                    typecast(String,Source_Type, [], Number),
                    throw(Source_Type)
                ),
                Term,
                true)
        ),
        Terms),
    sort(Terms, Sorted),
    Sorted = [
        'http://www.w3.org/2001/XMLSchema#byte',
        'http://www.w3.org/2001/XMLSchema#decimal',
        'http://www.w3.org/2001/XMLSchema#int',
        'http://www.w3.org/2001/XMLSchema#integer',
        'http://www.w3.org/2001/XMLSchema#long',
        'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
        'http://www.w3.org/2001/XMLSchema#positiveInteger',
        'http://www.w3.org/2001/XMLSchema#short',
        'http://www.w3.org/2001/XMLSchema#unsignedByte',
        'http://www.w3.org/2001/XMLSchema#unsignedInt',
        'http://www.w3.org/2001/XMLSchema#unsignedLong',
        'http://www.w3.org/2001/XMLSchema#unsignedShort',
        error(casting_error("1",'http://www.w3.org/2001/XMLSchema#negativeInteger'),_),
        error(casting_error("1",'http://www.w3.org/2001/XMLSchema#nonPositiveInteger'),_)
    ].

test(gyear_to_string, []) :-
    typecast(gyear(1990,0.0)^^'http://www.w3.org/2001/XMLSchema#gYear',
             'http://www.w3.org/2001/XMLSchema#string', [], "1990"^^'http://www.w3.org/2001/XMLSchema#string').

test(float_cast, []) :-
    typecast("0.5679"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal',
             [],
             0.5679^^'http://www.w3.org/2001/XMLSchema#decimal').

:- end_tests(typecast).
