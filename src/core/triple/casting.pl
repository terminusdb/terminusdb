:- module(casting,[
              typecast/4,
              typecast_switch/5,
              string_decimal_to_rational/2,
              rational_to_decimal_string/3,
              decimal_precision/1
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

/*
 * decimal_precision(-Digits) is det.
 *
 * Standard precision for xsd:decimal string serialization.
 * Per XSD decimal specification: 20 digits of precision.
 * This constant is used for rational-to-string conversions.
 */
decimal_precision(20).

:- use_module(core(query/jsonld)). % dubious. we should not be importing query stuff here.

:- use_module(library(lists)).

:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(url)).
:- use_module(library(http/json), [atom_json_dict/3]).

/*
 * string_decimal_to_rational(+String, -Rational) is semidet.
 *
 * Parse decimal string to rational number for arbitrary precision.
 * Uses GMP-backed rationals to avoid floating-point precision loss.
 * The rational is stored with metadata about original precision.
 *
 * Examples:
 *   string_decimal_to_rational("123.456", R) => R = 123456 rdiv 1000
 *   string_decimal_to_rational("0.1", R) => R = 1 rdiv 10
 */
string_decimal_to_rational(String, Rational) :-
    % Handle negative numbers
    (   sub_string(String, 0, 1, _, "-")
    ->  sub_string(String, 1, _, 0, PositiveString),
        string_decimal_to_rational(PositiveString, PositiveRat),
        Rational is -PositiveRat
    ;   % Canonicalize: add leading zero if starts with decimal point
        % ".1" -> "0.1" per XSD canonical form
        (   sub_string(String, 0, 1, _, ".")
        ->  string_concat("0", String, CanonicalString),
            string_decimal_to_rational(CanonicalString, Rational)
        ;   % Split on decimal point to preserve original precision
            (   split_string(String, ".", "", [IntStr, FracStr])
            ->  % Has decimal point - construct numerator from string parts
                % CRITICAL: Parse as arbitrary precision integers, NOT floats
                % This preserves full precision (e.g., 20+ digits)
                (   IntStr = ""
                ->  % Empty integer part means leading decimal point (already handled above)
                    IntPart = 0
                ;   atom_number(IntStr, IntPart)
                ),
                string_length(FracStr, FracLen),
                % Parse fractional part as integer (don't convert to float!)
                atom_number(FracStr, FracPart),
                Denominator is 10^FracLen,
                Numerator is IntPart * Denominator + FracPart,
                Rational is Numerator rdiv Denominator
            ;   % No decimal point, just an integer
                atom_number(String, Rational)
            )
        )
    ).

/*
 * rational_to_decimal_string(+Rational, -String, +MaxDecimals) is det.
 *
 * Convert rational to decimal string with up to MaxDecimals precision.
 * Per XSD_DECIMAL_SPECIFICATION.md: compute 20 digits, preserve all significant digits.
 */
rational_to_decimal_string(Rational, String, MaxDecimals) :-
    (   rational(Rational)
    ->  % Use exact rational arithmetic
        (   Rational < 0
        ->  Sign = "-",
            AbsRational is abs(Rational)
        ;   Sign = "",
            AbsRational = Rational
        ),
        % Extract numerator and denominator (may be simplified by GCD)
        rational(AbsRational, Num, Den),
        % Integer part
        IntPart is Num // Den,
        % Fractional part
        Remainder is Num mod Den,
        (   Remainder =:= 0
        ->  % No fractional part
            format(string(String), "~w~w", [Sign, IntPart])
        ;   % Compute MaxDecimals digits of precision WITH ROUNDING
            Multiplier is 10^MaxDecimals,
            % Round to nearest: add 0.5 before truncating
            % (Remainder * Multiplier * 2 + Den) // (Den * 2) achieves rounding
            FracDigits is (Remainder * Multiplier * 2 + Den) // (Den * 2),
            % Format with leading zeros if needed, then trim trailing zeros
            format_and_trim_decimal(FracDigits, MaxDecimals, FracStr),
            format(string(String), "~w~w.~w", [Sign, IntPart, FracStr])
        )
    ;   number(Rational)
    ->  % Regular number - format as-is
        format(string(String), "~w", [Rational])
    ;   throw(error(type_error(number, Rational), _))
    ).

/*
 * format_and_trim_decimal(+FracDigits, +MaxLen, -FracStr) is det.
 *
 * Format fractional digits with leading zeros, then trim trailing zeros.
 * Preserves all significant digits (non-zero or zeros between non-zeros).
 */
format_and_trim_decimal(FracDigits, MaxLen, FracStr) :-
    % Pad with leading zeros to MaxLen
    format(string(Padded), "~w", [FracDigits]),
    atom_length(Padded, ActualLen),
    (   ActualLen < MaxLen
    ->  ZeroCount is MaxLen - ActualLen,
        format(string(Zeros), "~*c", [ZeroCount, 0'0]),
        string_concat(Zeros, Padded, FullStr)
    ;   FullStr = Padded
    ),
    % Trim trailing zeros
    atom_codes(FullStr, Codes),
    reverse(Codes, RevCodes),
    trim_trailing_zeros(RevCodes, TrimmedRev),
    reverse(TrimmedRev, TrimmedCodes),
    atom_codes(FracStr, TrimmedCodes).

trim_trailing_zeros([0'0|Rest], Trimmed) :- !, trim_trailing_zeros(Rest, Trimmed).
trim_trailing_zeros(Codes, Codes).

/*
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Hint, Cast) :-
    (   var(Val)
    ->  format(atom(M), 'Variable unbound in typecast to ~q', [Type]),
        throw(error(M))
    ;   % Expand prefixes if provided (e.g., xsd:decimal → full URI)
        (   option(prefixes(Prefixes), Hint)
        ->  prefix_expand(Type, Prefixes, Expanded_Type)
        ;   Expanded_Type = Type
        ),
        (   \+ (   base_type(Expanded_Type)
               ;   Expanded_Type = 'http://www.w3.org/2002/07/owl#Thing'
               ;   Expanded_Type = 'http://terminusdb.com/schema/sys#Top'
               ;   Expanded_Type = 'http://terminusdb.com/schema/sys#Dictionary')
        ->  throw(error(unknown_type_casting_error(Val, Expanded_Type), _))
        ;   Val = Bare_Literal^^Source_Type,
            (   % Try explicit typecast rule FIRST (e.g., xdd:json → xsd:string)
                catch(typecast_switch(Expanded_Type,Source_Type,Bare_Literal,Hint,Cast), _, fail)
            ->  true
            ;   basetype_subsumption_of(Source_Type,'http://www.w3.org/2001/XMLSchema#string')
                % Upcast to xsd:string for downcast
            ->  typecast_switch(Expanded_Type,'http://www.w3.org/2001/XMLSchema#string',Bare_Literal,Hint,Cast)
            ;   basetype_subsumption_of(Source_Type,'http://www.w3.org/2001/XMLSchema#decimal')
                % Upcast to xsd:decimal for downcast
            ->  typecast_switch(Expanded_Type,'http://www.w3.org/2001/XMLSchema#decimal',Bare_Literal,Hint,Cast)
            ;   typecast_switch(Expanded_Type,Source_Type,Bare_Literal,Hint,Cast)
            )
        ->  true
        ;   Val = Bare_Literal@_, % upcast to string
            typecast_switch(Expanded_Type,'http://www.w3.org/2001/XMLSchema#string',Bare_Literal,Hint,Cast)
        ->  true
        ;   atom(Val)
        ->  typecast_switch(Expanded_Type,'http://www.w3.org/2002/07/owl#Thing',Val,Hint,Cast)
        ;   string(Val)
        ->  typecast_switch(Expanded_Type,'http://www.w3.org/2001/XMLSchema#string',Val,Hint,Cast)
        ;   is_dict(Val)
        ->  typecast_switch(Expanded_Type,'http://terminusdb.com/schema/sys#Dictionary',Val,Hint,Cast)
        ;   number(Val)  % Handle raw numbers (including rationals from sys:JSON)
        ->  typecast_switch(Expanded_Type,'http://www.w3.org/2001/XMLSchema#decimal',Val,Hint,Cast)
        ;   throw(error(casting_error(Val, Type), _))
        )
    ).

/*
 * typecast_switch(+Target_Type, +Source_Type, +Bare_Literal, +Hint, -Casted_Literal) is det.
 *
 * Casts from source type to target type.
 */
%%% xsd:string Self Cast (handles atoms)
%%% NOTE: Should not be necessary as we shouldn't be able to have an atom in the first place
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val_String^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(Val),
    !,
    atom_string(Val,Val_String).
%%% xsd:string Self Cast (handles dicts from xdd:json subsumption)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string', Dict, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    is_dict(Dict),
    !,
    (   (catch(atom_json_dict(Val,Dict,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(Dict,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string Self Cast (handles lists/arrays from xdd:json subsumption)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string', List, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    is_list(List),
    !,
    (   (catch(atom_json_dict(Val,List,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(List,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string Self Cast (handles primitives from xdd:json subsumption - atoms for strings, booleans, null)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string', Atom, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(Atom),
    Atom \= '',  % Not the empty atom case (handled elsewhere)
    !,
    (   (catch(atom_json_dict(Val,Atom,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(Atom,'http://www.w3.org/2001/XMLSchema#string'),_))).
%%% xsd:string Self Cast (handles numbers from xdd:json subsumption)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string', Num, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    number(Num),
    !,
    (   (catch(atom_json_dict(Val,Num,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(Num,'http://www.w3.org/2001/XMLSchema#string'),_))).
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
%%% xsd:string => sys:Dictionary (parse JSON string to dict, or pass through if already dict)
typecast_switch('http://terminusdb.com/schema/sys#Dictionary', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Dict) :-
    !,
    (   is_dict(Val)
    ->  Dict = Val  % Already a dict (from xdd:json subsumption), just pass through
    ;   (catch(atom_json_dict(Val,Parsed,[]),_,fail), is_dict(Parsed))  % Parse string and validate it's a dict
    ->  Dict = Parsed
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/sys#Dictionary'),_))).
%%% xdd:json => sys:Dictionary (unwrap typed dict to plain dict, MUST be a dict)
typecast_switch('http://terminusdb.com/schema/sys#Dictionary', 'http://terminusdb.com/schema/xdd#json', Dict, _, Dict) :-
    !,
    (   is_dict(Dict)
    ->  true
    ;   throw(error(casting_error(Dict,'http://terminusdb.com/schema/sys#Dictionary'),_))).
%%% sys:Dictionary => xdd:json (wrap plain dict with type)
typecast_switch('http://terminusdb.com/schema/xdd#json', 'http://terminusdb.com/schema/sys#Dictionary', Dict, _, Dict^^'http://terminusdb.com/schema/xdd#json') :-
    is_dict(Dict),
    !.
%%% sys:Dictionary => xsd:string (serialize dict to JSON string)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/sys#Dictionary', Dict, _, Val^^'http://www.w3.org/2001/XMLSchema#string') :-
    is_dict(Dict),
    !,
    (   (catch(atom_json_dict(Val,Dict,[]),_,fail))
    ->  true
    ;   throw(error(casting_error(Dict,'http://www.w3.org/2001/XMLSchema#string'),_))).
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
% Parse as rational for arbitrary precision (GMP-backed)
typecast_switch('http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    (   string_decimal_to_rational(Val, Casted)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#decimal'),_))).
%%% xsd:decimal => xsd:string
% Convert with standard decimal precision per XSD_DECIMAL_SPECIFICATION.md
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#decimal', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   (rational(Val) ; number(Val))
    ->  decimal_precision(Precision),
        rational_to_decimal_string(Val, S, Precision)
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
% CRITICAL: Convert to float for IEEE 754 canonicalization
% This ensures "33" and "33.0" both become the same float value
typecast_switch('http://www.w3.org/2001/XMLSchema#double', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#double') :-
    !,
    (   number_string(NumericValue,Val)
    ->  Casted is float(NumericValue)  % Convert to float for canonicalization
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))).
%%% xsd:double => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#double', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   number(Val)
    ->  % CRITICAL FIX: Ensure doubles always have decimal point
        % xsd:double values must have .0 for whole numbers (IEEE 754 semantics)
        (   (float(Val) ; integer(Val)),
            Val =:= floor(Val)
        ->  % Whole number: add .0 suffix
            Truncated is truncate(Val),
            format(string(S), "~w.0", [Truncated])
        ;   % Fractional float: use normal representation
            format(string(S), "~w", [Val])
        )
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))).
%%% xsd:string => xsd:float
% CRITICAL: Convert to float for IEEE 754 canonicalization
% This ensures "33" and "33.0" both become the same float value
typecast_switch('http://www.w3.org/2001/XMLSchema#float', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#float') :-
    !,
    (   number_string(NumericValue,Val)
    ->  Casted is float(NumericValue)  % Convert to float for canonicalization
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#float'),_))).
%%% xsd:float => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#float', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   number(Val)
    ->  % CRITICAL FIX: Ensure floats always have decimal point
        % xsd:float values must have .0 for whole numbers (IEEE 754 semantics)
        (   (float(Val) ; integer(Val)),
            Val =:= floor(Val)
        ->  % Whole number: add .0 suffix
            Truncated is truncate(Val),
            format(string(S), "~w.0", [Truncated])
        ;   % Fractional float: use normal representation
            format(string(S), "~w", [Val])
        )
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
%%%
%%% Cross-Family Numeric Typecasts
%%%

%%% xsd:double => xsd:decimal
typecast_switch('http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#double', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    (   float(Val)
    ->  Casted = Val  % Convert IEEE 754 float to rational (via Prolog's automatic conversion)
    ;   integer(Val)
    ->  Casted = Val
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#decimal'),_))
    ).

%%% xsd:float => xsd:decimal
typecast_switch('http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#float', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    (   float(Val)
    ->  Casted = Val  % Convert IEEE 754 float to rational
    ;   integer(Val)
    ->  Casted = Val
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#decimal'),_))
    ).

%%% xsd:double => xsd:integer
typecast_switch('http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#double', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#integer') :-
    !,
    (   number(Val),
        Val =:= floor(Val)  % Must be a whole number
    ->  Casted is truncate(Val)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#integer'),_))
    ).

%%% xsd:float => xsd:integer
typecast_switch('http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#float', Val, _, Casted^^'http://www.w3.org/2001/XMLSchema#integer') :-
    !,
    (   number(Val),
        Val =:= floor(Val)  % Must be a whole number
    ->  Casted is truncate(Val)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#integer'),_))
    ).

%%% xsd:double => xsd:float
typecast_switch('http://www.w3.org/2001/XMLSchema#float', 'http://www.w3.org/2001/XMLSchema#double', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#float') :-
    !,
    (   float(Val)
    ->  true  % Both are IEEE 754 floats in Prolog
    ;   integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#float'),_))
    ).

%%% xsd:float => xsd:double
typecast_switch('http://www.w3.org/2001/XMLSchema#double', 'http://www.w3.org/2001/XMLSchema#float', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#double') :-
    !,
    (   float(Val)
    ->  true  % Both are IEEE 754 floats in Prolog
    ;   integer(Val)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))
    ).

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
    typecast(gyear(1990,0)^^'http://www.w3.org/2001/XMLSchema#gYear',
             'http://www.w3.org/2001/XMLSchema#string', [], "1990"^^'http://www.w3.org/2001/XMLSchema#string').

test(string_to_gyear, []) :-
    typecast("1990"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#gYear', [],
             gyear(1990,0)^^'http://www.w3.org/2001/XMLSchema#gYear').

test(integer_to_gyear, []) :-
    typecast(1990^^'http://www.w3.org/2001/XMLSchema#integer',
             'http://www.w3.org/2001/XMLSchema#gYear', [],
             gyear(1990,0)^^'http://www.w3.org/2001/XMLSchema#gYear').

test(float_cast, []) :-
    % Updated for decimal precision: xsd:decimal now uses rationals, not floats
    typecast("0.5679"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal',
             [],
             Result^^'http://www.w3.org/2001/XMLSchema#decimal'),
    % Check it's a rational with correct value
    rational(Result),
    Result =:= 5679 rdiv 10000.

test(decimal_to_string_cast, []) :-
    % Test round-trip: decimal to string preserves precision
    typecast("0.5679"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal',
             [],
             Decimal^^'http://www.w3.org/2001/XMLSchema#decimal'),
    typecast(Decimal^^'http://www.w3.org/2001/XMLSchema#decimal',
             'http://www.w3.org/2001/XMLSchema#string',
             [],
             "0.5679"^^'http://www.w3.org/2001/XMLSchema#string').

:- end_tests(typecast).
