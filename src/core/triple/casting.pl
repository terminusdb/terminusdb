:- module(casting,[
              typecast/3,
              typecast/4,
              typecast_switch/5,
              context_hint_for_type/3,
              context_prefix_datatype/1,
              default_type_expand/2,
              string_decimal_to_rational/2,
              rational_to_decimal_string/3,
              decimal_precision/1,
              xsd_version/1,
              xsd_version_param/2,
              with_xsd_version/2,
              decimal_integer_lexical_hook/3,
              normalise_interval_start/2,
              normalise_interval_end/2,
              interval_as_term/2,
              interval_term_lexical/2,
              interval_component_string/3,
              interval_component_end_string/3
          ]).

:- discontiguous typecast_switch/5.

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
:- multifile decimal_precision/1.
decimal_precision(20).

/**
 * Request-scoped XSD canonicalization version.
 *
 * The endpoints that render literals scope each request with
 * with_xsd_version/2; absent a scope the default is '1.1'. The flag is
 * thread-local so concurrent requests cannot interfere.
 */
:- thread_local request_xsd_version_/1.
:- meta_predicate with_xsd_version(+, 0).

% Extension hook for downstream editions to override the lexical form of
% integer-valued decimals; no clauses in community.
:- multifile decimal_integer_lexical_hook/3.

xsd_version(Version) :-
    (   request_xsd_version_(Version)
    ->  true
    ;   Version = '1.1').

with_xsd_version(Version, Goal) :-
    setup_call_cleanup(
        asserta(request_xsd_version_(Version)),
        Goal,
        retractall(request_xsd_version_(Version))).

/**
 * xsd_version_param(+Value, -Version) is det.
 *
 * Validate an 'xsd' endpoint parameter. '1.0' renders decimal canonical
 * forms per XSD 1.0 and requires enterprise edition; '1.1' is the
 * XSD 1.1 canonical form and the default.
 */
xsd_version_param(Version, Version) :-
    memberchk(Version, ['1.0', '1.1']),
    !,
    (   Version = '1.0',
        \+ config:is_enterprise
    ->  throw(error(unsupported_xsd_version('1.0'), _))
    ;   true).
xsd_version_param(Version, _) :-
    throw(error(bad_parameter_type(xsd, "1.0 or 1.1", Version), _)).

:- use_module(core(query/jsonld)). % dubious. we should not be importing query stuff here.

:- use_module(library(lists)).

:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(url), [is_absolute_url/1]).
:- use_module(library(uri), [uri_components/2]).
:- use_module(core(query/global_prefixes), [default_prefixes/1, global_prefixes/2]).
:- use_module(library(json), [atom_json_dict/3]).

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
        ->  % No fractional part: canonical drops the fraction.
            (   decimal_integer_lexical_hook(Sign, IntPart, String)
            ->  true
            ;   format(string(String), "~w~w", [Sign, IntPart]))
        ;   % Compute MaxDecimals digits of precision WITH ROUNDING
            Multiplier is 10^MaxDecimals,
            % Round to nearest: add 0.5 before truncating
            % (Remainder * Multiplier * 2 + Den) // (Den * 2) achieves rounding
            FracDigits is (Remainder * Multiplier * 2 + Den) // (Den * 2),
            (   FracDigits =:= Multiplier
            ->  % fraction rounded up past the decimal boundary - carry
                Carried is IntPart + 1,
                format(string(String), "~w~w", [Sign, Carried])
            ;   % Format with leading zeros if needed, then trim trailing zeros
                format_and_trim_decimal(FracDigits, MaxDecimals, FracStr),
                % a fraction that rounds to all zeros yields an empty
                % FracStr; emitting "2." would be malformed, so drop the dot
                (   FracStr = ''
                ->  format(string(String), "~w~w", [Sign, IntPart])
                ;   format(string(String), "~w~w.~w", [Sign, IntPart, FracStr])
                )
            )
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

/* context_prefix_datatype(+Type) is semidet.
 *
 * The datatype targets whose typecast_switch arms read the `prefixes(Dict)`
 * hint: the expanded names (xsd:QName, xbrl:SQName) whose lexicals bind
 * `prefix:local` through the map, and the IRI targets (owl:Thing, sys:Top)
 * that expand their value with it. Spelled `p:local` forms count too — the
 * database map may bind their prefix where the global defaults cannot.
 */
:- table context_prefix_datatype/1 as private.
context_prefix_datatype(_:Local) :-
    memberchk(Local, ['QName', 'SQName', 'Thing', 'Top']),
    !.
context_prefix_datatype(Type) :-
    nonvar(Type),
    (   atom(Type)
    ->  Type_Atom = Type
    ;   atom_string(Type_Atom, Type)
    ),
    (   memberchk(Type_Atom, ['http://www.w3.org/2001/XMLSchema#QName',
                              'https://xbrl.org/2021SQName',
                              'http://www.w3.org/2002/07/owl#Thing',
                              'http://terminusdb.com/schema/sys#Top'])
    ->  true
    ;   \+ uri_has_protocol(Type_Atom),
        uri_has_prefix_unsafe(Type_Atom, Match),
        memberchk(Match.suffix, ["QName", "SQName", "Thing", "Top"])
    ).

/* context_hint_for_type(+Type, +Context, -Hint) is det.
 *
 * [prefixes(Context)] for the datatypes that can read the map, [] for the
 * rest — callers pass the result straight to typecast/4 so a per-field
 * prefix-map lookup is only paid where the cast can use it.
 */
context_hint_for_type(Type, Context, Hint) :-
    (   context_prefix_datatype(Type)
    ->  Hint = [prefixes(Context)]
    ;   Hint = []
    ).

/* default_type_expand(+Type, -Expanded) is det.
 *
 * Resolve a datatype name against the global prefixes only — pure Prolog,
 * no FFI. `P:S` compounds, atom and string `p:s` spellings expand through
 * global_prefixes/2; everything else passes through and the caller's own
 * validation reports it.
 */
:- table default_type_expand/2 as private.
default_type_expand(Type, Expanded) :-
    (   Type = (Prefix : Local),
        atom(Prefix),
        atomic(Local)
    ->  (   global_prefixes(Prefix, Base)
        ->  atomic_list_concat([Base, Local], Expanded)
        ;   Expanded = Type )
    ;   (   atom(Type)
        ->  Type_Atom = Type
        ;   string(Type)
        ->  atom_string(Type_Atom, Type)
        ),
        uri_has_prefix(Type_Atom, Match),
        atom_string(Prefix_Atom, Match.prefix),
        global_prefixes(Prefix_Atom, Base)
    ->  atomic_list_concat([Base, Match.suffix], Expanded)
    ;   Expanded = Type
    ).

/*
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Cast) :-
    typecast(Val, Type, [], Cast).

typecast(Val, Type, Hint, Cast) :-
    (   var(Val)
    ->  format(atom(M), 'Variable unbound in typecast to ~q', [Type]),
        throw(error(M))
    ;   % The context map only matters for the datatype arms that read it on
        % the value side; every other target expands against the global
        % defaults — the FFI walks the whole context dict on each call.
        (   option(prefixes(Prefixes), Hint),
            context_prefix_datatype(Type)
        ->  prefix_expand(Type, Prefixes, Expanded_Type)
        ;   default_type_expand(Type, Expanded_Type)
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
        ->  (   % An enum member's canonical string form is its lexical
                % value, not the schema node IRI it is stored as.
                Expanded_Type = 'http://www.w3.org/2001/XMLSchema#string',
                option(enum_members(Members), Hint),
                memberchk(Val-Lexical, Members)
            ->  Cast = Lexical^^'http://www.w3.org/2001/XMLSchema#string'
            ;   typecast_switch(Expanded_Type,'http://www.w3.org/2002/07/owl#Thing',Val,Hint,Cast)
            )
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
%%% xdd:dateTimeInterval — datatype contract:
%%% terminusdb-enterprise/docs/specifications/xdd-datetimeinterval.md
%%% (spelling is the literal; bare T = point [T,T); bare D = the local day
%%% [D,D+1); date/date inclusive; zoneless stays local; flag 3 = point.)
%%% xdd:dateRange => xdd:dateTimeInterval
typecast_switch('http://terminusdb.com/schema/xdd#dateTimeInterval', 'http://terminusdb.com/schema/xdd#dateRange', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateTimeInterval') :-
    !,
    (   is_date_range(Val),
        Val = date_range(D1,D2)
    ->  % An inclusive date range is the date/date pair — the stored
        % literal keeps the date-kind spellings.
        interval_component_string(D1, none, S1),
        interval_component_string(D2, none, S2),
        format(string(Lex), '~w/~w', [S1,S2]),
        '$xdd':interval_parse(Lex, Cast)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateRange'),_))).
%%% xdd:dateTimeInterval => xdd:dateRange
typecast_switch('http://terminusdb.com/schema/xdd#dateRange', 'http://terminusdb.com/schema/xdd#dateTimeInterval', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateRange') :-
    !,
    (   interval_as_term(Val, Term),
        Term = date_time_interval(C1, C2, _, _, _, _)
    ->  component_to_parts(C1, S1, _),
        component_to_parts(C2, S2, NS2),
        % The interval end is exclusive: the last covered instant is
        % end - 1ns. An end with zero nanoseconds lands exactly on a
        % second boundary, so back off one second to find its date.
        (   NS2 > 0
        ->  EStamp = S2
        ;   EStamp is S2 - 1
        ),
        stamp_date_time(S1, date(Y1,M1,D1,_,_,_,0,'UTC',-), 'UTC'),
        stamp_date_time(EStamp, date(Y2,M2,D2,_,_,_,0,'UTC',-), 'UTC'),
        Cast = date_range(date(Y1,M1,D1,0), date(Y2,M2,D2,0))
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateTimeInterval'),_))).
%%% xsd:string => xdd:dateTimeInterval
%%% The Rust lexical grammar is the authority: the canonical spelling IS
%%% the stored value, so Cast is the canonicalised string itself.
typecast_switch('http://terminusdb.com/schema/xdd#dateTimeInterval', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateTimeInterval') :-
    !,
    (   '$xdd':interval_parse(Val, Cast)
    ->  true
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateTimeInterval'),_))
    ).

%%% xsd:date => xdd:dateRange (a date is an inclusive one-day range)
typecast_switch('http://terminusdb.com/schema/xdd#dateRange', 'http://www.w3.org/2001/XMLSchema#date', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateRange') :-
    !,
    (   is_date(Val)
    ->  Cast = date_range(Val, Val)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateRange'),_))).

%%% xsd:date => xdd:dateTimeInterval ([midnight, next midnight), same as
%%% the dateRange arm — a date denotes its whole day)
typecast_switch('http://terminusdb.com/schema/xdd#dateTimeInterval', 'http://www.w3.org/2001/XMLSchema#date', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateTimeInterval') :-
    !,
    (   is_date(Val)
    ->  normalise_interval_start(Val, N1),
        normalise_interval_end(Val, N2),
        interval_component_string(N1, tz(0), S1),
        interval_component_string(N2, tz(0), S2),
        format(string(Lex), '~w/~w', [S1,S2]),
        '$xdd':interval_parse(Lex, Cast)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateTimeInterval'),_))).

%%% xsd:dateTime => xdd:dateTimeInterval
%%% A stored dateTime is already UTC-normalised (zone consumed at parse);
%%% it becomes the zero-length pair [T, T) rendered with 'Z'.
typecast_switch('http://terminusdb.com/schema/xdd#dateTimeInterval', 'http://www.w3.org/2001/XMLSchema#dateTime', Val, _, Cast^^'http://terminusdb.com/schema/xdd#dateTimeInterval') :-
    !,
    (   is_date_time(Val)
    ->  interval_date_time_string(Val, tz(0), S),
        format(string(Lex), '~w/~w', [S,S]),
        '$xdd':interval_parse(Lex, Cast)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateTimeInterval'),_))).

%%% xdd:dateTimeInterval => xsd:dateTime
%%% Only a zero-length interval (start == end) names an instant.
typecast_switch('http://www.w3.org/2001/XMLSchema#dateTime', 'http://terminusdb.com/schema/xdd#dateTimeInterval', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    !,
    (   interval_as_term(Val, date_time_interval(C, C, _, _, _, _)),
        is_date_time(C)
    ->  Cast = C
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#dateTime'),_))).

%%% xdd:dateTimeInterval => xsd:date
%%% Only a zero-length interval at a day boundary names a date.
typecast_switch('http://www.w3.org/2001/XMLSchema#date', 'http://terminusdb.com/schema/xdd#dateTimeInterval', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#date') :-
    !,
    (   interval_as_term(Val, date_time_interval(C, C, _, _, _, _)),
        interval_component_to_date(C, _)
    ->  interval_component_to_date(C, Cast)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#date'),_))).

%%% xdd:dateRange => xsd:date (only a single-day range names a date)
typecast_switch('http://www.w3.org/2001/XMLSchema#date', 'http://terminusdb.com/schema/xdd#dateRange', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#date') :-
    !,
    (   is_date_range(Val),
        Val = date_range(D, D)
    ->  Cast = D
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#date'),_))).

%%% xdd:dateRange => xsd:dateTime (a single-day range names its midnight)
typecast_switch('http://www.w3.org/2001/XMLSchema#dateTime', 'http://terminusdb.com/schema/xdd#dateRange', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    !,
    (   is_date_range(Val),
        Val = date_range(D, D),
        D = date(Y,M,Dy,_)
    ->  Cast = date_time(Y,M,Dy,0,0,0,0)
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#dateTime'),_))).

% A date component keeps its zone; a midnight dateTime becomes the UTC date.
interval_component_to_date(date(Y,M,D,Off), date(Y,M,D,Off)) :- !.
interval_component_to_date(date_time(Y,M,D,0,0,0,0), date(Y,M,D,0)).

parse_date_time_interval(Val, Cast) :-
    atom_codes(Val, Codes),
    phrase(dateTimeInterval(X, Y, Z), Codes),
    !,
    normalise_interval_start(X, NX),
    normalise_interval_end(Y, NY),
    component_tz(X, TzX),
    component_tz(Y, TzY),
    Cast = date_time_interval(NX, NY, Z, explicit, TzX, TzY).
parse_date_time_interval(Val, Cast) :-
    atom_codes(Val, Codes),
    phrase(dateTimeInterval(X, Y), Codes),
    !,
    parse_two_component_interval(X, Y, Cast).
% A bare finite component is a single-instant literal: a dateTime is the
% as-of point [T,T); a bare ISO date is the duration of the day [D,D+1),
% the same interval as the explicit date/date pair (XBRL CSV / XBRL 2.1).
% The point flag keeps the supplied single-component spelling distinct
% from an explicit T/T pair at equal bounds. Matches parse_lexical_interval.
parse_date_time_interval(Val, Cast) :-
    atom_codes(Val, Codes),
    phrase(dateTimeIntervalComponent(X), Codes),
    X \= open,
    X \= unknown,
    \+ is_duration(X),
    !,
    (   X = date(Y,M,D,_)
    ->  % a bare date covers its whole day: keep date components so the
        % stored literal is the supplied date spelling
        NX = X, Tz = none,
        date_time_stamp(date(Y,M,D,0,0,0,0,-,-), Stamp),
        Next is Stamp + 86400,
        stamp_date_time(Next, date(NY,NM,ND,_,_,_,_,'UTC',_), 'UTC'),
        NE = date(NY,NM,ND,0)
    ;   normalise_interval_start(X, NX),
        NE = NX,
        component_tz(X, Tz)),
    compute_duration_between(NX, NE, Dur),
    Cast = date_time_interval(NX, NE, Dur, point, Tz, Tz).

parse_two_component_interval(X, Y, date_time_interval(NX, NY, X, duration_end, TzY, TzY)) :-
    is_duration(X),
    Y \= open,
    Y \= unknown,
    !,
    interval_component_tz(Y, TzY),
    % Anchor the duration on the exclusive (bumped) end: "P1M/2025-12-31"
    % is the month whose last covered day is Dec 31 → [Dec 1, Jan 1).
    % Matches the Rust parser, which bumps the date end before subtracting.
    interval_end_component(Y, NY),
    subtract_duration_from_component(NY, X, NX).
parse_two_component_interval(X, Y, date_time_interval(NX, NY, Y, start_duration, TzX, TzX)) :-
    is_duration(Y),
    X \= open,
    X \= unknown,
    !,
    interval_component_tz(X, TzX),
    interval_start_component(X, NX),
    add_duration_to_component(NX, Y, NY).
parse_two_component_interval(X, Y, date_time_interval(NX, NY, Dur, explicit, TzX, TzY)) :-
    \+ is_duration(X),
    \+ is_duration(Y),
    interval_component_tz(X, TzX),
    interval_component_tz(Y, TzY),
    interval_start_component(X, NX),
    interval_end_component(Y, NY),
    (   (NX == neg_infinity ; NX == unknown ; NY == infinity ; NY == unknown)
    ->  Dur = duration(1,0,0,0,0,0,0,0)
    ;   compute_duration_between(NX, NY, Dur)).

%%% xdd:dateTimeInterval => xsd:string
%%% The stored value is the canonical spelling — a text inner value
%%% re-validates to the canonical form, and a legacy marshalled term
%%% renders then canonicalises through the Rust grammar.
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://terminusdb.com/schema/xdd#dateTimeInterval', Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   (string(Val) ; atom(Val))
    ->  '$xdd':interval_parse(Val, S)
    ;   is_date_time_interval(Val)
    ->  interval_term_lexical(Val, S)
    ;   throw(error(casting_error(Val,'http://terminusdb.com/schema/xdd#dateTimeInterval'),_))
    ).

% interval_as_term(+Value, ?Term) — the marshalled date_time_interval/6
% term for a value: terms pass through, canonical spellings rebuild
% through the DCG. Spellings the term cannot carry (X* endpoints,
% deeper-than-nanos fractions, i64-out years) fail rather than truncate.
% The functor check precedes unification with the caller's pattern —
% head-matching (T,T) would reject a nonzero term before the body ran.
interval_as_term(In, Term) :-
    nonvar(In),
    In = date_time_interval(_,_,_,_,_,_),
    !,
    Term = In.
interval_as_term(In, Term) :-
    (   string(In)
    ->  atom_string(A, In)
    ;   atom(In)
    ->  A = In
    ;   !, fail
    ),
    parse_date_time_interval(A, Term).

% interval_term_lexical(+Term, -Lexical) — canonical spelling for a
% marshalled term: render, then re-validate through the Rust grammar so
% a Prolog arithmetic slip surfaces instead of storing bad bounds.
interval_term_lexical(T, Lex) :-
    interval_to_string(T, Raw),
    '$xdd':interval_parse(Raw, Lex).

interval_to_string(Interval, S) :-
    Interval = date_time_interval(_,_,_,Flag,_,_),
    interval_to_string_as(Interval, Flag, S).

% A point literal renders the supplied single component — the bare
% instant or the bare date — zoneless, Z, or offset alike.
interval_to_string_as(date_time_interval(C,_,_,point,Tz,_), point, S) :-
    !,
    interval_point_string(C, Tz, S).
% An explicit pair keeps both endpoints, even a zero-duration T/T: only
% the point flag collapses to the single spelling.
interval_to_string_as(date_time_interval(C1,C2,_Dur,_Flag,Tz1,Tz2), explicit, S) :-
    !,
    interval_component_string(C1,Tz1,Val1),
    interval_component_end_string(C2,Tz2,Val2),
    format(string(S), '~w/~w', [Val1,Val2]).
interval_to_string_as(date_time_interval(C1,_C2,Dur,_Flag,Tz1,_), start_duration, S) :-
    !,
    interval_component_string(C1,Tz1,Val1),
    interval_component_string(Dur,none,Val2),
    format(string(S), '~w/~w', [Val1,Val2]).
interval_to_string_as(date_time_interval(_C1,C2,Dur,_Flag,_,Tz2), duration_end, S) :-
    !,
    interval_component_string(Dur,none,Val1),
    interval_component_end_string(C2,Tz2,Val2),
    format(string(S), '~w/~w', [Val1,Val2]).

%% Normalise a start component: dates become date_time at 00:00:00 UTC (no bump).
%% 'open' normalises to neg_infinity at the start; 'unknown' stays unknown.
normalise_interval_start(open, neg_infinity) :- !.
normalise_interval_start(neg_infinity, neg_infinity) :- !.
normalise_interval_start(unknown, unknown) :- !.
normalise_interval_start(date(Y,M,D,_Offset), date_time(Y,M,D,0,0,0,0)) :- !.
normalise_interval_start(date_time(Y,Mo,D,H,M,S,NS), date_time(Y,Mo,D,H,M,S,NS)) :- !.
normalise_interval_start(date_time(Y,Mo,D,H,M,S,NS,none), date_time(Y,Mo,D,H,M,S,NS)) :- !.
normalise_interval_start(date_time(Y,Mo,D,H,M,S,NS,Offset), Norm) :- !,
    remove_date_time_offset(Y,Mo,D,H,M,S,NS,Offset,Norm).
normalise_interval_start(duration(Sign,W,Y,Mo,D,H,M,S), duration(Sign,W,Y,Mo,D,H,M,S)) :- !.

%% Normalise an end component: dates become date_time at 00:00:00 UTC on the
%% following day (the "day-after bump"), so that a date-only end is exclusive.
%% 'open' normalises to infinity at the end; 'unknown' stays unknown.
normalise_interval_end(open, infinity) :- !.
normalise_interval_end(infinity, infinity) :- !.
normalise_interval_end(unknown, unknown) :- !.
normalise_interval_end(date(Y,M,D,_Offset), Norm) :- !,
    date_time_stamp(date(Y,M,D,0,0,0,0,-,-), Stamp),
    NextStamp is Stamp + 86400,
    stamp_date_time(NextStamp, date(NY,NM,ND,_H,_Mi,_S,_Off,'UTC',_DST), 'UTC'),
    Norm = date_time(NY,NM,ND,0,0,0,0).
normalise_interval_end(date_time(Y,Mo,D,H,M,S,NS), date_time(Y,Mo,D,H,M,S,NS)) :- !.
normalise_interval_end(date_time(Y,Mo,D,H,M,S,NS,none), date_time(Y,Mo,D,H,M,S,NS)) :- !.
normalise_interval_end(date_time(Y,Mo,D,H,M,S,NS,Offset), Norm) :- !,
    remove_date_time_offset(Y,Mo,D,H,M,S,NS,Offset,Norm).
normalise_interval_end(duration(Sign,W,Y,Mo,D,H,M,S), duration(Sign,W,Y,Mo,D,H,M,S)) :- !.

%% Interval component forms that keep the supplied kind: internal instants
%% stay UTC, but the literal presents local time — a zoneless date is a
%% date, not a Z-suffixed midnight. A zoned date has no lexical form in
%% storage (kind DATE is always zoneless), so it normalises to a date_time
%% carrying its zone rather than silently dropping it.
interval_start_component(date(Y,M,D,0), date(Y,M,D,0)) :- !.
interval_start_component(X, NX) :- normalise_interval_start(X, NX).

% A zoneless date end stores the exclusive day after as a date component —
% [D 00:00, D+1 00:00) covers the supplied date's whole day, and the
% canonical renders it back as the inclusive last day.
interval_end_component(date(Y,M,D,0), date(NY,NM,ND,0)) :- !,
    date_time_stamp(date(Y,M,D,0,0,0,0,-,-), Stamp),
    Next is Stamp + 86400,
    stamp_date_time(Next, date(NY,NM,ND,_,_,_,_,'UTC',_), 'UTC').
interval_end_component(X, NX) :- normalise_interval_end(X, NX).

% kind DATE is zoneless: tz(0) would render a phantom 'Z' the input
% never had.
interval_component_tz(date(_,_,_,0), none) :- !.
interval_component_tz(X, Tz) :- component_tz(X, Tz).

compute_duration_between(Start, End, duration(Sign, 0, 0, 0, Days, Hours, Minutes, Seconds)) :-
    component_to_parts(Start, T1, NS1),
    component_to_parts(End, T2, NS2),
    % Whole seconds as an exact integer difference — stamps are floats,
    % so round() restores the integer, and NS stays integer: anything less
    % exact would collapse the rational seconds back to f64.
    SecDiff is round(T2 - T1),
    NSDiff is NS2 - NS1,
    (   NSDiff < 0
    ->  SecNorm is SecDiff - 1, NSNorm is NSDiff + 1000000000
    ;   SecNorm = SecDiff, NSNorm = NSDiff
    ),
    (   SecNorm < 0
    ->  Sign = -1, AbsDiff is -SecNorm
    ;   Sign = 1, AbsDiff = SecNorm
    ),
    Days is truncate(AbsDiff / 86400),
    Rem1 is AbsDiff - Days * 86400,
    Hours is truncate(Rem1 / 3600),
    Rem2 is Rem1 - Hours * 3600,
    Minutes is truncate(Rem2 / 60),
    Seconds is Rem2 - Minutes * 60 + NSNorm rdiv 1000000000.

component_to_parts(date(Y,M,D,_), T, 0) :-
    !,
    date_time_stamp(date(Y,M,D,0,0,0,0,-,-), T).
component_to_parts(date_time(Y,Mo,D,H,Mi,S,NS), T, NS) :-
    date_time_stamp(date(Y,Mo,D,H,Mi,S,0,-,-), T).

interval_component_string(neg_infinity, _Tz, "..") :- !.
interval_component_string(infinity, _Tz, "..") :- !.
interval_component_string(unknown, _Tz, "") :- !.
interval_component_string(date(Y,M,D,_Offset), _Tz, S) :- !,
    format(string(S), '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+', [Y,M,D]).
interval_component_string(duration(Sign,W,Y,Mo,D,H,M,SS), _Tz, S) :- !, duration_string(duration(Sign,W,Y,Mo,D,H,M,SS), S).
interval_component_string(DT, Tz, S) :- interval_date_time_string(DT, Tz, S).

%% End-position rendering: a stored date-kind end is the exclusive day
%% after — the supplied inclusive date is one calendar day before.
interval_component_end_string(date(Y,M,D,Offset), Tz, S) :-
    !,
    date_time_stamp(date(Y,M,D,0,0,0,0,-,-), Stamp),
    Prev is Stamp - 86400,
    stamp_date_time(Prev, date(PY,PM,PD,_,_,_,_,'UTC',_), 'UTC'),
    interval_component_string(date(PY,PM,PD,Offset), Tz, S).
interval_component_end_string(C, Tz, S) :-
    interval_component_string(C, Tz, S).

interval_point_string(date(Y,M,D,_Offset), _Tz, S) :-
    !,
    format(string(S), '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+', [Y,M,D]).
interval_point_string(C, Tz, S) :-
    interval_component_string(C, Tz, S).

%% Extract the supplied zone from a parsed interval component into term tz
%% form: tz(OffsetSeconds) | none (zone-less local time).
component_tz(date(_,_,_,Offset), tz(Offset)) :- !.
component_tz(date_time(_,_,_,_,_,_,_,none), none) :- !.
component_tz(date_time(_,_,_,_,_,_,_,Offset), tz(Offset)) :- !.
component_tz(duration(_,_,_,_,_,_,_,_), none) :- !.
component_tz(open, none) :- !.
component_tz(unknown, none) :- !.

%% Render a normalized UTC component in the supplied zone.
%% tz(0) -> 'Z', tz(OffSecs) -> ±hh:mm wall clock, none -> no designator.
interval_date_time_string(DT, Tz, S) :-
    component_to_parts(DT, TS, NS),
    (   Tz = tz(Off)
    ->  LocalTS is TS - Off
    ;   LocalTS = TS
    ),
    stamp_date_time(LocalTS, date(Y,M,D,H,Mi,Se,0,'UTC',-), 'UTC'),
    SF is floor(Se),
    zone_suffix(Tz, ZS),
    (   NS =:= 0
    ->  format(string(S), '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+~w', [Y,M,D,H,Mi,SF,ZS])
    ;   0 is NS mod 1000000
    ->  Ms is NS div 1000000,
        format(string(S), '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~3+~w', [Y,M,D,H,Mi,SF,Ms,ZS])
    ;   0 is NS mod 1000
    ->  Us is NS div 1000,
        format(string(S), '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~6+~w', [Y,M,D,H,Mi,SF,Us,ZS])
    ;   format(string(S), '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~9+~w', [Y,M,D,H,Mi,SF,NS,ZS])
    ).

zone_suffix(none, '').
zone_suffix(tz(0), 'Z').
zone_suffix(tz(Off), S) :- Off < 0, !, AH is -Off // 3600, AM is (-Off mod 3600) // 60, format(string(S),'+~|~`0t~d~2+:~|~`0t~d~2+',[AH,AM]).
zone_suffix(tz(Off), S) :- Off > 0, AH is Off // 3600, AM is (Off mod 3600) // 60, format(string(S),'-~|~`0t~d~2+:~|~`0t~d~2+',[AH,AM]).
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
        Cast = duration(_Sign,0,_Y,_M,0,0,0,0)
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#yearMonthduration'),_))).
%%% xsd:string => xsd:dayTimeDuration
typecast_switch('http://www.w3.org/2001/XMLSchema#dayTimeDuration', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Cast^^'http://www.w3.org/2001/XMLSchema#dayTimeDuration') :-
    !,
    (   duration_string(Cast,Val),
        Cast = duration(_Sign,_W,0,0,_D,_H,_M,_S)
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
%%% Per XSD 1.1 spec and RFC 3987, xsd:anyURI accepts both absolute and relative URI references
typecast_switch('http://www.w3.org/2001/XMLSchema#anyURI', 'http://www.w3.org/2001/XMLSchema#string', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#anyURI') :-
    !,
    (   uri_components(Val, _)
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

/*
 * Expanded names: xsd:QName and xbrl:SQName.
 *
 * Stored form is always Clark notation `{uri}local`; the active prefix map
 * only affects the `prefix:local` edge form. The `prefixes(Dict)` hint option
 * overrides the global defaults (used by WOQL Typecast's `prefixes` field);
 * when absent the global defaults apply, which always bind xsd:/xbrl:.
 */
qname_prefixes(Hint, Prefixes) :-
    (   option(prefixes(Prefixes), Hint)
    ->  true
    ;   default_prefixes(Prefixes)
    ).

%%% xsd:string => xsd:QName (lexical or Clark in, Clark stored)
typecast_switch('http://www.w3.org/2001/XMLSchema#QName', 'http://www.w3.org/2001/XMLSchema#string', Val, Hint, Cast^^'http://www.w3.org/2001/XMLSchema#QName') :-
    !,
    qname_prefixes(Hint, Prefixes),
    lexical_to_clark(Val, Prefixes, 'http://www.w3.org/2001/XMLSchema#QName', Cast).
%%% xsd:QName => xsd:string (Clark => prefix:local, Clark verbatim if unbound)
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#QName', Val, Hint, Cast^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    qname_prefixes(Hint, Prefixes),
    clark_to_lexical(Val, Prefixes, Cast).
%%% xsd:string => xbrl:SQName (OIM §3.1: any non-whitespace localname)
typecast_switch('https://xbrl.org/2021SQName', 'http://www.w3.org/2001/XMLSchema#string', Val, Hint, Cast^^'https://xbrl.org/2021SQName') :-
    !,
    qname_prefixes(Hint, Prefixes),
    lexical_to_clark(Val, Prefixes, 'https://xbrl.org/2021SQName', Cast).
%%% xbrl:SQName => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 'https://xbrl.org/2021SQName', Val, Hint, Cast^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    qname_prefixes(Hint, Prefixes),
    clark_to_lexical(Val, Prefixes, Cast).
%%% xsd:QName => xbrl:SQName (QName names are SQName names — retag only)
typecast_switch('https://xbrl.org/2021SQName', 'http://www.w3.org/2001/XMLSchema#QName', Val, _, Val^^'https://xbrl.org/2021SQName') :-
    !.
%%% xbrl:SQName => xsd:QName (only when the localname is an NCName)
typecast_switch('http://www.w3.org/2001/XMLSchema#QName', 'https://xbrl.org/2021SQName', Val, _, Val^^'http://www.w3.org/2001/XMLSchema#QName') :-
    !,
    (   (qname_local(Val, Local), is_ncname(Local))
    ->  true
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#QName'),_))).

%%% xsd:anyURI => xsd:QName / xbrl:SQName
%%% The IRI is decomposed through the prefix map: longest bound expansion
%%% match, else `prefix:local` lexical, else Clark passthrough. A namespace
%%% with no bound prefix is a casting_error — the engine never guesses the
%%% boundary; declare the prefix in the context and retry.
typecast_switch('http://www.w3.org/2001/XMLSchema#QName', 'http://www.w3.org/2001/XMLSchema#anyURI', Val, Hint, Cast^^'http://www.w3.org/2001/XMLSchema#QName') :-
    !,
    qname_prefixes(Hint, Prefixes),
    anyuri_to_clark(Val, Prefixes, 'http://www.w3.org/2001/XMLSchema#QName', Cast).
typecast_switch('https://xbrl.org/2021SQName', 'http://www.w3.org/2001/XMLSchema#anyURI', Val, Hint, Cast^^'https://xbrl.org/2021SQName') :-
    !,
    qname_prefixes(Hint, Prefixes),
    anyuri_to_clark(Val, Prefixes, 'https://xbrl.org/2021SQName', Cast).

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

test(decimal_canonical_integer_valued, []) :-
    % XSD 1.1 canonical form (issue #2632): an integer-valued decimal
    % renders without the fraction — "1.0" canonicalizes to "1".
    forall(
        member(Lexical-Canonical,
               ["1"     - "1",
                "1.0"   - "1",
                "1.50"  - "1.5",
                "100000.00" - "100000",
                "0.0"   - "0",
                "-1.0"  - "-1"]),
        (   typecast(Lexical^^'http://www.w3.org/2001/XMLSchema#string',
                     'http://www.w3.org/2001/XMLSchema#decimal',
                     [],
                     Decimal^^'http://www.w3.org/2001/XMLSchema#decimal'),
            typecast(Decimal^^'http://www.w3.org/2001/XMLSchema#decimal',
                     'http://www.w3.org/2001/XMLSchema#string',
                     [],
                     Canonical^^'http://www.w3.org/2001/XMLSchema#string')
        )
    ).

test(decimal_canonical_strips_leading_plus_and_zeros, []) :-
    % Canonical form carries no leading '+' and no insignificant zeros.
    typecast("+100000.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal',
             [],
             Decimal^^'http://www.w3.org/2001/XMLSchema#decimal'),
    typecast(Decimal^^'http://www.w3.org/2001/XMLSchema#decimal',
             'http://www.w3.org/2001/XMLSchema#string',
             [],
             "100000"^^'http://www.w3.org/2001/XMLSchema#string').

test(xsd_version_param_accepts_supported_versions, []) :-
    xsd_version_param('1.1', '1.1'),
    setup_call_cleanup(
        set_prolog_flag(terminusdb_enterprise, true),
        xsd_version_param('1.0', '1.0'),
        set_prolog_flag(terminusdb_enterprise, false)).

test(xsd_version_param_10_requires_enterprise,
     [condition(\+ config:is_enterprise),
      error(unsupported_xsd_version('1.0'), _)]) :-
    xsd_version_param('1.0', _).

test(xsd_version_param_rejects_unknown_version,
     [error(bad_parameter_type(xsd, _, _), _)]) :-
    xsd_version_param('2.0', _).

test(anyuri_bare_filename, []) :-
    % Per XSD 1.1 spec and RFC 3987, xsd:anyURI accepts relative URI references
    typecast("helloWorld.xsd"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "helloWorld.xsd"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_relative_path, []) :-
    % Relative path with directory
    typecast("../schemas/test.xsd"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "../schemas/test.xsd"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_absolute_url, []) :-
    % Absolute URL should also work
    typecast("http://example.com/schema.xsd"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "http://example.com/schema.xsd"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_fragment, []) :-
    % Fragment-only URI reference
    typecast("#section1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "#section1"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_to_string, []) :-
    % Downcast from anyURI to string
    typecast("helloWorld.xsd"^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#string',
             [],
             "helloWorld.xsd"^^'http://www.w3.org/2001/XMLSchema#string').

test(anyuri_prefixed_iri, []) :-
    % Prefixed IRI (CURIE-style) - parsed as scheme:path
    typecast("schema:Person"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "schema:Person"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_prefixed_iri_with_path, []) :-
    % Prefixed IRI with path segments
    typecast("foaf:knows/friend"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "foaf:knows/friend"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_rdf_prefix, []) :-
    % RDF-style prefixed IRI
    typecast("rdf:type"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "rdf:type"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_xsd_prefix, []) :-
    % XSD-style prefixed IRI
    typecast("xsd:string"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "xsd:string"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_iri_japanese, []) :-
    % IRI with Japanese characters (per RFC 3987)
    typecast("http://example.com/日本語"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "http://example.com/日本語"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_iri_cyrillic, []) :-
    % IRI with Cyrillic characters
    typecast("файл.txt"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "файл.txt"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_iri_accented, []) :-
    % IRI with accented Latin characters
    typecast("données/café.xml"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "données/café.xml"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(anyuri_iri_chinese, []) :-
    % IRI with Chinese characters
    typecast("http://例え.jp/文件"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI',
             [],
             "http://例え.jp/文件"^^'http://www.w3.org/2001/XMLSchema#anyURI').

%% --- DateTimeInterval normalization tests ---

test(interval_explicit_dates_keep_date_kind, []) :-
    parse_date_time_interval('2025-01-01/2025-03-31',
                             date_time_interval(Start, End, _Dur, explicit, Tz1, Tz2)),
    % date components keep their kind — the stored end is the exclusive
    % day after, and the literal stays the supplied date spellings
    Start = date(2025,1,1,0),
    End = date(2025,4,1,0),
    Tz1 = none,
    Tz2 = none.

test(interval_explicit_datetimes_preserved, []) :-
    parse_date_time_interval('2025-01-01T10:30:00Z/2025-03-31T15:45:00Z',
                             date_time_interval(Start, End, _Dur, explicit, Tz1, Tz2)),
    Start = date_time(2025,1,1,10,30,0,0),
    End = date_time(2025,3,31,15,45,0,0),
    Tz1 = tz(0),
    Tz2 = tz(0).

test(interval_datetime_with_timezone_normalized_to_utc, []) :-
    % Instants still normalise to UTC; the supplied offsets travel as
    % tz(SecondsWest) metadata for lexical round-trip.
    parse_date_time_interval('2025-01-31T09:00:00+02:00/2025-03-31T09:00:00-05:00',
                             date_time_interval(Start, End, _Dur, explicit, Tz1, Tz2)),
    Start = date_time(2025,1,31,7,0,0,0),
    End = date_time(2025,3,31,14,0,0,0),
    Tz1 = tz(-7200),
    Tz2 = tz(18000).

test(interval_start_duration_date_normalizes, []) :-
    parse_date_time_interval('2025-01-01/P3M',
                             date_time_interval(Start, End, Dur, start_duration, Tz1, Tz2)),
    Start = date(2025,1,1,0),
    End = date(2025,4,1,0),
    Dur = duration(1,0,0,3,0,0,0,0),
    Tz1 = none,
    Tz2 = none.

test(interval_duration_end_date_normalizes_with_bump, []) :-
    parse_date_time_interval('P3M/2025-03-31',
                             date_time_interval(Start, End, Dur, duration_end, Tz1, Tz2)),
    Start = date(2025,1,1,0),
    End = date(2025,4,1,0),
    Dur = duration(1,0,0,3,0,0,0,0),
    Tz1 = none,
    Tz2 = none.

test(interval_date_end_leap_year_bump, []) :-
    parse_date_time_interval('2020-02-01/2020-02-29',
                             date_time_interval(_Start, End, _Dur, explicit, _, _)),
    End = date(2020,3,1,0).

test(interval_open_end_parse, []) :-
    parse_date_time_interval('2025-01-01/..',
                             date_time_interval(Start, End, Dur, explicit, Tz1, Tz2)),
    Start = date(2025,1,1,0),
    End = infinity,
    Dur = duration(1,0,0,0,0,0,0,0),
    Tz1 = none,
    Tz2 = none.

test(interval_open_start_parse, []) :-
    parse_date_time_interval('../2025-01-01T10:30:00Z',
                             date_time_interval(Start, End, _Dur, explicit, _, _)),
    Start = neg_infinity,
    End = date_time(2025,1,1,10,30,0,0).

test(interval_forever_parse, []) :-
    parse_date_time_interval('../..',
                             date_time_interval(neg_infinity, infinity, _, explicit, none, none)).

test(interval_unknown_end_parse, []) :-
    parse_date_time_interval('2025-01-01/',
                             date_time_interval(Start, End, Dur, explicit, _, _)),
    Start = date(2025,1,1,0),
    End = unknown,
    Dur = duration(1,0,0,0,0,0,0,0).

test(interval_unknown_start_parse, []) :-
    parse_date_time_interval('/2025-01-01',
                             date_time_interval(Start, End, _Dur, explicit, _, _)),
    Start = unknown,
    End = date(2025,1,2,0).

test(interval_both_unknown_parse, []) :-
    parse_date_time_interval('/',
                             date_time_interval(unknown, unknown, _, explicit, none, none)).

test(interval_duration_with_open_end_fails, [fail]) :-
    parse_date_time_interval('P1Y/..', _).

test(interval_duration_with_unknown_end_fails, [fail]) :-
    parse_date_time_interval('P1Y/', _).

test(interval_open_start_duration_fails, [fail]) :-
    parse_date_time_interval('../P1Y', _).

test(interval_roundtrip_open_end, []) :-
    typecast("2025-01-01/.."^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/..".

test(interval_roundtrip_forever, []) :-
    typecast("../.."^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "../..".

test(interval_roundtrip_unknown_end, []) :-
    typecast("2025-01-01/"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/".

test(interval_roundtrip_both_unknown, []) :-
    typecast("/"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "/".

test(interval_date_end_year_boundary_bump, []) :-
    parse_date_time_interval('2024-01-01/2024-12-31',
                             date_time_interval(_Start, End, _Dur, explicit, _, _)),
    End = date(2025,1,1,0).

test(interval_roundtrip_explicit_dates, []) :-
    typecast("2025-01-01/2025-03-31"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/2025-03-31".

test(interval_roundtrip_start_duration, []) :-
    typecast("2025-01-01/P3M"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/P3M".

test(interval_roundtrip_duration_end, []) :-
    typecast("P3M/2025-03-31"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "P3M/2025-03-31".

test(interval_roundtrip_datetime_with_timezone, []) :-
    typecast("2025-01-31T09:00:00+02:00/2025-03-31T09:00:00+02:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-31T09:00:00+02:00/2025-03-31T09:00:00+02:00".

test(interval_roundtrip_datetime_with_time, []) :-
    typecast("2025-01-01T10:30:00Z/2025-03-31T15:45:00Z"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01T10:30:00Z/2025-03-31T15:45:00Z".

test(interval_roundtrip_duration_with_hours, []) :-
    typecast("2025-01-01/PT1H"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/PT1H".

test(dateTimeInterval_string_from_dates, []) :-
    typecast(date_time_interval(date(2025,1,1,0), date(2025,4,1,0), duration(1,0,0,0,90,0,0,0.0), explicit, tz(0), tz(0))^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2025-01-01/2025-03-31"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_string_from_datetimes, []) :-
    typecast(date_time_interval(date_time(2025,1,1,10,30,0,0), date_time(2025,4,1,15,45,0,0), duration(1,0,0,0,90,5,15,0.0), explicit, tz(0), tz(0))^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2025-01-01T10:30:00Z/2025-04-01T15:45:00Z"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_string_mixed_date_datetime, []) :-
    typecast(date_time_interval(date(2025,1,1,0), date_time(2025,4,1,12,0,0,0), duration(1,0,0,0,90,12,0,0.0), explicit, tz(0), tz(0))^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2025-01-01/2025-04-01T12:00:00Z"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_string_from_dates_roundtrip, []) :-
    typecast("2025-01-01/2025-04-01"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2025-01-01/2025-04-01"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_unqualified_end_bumps_to_next_year, []) :-
    typecast("2019-01-01/2019-12-31"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2019-01-01/2019-12-31"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_unqualified_end_bumps_over_leap_day, []) :-
    typecast("2020-02-01/2020-02-29"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2020-02-01/2020-02-29"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_unqualified_end_bumps_to_leap_day, []) :-
    typecast("2020-02-27/2020-02-28"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2020-02-27/2020-02-28"^^'http://www.w3.org/2001/XMLSchema#string').

test(dateTimeInterval_datetime_end_is_not_bumped, []) :-
    typecast("2019-01-01T00:00:00/2020-01-01T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             "2019-01-01T00:00:00/2020-01-01T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string').
test(date_range_to_interval_inclusive, []) :-
    % [D1, D2] covers both whole days: the interval end is the exclusive
    % day after D2, kept as a date component so it renders inclusively.
    typecast(date_range(date(2025,1,1,0), date(2025,3,31,0))^^'http://terminusdb.com/schema/xdd#dateRange',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             IV^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    IV = "2025-01-01/2025-03-31",
    interval_as_term(IV,
                     date_time_interval(date(2025,1,1,0), date(2025,4,1,0),
                                        _, explicit, _, _)).

test(interval_to_date_range_exclusive_end, []) :-
    % An exclusive end at exactly midnight belongs to the next day, so the
    % last included date is the day before it.
    typecast(date_time_interval(date_time(2025,1,1,0,0,0,0), date_time(2025,4,1,0,0,0,0), duration(1,0,0,0,90,0,0,0), explicit, tz(0), tz(0))^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://terminusdb.com/schema/xdd#dateRange', [],
             date_range(date(2025,1,1,0), date(2025,3,31,0))^^'http://terminusdb.com/schema/xdd#dateRange').

test(interval_to_date_range_midday_end, []) :-
    % An end instant inside a day includes that day's date.
    typecast(date_time_interval(date_time(2025,1,1,9,30,0,0), date_time(2025,3,31,17,15,0,0), duration(1,0,0,0,0,0,0,0), explicit, tz(0), tz(0))^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://terminusdb.com/schema/xdd#dateRange', [],
             date_range(date(2025,1,1,0), date(2025,3,31,0))^^'http://terminusdb.com/schema/xdd#dateRange').

test(date_range_interval_roundtrip, []) :-
    typecast(date_range(date(2025,1,1,0), date(2025,3,31,0))^^'http://terminusdb.com/schema/xdd#dateRange',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://terminusdb.com/schema/xdd#dateRange', [],
             date_range(date(2025,1,1,0), date(2025,3,31,0))^^'http://terminusdb.com/schema/xdd#dateRange').

test(rational_decimal_full_precision, []) :-
    % 20 significant decimal places survive unchanged. Regression anchor:
    % JSON decimals are kept as rationals end to end, so the document API
    % must return the exact lexical value, not a float-truncated form like
    % 0.01234567890123457.
    rational_to_decimal_string(1234567890123456789r100000000000000000000,
                               "0.01234567890123456789", 20).

test(rational_decimal_fraction_rounds_to_zero, []) :-
    % Why this test exists: the rounded fraction (1/10^22 at 20 digits)
    % becomes all zeros, and the old code emitted the malformed literal
    % "2." — an empty fractional part after the dot. The Rust store then
    % panicked in Decimal::new (DecimalValidationError) and a raw JSON
    % insert returned HTTP 500. A fraction that rounds to nothing must
    % produce a bare integer instead.
    rational_to_decimal_string(20000000000000000000001r10000000000000000000000,
                               "2", 20).

test(rational_decimal_rounds_into_integer, []) :-
    % Why this test exists: rounding 2.999...9 to 20 decimals produces
    % 10^20 fractional digits, which must carry into the integer part
    % (result "3"). The old code emitted "2.1" — the digit string wrapped
    % back past the decimal point — silently storing a wrong value.
    rational_to_decimal_string(2999999999999999999999r1000000000000000000000,
                               "3", 20).

test(rational_decimal_negative_trimming, []) :-
    % Baseline: sign, leading zeros after the point, and interior zeros
    % survive; only trailing zeros are trimmed.
    rational_to_decimal_string(-156r100, "-1.56", 20),
    rational_to_decimal_string(2005r100, "20.05", 20).


test(interval_zone_less_roundtrip_stays_zone_less, []) :-
    typecast("2025-01-31T09:00:00/2025-03-31T09:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-31T09:00:00/2025-03-31T09:00:00".

test(interval_roundtrip_mixed_timezones, []) :-
    typecast("2025-01-31T09:00:00-05:00/2025-03-31T18:30:00+05:30"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-31T09:00:00-05:00/2025-03-31T18:30:00+05:30".

% --- Bare single-component literals: the point flag keeps the spelling ---

test(interval_bare_datetime_offset_renders_single, []) :-
    typecast("2025-03-15T00:00:00+02:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-03-15T00:00:00+02:00",
    interval_as_term(Interval,
                     date_time_interval(C,C,_,point,tz(-7200),tz(-7200))),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-03-15T00:00:00+02:00".

test(interval_bare_datetime_z_renders_single, []) :-
    typecast("2025-03-15T00:00:00Z"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-03-15T00:00:00Z",
    interval_as_term(Interval,
                     date_time_interval(C,C,_,point,tz(0),tz(0))),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-03-15T00:00:00Z".

test(interval_bare_datetime_zoneless_renders_single, []) :-
    typecast("2025-03-15T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-03-15T00:00:00",
    interval_as_term(Interval,
                     date_time_interval(C,C,_,point,none,none)),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-03-15T00:00:00".

test(interval_bare_date_covers_whole_day, []) :-
    typecast("2026-10-01"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    % the bare date is the same [D, D+1) interval as the explicit D/D pair
    typecast("2026-10-01/2026-10-01"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Pair^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2026-10-01",
    Pair = "2026-10-01/2026-10-01",
    interval_as_term(Interval,
                     date_time_interval(S,E,Dur,point,none,none)),
    interval_as_term(Pair,
                     date_time_interval(SP,EP,_,explicit,_,_)),
    S = date(2026,10,1,0),
    E = date(2026,10,2,0),
    Dur = duration(1,0,0,0,1,0,0,_),
    component_to_parts(S, TS, _),
    component_to_parts(E, TE, _),
    component_to_parts(SP, TS, _),
    component_to_parts(EP, TE, _),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "2026-10-01".

test(interval_explicit_zero_duration_pair_stays_pair, []) :-
    typecast("2025-03-15T00:00:00/2025-03-15T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-03-15T00:00:00/2025-03-15T00:00:00",
    interval_as_term(Interval,
                     date_time_interval(_,_,_,explicit,_,_)),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-03-15T00:00:00/2025-03-15T00:00:00".

test(interval_explicit_date_pair_renders_pair, []) :-
    typecast("2026-10-01/2026-10-01"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2026-10-01/2026-10-01",
    interval_as_term(Interval,
                     date_time_interval(_,_,_,explicit,_,_)),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    % the pair keeps the supplied date spellings — inclusive on both ends
    S = "2026-10-01/2026-10-01".

% --- Date components in pairs keep their kind and zoneless spelling ---
% Internal instants are UTC, but the literal presents local time: a date
% renders as the date, a zoneless dateTime carries no designator.

test(interval_date_pair_end_is_inclusive_whole_day, []) :-
    typecast("2025-01-01/2025-03-31"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-01-01/2025-03-31",
    interval_as_term(Interval,
                     date_time_interval(S,E,Dur,explicit,none,none)),
    S = date(2025,1,1,0),
    % the stored end is the exclusive day after — the pair covers Mar 31
    E = date(2025,4,1,0),
    Dur = duration(1,0,0,0,90,0,0,_),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "2025-01-01/2025-03-31".

test(interval_date_pair_same_period_as_zoneless_datetime_pair, []) :-
    typecast("2025-01-01/2025-03-31"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             DI^^_),
    typecast("2025-01-01T00:00:00/2025-04-01T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             TI^^_),
    interval_as_term(DI, date_time_interval(DS,DE,_,_,_,_)),
    interval_as_term(TI, date_time_interval(TS,TE,_,_,_,_)),
    component_to_parts(DS, Start, _),
    component_to_parts(DE, End, _),
    component_to_parts(TS, Start, _),
    component_to_parts(TE, End, _).

test(interval_zoneless_datetime_pair_renders_zoneless, []) :-
    typecast("2025-01-01T00:00:00/2025-04-01T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-01-01T00:00:00/2025-04-01T00:00:00",
    interval_as_term(Interval,
                     date_time_interval(_,_,_,explicit,none,none)),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01T00:00:00/2025-04-01T00:00:00".

test(interval_mixed_date_start_datetime_end_renders_kinds, []) :-
    typecast("2025-01-01/2025-04-01T00:00:00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/2025-04-01T00:00:00".

test(interval_mixed_datetime_start_date_end_renders_kinds, []) :-
    typecast("2025-01-01T00:00:00/2025-04-01"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "2025-01-01T00:00:00/2025-04-01",
    interval_as_term(Interval,
                     date_time_interval(_,date(2025,4,2,0),_,explicit,_,none)),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01T00:00:00/2025-04-01".

test(interval_duration_end_with_date_renders_date_spelling, []) :-
    typecast("P3M/2025-03-31"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Interval = "P3M/2025-03-31",
    interval_as_term(Interval,
                     date_time_interval(_,date(2025,4,1,0),_,duration_end,_,none)),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "P3M/2025-03-31".

test(interval_daterange_cast_renders_date_pair, []) :-
    typecast(date_range(date(2025,1,1,0),date(2025,3,31,0))^^'http://terminusdb.com/schema/xdd#dateRange',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    typecast(Interval^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#string', [],
             S^^'http://www.w3.org/2001/XMLSchema#string'),
    S = "2025-01-01/2025-03-31".

% --- Expanded names: xsd:QName / xbrl:SQName (Clark storage) ---

test(string_to_qname_expands_bound_prefix, []) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast("eg:foo"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://example.org/ns#}foo'.

test(string_to_sqname_first_colon_split, []) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast("eg:a:b:c"^^'http://www.w3.org/2001/XMLSchema#string',
             'https://xbrl.org/2021SQName', [prefixes(P)],
             Cast^^'https://xbrl.org/2021SQName'),
    Cast = '{http://example.org/ns#}a:b:c'.

test(string_to_qname_clark_passthrough, []) :-
    typecast("{http://unbound.example/}x"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#QName', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = "{http://unbound.example/}x".

test(string_to_qname_unbound_prefix, [error(unbound_prefix(no,"no:foo",_),_)]) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast("no:foo"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)], _).

test(string_to_qname_malformed, []) :-
    forall(member(Bad, ["nocolon", ":x", "p:", "{}x", "{ns"]),
           (   catch(typecast(Bad^^'http://www.w3.org/2001/XMLSchema#string',
                              'http://www.w3.org/2001/XMLSchema#QName', [], _),
                     E, true),
               E = error(casting_error(_,_),_)
           )).

test(string_to_sqname_rejects_whitespace_local, [error(casting_error(_,_),_)]) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast("eg:a b"^^'http://www.w3.org/2001/XMLSchema#string',
             'https://xbrl.org/2021SQName', [prefixes(P)], _).

test(string_to_qname_rejects_non_ncname_local, [error(casting_error(_,_),_)]) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast("eg:a:b"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)], _).

test(qname_to_string_exact_match, []) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast("{http://example.org/ns#}foo"^^'http://www.w3.org/2001/XMLSchema#QName',
             'http://www.w3.org/2001/XMLSchema#string', [prefixes(P)],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "eg:foo".

test(qname_to_string_unbound_keeps_clark, []) :-
    typecast("{http://unbound.example/}x"^^'http://www.w3.org/2001/XMLSchema#QName',
             'http://www.w3.org/2001/XMLSchema#string', [],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "{http://unbound.example/}x".

test(qname_to_string_no_substring_match, []) :-
    % eg -> http://example.org/ns# must not contract http://example.org/ns#sub
    P = _{eg: 'http://example.org/ns#'},
    typecast("{http://example.org/ns#sub}foo"^^'http://www.w3.org/2001/XMLSchema#QName',
             'http://www.w3.org/2001/XMLSchema#string', [prefixes(P)],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "{http://example.org/ns#sub}foo".

test(qname_to_string_smallest_prefix_on_tie, []) :-
    P = _{zz: 'http://dup.example/ns', aa: 'http://dup.example/ns'},
    typecast("{http://dup.example/ns}x"^^'http://www.w3.org/2001/XMLSchema#QName',
             'http://www.w3.org/2001/XMLSchema#string', [prefixes(P)],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "aa:x".

test(qname_to_sqname_retag, []) :-
    typecast('{http://ns/}foo'^^'http://www.w3.org/2001/XMLSchema#QName',
             'https://xbrl.org/2021SQName', [],
             Cast^^'https://xbrl.org/2021SQName'),
    Cast = '{http://ns/}foo'.

test(sqname_to_qname_ncname_local_ok, []) :-
    typecast('{http://ns/}foo'^^'https://xbrl.org/2021SQName',
             'http://www.w3.org/2001/XMLSchema#QName', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://ns/}foo'.

test(sqname_to_qname_rejects_non_ncname_local, [error(casting_error(_,_),_)]) :-
    typecast('{http://ns/}a:b'^^'https://xbrl.org/2021SQName',
             'http://www.w3.org/2001/XMLSchema#QName', [], _).

test(sqname_to_string_lexical_round_trip, []) :-
    P = _{xbrl: 'https://xbrl.org/2021'},
    typecast("xbrl:a:b}c"^^'http://www.w3.org/2001/XMLSchema#string',
             'https://xbrl.org/2021SQName', [prefixes(P)],
             Clark^^'https://xbrl.org/2021SQName'),
    typecast(Clark^^'https://xbrl.org/2021SQName',
             'http://www.w3.org/2001/XMLSchema#string', [prefixes(P)],
             Lex^^'http://www.w3.org/2001/XMLSchema#string'),
    Lex = "xbrl:a:b}c".

% --- Context-prefix gating: only expanded-name/IRI types consult the map ---

test(typecast_nonmember_iri_ignores_prefix_map, []) :-
    % xsd rebound to a bogus namespace: a non-member type must not consult
    % the map at all — if it did, expansion lands outside xsd and fails.
    P = _{xsd: 'http://bogus.example/'},
    typecast("abc"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#string', [prefixes(P)],
             "abc"^^'http://www.w3.org/2001/XMLSchema#string').

test(typecast_nonmember_spelled_uses_defaults_not_map, []) :-
    % A spelled non-member type resolves through the global defaults, never
    % the context map — a poisoned binding cannot corrupt it.
    P = _{xsd: 'http://bogus.example/'},
    typecast("abc"^^'http://www.w3.org/2001/XMLSchema#string',
             'xsd:string', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#string'),
    Cast = "abc".

test(typecast_nonmember_compound_spelling_never_consults_map, []) :-
    % `xsd:string` as a `P:S` term: resolves via defaults, context untouched.
    typecast("x"^^'http://www.w3.org/2001/XMLSchema#string',
             xsd:string, [prefixes(_)],
             Cast^^'http://www.w3.org/2001/XMLSchema#string'),
    Cast = "x".

test(typecast_member_still_consults_prefix_map, []) :-
    % The gate must not starve expanded names: a database alias for the xsd
    % namespace names the QName type only through the context map.
    P = _{x: 'http://www.w3.org/2001/XMLSchema#', eg: 'http://example.org/ns#'},
    typecast("eg:foo"^^'http://www.w3.org/2001/XMLSchema#string',
             'x:QName', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://example.org/ns#}foo'.

test(typecast_member_full_iri_keeps_value_resolution, []) :-
    % Full-IRI member: the map still binds the lexical's prefix.
    P = _{eg: 'http://example.org/ns#'},
    typecast("eg:foo"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://example.org/ns#}foo'.

test(typecast_anyuri_target_never_expands_lexical, []) :-
    % anyURI accepts relative references: a `prefix:local` lexical stores
    % verbatim — the target arm does not read the map.
    P = _{eg: 'http://example.org/ns#'},
    typecast("eg:foo"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#anyURI', [prefixes(P)],
             "eg:foo"^^'http://www.w3.org/2001/XMLSchema#anyURI').

test(typecast_3_matches_empty_hint_4, []) :-
    % The hint slot only exists for context-map readers; a caller with no
    % map calls the 3-ary form and gets the empty-hint behaviour verbatim.
    typecast("12"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#long', Cast3),
    typecast("12"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#long', [], Cast4),
    assertion(Cast3 == Cast4).

test(context_hint_for_type_membership, []) :-
    context_hint_for_type('http://www.w3.org/2001/XMLSchema#QName', ctx,
                          [prefixes(ctx)]),
    context_hint_for_type('https://xbrl.org/2021SQName', ctx,
                          [prefixes(ctx)]),
    context_hint_for_type('http://www.w3.org/2002/07/owl#Thing', ctx,
                          [prefixes(ctx)]),
    context_hint_for_type('http://terminusdb.com/schema/sys#Top', ctx,
                          [prefixes(ctx)]),
    context_hint_for_type('xsd:QName', ctx, [prefixes(ctx)]),
    context_hint_for_type('xbrl:SQName', ctx, [prefixes(ctx)]),
    context_hint_for_type('http://www.w3.org/2001/XMLSchema#string', ctx, []),
    context_hint_for_type('http://www.w3.org/2001/XMLSchema#decimal', ctx, []),
    context_hint_for_type('xsd:string', ctx, []),
    context_hint_for_type('http://www.w3.org/2001/XMLSchema#anyURI', ctx, []).

% --- xsd:anyURI => QNames: prefix-map decomposition ---

test(anyuri_to_qname_expansion_match, []) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast('http://example.org/ns#foo'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://example.org/ns#}foo'.

test(anyuri_to_qname_longest_expansion_wins, []) :-
    P = _{a: 'http://example.org/', ab: 'http://example.org/ns/'},
    typecast('http://example.org/ns/foo'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://example.org/ns/}foo'.

test(anyuri_to_qname_lexical_prefix, []) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast('eg:foo'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://example.org/ns#}foo'.

test(anyuri_to_qname_clark_passthrough, []) :-
    typecast('{http://ns/}foo'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#QName', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#QName'),
    Cast = '{http://ns/}foo'.

test(anyuri_to_qname_unbound_namespace_fails, [error(casting_error(_,_),_)]) :-
    % No delimiter guessing: an IRI whose namespace is not bound in the
    % prefix map cannot name a (S)QName — declare the prefix first.
    forall(member(IRI, ['https://example.org/path#frag',
                        'https://example.org/path/name']),
           catch(typecast(IRI^^'http://www.w3.org/2001/XMLSchema#anyURI',
                          'http://www.w3.org/2001/XMLSchema#QName', [], _),
                 E, throw(E))).

test(anyuri_to_qname_rejects_non_ncname_local, [error(casting_error(_,_),_)]) :-
    P = _{ns: 'http://ns/'},
    typecast('http://ns/a:b'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#QName', [prefixes(P)], _).

test(anyuri_to_qname_unsplittable, [error(casting_error(_,_),_)]) :-
    typecast('plainword'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'http://www.w3.org/2001/XMLSchema#QName', [], _).

test(anyuri_to_sqname_expansion_match, []) :-
    P = _{eg: 'http://example.org/ns#'},
    typecast('http://example.org/ns#a:b'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'https://xbrl.org/2021SQName', [prefixes(P)],
             Cast^^'https://xbrl.org/2021SQName'),
    Cast = '{http://example.org/ns#}a:b'.

test(anyuri_to_sqname_non_ncname_local_ok, []) :-
    P = _{ns: 'http://ns/'},
    typecast('http://ns/a:b'^^'http://www.w3.org/2001/XMLSchema#anyURI',
             'https://xbrl.org/2021SQName', [prefixes(P)],
             Cast^^'https://xbrl.org/2021SQName'),
    Cast = '{http://ns/}a:b'.

% --- date/dateTime <=> intervals ---

test(date_to_daterange_single_day, []) :-
    typecast(date(2020,1,15,0)^^'http://www.w3.org/2001/XMLSchema#date',
             'http://terminusdb.com/schema/xdd#dateRange', [],
             Cast^^'http://terminusdb.com/schema/xdd#dateRange'),
    Cast = date_range(date(2020,1,15,0), date(2020,1,15,0)).

test(date_to_datetimeinterval_covers_day, []) :-
    typecast(date(2020,1,15,0)^^'http://www.w3.org/2001/XMLSchema#date',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Cast^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Cast = "2020-01-15T00:00:00Z/2020-01-16T00:00:00Z",
    interval_as_term(Cast,
                     date_time_interval(date_time(2020,1,15,0,0,0,0),
                                        date_time(2020,1,16,0,0,0,0),
                                        duration(1,0,0,0,1,0,0,0),
                                        explicit, tz(0), tz(0))).

test(datetime_to_datetimeinterval_zero_point, []) :-
    T = date_time(2020,1,15,12,30,45,0),
    typecast(T^^'http://www.w3.org/2001/XMLSchema#dateTime',
             'http://terminusdb.com/schema/xdd#dateTimeInterval', [],
             Cast^^'http://terminusdb.com/schema/xdd#dateTimeInterval'),
    Cast = "2020-01-15T12:30:45Z/2020-01-15T12:30:45Z",
    interval_as_term(Cast,
                     date_time_interval(T, T,
                                        duration(1,0,0,0,0,0,0,0),
                                        explicit, tz(0), tz(0))).

test(datetimeinterval_point_to_datetime, []) :-
    T = date_time(2020,1,15,12,30,45,0),
    typecast(date_time_interval(T, T, duration(1,0,0,0,0,0,0,0.0),
                                explicit, tz(0), tz(0))
             ^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#dateTime', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#dateTime'),
    Cast = T.

test(datetimeinterval_nonzero_to_datetime_fails,
     [error(casting_error(_,_),_)]) :-
    typecast(date_time_interval(date_time(2020,1,15,0,0,0,0),
                                date_time(2020,1,16,0,0,0,0),
                                duration(1,0,0,0,1,0,0,0.0),
                                explicit, tz(0), tz(0))
             ^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#dateTime', [], _).

test(datetimeinterval_midnight_point_to_date, []) :-
    T = date_time(2020,1,15,0,0,0,0),
    typecast(date_time_interval(T, T, duration(1,0,0,0,0,0,0,0.0),
                                explicit, tz(0), tz(0))
             ^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#date', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#date'),
    Cast = date(2020,1,15,0).

test(datetimeinterval_noon_point_to_date_fails,
     [error(casting_error(_,_),_)]) :-
    T = date_time(2020,1,15,12,0,0,0),
    typecast(date_time_interval(T, T, duration(1,0,0,0,0,0,0,0.0),
                                explicit, tz(0), tz(0))
             ^^'http://terminusdb.com/schema/xdd#dateTimeInterval',
             'http://www.w3.org/2001/XMLSchema#date', [], _).

test(daterange_single_day_to_date, []) :-
    D = date(2020,1,15,0),
    typecast(date_range(D, D)^^'http://terminusdb.com/schema/xdd#dateRange',
             'http://www.w3.org/2001/XMLSchema#date', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#date'),
    Cast = D.

test(daterange_multi_day_to_date_fails, [error(casting_error(_,_),_)]) :-
    typecast(date_range(date(2020,1,15,0), date(2020,1,16,0))
             ^^'http://terminusdb.com/schema/xdd#dateRange',
             'http://www.w3.org/2001/XMLSchema#date', [], _).

test(daterange_single_day_to_datetime_midnight, []) :-
    D = date(2020,1,15,0),
    typecast(date_range(D, D)^^'http://terminusdb.com/schema/xdd#dateRange',
             'http://www.w3.org/2001/XMLSchema#dateTime', [],
             Cast^^'http://www.w3.org/2001/XMLSchema#dateTime'),
    Cast = date_time(2020,1,15,0,0,0,0).

:- end_tests(typecast).

/** The $xdd:interval_parse/2 foreign predicate — the storage parser
 *  exposed to Prolog. Asserts canonical round-trip behaviour and
 *  measures per-call cost for comparison against the Prolog casting path.
 */

:- begin_tests(xdd_ffi).

test(point_zoned_renders_single_timestamp) :-
    '$xdd':interval_parse("2025-03-15T00:00:00+02:00", Out),
    assertion(Out == "2025-03-15T00:00:00+02:00").

test(date_pair_verbatim) :-
    '$xdd':interval_parse("2025-01-01/2025-03-31", Out),
    assertion(Out == "2025-01-01/2025-03-31").

test(deep_negative_year_point) :-
    '$xdd':interval_parse("-13800000000-01-01", Out),
    assertion(Out == "-13800000000-01-01").

test(forty_nine_digit_fraction) :-
    '$xdd':interval_parse("2025-01-01T00:00:00.1234567890123456789012345678901234567890123456789Z", Out),
    assertion(Out == "2025-01-01T00:00:00.1234567890123456789012345678901234567890123456789Z").

test(negative_duration_rejected, [fail]) :-
    '$xdd':interval_parse("-P1D/2025-03-15", _).

test(bench_ffi_vs_prolog, [true]) :-
    ffi_bench(20000, "2025-03-15T00:00:00+02:00", P),
    format("ffi point tz        : ~3f us/call~n", [P]),
    ffi_bench(20000, "2025-01-01/2025-03-31", D),
    format("ffi date/date       : ~3f us/call~n", [D]),
    ffi_bench(20000, "2025-01-01/P3M", DP),
    format("ffi date/period     : ~3f us/call~n", [DP]),
    ffi_bench(20000, "2025-01-01T00:00:00.111111111Z", F9),
    format("ffi point frac9     : ~3f us/call~n", [F9]),
    ffi_bench(20000, "-13800000000-01-01/P380000Y", Deep),
    format("ffi deep year D/P   : ~3f us/call~n", [Deep]).

ffi_bench(N, Lit, Micros) :-
    garbage_collect,
    statistics(cputime, T0),
    forall(between(1, N, _), '$xdd':interval_parse(Lit, _)),
    statistics(cputime, T1),
    Micros is (T1-T0)*1000000/N.

:- end_tests(xdd_ffi).
