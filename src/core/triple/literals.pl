:- module(literals, [
              literal_to_string/2,
              literal_to_turtle/2,
              normalise_triple/2,
              reset_normalise_warning/0,
              object_storage/2,
              ground_object_storage/2,
              storage_object/2,
              date_string/2,
              date_time_string/2,
              time_string/2,
              duration_string/2,
              gyear_string/2,
              gmonth_string/2,
              gyear_month_string/2,
              gmonth_day_string/2,
              gday_string/2,
              current_xsd_date_time/1,
              uri_to_prefixed/3,
              schema_uri_to_prefixed/3,
              instance_uri_to_prefixed/3,
              prefixed_to_uri/3,
              prefixed_to_property/3,
              uri_eq/3
          ]).

/** <module> Literals
 *
 * Literal handling
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(library(pcre)).
:- use_module(library(apply)).
:- use_module(library(lists), [reverse/2]).
:- use_module(library(sort)).
:- use_module(core(util)).
:- use_module(core(triple/casting), [typecast/4, decimal_precision/1]).
:- use_module(core(triple/base_type), [basetype_subsumption_of/2]).

/*
 * Decimal precision utilities (inlined to avoid circular dependency with casting.pl)
 */

/*
 * string_decimal_to_rational(+String, -Rational) is semidet.
 *
 * Parse decimal string to rational number for arbitrary precision.
 * Uses GMP-backed rationals to avoid floating-point precision loss.
 */
string_decimal_to_rational(String, Rational) :-
    % Handle negative numbers
    (   sub_string(String, 0, 1, _, "-")
    ->  sub_string(String, 1, _, 0, PositiveString),
        string_decimal_to_rational(PositiveString, PositiveRat),
        Rational is -PositiveRat
    ;   % Split on decimal point to preserve original precision
        (   split_string(String, ".", "", [IntStr, FracStr])
        ->  % Has decimal point - track the precision
            number_string(IntPart, IntStr),
            string_length(FracStr, FracLen),
            number_string(FracPart, FracStr),
            Denominator is 10^FracLen,
            Numerator is IntPart * Denominator + FracPart,
            Rational is Numerator rdiv Denominator
        ;   % No decimal point, just an integer
            number_string(Rational, String)
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
 * date_time_string(-Date_Time,+String) is det.
 * date_time_string(+Date_Time,-String) is det.
 */
date_time_string(Date_Time,String) :-
    nonvar(Date_Time),
    !,
    % ToDo, add appropriate time zone! Doesn't work in xsd_time_string!
    Date_Time = date_time(Y,M,D,HH,MM,SS,NS),

    (   NS = 0
    ->  format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+Z',
               [Y,M,D,HH,MM,SS])
    ;   0 is NS mod 1 000 000
    ->  MS is NS div 1 000 000,
        format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~3+Z',
               [Y,M,D,HH,MM,SS,MS])
    ;   0 is NS mod 1 000
    ->  MuS is NS div 1 000,
        format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~6+Z',
               [Y,M,D,HH,MM,SS,MuS])
    ;   format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~9+Z',
               [Y,M,D,HH,MM,SS,NS])

    ).
date_time_string(Date_Time,String) :-
    % So expensive! Let's do this faster somehow.
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(dateTime(Y,M,D,HH,MM,SS,NS,Offset),Codes),
    remove_date_time_offset(Y,M,D,HH,MM,SS,NS,Offset, Date_Time).

date_time_stamp_string(Date_Time,String) :-
    nonvar(Date_Time),
    !,
    % ToDo, add appropriate time zone! Doesn't work in xsd_time_string!
    Date_Time = date_time(Y,M,D,HH,MM,SS,NS),
    (   NS = 0
    ->  format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+Z',
               [Y,M,D,HH,MM,SS])
    ;   0 is NS mod 1 000 000
    ->  MS is NS div 1 000 000,
        format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~3+Z',
               [Y,M,D,HH,MM,SS,MS])
    ;   0 is NS mod 1 000
    ->  MuS is NS div 1 000,
        format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~6+Z',
               [Y,M,D,HH,MM,SS,MuS])
    ;   format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~9+Z',
               [Y,M,D,HH,MM,SS,NS])

    ).
date_time_stamp_string(Date_Time,String) :-
    % So expensive! Let's do this faster somehow.
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(dateTimeStamp(Y,M,D,HH,MM,SS,NS,Offset),Codes),
    remove_date_time_offset(Y,M,D,HH,MM,SS,Offset,NS,Date_Time).

remove_date_time_offset(Y,M,D,HH,MM,SS,NS,Offset,date_time(Y1,M1,D1,HH1,MM1,SS_Floor,NS)) :-
    date_time_stamp(date(Y, M, D, HH, MM, SS, Offset, -, -), TS),
    stamp_date_time(TS, date(Y1, M1, D1, HH1, MM1, SS1, 0, 'UTC', -), 'UTC'),
    SS_Floor is floor(SS1).

/*
 * date_string(-Date,+String) is det.
 * date_string(+Date,-String) is det.
 */
date_string(Date,String) :-
    nonvar(Date),
    !,
    % ToDo, add appropriate time zone! Doesn't work in xsd_time_string!
    Date = date(Y,M,D,Offset),
    (   Offset =:= 0
    ->  format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+',
               [Y,M,D])
    ;   offset_to_sign_hour_minute(Offset, Sign, Hour, Minute),
        format(string(String),
               '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+~w~|~`0t~d~2+:~|~`0t~d~2+',
               [Y,M,D, Sign, Hour, Minute])
    ).
date_string(date(Y,M,D,Offset),String) :-
    % So expensive! Let's do this faster somehow.
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(date(Y,M,D,Offset),Codes).

gyear_string(GYear, String) :-
    nonvar(GYear),
    !,
    GYear = gyear(Year,Offset),
    (   Offset =:= 0
    ->  format(string(String), '~|~`0t~d~4+', [Year])
    ;   offset_to_sign_hour_minute(Offset,Sign,Hour,Minute),
        format(string(String), '~|~`0t~d~4+~w~|~`0t~d~2+:~|~`0t~d~2+', [Year,Sign,Hour,Minute])
    ).
gyear_string(gyear(Year,Offset), String) :-
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(gYear(Year,Offset),Codes).

gmonth_string(GMonth, String) :-
    nonvar(GMonth),
    !,
    GMonth = gmonth(Month,Offset),
    (   Offset =:= 0
    ->  format(string(String), '--~|~`0t~d~2+', [Month])
    ;   offset_to_sign_hour_minute(Offset,Sign,Hour,Minute),
        format(string(String), '--~|~`0t~d~2+~w~|~`0t~d~2+:~|~`0t~d~2+', [Month,Sign,Hour,Minute])
    ).
gmonth_string(gmonth(Month,Offset), String) :-
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(gMonth(Month,Offset),Codes).


gyear_month_string(GYearMonth, String) :-
    nonvar(GYearMonth),
    !,
    GYearMonth = gyear_month(Year,Month,Offset),
    (   Offset =:= 0
    ->  format(string(String), '~|~`0t~d~4+-~|~`0t~d~2+', [Year,Month])
    ;   offset_to_sign_hour_minute(Offset,Sign,Hour,Minute),
        format(string(String), '~|~`0t~d~4+-~|~`0t~d~2+~|~`0t~d~2+:~|~`0t~d~2+', [Year,Month,Sign,Hour,Minute])
    ).
gyear_month_string(gyear_month(Year,Month,Offset), String) :-
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(gYearMonth(Year,Month,Offset),Codes).

gmonth_day_string(GMonthDay, String) :-
    nonvar(GMonthDay),
    !,
    GMonthDay = gmonth_day(Month,Day,Offset),
    (   Offset =:= 0
    ->  format(string(String), '-~|~`0t~d~2+-~|~`0t~d~2+', [Month,Day])
    ;   offset_to_sign_hour_minute(Offset,Sign,Hour,Minute),
        format(string(String), '-~|~`0t~d~2+-~|~`0t~d~2+~w~|~`0t~d~2+:~|~`0t~d~2+', [Month,Day,Sign,Hour,Minute])
    ).
gmonth_day_string(gmonth_day(Month,Day,Offset), String) :-
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(gMonthDay(Month,Day,Offset),Codes).

gday_string(GDay, String) :-
    nonvar(GDay),
    !,
    GDay = gday(Day,Offset),
    (   Offset =:= 0
    ->  format(string(String), '---~|~`0t~d~2+', [Day])
    ;   offset_to_sign_hour_minute(Offset,Sign,Hour,Minute),
        format(string(String), '---~|~`0t~d~2+~w~|~`0t~d~2+:~|~`0t~d~2+', [Day,Sign,Hour,Minute])
    ).
gday_string(gday(Day,Offset), String) :-
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(gDay(Day,Offset),Codes).

offset_to_sign_hour_minute(Offset, Sign, Hour, Minute) :-
    nonvar(Offset),
    !,
    H_Off is Offset / 3600,
    M_Off is (Offset - (H_Off * 3600)) / 60,
    (   Offset >= 0
    ->  Sign = '+',
        Hour = H_Off,
        Minute = M_Off
    ;   Sign = '-',
        Hour is -H_Off,
        Minute is -M_Off).

/*
 * time_string(-Time,+String) is det.
 * time_string(+Time,-String) is det.
 */
time_string(Time,String) :-
    nonvar(Time),
    !,
    % ToDo, add appropriate time zone! Doesn't work in xsd_time_string!
    Time = time(HH,MM,SS),
    (   integer(SS)
    ->  format(string(String),
               '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+Z',
               [HH,MM,SS])
    ;   S is floor(SS),
        MS is floor((SS - S) * 1000),
        format(string(String),
               '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+.~|~`0t~d~3+Z',
               [HH,MM,S,MS])
    ).
time_string(time(HN,MN,SN),String) :-
    % So expensive! Let's do this faster somehow.
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(time(HH,MM,SS,_NS,Offset),Codes),
    time_to_internal_time(time(HH,MM,SS,Offset),time(HN,MN,SN)).

duration_string(Duration,String) :-
    nonvar(Duration),
    !,
    Duration = duration(S,Y,M,D,HH,MM,SS),
    (   S = 1
    ->  SP = ''
    ;   SP = '-'),
    (   Y \= 0
    ->  format(atom(YP),'~wY',[Y])
    ;   YP = ''),
    (   M \= 0
    ->  format(atom(MP),'~wM',[M])
    ;   MP = ''),
    (   D \= 0
    ->  format(atom(DP),'~wD',[D])
    ;   DP = ''),
    (   \+ (HH =:= 0, MM =:= 0, SS =:= 0.0)
    ->  TP = 'T'
    ;   TP = ''),
    (   HH \= 0
    ->  format(atom(HHP),'~wH',[HH])
    ;   HHP = ''),
    (   MM \= 0
    ->  format(atom(MMP),'~wM',[MM])
    ;   MMP = ''),
    (   SS \= 0.0
    ->  format(atom(SSP),'~wS',[SS])
    ;   SSP = ''),
    atomic_list_concat([SP,'P',YP,MP,DP,TP,HHP,MMP,SSP],Atom),
    atom_string(Atom,String).
duration_string(duration(Sign,Y,M,D,HH,MM,SS),String) :-
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(duration(Sign,Y,M,D,HH,MM,SS),Codes).

current_xsd_date_time(XSD) :-
    get_time(Unix),
    stamp_date_time(Unix, date(YY,MM,DD,H,M,S,_,_,_), 0),
    SS is floor(S),
    % Force milisecond precision
    NS is floor((S - SS) * 1 000) * 1 000 000,
    date_time_string(date_time(YY,MM,DD,H,M,SS,NS),XSD).

is_number_type(Type) :-
    (   Type = 'http://www.w3.org/2001/XMLSchema#float'
    ;   Type = 'http://www.w3.org/2001/XMLSchema#double'
    ;   basetype_subsumption_of(Type, 'http://www.w3.org/2001/XMLSchema#decimal')
    ).

literal_to_string(X^^'http://www.w3.org/2001/XMLSchema#string', X) :-
    text(X).


/*
 * literal_to_turtle(+Literal,-Turtle_Literal) is det.
 *
 * Deal with precularities of rdf_process_turtle
 */
literal_to_turtle(String@Lang,literal(lang(Lang,S))) :-
    atom_string(S,String).
literal_to_turtle(Elt^^Type,literal(type(Type,S))) :-
    typecast(Elt^^Type, 'http://www.w3.org/2001/XMLSchema#string', [], Val^^_),
    atom_string(S,Val).

/*
 * turtle_to_literal(+Turtle_Literal,+Literal) is det.
 *
 * Deal with precularities of rdf_process_turtle
 */
turtle_to_literal(literal(lang(Lang,S)),String@Lang) :-
    (   atom(S)
    ->  atom_string(S,String)
    ;   S = String),
    !.
turtle_to_literal(literal(type(Type,A)),Val^^Type) :-
    atom_string(A,S),
    typecast(S^^'http://www.w3.org/2001/XMLSchema#string', Type, [], Val^^_),
    !.
turtle_to_literal(literal(L),String^^'http://www.w3.org/2001/XMLSchema#string') :-
    (   atom(L)
    ->  atom_string(L,String)
    ;   L = String).

:- dynamic normalise_warning_/0.

set_normalise_warning :-
    assertz(normalise_warning_).

reset_normalise_warning :-
    retractall(normalise_warning_).

normalise_triple(rdf(X,P,Y,G),rdf(XF,P,YF)) :-
    (   normalise_warning_
    ->  true
    ;   json_log_warning_formatted('Warning: ignoring graph ~q~n', [G]),
        set_normalise_warning
    ),
    normalise_triple(rdf(X,P,Y),rdf(XF,P,YF)).
normalise_triple(rdf(X,P,Y),rdf(XF,P,YF)) :-
    (   X = node(N)
    ->  atomic_list_concat(['_:',N], XF)
    ;   X = XF),

    (   Y = node(M)
    ->  atomic_list_concat(['_:',M], YF)
    %   Bare atom literal needs to be lifted.
    ;   Y = literal(_)
    ->  turtle_to_literal(Y,YF)
    %   Otherwise walk on by...
    ;   Y = YF).

ground_object_storage(String@Lang, lang(String,Lang)) :-
    !.
% CRITICAL: Handle xsd:decimal type - convert to rational then to string with full precision
% This clause must come BEFORE the generic rational clause to handle floats typed as decimals
ground_object_storage(Val^^Type, value(StorageVal,Type)) :-
    (   Type = 'http://www.w3.org/2001/XMLSchema#decimal'
    ;   Type = 'xsd:decimal'),
    !,
    % Convert to rational if needed (handles both floats and existing rationals)
    (   rational(Val)
    ->  Rational = Val
    ;   float(Val)
    ->  Rational is rationalize(Val)
    ;   number(Val)
    ->  Rational is rationalize(Val)
    ;   Rational = Val  % Already in correct form
    ),
    % Now convert rational to decimal string with full precision
    rational(Rational, Num, Den),
    (   Den =:= 1
    ->  StorageVal = Num  % Keep integers as integers
    ;   decimal_precision(Precision),
        rational_to_decimal_string(Rational, StorageVal, Precision)
    ).
% DO NOT convert xsd:decimal rationals - they need to stay as rationals for precision
ground_object_storage(Val^^Type, value(StorageVal,Type)) :-
    rational(Val),
    % Exclude xsd:decimal - it has its own storage handling to preserve precision
    \+ (Type = 'http://www.w3.org/2001/XMLSchema#decimal'),
    \+ (Type = 'xsd:decimal'),
    !,
    % If rational is an integer (denominator=1), keep as integer
    % For xsd:float/double: convert to float
    rational(Val, Num, Den),
    (   Den =:= 1
    ->  StorageVal = Num  % Keep integers as integers
    ;   % xsd:float/double: convert to float
        StorageVal is float(Val)
    ).
ground_object_storage(Val^^Type, value(Val,Type)) :-
    !.
ground_object_storage(O, node(O)).

/*
{{ ... }}
 * We can only make a concrete referrent if all parts are bound.
 */
nonvar_literal(String@Lang, lang(String,Lang)) :-
    nonvar(Lang),
    nonvar(String),
    !.
% CRITICAL: Handle xsd:decimal type - convert to rational then to string with full precision
% This clause must come BEFORE the generic rational clause to handle floats typed as decimals
nonvar_literal(Term^^Type, value(StorageTerm,Type)) :-
    nonvar(Term),
    (   Type = 'http://www.w3.org/2001/XMLSchema#decimal'
    ;   Type = 'xsd:decimal'),
    !,
    % Convert to rational if needed (handles both floats and existing rationals)
    (   rational(Term)
    ->  Rational = Term
    ;   float(Term)
    ->  Rational is rationalize(Term)
    ;   number(Term)
    ->  Rational is rationalize(Term)
    ;   Rational = Term  % Already in correct form
    ),
    % Now convert rational to decimal string with full precision
    rational(Rational, Num, Den),
    (   Den =:= 1
    ->  StorageTerm = Num  % Keep integers as integers
    ;   decimal_precision(Precision),
        rational_to_decimal_string(Rational, StorageTerm, Precision)
    ).
% CRITICAL: Convert rationals for Rust storage (for non-decimal types)
nonvar_literal(Term^^Type, value(StorageTerm,Type)) :-
    nonvar(Term),
    rational(Term),
    !,
    % If rational is an integer (denominator=1), keep as integer
    % For xsd:float/double: convert to float
    rational(Term, Num, Den),
    (   Den =:= 1
    ->  StorageTerm = Num  % Keep integers as integers
    ;   % xsd:float/double: convert to float
        StorageTerm is float(Term)
    ).
nonvar_literal(Term^^Type, value(Term,Type)) :-
    nonvar(Type),
    nonvar(Term),
    !.
nonvar_literal(Val^^Type, _) :-
    once(var(Val) ; var(Type)),
    !.
nonvar_literal(Val@Lang, _) :-
    once(var(Val) ; var(Lang)),
    !.
nonvar_literal(id(Id), id(Id)) :-
    !.
nonvar_literal(O, node(S)) :-
    nonvar(O),
    atom_string(O,S).

object_storage(O,V) :-
    nonvar(O),
    !,
    nonvar_literal(O,V).
object_storage(_O,_V). % Do nothing if input is a variable

storage_atom(TS,T) :-
    var(T),
    !,
    TS = T.
storage_atom(TS,T) :-
    (   atom(T)
    ->  TS = T
    ;   atom_string(TS,T)).

storage_value(X,V) :-
    var(V),
    !,
    X = V.
storage_value(X,V) :-
    (   string(V)
    ->  X = V
    ;   atom_string(V,X)).

storage_literal(X1^^T1,X3^^T2) :-
    storage_atom(T1,T2),
    storage_value(X1,X2),
    % Exclude xsd:decimal from number_string conversion
    % xsd:decimal needs to stay as string to preserve 20-digit precision
    % It will be handled specially by storage_object which converts to rational
    (   is_number_type(T2),
        \+ (T2 = 'http://www.w3.org/2001/XMLSchema#decimal'),
        \+ (T2 = 'xsd:decimal')
    ->  (   string(X2)
        ->  number_string(X3,X2)
        ;   X2 = X3)
    % xsd:decimal: pass through as string (will be converted by storage_object)
    ;   (   T2 = 'http://www.w3.org/2001/XMLSchema#decimal'
        ;   T2 = 'xsd:decimal')
    ->  X2 = X3
    % NOTE: This is considered special because the anyURI condition
    % used to be less strict.
    % There may be data in the DB that does not meet the current
    % datatype specification.
    ;   T2 = 'http://www.w3.org/2001/XMLSchema#anyURI'
    ->  X2 = X3
    ;   typecast(X2^^'http://www.w3.org/2001/XMLSchema#string',T2,
                 [], X3^^_)
    ).
storage_literal(X1@L1,X2@L2) :-
    storage_atom(L1,L2),
    storage_value(X1,X2).

storage_object(lang(S,L),S@L).
% Special handling for xsd:decimal - convert strings/floats from storage back to rationals
storage_object(value(Val,T),Rational^^T) :-
    (   T = 'http://www.w3.org/2001/XMLSchema#decimal'
    ;   T = 'xsd:decimal'),
    !,
    % xsd:decimal can be stored as string (new way with full precision) or float (legacy)
    (   string(Val)
    ->  % New way: stored as decimal string with full 20-digit precision
        string_decimal_to_rational(Val, Rational)
    ;   number(Val)
    ->  % Legacy way: stored as float, convert via string (loses precision)
        format(string(S), "~w", [Val]),
        string_decimal_to_rational(S, Rational)
    ;   throw(error(type_error(decimal_value, Val), _))
    ).
storage_object(value(S,T),S^^T).
storage_object(id(Id),id(Id)).
storage_object(node(S),O) :-
    (   nonvar(O)
    ->  (   atom(O)
        ->  atom_string(O,S)
        ;   O = S)
    ;   atom_string(O,S)).

try_prefix_uri(X,_,X) :-
    nonvar(X),
    X = _A:_B,
    !.
try_prefix_uri(URI,[],URI) :-
    !.
try_prefix_uri(URI,[Prefix-URI_Base|_], Prefixed) :-
    atom(URI_Base),
    escape_pcre(URI_Base,URI_Escaped),
    atomic_list_concat(['^(?P<base>',URI_Escaped,')(?P<rest>.*)$'], Pattern),
    re_matchsub(Pattern, URI, Match, []),
    atom_string(Suffix,Match.rest),
    Prefixed = Prefix : Suffix,
    !.
try_prefix_uri(URI,[_|Rest], Prefixed) :-
    try_prefix_uri(URI,Rest, Prefixed).

length_comp((<),A-_,B-_) :-
    string_length(A,N),
    string_length(B,M),
    N < M,
    !.
length_comp((>),A-_,B-_) :-
    string_length(A,N),
    string_length(B,M),
    N > M,
    !.
% choose lexical if length is identical
length_comp((<),A-_,B-_) :-
    A @< B,
    !.
length_comp((>),A-_,B-_) :-
    A @> B,
    !.
length_comp((=),_,_).

is_special_context_element(A-_) :-
    atom_concat('@', _, A).

uri_to_prefixed(URI, Ctx, Prefixed) :-
    dict_pairs(Ctx,_,Pairs),
    exclude(is_special_context_element, Pairs, Normal_Pairs),
    predsort(length_comp, Normal_Pairs, Sorted_Pairs),
    try_prefix_uri(URI,Sorted_Pairs,Prefixed).

schema_uri_to_prefixed(URI, Ctx, Prefixed) :-
    Schema_URI = (Ctx.'@schema'),
    atom_concat(Schema_URI, Prefixed, URI),
    !.
schema_uri_to_prefixed(URI, Ctx, Prefixed) :-
    uri_to_prefixed(URI, Ctx, Prefixed).

instance_uri_to_prefixed(URI, Ctx, Prefixed) :-
    Instance_URI = (Ctx.'@base'),
    atom_concat(Instance_URI, Prefixed, URI),
    !.
instance_uri_to_prefixed(URI, Ctx, Prefixed) :-
    uri_to_prefixed(URI, Ctx, Prefixed).

uri_eq(Uri1, Uri2, Prefixes) :-
    prefixed_to_uri(Uri1, Prefixes, UriExp1),
    prefixed_to_uri(Uri2, Prefixes, UriExp2),
    atom_string(Uri, UriExp1),
    atom_string(Uri, UriExp2).

prefixed_to_uri(Prefix:Suffix, Ctx, URI) :-
    (   get_dict(Prefix, Ctx, Base)
    ->  true
    ;   throw(error(prefix_error(Prefix, Suffix), _))
    ),
    !,
    atomic_list_concat([Base, Suffix], URI).
prefixed_to_uri(X, Ctx, URI) :-
    atom(X),
    \+ uri_has_protocol(X),
    !,
    (   get_dict('@base', Ctx, Base)
    ->  true
    ;   throw(error(prefix_error('@base', X), _))
    ),
    atomic_list_concat([Base, X], URI).
prefixed_to_uri(URI, _, URI).

prefixed_to_property(Term, Ctx, URI) :-
    get_dict('@schema', Ctx, Schema),
    put_dict('@base', Ctx, Schema, New_Ctx),
    prefixed_to_uri(Term, New_Ctx, URI).

:- begin_tests(turtle_literal_marshalling).

test(literal_to_string, []) :-
    \+ literal_to_string(42^^'http://www.w3.org/2001/XMLSchema#string', _),
    \+ literal_to_string("a"^^'http://www.w3.org/2001/XMLSchema#boolean', _),
    literal_to_string('http://b'^^'http://www.w3.org/2001/XMLSchema#string', 'http://b').

test(date, []) :-
    literal_to_turtle(date_time(-228, 10, 10, 0, 0, 0, 0)^^'http://www.w3.org/2001/XMLSchema#dateTime', literal(type('http://www.w3.org/2001/XMLSchema#dateTime','-228-10-10T00:00:00Z'))).

test(bool, []) :-
    literal_to_turtle(false^^'http://www.w3.org/2001/XMLSchema#boolean', literal(type('http://www.w3.org/2001/XMLSchema#boolean',false))).

test(double, []) :-
    literal_to_turtle(33.4^^'http://www.w3.org/2001/XMLSchema#double', literal(type('http://www.w3.org/2001/XMLSchema#double','33.4'))).

:- end_tests(turtle_literal_marshalling).
