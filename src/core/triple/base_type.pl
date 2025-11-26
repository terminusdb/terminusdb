:- module(base_type,[
              base_type/1,
              base_type_parent/2,
              basetype_subsumption_of/2
          ]).

/** <module> Datatypes for XSD and extensions
 *
 * This module exposes a predicate classifying all basetypes and
 * a subsumption relation between these basetypes.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

%:- use_module(library(mavis)).
:- use_module(core(util)).

%%%% Dacura types prefix

/**
 * base_type(?BaseTypeURI:uri) is nondet.
 *
 * Predicate which gives the available basetypes.

 # Core types
 | xsd:string	| Character strings (but not all Unicode character strings) |
 | xsd:boolean	| true, false |
 | xsd:decimal	| Arbitrary-precision decimal numbers |
 | xsd:integer	| Arbitrary-size integer numbers |
 # IEEE floating-point numbers
 | xsd:double	| 64-bit floating point numbers incl. ±Inf, ±0, NaN |
 | xsd:float	| 32-bit floating point numbers incl. ±Inf, ±0, NaN |
 # Time and date
 | xsd:date	| Dates (yyyy-mm-dd) with or without timezone |
 | xsd:time	| Times (hh:mm:ss.sss…) with or without timezone |
 | xsd:dateTime	| Date and time with or without timezone |
 | xsd:dateTimeStamp	| Date and time with required timezone |
 # Recurring and partial dates
 | xsd:gYear | 	Gregorian calendar year |
 | xsd:gMonth |	Gregorian calendar month |
 | xsd:gDay	| Gregorian calendar day of the month |
 | xsd:gYearMonth |	Gregorian calendar year and month |
 | xsd:gMonthDay |	Gregorian calendar month and day |
 | xsd:duration	| Duration of time |
 | xsd:yearMonthDuration |	Duration of time (months and years only) |
 | xsd:dayTimeDuration |	Duration of time (days, hours, minutes, seconds only) |
 # Limited-range integer numbers
 | xsd:byte	| -128…+127 (8 bit) |
 | xsd:short |	-32768…+32767 (16 bit) |
 | xsd:int |	-2147483648…+2147483647 (32 bit) |
 | xsd:long |	-9223372036854775808…+9223372036854775807 (64 bit) |
 | xsd:unsignedByte |	0…255 (8 bit) |
 | xsd:unsignedShort |	0…65535 (16 bit) |
 | xsd:unsignedInt |	0…4294967295 (32 bit) |
 | xsd:unsignedLong |	0…18446744073709551615 (64 bit) |
 | xsd:positiveInteger |	Integer numbers >0 |
 | xsd:nonNegativeInteger |	Integer numbers ≥0 |
 | xsd:negativeInteger |	Integer numbers <0 |
 | xsd:nonPositiveInteger |	Integer numbers ≤0 |
 # Encoded binary data
 | xsd:hexBinary |	Hex-encoded binary data |
 | xsd:base64Binary |	Base64-encoded binary data |
 # Miscellaneous XSD types
 | xsd:anyURI |	Absolute or relative URIs and IRIs |
 | xsd:language |	Language tags per [BCP47] |
 | xsd:normalizedString |	Whitespace-normalized strings |
 | xsd:token |	Tokenized strings |
 | xsd:NMTOKEN |	XML NMTOKENs |
 | xsd:Name |	XML Names |
 | xsd:NCName |	XML NCNames |
 */
base_type('http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type('http://www.w3.org/2001/XMLSchema#string').
base_type('http://www.w3.org/2001/XMLSchema#boolean').
base_type('http://www.w3.org/2001/XMLSchema#decimal').
base_type('http://www.w3.org/2001/XMLSchema#double').
base_type('http://www.w3.org/2001/XMLSchema#float').
base_type('http://www.w3.org/2001/XMLSchema#time').
base_type('http://www.w3.org/2001/XMLSchema#date').
base_type('http://www.w3.org/2001/XMLSchema#dateTime').
base_type('http://www.w3.org/2001/XMLSchema#dateTimeStamp').
base_type('http://www.w3.org/2001/XMLSchema#gYear').
base_type('http://www.w3.org/2001/XMLSchema#gMonth').
base_type('http://www.w3.org/2001/XMLSchema#gDay').
base_type('http://www.w3.org/2001/XMLSchema#gYearMonth').
base_type('http://www.w3.org/2001/XMLSchema#gMonthDay').
base_type('http://www.w3.org/2001/XMLSchema#duration').
base_type('http://www.w3.org/2001/XMLSchema#yearMonthDuration').
base_type('http://www.w3.org/2001/XMLSchema#dayTimeDuration').
base_type('http://www.w3.org/2001/XMLSchema#byte').
base_type('http://www.w3.org/2001/XMLSchema#short').
base_type('http://www.w3.org/2001/XMLSchema#int').
base_type('http://www.w3.org/2001/XMLSchema#long').
base_type('http://www.w3.org/2001/XMLSchema#unsignedByte').
base_type('http://www.w3.org/2001/XMLSchema#unsignedShort').
base_type('http://www.w3.org/2001/XMLSchema#unsignedInt').
base_type('http://www.w3.org/2001/XMLSchema#unsignedLong').
base_type('http://www.w3.org/2001/XMLSchema#integer').
base_type('http://www.w3.org/2001/XMLSchema#positiveInteger').
base_type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
base_type('http://www.w3.org/2001/XMLSchema#negativeInteger').
base_type('http://www.w3.org/2001/XMLSchema#nonPositiveInteger').
base_type('http://www.w3.org/2001/XMLSchema#base64Binary').
base_type('http://www.w3.org/2001/XMLSchema#hexBinary').
base_type('http://www.w3.org/2001/XMLSchema#anyURI').
base_type('http://www.w3.org/2001/XMLSchema#language').
base_type('http://www.w3.org/2001/XMLSchema#normalizedString').
base_type('http://www.w3.org/2001/XMLSchema#token').
base_type('http://www.w3.org/2001/XMLSchema#NMTOKEN').
base_type('http://www.w3.org/2001/XMLSchema#Name').
base_type('http://www.w3.org/2001/XMLSchema#NCName').
base_type('http://www.w3.org/2001/XMLSchema#NOTATION'). % unimplemented.
base_type('http://www.w3.org/2001/XMLSchema#QName'). % unimplemented.
base_type('http://www.w3.org/2001/XMLSchema#ID'). % unimplemented.
base_type('http://www.w3.org/2001/XMLSchema#IDREF'). % unimplemented.
base_type('http://www.w3.org/2001/XMLSchema#ENTITY'). % unimplemented.
base_type('http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'). % Not fullly implemented
base_type('http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral').
base_type('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString').
base_type('http://www.w3.org/2000/01/rdf-schema#Literal').
base_type('http://terminusdb.com/schema/xdd#coordinate').
base_type('http://terminusdb.com/schema/xdd#coordinatePolygon').
base_type('http://terminusdb.com/schema/xdd#coordinatePolyline').
base_type('http://terminusdb.com/schema/xdd#dateRange').
base_type('http://terminusdb.com/schema/xdd#gYearRange').
base_type('http://terminusdb.com/schema/xdd#integerRange').
base_type('http://terminusdb.com/schema/xdd#decimalRange').
base_type('http://terminusdb.com/schema/xdd#json').
base_type('http://terminusdb.com/schema/xdd#url').
base_type('http://terminusdb.com/schema/xdd#email').
base_type('http://terminusdb.com/schema/xdd#html').
base_type('http://terminusdb.com/schema/sys#JSONDocument').
base_type('http://terminusdb.com/schema/sys#JSON').
base_type('http://terminusdb.com/schema/sys#Dictionary').

/**
 * base_type_parent(+ChildXSDURI:uri,+ParentXSDURI:uri) is det.
 * base_type_parent(+ChildXSDURI:uri,-ParentXSDURI:uri) is det.
 * base_type_parent(-ChildXSDURI:uri,+ParentXSDURI:uri) is nondet.
 * base_type_parent(?ChildXSDURI:uri,?ParentXSDURI:uri) is nondet.
 *
 * Implements the child parent relationship between basetypes.
 *
 * We visually represent the hierarchy with whitespace - please maintain!
 */
base_type_parent('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString','http://www.w3.org/2000/01/rdf-schema#Literal').
base_type_parent('http://www.w3.org/2000/01/rdf-schema#XMLLiteral','http://www.w3.org/2000/01/rdf-schema#Literal').
base_type_parent('http://www.w3.org/2001/XMLSchema#anySimpleType','http://www.w3.org/2000/01/rdf-schema#Literal').
base_type_parent('http://www.w3.org/2001/XMLSchema#string','http://www.w3.org/2001/XMLSchema#anySimpleType').
 base_type_parent('http://www.w3.org/2001/XMLSchema#anyURI','http://www.w3.org/2001/XMLSchema#string').
 base_type_parent('http://terminusdb.com/schema/xdd#url','http://www.w3.org/2001/XMLSchema#string').
 base_type_parent('http://terminusdb.com/schema/xdd#email','http://www.w3.org/2001/XMLSchema#string').
 base_type_parent('http://terminusdb.com/schema/xdd#json','http://www.w3.org/2001/XMLSchema#string').
 base_type_parent('http://terminusdb.com/schema/xdd#html','http://www.w3.org/2001/XMLSchema#string').
 base_type_parent('http://www.w3.org/2001/XMLSchema#normalizedString', 'http://www.w3.org/2001/XMLSchema#string').
   base_type_parent('http://www.w3.org/2001/XMLSchema#token', 'http://www.w3.org/2001/XMLSchema#normalizedString').
      base_type_parent('http://www.w3.org/2001/XMLSchema#language', 'http://www.w3.org/2001/XMLSchema#token').
      base_type_parent('http://www.w3.org/2001/XMLSchema#NMTOKEN', 'http://www.w3.org/2001/XMLSchema#token').
      base_type_parent('http://www.w3.org/2001/XMLSchema#Name', 'http://www.w3.org/2001/XMLSchema#token').
        base_type_parent('http://www.w3.org/2001/XMLSchema#NCName', 'http://www.w3.org/2001/XMLSchema#Name').
          base_type_parent('http://www.w3.org/2001/XMLSchema#ID','http://www.w3.org/2001/XMLSchema#NCName'). % unimplemented.
          base_type_parent('http://www.w3.org/2001/XMLSchema#IDREF','http://www.w3.org/2001/XMLSchema#NCName'). % unimplemented.
          base_type_parent('http://www.w3.org/2001/XMLSchema#ENTITY','http://www.w3.org/2001/XMLSchema#NCName'). % unimplemented.
 base_type_parent('http://terminusdb.com/schema/xdd#url','http://www.w3.org/2001/XMLSchema#string').
 base_type_parent('http://terminusdb.com/schema/xdd#email','http://www.w3.org/2001/XMLSchema#string').
base_type_parent('http://www.w3.org/2001/XMLSchema#decimal','http://www.w3.org/2001/XMLSchema#anySimpleType').
  base_type_parent('http://www.w3.org/2001/XMLSchema#integer','http://www.w3.org/2001/XMLSchema#decimal').
    base_type_parent('http://www.w3.org/2001/XMLSchema#nonPositiveInteger','http://www.w3.org/2001/XMLSchema#integer').
      base_type_parent('http://www.w3.org/2001/XMLSchema#negativeInteger','http://www.w3.org/2001/XMLSchema#nonPositiveInteger').
    base_type_parent('http://www.w3.org/2001/XMLSchema#long','http://www.w3.org/2001/XMLSchema#integer').
      base_type_parent('http://www.w3.org/2001/XMLSchema#int','http://www.w3.org/2001/XMLSchema#long').
        base_type_parent('http://www.w3.org/2001/XMLSchema#short','http://www.w3.org/2001/XMLSchema#int').
          base_type_parent('http://www.w3.org/2001/XMLSchema#byte','http://www.w3.org/2001/XMLSchema#short').
    base_type_parent('http://www.w3.org/2001/XMLSchema#nonNegativeInteger','http://www.w3.org/2001/XMLSchema#integer').
      base_type_parent('http://www.w3.org/2001/XMLSchema#unsignedLong','http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
        base_type_parent('http://www.w3.org/2001/XMLSchema#unsignedInt','http://www.w3.org/2001/XMLSchema#unsignedLong').
          base_type_parent('http://www.w3.org/2001/XMLSchema#unsignedShort','http://www.w3.org/2001/XMLSchema#unsignedInt').
            base_type_parent('http://www.w3.org/2001/XMLSchema#unsignedByte','http://www.w3.org/2001/XMLSchema#unsignedShort').
      base_type_parent('http://www.w3.org/2001/XMLSchema#positiveInteger','http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
base_type_parent('http://www.w3.org/2001/XMLSchema#NOTATION','http://www.w3.org/2001/XMLSchema#anySimpleType'). % unimplemented.
base_type_parent('http://www.w3.org/2001/XMLSchema#QName','http://www.w3.org/2001/XMLSchema#anySimpleType'). % unimplemented.
base_type_parent('http://www.w3.org/2001/XMLSchema#double','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#boolean','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#base64Binary','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#hexBinary','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#anyURI','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#date','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#time','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#dateTime','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#dateTimeStamp','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#gYear','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#gYearMonth','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#gMonth','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#gMonthDay','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#gDay','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://www.w3.org/2001/XMLSchema#duration','http://www.w3.org/2001/XMLSchema#anySimpleType').
  base_type_parent('http://www.w3.org/2001/XMLSchema#dayTimeDuration','http://www.w3.org/2001/XMLSchema#duration').
  base_type_parent('http://www.w3.org/2001/XMLSchema#yearMonthDuration','http://www.w3.org/2001/XMLSchema#duration').
base_type_parent('http://terminusdb.com/schema/xdd#coordinate','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://terminusdb.com/schema/xdd#coordinatePolygon','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://terminusdb.com/schema/xdd#coordinatePolyLine','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://terminusdb.com/schema/xdd#dateRange','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://terminusdb.com/schema/xdd#gYearRange','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://terminusdb.com/schema/xdd#integerRange','http://www.w3.org/2001/XMLSchema#anySimpleType').
base_type_parent('http://terminusdb.com/schema/xdd#decimalRange','http://www.w3.org/2001/XMLSchema#anySimpleType').

/**
 * basetype_subsumption_of(?Sub,?Super) is nondet.
 *
 * Implements the subsumption latice for concrete base types by making use of reflexivity and
 * base_type_parent\2.
 */
basetype_subsumption_of(T,T) :- base_type(T).
basetype_subsumption_of(Sub,Super) :-
    base_type_parent(Sub,Parent), basetype_subsumption_of(Parent,Super).
