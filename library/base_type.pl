:- module(base_type,[
              baseType/1,
              baseTypeParent/2
          ]).

/** <module> Datatypes for XSD and extensions
 * 
 * This module exposes a predicate classifying all basetypes and 
 * a subsumption relation between these basetypes.
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of RegulumDB.                                      *
 *                                                                       *
 *  RegulumDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  RegulumDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

%:- use_module(library(mavis)).
:- use_module(utils).

%%%% Dacura types prefix
%% dacura:coordinatePolygon
%% dacura:dateRange ???

/** 
 * baseType(?BaseTypeURI:uri) is nondet.
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
 | xsd:integer |	-2147483648…+2147483647 (32 bit) |
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
baseType('http://www.w3.org/2001/XMLSchema#anySimpleType').
baseType('http://www.w3.org/2001/XMLSchema#string'). 
baseType('http://www.w3.org/2001/XMLSchema#boolean'). 
baseType('http://www.w3.org/2001/XMLSchema#decimal'). 
baseType('http://www.w3.org/2001/XMLSchema#double'). 
baseType('http://www.w3.org/2001/XMLSchema#float'). 
baseType('http://www.w3.org/2001/XMLSchema#time').
baseType('http://www.w3.org/2001/XMLSchema#date').  % Unimplemented!!! DDD
baseType('http://www.w3.org/2001/XMLSchema#dateTime'). 
baseType('http://www.w3.org/2001/XMLSchema#dateTimeStamp').
baseType('http://www.w3.org/2001/XMLSchema#gYear'). 
baseType('http://www.w3.org/2001/XMLSchema#gMonth'). 
baseType('http://www.w3.org/2001/XMLSchema#gDay'). 
baseType('http://www.w3.org/2001/XMLSchema#gYearMonth'). 	
baseType('http://www.w3.org/2001/XMLSchema#gMonthDay'). 
baseType('http://www.w3.org/2001/XMLSchema#duration'). 
baseType('http://www.w3.org/2001/XMLSchema#yearMonthDuration'). 
baseType('http://www.w3.org/2001/XMLSchema#dayTimeDuration'). 
baseType('http://www.w3.org/2001/XMLSchema#byte'). 
baseType('http://www.w3.org/2001/XMLSchema#short'). 
baseType('http://www.w3.org/2001/XMLSchema#integer'). 
baseType('http://www.w3.org/2001/XMLSchema#long'). 
baseType('http://www.w3.org/2001/XMLSchema#unsignedByte'). 
baseType('http://www.w3.org/2001/XMLSchema#unsignedInt'). 
baseType('http://www.w3.org/2001/XMLSchema#unsignedLong'). 
baseType('http://www.w3.org/2001/XMLSchema#positiveInteger'). 
baseType('http://www.w3.org/2001/XMLSchema#nonNegativeInteger'). 
baseType('http://www.w3.org/2001/XMLSchema#negativeInteger'). 
baseType('http://www.w3.org/2001/XMLSchema#nonPositiveInteger'). 
baseType('http://www.w3.org/2001/XMLSchema#base64Binary'). 
baseType('http://www.w3.org/2001/XMLSchema#anyURI'). 
baseType('http://www.w3.org/2001/XMLSchema#language'). 
baseType('http://www.w3.org/2001/XMLSchema#normalizedString'). 
baseType('http://www.w3.org/2001/XMLSchema#token'). 
baseType('http://www.w3.org/2001/XMLSchema#NMTOKEN'). 
baseType('http://www.w3.org/2001/XMLSchema#Name'). 
baseType('http://www.w3.org/2001/XMLSchema#NCName').
baseType('http://www.w3.org/2001/XMLSchema#NOTATION'). % unimplemented.
baseType('http://www.w3.org/2001/XMLSchema#QName'). % unimplemented.
baseType('http://www.w3.org/2001/XMLSchema#ID'). % unimplemented.
baseType('http://www.w3.org/2001/XMLSchema#IDREF'). % unimplemented.
baseType('http://www.w3.org/2001/XMLSchema#ENTITY'). % unimplemented.
baseType('http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'). % Not fullly implemented
baseType('http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral').
baseType('http://www.w3.org/2000/01/rdf-schema#Literal').
baseType('https://datachemist.net/ontology/xdd#coordinatePolygon').
baseType('https://datachemist.net/ontology/xdd#coordinatePolyline').
baseType('https://datachemist.net/ontology/xdd#gYearRange').
baseType('https://datachemist.net/ontology/xdd#integerRange').
baseType('https://datachemist.net/ontology/xdd#decimalRange').
baseType('https://datachemist.net/ontology/xdd#pesel').
baseType('https://datachemist.net/ontology/xdd#url').
baseType('https://datachemist.net/ontology/xdd#email').

/** 
 * baseTypeParent(+ChildXSDURI:uri,+ParentXSDURI:uri) is det.
 * baseTypeParent(+ChildXSDURI:uri,-ParentXSDURI:uri) is det.
 * baseTypeParent(-ChildXSDURI:uri,+ParentXSDURI:uri) is nondet.
 * baseTypeParent(?ChildXSDURI:uri,?ParentXSDURI:uri) is nondet.
 *
 * Implements the child parent relationship between basetypes.
 * 
 * We visually represent the heirarchy with whitespace - please maintain!
 */
baseTypeParent('http://www.w3.org/2000/01/rdf-schema#XMLLiteral','http://www.w3.org/2000/01/rdf-schema#Literal').
baseTypeParent('http://www.w3.org/2001/XMLSchema#anySimpleType','http://www.w3.org/2000/01/rdf-schema#Literal').
baseTypeParent('http://www.w3.org/2001/XMLSchema#string','http://www.w3.org/2001/XMLSchema#anySimpleType').
 baseTypeParent('http://www.w3.org/2001/XMLSchema#normalizedString', 'http://www.w3.org/2001/XMLSchema#string').
   baseTypeParent('http://www.w3.org/2001/XMLSchema#token', 'http://www.w3.org/2001/XMLSchema#normalizedString').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#language', 'http://www.w3.org/2001/XMLSchema#token').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#NMTOKEN', 'http://www.w3.org/2001/XMLSchema#token').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#Name', 'http://www.w3.org/2001/XMLSchema#token').
        baseTypeParent('http://www.w3.org/2001/XMLSchema#NCName', 'http://www.w3.org/2001/XMLSchema#Name').
          baseTypeParent('http://www.w3.org/2001/XMLSchema#ID','http://www.w3.org/2001/XMLSchema#NCName'). % unimplemented.
          baseTypeParent('http://www.w3.org/2001/XMLSchema#IDREF','http://www.w3.org/2001/XMLSchema#NCName'). % unimplemented.
          baseTypeParent('http://www.w3.org/2001/XMLSchema#ENTITY','http://www.w3.org/2001/XMLSchema#NCName'). % unimplemented.
 baseTypeParent('https://datachemist.net/ontology/xdd#url','http://www.w3.org/2001/XMLSchema#string').
 baseTypeParent('https://datachemist.net/ontology/xdd#email','http://www.w3.org/2001/XMLSchema#string').
baseTypeParent('http://www.w3.org/2001/XMLSchema#decimal','http://www.w3.org/2001/XMLSchema#anySimpleType').
  baseTypeParent('http://www.w3.org/2001/XMLSchema#integer','http://www.w3.org/2001/XMLSchema#decimal').
    baseTypeParent('http://www.w3.org/2001/XMLSchema#nonPositiveInteger','http://www.w3.org/2001/XMLSchema#integer').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#negativeInteger','http://www.w3.org/2001/XMLSchema#nonPositiveInteger').
    baseTypeParent('http://www.w3.org/2001/XMLSchema#long','http://www.w3.org/2001/XMLSchema#integer').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#short','http://www.w3.org/2001/XMLSchema#long').
        baseTypeParent('http://www.w3.org/2001/XMLSchema#byte','http://www.w3.org/2001/XMLSchema#short').
    baseTypeParent('http://www.w3.org/2001/XMLSchema#nonNegativeInteger','http://www.w3.org/2001/XMLSchema#integer').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#unsignedLong','http://www.w3.org/2001/XMLSchema#nonNegativeInteger'). 
        baseTypeParent('http://www.w3.org/2001/XMLSchema#unsignedInt','http://www.w3.org/2001/XMLSchema#unsginedLong').
          baseTypeParent('http://www.w3.org/2001/XMLSchema#unsignedShort','http://www.w3.org/2001/XMLSchema#unsignedInt').
            baseTypeParent('http://www.w3.org/2001/XMLSchema#unsignedByte','http://www.w3.org/2001/XMLSchema#unsignedShort').
      baseTypeParent('http://www.w3.org/2001/XMLSchema#positiveInteger','http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
    baseTypeParent('https://datachemist.net/ontology/xdd#pesel','http://www.w3.org/2001/XMLSchema#integer').
baseTypeParent('http://www.w3.org/2001/XMLSchema#NOTATION','http://www.w3.org/2001/XMLSchema#anySimpleType'). % unimplemented.
baseTypeParent('http://www.w3.org/2001/XMLSchema#QName','http://www.w3.org/2001/XMLSchema#anySimpleType'). % unimplemented.
baseTypeParent('http://www.w3.org/2001/XMLSchema#double','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#float','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#boolean','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#base64Binary','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#hexBinary','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#anyURI','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#date','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#time','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#dateTime','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#gYear','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#gYearMonth','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#gMonth','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#gMonthDay','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#gDay','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('http://www.w3.org/2001/XMLSchema#duration','http://www.w3.org/2001/XMLSchema#anySimpleType').
  baseTypeParent('http://www.w3.org/2001/XMLSchema#dayTimeDuration','http://www.w3.org/2001/XMLSchema#duration').
  baseTypeParent('http://www.w3.org/2001/XMLSchema#yearMonthDuration','http://www.w3.org/2001/XMLSchema#duration').
baseTypeParent('https://datachemist.net/ontology/xdd#coordinatePolygon','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#coordinatePolyLine','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#coordinate','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#gYearRange','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#integerRange','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#decimalRange','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#url','http://www.w3.org/2001/XMLSchema#anySimpleType').
baseTypeParent('https://datachemist.net/ontology/xdd#email','http://www.w3.org/2001/XMLSchema#anySimpleType').
