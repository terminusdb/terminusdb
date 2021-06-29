## Reference: White List of XSD datatypes

TerminusDB attempts to be compliant with the (W3C XSD
datatype)[https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/]
specification wherever possible.

The following XSD datetypes should work in TerminusDB:

* `xsd:anySimpleType`
* `xsd:string`
* `xsd:boolean`
* `xsd:decimal`: Currently large floating-point decimals are treated
  as doubles while large integers are treated as bignums. We intend to
  fully support arbitrary precision decimals in a later release.
* `xsd:double`
* `xsd:float`
* `xsd:time`
* `xsd:date`
* `xsd:dateTime`
* `xsd:dateTimeStamp`
* `xsd:gYear`
* `xsd:gMonth`
* `xsd:gDay`
* `xsd:gYearMonth`
* `xsd:gMonthDay`
* `xsd:duration`
* `xsd:yearMonthDuration`
* `xsd:dayTimeDuration`
* `xsd:byte`
* `xsd:short`
* `xsd:int`
* `xsd:long`
* `xsd:unsignedByte`
* `xsd:unsignedShort`
* `xsd:unsignedInt`
* `xsd:unsignedLong`
* `xsd:integer`
* `xsd:positiveInteger`
* `xsd:nonNegativeInteger`
* `xsd:negativeInteger`
* `xsd:nonPositiveInteger`
* `xsd:base64Binary`
* `xsd:hexBinary`
* `xsd:anyURI`
* `xsd:language`
* `xsd:normalizedString`
* `xsd:token`
* `xsd:NMTOKEN`
* `xsd:Name`
* `xsd:NCName`

The following RDF datatypes should work:

* `rdf:PlainLiteral`
* `rdf:langString`
* `rdfs#Literal`

We have also extended the datatype space with the following types:

* `http://terminusdb.com/schema/xdd#coordinate`: has the form `"[3.3,43.1]"` representing coordinates.
* `http://terminusdb.com/schema/xdd#coordinatePolygon`: has the form `"[[3.3,43.1],[15.3,27.1]]"` representing coordinate polygons (closed polylines).
* `http://terminusdb.com/schema/xdd#coordinatePolyline`: has the form `"[[3.3,43.1],[15.3,27.1]]"` representing coordinate polylines.
* `http://terminusdb.com/schema/xdd#dateRange`: has the form `"[2017-10-09,2017-10-09]"` representing ranges of dates.
* `http://terminusdb.com/schema/xdd#gYearRange`: has the form `"[2017,2525]"` representing ranges of years.
* `http://terminusdb.com/schema/xdd#integerRange`: has the form `"[-123,2525]"` representing ranges of years.
* `http://terminusdb.com/schema/xdd#decimalRange`: has the form `"[-123.234,2525.432]"` representing ranges of decimals (with the caveate listed above under `xsd:decimal`.
* `http://terminusdb.com/schema/xdd#json`: Expresses a JSON object as a string
* `http://terminusdb.com/schema/xdd#url`: Is a fully specified absolute URL
* `http://terminusdb.com/schema/xdd#email`: A valid e-mail address
* `http://terminusdb.com/schema/xdd#html`: An HTML document or fragment.

If you feel something important is missing, place feel free to tell
us in our [discord server](https://discord.gg/yTJKAma).
