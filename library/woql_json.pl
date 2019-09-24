:- module(woql_json,[]).

/** <module> WOQL JSON-LD syntax
 * 
 * This file contains the code for conversion of JSON-LD to the WOQL 
 * AST.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* 

Example: 

prefixes([ g='http://localhost/',
       rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#',
       rdfs='http://www.w3.org/2000/01/rdf-schema#',
       dcog='https://datachemist.net/ontology/dcog#'],
       from(g/testDB004,
           limit( 10, start( 0,
                         select([v('Object'),v('Class'),v('Class_Label'),v('Label'),v('Type')],
                         ( (t(v('Object'), rdf/type, v('Class')),
                          ( v('Class') << (dcog/'Entity')), v('Type') = "Entity"),
                         t(v('Class'), rdfs/label, v('Class_Label'),g/testDB004/graph/main/schema),
                         opt(t(v('Object'), rdfs/label, v('Label'))))))))) 


{"@context" : {"@import": "https://terminusdb/contexts/woql/syntax/context.jsonld",
               "@propagate": true,
                "db" : "http://localhost/testDB004/graph"}],
 "from" : [ "db:main", 
            { "select" : [ [ "v:Object", "v:Class", "v:Class_Label", "v:Label", "v:Type" ] , 
                           {"and" : [{ "triple" : ["v:Object", "rdf:type", "v:Class"] }, 
                                     { "<<" : ["v:Class", "dcog:Entity"] }, 
                                     { "=" : ["v:Type", {"@value" :"Entity", "@type" : "xsd:string"}]},
                                     { "quad" : ["v:Class", "rdfs:label", "v:Class_Label", "db:schema"]}, 
                                     { "opt" : [{"triple" : ["v:Object", "rdfs:label", "v:Label"]}]}
                                    ]
                           }
                         ]
            }
          ]
} 


"https://terminusdb/contexts/woql/syntax/context.jsonld"

{"@context" : 
 ["http://terminusdb.com/woql/",
  { 
      "rdf" : "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs" : "http://www.w3.org/2000/01/rdf-schema#",
      "dcog" : "https://datachemist.net/ontology/dcog#",
      "select" : {"@type" : "@id"},
      "from" : {"@type" : "@id"}, 
      "and" : {"@type" : "@id"},
      "triple" : {"@type" : "@id"},   
   }
 ]}
 
 */ 

json_woql(JSON,WOQL) :-
    json_woql(JSON,_{},WOQL).

json_woql(JSON,Ctx,WOQL) :-
    true.

 
