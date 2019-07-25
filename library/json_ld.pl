:- module(json_ld, [expand/2, expand/3, compress/3]).

/** <module> JSON_LD
 * 
 * Definitions for translating and manipulating JSON_LD
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

:- use_module(library(pairs)).
:- use_module(utils).
:- use_module(library(http/json)).
:- use_module(library(mavis)).

/** 
 * expand(+JSON_LD, -JSON) is det.
 * 
 * Expands from JSON_LD prefixed format to fully expanded form.
 */ 
expand(JSON_LD, JSON) :-
    get_dict_default('@context', JSON_LD, Context, _{}),
    expand_context(Context,Context_Expanded),
    expand(JSON_LD, Context_Expanded, JSON).

/** 
 * expand(+JSON_LD, +Context:dict, -JSON) is det.
 * 
 * Expands from JSON_LD prefixed format to fully expanded form.
 */ 
expand(JSON_LD, Context, JSON) :-
    is_dict(JSON_LD),
    !,
    get_dict_default('@context', JSON_LD, New_Context, _{}),
    
    merge_dictionaries(Context,New_Context,Local_Context),
    
    dict_keys(JSON_LD,Keys),
    findall(Key-Value,
            (
                member(K,Keys),
                get_dict(K,JSON_LD,V),

                * format('~nKey ~q', [K]),
                
                (   member(K,['@id','@type'])
                ->  prefix_expand(V,Local_Context,Value),
                    Key = K
                ;   K='@context'
                ->  Key = K,
                    Value = V
                ;   expand_key(K,Local_Context,Key_Candidate,Key_Context),
                    expand(V,Local_Context,Expanded),
                    (   is_dict(Expanded)
                    ->  merge_dictionaries(Key_Context,Expanded,Value),
                        Key = Key_Candidate
                    ;   _{'@type' : "@id"} = Key_Context
                    ->  Key = Key_Candidate,
                        Value = _{'@id' : Expanded}
                    ;   _{'@type' : "@id", '@id' : ID} = Key_Context,
                        prefix_expand(ID,Local_Context,Key),
                        Value = _{'@id' : Expanded}
                    ;   _{'@type' : Type} = Key_Context
                    ->  prefix_expand(Type,Local_Context,EType),
                        Key = Key_Candidate,
                        Value = _{'@value' : Expanded,
                                  '@type' : EType}
                    ;   _{} = Key_Context
                    ->  Key = Key_Candidate,
                        Value = Expanded
                    ;   format(atom(M),'Unknown key context ~q', [Key_Context]),
                        throw(error(M))
                    )
                ),

                * format('~nValue ~q', [Value])
            ),
            Data),
    dict_create(JSON,_,Data).
expand(JSON_LD, Context, JSON) :-
    is_list(JSON_LD),
    !,
    maplist({Context}/[JL,J]>>(expand(JL,Context,J)),JSON_LD,JSON).
expand(JSON, _Context, JSON) :-
    string(JSON),
    !,
    true.

prefix_expand(K,Context,Key) :-
    (   split_atom(K,':',[Prefix,Suffix]),
        get_dict(Prefix,Context,Expanded)
    ->  atom_concat(Expanded,Suffix,Key)
    ;   K = Key).

/* 
 * expand_context(+Context,-Context_Expanded) is det. 
 *
 * Expand all prefixes in the context for other elements of the context
 */
expand_context(Context,Context_Expanded) :-
    dict_pairs(Context,_,Pairs),
    maplist({Context}/[K-V,Key-V]>>(prefix_expand(K,Context,Key)), Pairs, Expanded_Pairs),
    dict_create(Context_Expanded, _, Expanded_Pairs).

/* 
 * expand_key(+K,+Context,-Key,-Value) is det.
 */
expand_key(K,Context,Key,Value) :-
    prefix_expand(K,Context,Key_Candidate),
    (   get_dict(Key_Candidate,Context,R)
    ->  (   is_dict(R)
        ->  Key = Key_Candidate,
            Value = R
        ;   string_to_atom(R,Key),
            Value = _{})
    ;   Key = Key_Candidate,
        Value = _{}).

/* 
 * merge_dictionaries(+Dict1,+Dict2,-Dict3) is det.
 *
 * Merge favouring left. 
 */ 
merge_dictionaries(Dict1,Dict2,Dict3) :-
    dict_pairs(Dict1,_,Dict1_Pairs),
    dict_pairs(Dict2,_,Dict2_Pairs),
    merge_set(Dict2_Pairs,Dict1_Pairs,Sorted),
    group_pairs_by_key(Sorted,Grouped),
    maplist([Key-[Value|_],Key-Value]>>(true),Grouped,Data),
    dict_create(Dict3,_,Data).



/* 
 * compress_step(Ctx,Step) is det.
 */
compress_step(Ctx,Step) :-
    dict_keys(Ctx,Keys),
    include(
    findall(Key-Value,
            (
                true
            ),
            Data),
    dict_create(Step,_,Data).

/* 
 * compress_context(Ctx,Comp) is det. 
 * 
 * Compresses a context until it reaches a fixed point. 
 */ 
compress_context(Ctx,Comp) :-
    compress_step(Ctx,Step),
    (   Ctx = Step
    ->  Comp = Step
    ;   compress_context(Ctx,Comp)
    ).

compress_context(Context,Context_Comp) :-
    compress_fixed_point(Context,Compressed),
    dict_create(Context_Comp,_,Compressed).

compress(JSON,Context,JSON_LD) :-
    compress_context(Context,Compressed),
    compress_aux(JSON,Compressed,JSON_LD).
    
compress_aux(JSON,Context,JSON_LD) :-
    is_dict(JSON),
    !,
    
    true.
compress_aux(JSON,Context,JSON_LD) :-
    is_list(JSON),
    !,
    true.
compress_aux(JSON,Context,JSON_LD) :-
    string(JSON),
    !,
    true.


/* 
Atom='
{
  "@context": {
    "ical": "http://www.w3.org/2002/12/cal/ical#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "ical:dtstart": {
      "@type": "xsd:dateTime"
    }
  },
  "ical:summary": "Lady Gaga Concert",
  "ical:location": "New Orleans Arena, New Orleans, Louisiana, USA",
  "ical:dtstart": "2011-04-09T20:00:00Z"
}',
atom_json_dict(Atom, JSON,[]).

Atom='
[
  {
    "http://www.w3.org/2002/12/cal/ical#dtstart": [
      {
        "@type": "http://www.w3.org/2001/XMLSchema#dateTime",
        "@value": "2011-04-09T20:00:00Z"
      }
    ],
    "http://www.w3.org/2002/12/cal/ical#location": [
      {
        "@value": "New Orleans Arena, New Orleans, Louisiana, USA"
      }
    ],
    "http://www.w3.org/2002/12/cal/ical#summary": [
      {
        "@value": "Lady Gaga Concert"
      }
    ]
  }
]',
atom_json_dict(Atom, JSON,[]).


    Atom='
{
  "@context": {
    "ical": "http://www.w3.org/2002/12/cal/ical#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "ical:dtstart": {
      "@type": "xsd:dateTime"
    }
  },
  "ical:summary": "Lady Gaga Concert",
  "ical:location": "New Orleans Arena, New Orleans, Louisiana, USA",
  "ical:dtstart": "2011-04-09T20:00:00Z"
}',
atom_json_dict(Atom, JSON,[]).



Atom = ' 

{
  "@context": {
    "@version": 1.1,
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "foaf": "http://xmlns.com/foaf/0.1/",
    "foaf:homepage": { "@type": "@id" },
    "picture": { "@id": "foaf:depiction", "@type": "@id" }
  },
  "@id": "http://me.markus-lanthaler.com/",
  "@type": "foaf:Person",
  "foaf:name": "Markus Lanthaler",
  "foaf:homepage": "http://www.markus-lanthaler.com/",
  "picture": "http://twitter.com/account/profile_image/markuslanthaler"
}', 
atom_json_dict(Atom,JSON, []), 
expand(JSON,JSON_Ex).


expand_key(picture, _4830{'@version':1.1, foaf:"http://xmlns.com/foaf/0.1/", 
                          'foaf:homepage':_4194{'@type':"@id"}, 
                          picture:_4238{'@id':"foaf:depiction", '@type':"@id"}, 
                          xsd:"http://www.w3.org/2001/XMLSchema#"}, 
           X, D).



Atom = '{
  "@context": {
    "ical": "http://www.w3.org/2002/12/cal/ical#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "ical:dtstart": {
      "@type": "xsd:dateTime"
    }
  },
  "ical:summary": "Lady Gaga Concert",
  "ical:location": "New Orleans Arena, New Orleans, Louisiana, USA",
  "ical:dtstart": "2011-04-09T20:00:00Z"
}',
atom_json_dict(Atom,JSON, []), 
expand(JSON,JSON_Ex).



{'@context':_4078{'http://www.w3.org/2002/12/cal/ical#dtstart':_4086{'@type':"xsd:dateTime"}}

*/
