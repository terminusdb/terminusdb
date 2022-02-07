:- module('document/patch',
          [
              simple_patch/3,
              diff_op/2,
              get_dict_or_null/3
          ]).

:- use_module(core(util)).
:- use_module(core('util/tables')).

:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(when)).
:- use_module(library(plunit)).

simple_patch(Diff,JSON_In,JSON_Out) :-
    is_dict(JSON_In),
    !,
    dict_keys(JSON_In,Doc_Keys),
    dict_keys(Diff,Diff_Keys),
    union(Doc_Keys,Diff_Keys,Keys),
    pairs_and_conflicts_from_keys(Keys,JSON_In,Diff,Pairs),
    dict_create(JSON_Out,_,Pairs).
simple_patch(Diff,Before,After) :-
    is_list(Diff),
    !,
    maplist([D,B,A]>>simple_patch(D,B,A), Diff, Before, After).
simple_patch(Diff,Before,After) :-
    diff_op(Diff,Op),
    !,
    simple_op_diff_value(Op, Diff, Before, After).
simple_patch(Before,Before,Before).

simple_patch_list(Diff,List_In,List_Out) :-
    is_list(List_In),
    !,
    maplist([Patch,Elt,Patched]>>simple_patch(Patch,Elt,Patched),
            Diff,List_In,List_Out).

simple_patch_table(Diff,Table_In,Table_Out) :-
    is_list(Table_In),
    !,
    rows(Diff,Diff_Row_Length),
    columns(Diff,Diff_Column_Length),
    rows(Table_In,Row_Length),
    columns(Table_In,Column_Length),
    Diff_Row_Length = Row_Length,
    Diff_Column_Length = Column_Length,
    maplist([Diff_Row,Row_In,Row_Out]>>
            maplist([Patch,Elt,Patched]>>
                    simple_patch(Patch,Elt,Patched),
                    Diff_Row,Row_In,Row_Out),
            Diff,Table_In,Table_Out).

%%% Conflict utils
pairs_and_conflicts_from_keys([], _, _, []).
pairs_and_conflicts_from_keys([Key|Keys], JSON, Diff, [Result|Values]) :-
    simple_patch_key_value(Key,JSON,Diff,Result),
    !,
    pairs_and_conflicts_from_keys(Keys, JSON, Diff, Values).


%%% Patch operations
simple_op_diff_value('ForceValue', Diff, _, After) :-
    get_dict('@after', Diff, After).
simple_op_diff_value('SwapValue', Diff, Before, After) :-
    get_dict('@before', Diff, Before),
    get_dict('@after', Diff, After).
simple_op_diff_value('CopyList', Diff, Before, After) :-
    get_dict('@to', Diff, To),
    get_dict('@rest', Diff, Next_Diff),
    nth_list_prefix_suffix(To,Before,Prefix,Suffix),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op,Next_Diff,Suffix, Rest),
    append(Prefix,Rest,After).
simple_op_diff_value('SwapList', Diff, Before, After) :-
    get_dict('@before', Diff, Before_Prefix),
    get_dict('@after', Diff, After_Prefix),
    get_dict('@rest', Diff, Next_Diff),
    append(Before_Prefix,Before_Suffix,Before),
    append(After_Prefix,After_Suffix,After),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op,Next_Diff, Before_Suffix, After_Suffix).
simple_op_diff_value('PatchList', Diff, Before, After) :-
    %% This should recover when the patch list length is smaller instead of failing.
    get_dict('@patch', Diff, Patch),
    get_dict('@rest', Diff, Rest),
    length(Patch, To),
    nth_list_prefix_suffix(To,Before,Prefix_Before,Suffix_Before),
    simple_patch_list(Patch,Prefix_Before,Prefix_After),
    diff_op(Rest,Op),
    simple_op_diff_value(Op,Rest,Suffix_Before,Suffix_After),
    append(Prefix_After,Suffix_After,After).
simple_op_diff_value('KeepList', _Diff, Same, Same).
simple_op_diff_value('CopyTable', Diff, Before, After) :-
    get_dict('@to_row', Diff, To_Row),
    get_dict('@to_column', Diff, To_Column),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    split_table(Before,To_Row,To_Column,Top_Left,Top_Right,Bottom_Left,Bottom_Right),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New),
    split_table(After, To_Row, To_Column, Top_Left, TR_New, BL_New, BR_New).
simple_op_diff_value('SwapTable', Diff, Before, After) :-
    get_dict('@before', Diff, Top_Left_Before),
    get_dict('@after', Diff, Top_Left_After),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    rows(Top_Left_Before,To_Row_Before),
    columns(Top_Left_Before,To_Column_Before),
    split_table(Before,To_Row_Before,To_Column_Before,
                 Top_Left_Before,Top_Right,Bottom_Left,Bottom_Right),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New),
    rows(Top_Left_After,To_Row_After),
    columns(Top_Left_After,To_Column_After),
    split_table(After, To_Row_After, To_Column_After, Top_Left_After, TR_New, BL_New, BR_New).
simple_op_diff_value('PatchTable', Diff, Before, After) :-
    get_dict('@top_left', Diff, TL_Diff),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    rows(TL_Diff,To_Row),
    columns(TL_Diff,To_Column),
    split_table(Before,To_Row,To_Column,
                 Top_Left_Before,Top_Right,Bottom_Left,Bottom_Right),
    simple_patch_table(TL_Diff,Top_Left_Before, TL_New),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New),
    rows(Top_Left_After,To_Row_After),
    columns(Top_Left_After,To_Column_After),
    split_table(After, To_Row_After, To_Column_After, TL_New, TR_New, BL_New, BR_New).
simple_op_diff_value('ModifyTable', Diff, Before, After) :-
    Diff = _{ '@op' : "ModifyTable",
              dimensions: _{ '@before' : [C1,R1],
                             '@after' : [C2,R2]
                           },
              copies : Copies,
              moves: Moves,
              inserts: Inserts,
              deletes: Deletes
            },
    columns(Before,C1),
    rows(Before,R1),
    empty_table(C2,R2,After0),
    table_check_deletes(Deletes,Before),
    table_add_copies(Copies,Before,After0,After1),
    table_add_moves(Moves,Before,After1,After2),
    table_add_inserts(Inserts,After2,After).
simple_op_diff_value('KeepTable', _Diff, Same, Same).

empty_row(N, Row) :-
    length(Row, N),
    maplist([null]>>true, Row).

empty_table(_,0, []) :- !.
empty_table(Columns,Rows,[Row|Table]) :-
    New_Rows is Rows - 1,
    empty_row(Columns,Row),
    empty_table(Columns,New_Rows,Table).

table_check_delete(Delete,Before) :-
    _{'@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
      '@value': V} = Delete,
    table_window(X,W,Y,H,Before,V).

table_check_deletes([],_Before).
table_check_deletes([Delete|Deletes],Before) :-
    table_check_delete(Delete,Before),
    table_check_deletes(Deletes,Before).

table_add_copy(Copy,Before,After0,After1) :-
    _{'@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
      '@value': Window} = Copy,
    table_window(X,W,Y,H,Before,Window),
    replace_table_window(X,Y,Window,After0,After1).

table_add_copies([],_,After,After).
table_add_copies([Copy|Copies],Before,After0,AfterN) :-
    table_add_copy(Copy,Before,After0,After1),
    table_add_copies(Copies,Before,After1,AfterN).

table_add_move(Move,Before,After0,After1) :-
    _{'@from' : _{'@height':H1,'@width':W1,'@x':X1,'@y':Y1},
      '@to' : _{'@height':_H2,'@width':_W2,'@x':X2,'@y':Y2},
      '@value': Window} = Move,
    table_window(X1,W1,Y1,H1,Before,Window),
    replace_table_window(X2,Y2,Window,After0,After1).

table_add_moves([],_,After,After).
table_add_moves([Move|Moves],Before,After0,AfterN) :-
    table_add_move(Move,Before,After0,After1),
    table_add_moves(Moves,Before,After1,AfterN).

table_add_insert(Insert,After0,After1) :-
    _{'@at' : _{'@height':_H,'@width':_W,'@x':X,'@y':Y},
      '@value': Window} = Insert,
    replace_table_window(X,Y,Window,After0,After1).

table_add_inserts([],After,After).
table_add_inserts([Insert|Inserts],After0,AfterN) :-
    table_add_insert(Insert,After0,After1),
    table_add_inserts(Inserts,After1,AfterN).

diff_op(Diff, Op) :-
    is_dict(Diff),
    get_dict('@op', Diff, Operation_String),
    atom_string(Op, Operation_String).

get_dict_or_null(Key,JSON,V) :-
    (   get_dict(Key,JSON,V)
    ->  true
    ;   V = null).

simple_patch_key_value(Key,JSON,Diff,Result) :-
    % If it's in the diff, we're changing it.
    get_dict(Key,Diff,Key_Diff),
    !,
    (   is_list(Key_Diff)
    ->  get_dict_or_null(Key,JSON,V),
        simple_patch_list(Key_Diff,V,Value),
        Result = Key-Value
    ;   diff_op(Key_Diff,Op)
    ->  get_dict_or_null(Key,JSON,V),
        simple_op_diff_value(Op,Key_Diff,V,Value),
        Result = Key-Value
    ;   get_dict_or_null(Key,JSON,V),
        simple_patch(Key_Diff,V,Value),
        Result = Key-Value
    ).
simple_patch_key_value(Key,JSON,_Diff,Key-Value) :-
    % This is an implicit copy instruction
    get_dict(Key,JSON,Value).

%%% List util
nth_list_prefix_suffix(0,List,[],List) :-
    !.
nth_list_prefix_suffix(N,[H|T],[H|Pr],Rest) :-
    M is N - 1,
    nth_list_prefix_suffix(M,T,Pr,Rest).


:- begin_tests(simple_patch).
:- use_module(core(util/test_utils)).

test(flat_patch, []) :-
    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ name : _{ '@op' : "SwapValue",
                         '@before' : "jim",
                         '@after' : "james" }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "james",
                dob : "2009-08-03"
              },

    simple_patch(Patch,Before,After).

test(bad_read_state, []) :-
    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ name : _{ '@op' : "SwapValue",
                         '@before' : "jake",
                         '@after' : "james" }
             },
    \+ simple_patch(Patch,Before,_After).

test(flat_patch_missing, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim"
              },
    Patch = _{ dob : _{ '@op' : "SwapValue",
                        '@before' : null,
                        '@after' :  "2009-08-03" }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },

    simple_patch(Patch,Before,After).

test(flat_drop_value, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ dob : _{ '@op' : "SwapValue",
                        '@before' : "2009-08-03",
                        '@after' : null }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob: null
              },

    simple_patch(Patch,Before,After).


test(deep_swap_drop_value, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "Beethoven",
                dob : "1770-12-17",
                address :
                _{
                    address1 : "Probusgasse 6",
                    address2 : "Heiligenstadt",
                    city : "Vienna",
                    country : "Austria"
                }
              },
    Patch = _{ address :
               _{ address1 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Probusgasse 6",
                                 '@after' : "Mölker Bastei 8" },
                  address2 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Heiligenstadt",
                                 '@after' : null }
                }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "Beethoven",
                dob : "1770-12-17",
                address :
                _{
                    address1 : "Mölker Bastei 8",
                    address2 : null,
                    city : "Vienna",
                    country : "Austria"
                }
              },

    simple_patch(Patch,Before,After).

test(deep_swap_wrong_value, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "Beethoven",
                dob : "1770-12-17",
                address :
                _{
                    address1 : "Probusgasse 6",
                    address2 : "Heiligenstadt",
                    city : "Vienna",
                    country : "Austria"
                }
              },
    Patch = _{ address :
               _{ address1 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Probusgasse 7",
                                 '@after' : "Mölker Bastei 8" },
                  address2 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Heiligenstadt",
                                 '@after' : null }
                }
             },

    \+ simple_patch(Patch,Before,_After).

test(flat_list_diff, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                friends : ["Person/2","Person/3","Person/4"]
              },
    Patch = _{ friends : _{ '@op' : "CopyList",
                            '@to' : 1,
                            '@rest' :
                            _{'@op' : "SwapList",
                              '@before' : ["Person/3"],
                              '@after' : [],
                              '@rest' : _{'@op':"KeepList"}
                             }
                          }
             },

    simple_patch(Patch,Before,After),
    After = _{ '@id' : "Person/1",
               '@type' : "Person",
               friends : ["Person/2","Person/4"],
               name : "jim"}.


test(flat_table_diff, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                table : [[1,2,3],
                         [4,5,6],
                         [7,8,9]]
              },
    Patch = _{ table : _{ '@op' : "CopyTable",
                          '@to_row' : 2,
                          '@to_column' : 2,
                          '@bottom_left' :
                          _{ '@op' : "KeepTable" },
                          '@top_right' :
                          _{ '@op' : "KeepTable" },
                          '@bottom_right' :
                           _{ '@op' : "SwapTable",
                              '@before' : [[9]],
                              '@after' : [[10]],
                              '@bottom_left' :
                              _{ '@op' : "KeepTable" },
                              '@top_right' :
                              _{ '@op' : "KeepTable" },
                              '@bottom_right' :
                              _{ '@op' : "KeepTable" }
                            }
                        }
             },

    simple_patch(Patch,Before,After),
    After = _{ '@id' : "Person/1",
               '@type' : "Person",
               name : "jim",
               table: [[1,2,3],
                       [4,5,6],
                       [7,8,10]]}.


test(flat_table_diff_conflict, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                table : [[1,2,3],
                         [4,5,6],
                         [7,8,9]]
              },
    Patch = _{ table : _{ '@op' : "CopyTable",
                          '@to_row' : 2,
                          '@to_column' : 2,
                          '@bottom_left' :
                          _{ '@op' : "KeepTable" },
                          '@top_right' :
                          _{ '@op' : "KeepTable" },
                          '@bottom_right' :
                           _{ '@op' : "SwapTable",
                              '@before' : [[8]],
                              '@after' : [[10]],
                              '@bottom_left' :
                              _{ '@op' : "KeepTable" },
                              '@top_right' :
                              _{ '@op' : "KeepTable" },
                              '@bottom_right' :
                              _{ '@op' : "KeepTable" }
                            }
                        }
             },

    \+ simple_patch(Patch,Before,_After).

test(deep_list_patch, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                addresses : [
                    _{ '@id' : "Person/jim/Address/addresses/1",
                       '@type' : "Address",
                       address1 : "Mölker Bastei 8",
                       address2 : null,
                       city : "Vienna",
                       country : "Austria"},
                    _{ '@id' : "Person/jim/Address/addresses/2",
                       '@type' : "Address",
                       address1 : "Probusgasse 7",
                       address2 : null,
                       city : "Vienna",
                                country : "Austria"}
                ]
              },
    Patch = _{ addresses :
               _{ '@op' : "CopyList",
                  '@to' : 1,
                  '@rest' :
                  _{ '@op' : "PatchList",
                     '@patch' : [
                         _{
                             address1
                             : _{ '@op' : "SwapValue",
                                  '@before' : "Probusgasse 6",
                                  '@after' : "Probusgasse 6" }
                         }],
                     '@rest' : _{ '@op' : "KeepList" }
                   }
                }
             },

    \+ simple_patch(Patch,Before,_After).

:- use_module(library(http/json)).

test(read_state, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig"
              },
    Patch = _{ '@id' : "Person/Ludwig",
               name : _{ '@op' : "SwapValue",
                         '@before' : "Ludwig",
                         '@after' : "Ludo" }
             },
    simple_patch(Patch,Before,After),

    After = _{ '@id' : "Person/Ludwig",
               '@type' : "Person",
               name : "Ludo"
             }.

test(deep_read_state, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 8",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
              },
    Patch = _{ name:_{'@after':"Ludo",
                      '@before':"Ludwig",
                      '@op':"SwapValue"},
               address: _{ city : "Vienna" }
             },
    simple_patch(Patch,Before,After),
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              name:"Ludo",
              address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                           '@type' : "Address",
                           address1 : "Mölker Bastei 8",
                           address2 : null,
                           city : "Vienna",
                           country : "Austria"}
             }.

test(deep_read_state_failure, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 8",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
              },
    Patch = _{ name:_{'@after':"Ludo",
                      '@before':"Ludwig",
                      '@op':"SwapValue"},
               address: _{ city : "Timbuktu" }
             },
    \+ simple_patch(Patch,Before,_After).


:- end_tests(simple_patch).

