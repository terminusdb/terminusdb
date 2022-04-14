:- module('document/patch',
          [
              simple_patch/3,
              simple_patch/5,
              diff_op/2,
              get_dict_or_null/3
          ]).

:- use_module(core(util)).
:- use_module(core('util/tables')).

:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(when)).
:- use_module(library(yall)).
:- use_module(library(plunit)).

:- use_module(library(option)).

simple_patch(Diff, JSON_In, JSON_Out) :-
    simple_patch(Diff,JSON_In,JSON_Out,Conflicts,[match_final_state(true)]),
    Conflicts = null.

simple_patch(Diff,JSON_In,JSON_Out,Conflict,Options) :-
    is_dict(JSON_In),
    !,
    dict_keys(JSON_In,Doc_Keys),
    dict_keys(Diff,Diff_Keys),
    union(Doc_Keys,Diff_Keys,Keys),
    pairs_and_conflicts_from_keys(Keys,JSON_In,Diff,Pairs,Conflict_Pairs,Options),
    dict_create(JSON_Out,json,Pairs),
    conflict_pairs_conflict(Conflict_Pairs,Conflict).
simple_patch(Diff,Before,After,Conflicts,Options) :-
    is_list(Diff),
    !,
    maplist({Options}/[D,B,A,C]>>simple_patch(D,B,A,C,Options), Diff, Before, After, Conflicts).
simple_patch(Diff,Before,After,Conflicts,Options) :-
    diff_op(Diff,Op),
    !,
    simple_op_diff_value(Op, Diff, Before, After, Conflicts, Options).
simple_patch(Before,Before,Before,null,_Options).

simple_patch_list(Diff,List_In,List_Out,Conflicts,Options) :-
    is_list(List_In),
    !,
    maplist({Options}/[Patch,Elt,Patched,Conflict]>>
            simple_patch(Patch,Elt,Patched,Conflict,Options),
            Diff,List_In,List_Out,Conflict_List),
    conflict_list_conflict(Conflict_List,Conflicts).

conflict_pairs_conflict(Pairs, C) :-
    (   maplist([_-null]>>true, Pairs)
    ->  C = null
    ;   dict_create(C,json,Pairs)
    ).

conflict_list_conflict(L,C) :-
    (   maplist([null]>>true, L)
    ->  C = null
    ;   L = C
    ).

simple_patch_table(Diff,Table_In,Table_Out,Conflict,Options) :-
    is_list(Table_In),
    !,
    rows(Diff,Diff_Row_Length),
    columns(Diff,Diff_Column_Length),
    rows(Table_In,Row_Length),
    columns(Table_In,Column_Length),
    Diff_Row_Length = Row_Length,
    Diff_Column_Length = Column_Length,
    maplist({Options}/[Diff_Row,Row_In,Row_Out,Row_Conflict]>>
            (   maplist({Options}/[Patch,Elt,Patched,Conflict]>>
                        simple_patch(Patch,Elt,Patched,Conflict,Options),
                        Diff_Row,Row_In,Row_Out,Row_Conflicts),
                conflict_list_conflict(Row_Conflicts, Row_Conflict)
            ),
            Diff,Table_In,Table_Out,Conflicts),
    conflict_list_conflict(Conflicts, Conflict).


%%% Conflict utils
pairs_and_conflicts_from_keys([], _, _, [], [], _Options).
pairs_and_conflicts_from_keys([Key|Keys], JSON, Diff,
                              [Result|Values],
                              [Conflict|Conflicts], Options) :-
    simple_patch_key_value(Key,JSON,Diff,Result,Conflict,Options),
    !,
    pairs_and_conflicts_from_keys(Keys, JSON, Diff, Values, Conflicts,Options).

check_before_after(Original, Before, After, Final, Conflict, Options) :-
    option(match_final_state(true), Options),
    !,
    (   Original = After
    ->  Final = After,
        Conflict = null
    ;   Original = Before
    ->  Final = After,
        Conflict = null
    ;   Final = null,
        Conflict = json{ '@op' : 'Conflict',
                         '@expected' : Before,
                         '@found' : After }
    ).
check_before_after(Original, Before, After, Final, Conflict, _Options) :-
    (   Original = Before
    ->  Final = After,
        Conflict = null
    ;   Final = null,
        Conflict = json{ '@op' : 'Conflict',
                         '@expected' : Before,
                         '@found' : After }
    ).

non_conflict_list(List, Conflict) :-
    maplist([_,null]>>true, List, Conflict).

non_conflict_table(Table, Conflict) :-
    maplist([Row,Conflict_Row]>>maplist([_,null]>>true, Row, Conflict_Row),
            Table, Conflict).

check_before_after_list([], [], [], [], [], _Options).
check_before_after_list([Original|Original_List],
                        [Before|Before_List],
                        [After|After_List],
                        [Final|Final_List],
                        [Conflict|Conflict_List], Options) :-
    check_before_after(Original,Before,After,Final,Conflict,Options),
    check_before_after_list(Original_List,Before_List,After_List,Final_List,Conflict_List,Options).

check_before_after_table([], [], [], [], [], _Options).
check_before_after_table([Original|Original_List],
                        [Before|Before_List],
                        [After|After_List],
                        [Final|Final_List],
                        [Conflict|Conflict_List], Options) :-
    check_before_after_list(Original,Before,After,Final,Conflict,Options),
    check_before_after_table(Original_List,Before_List,After_List,Final_List,Conflict_List,Options).

%%% Patch operations
simple_op_diff_value('ForceValue', Diff, _, After, null, _Options) :-
    get_dict('@after', Diff, After).
simple_op_diff_value('SwapValue', Diff, Original, Final, Conflict, Options) :-
    get_dict('@before', Diff, Before),
    get_dict('@after', Diff, After),
    check_before_after(Original, Before, After, Final, Conflict, Options).
simple_op_diff_value('CopyList', Diff, Original, Final, Conflict, Options) :-
    get_dict('@to', Diff, To),
    get_dict('@rest', Diff, Next_Diff),
    nth_list_prefix_suffix(To,Original,Original_Prefix,Original_Suffix),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op,Next_Diff, Original_Suffix, Rest, Conflict_Rest, Options),
    append(Original_Prefix,Rest,Final),
    non_conflict_list(Original_Prefix,Conflict_Prefix),
    append(Conflict_Prefix,Conflict_Rest, Conflict).
simple_op_diff_value('SwapList', Diff, Original, Final, Conflict, Options) :-
    get_dict('@before', Diff, Before_Prefix),
    get_dict('@after', Diff, After_Prefix),
    get_dict('@rest', Diff, Next_Diff),
    length(Before_Prefix, Before_Length),
    nth_list_prefix_suffix(Before_Length,Original,Original_Prefix,Original_Suffix),
    length(After_Prefix, After_Length),
    nth_list_prefix_suffix(After_Length,Final,Final_Prefix,Final_Suffix),
    check_before_after_list(Original_Prefix,Before_Prefix,After_Prefix,Final_Prefix,
                            Conflict_Prefix, Options),
    % Should this be an error here if we do not find it?
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op, Next_Diff, Original_Suffix, Final_Suffix, Conflict_Suffix, Options),
    append(Final_Prefix, Final_Suffix, Final),
    append(Conflict_Prefix, Conflict_Suffix, Conflict).
simple_op_diff_value('PatchList', Diff, Original, Final, Conflict, Options) :-
    %% This should recover when the patch list length is smaller instead of failing.
    get_dict('@patch', Diff, Patch),
    get_dict('@rest', Diff, Rest),
    length(Patch, To),
    nth_list_prefix_suffix(To,Original,Original_Prefix,Original_Suffix),
    simple_patch_list(Patch,Original_Prefix,Final_Prefix,Conflict_Prefix,Options),
    diff_op(Rest,Op),
    simple_op_diff_value(Op,Rest,Original_Suffix,Final_Suffix,Conflict_Suffix,Options),
    append(Final_Prefix,Final_Suffix,Final),
    append(Conflict_Prefix,Conflict_Suffix,Conflict).
simple_op_diff_value('KeepList', _Diff, Same, Same, Conflict, _Options) :-
    non_conflict_list(Same, Conflict).
simple_op_diff_value('CopyTable', Diff, Original, Final, Conflict, Options) :-
    get_dict('@to_row', Diff, To_Row),
    get_dict('@to_column', Diff, To_Column),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    split_table(Original,To_Row,To_Column,Top_Left,Top_Right,Bottom_Left,Bottom_Right),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New,BL_Conflict,Options),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New,TR_Conflict,Options),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New, BR_Conflict,Options),
    split_table(Final, To_Row, To_Column, Top_Left, TR_New, BL_New, BR_New),
    non_conflict_table(Top_Left, TL_Conflict),
    split_table(Conflict, To_Row, To_Column, TL_Conflict, TR_Conflict, BL_Conflict, BR_Conflict).
simple_op_diff_value('SwapTable', Diff, Original, Final, Conflict, Options) :-
    get_dict('@before', Diff, TL_Before),
    get_dict('@after', Diff, TL_After),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    rows(TL_Before,To_Row_Before),
    columns(TL_Before,To_Column_Before),
    split_table(Original,To_Row_Before,To_Column_Before,
                TL_Original,TR_Original,
                BL_Original,BR_Original),
    check_before_after(TL_Original,TL_Before,
                       TL_After,TL_Final,
                       TL_Conflict, Options),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,BL_Original,BL_Final,BL_Conflict,Options),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,TR_Original,TR_Final,TR_Conflict,Options),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,BR_Original,BR_Final,BR_Conflict,Options),
    rows(TL_After,To_Row_After),
    columns(TL_After,To_Column_After),
    split_table(Final, To_Row_After, To_Column_After, TL_Final, TR_Final, BL_Final, BR_Final),
    split_table(Conflict, To_Row_After, To_Column_After,
                TL_Conflict, TR_Conflict, BL_Conflict, BR_Conflict).
simple_op_diff_value('PatchTable', Diff, Original, Final, Conflict, Options) :-
    get_dict('@top_left', Diff, TL_Diff),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    rows(TL_Diff,To_Row),
    columns(TL_Diff,To_Column),
    split_table(Original,To_Row,To_Column,
                TL_Original,TR_Original,BL_Original,BR_Original),
    simple_patch_table(TL_Diff,TL_Original, TL_Final, TL_Conflict, Options),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,BL_Original,BL_Final,BL_Conflict, Options),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,TR_Original,TR_Final,TR_Conflict, Options),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,BR_Original,BR_Final,BR_Conflict, Options),
    rows(Top_Left_After,To_Row_After),
    columns(Top_Left_After,To_Column_After),
    split_table(Final, To_Row_After, To_Column_After,
                TL_Final, TR_Final, BL_Final, BR_Final),
    split_table(Conflict, To_Row_After, To_Column_After,
                TL_Conflict, TR_Conflict, BL_Conflict, BR_Conflict).
simple_op_diff_value('ModifyTable', Diff, Original, Final, Conflict, Options) :-
    Diff = _{ '@op' : "ModifyTable",
              dimensions: _{ '@before' : [C1,R1],
                             '@after' : [C2,R2]
                           },
              copies : Copies,
              moves: Moves,
              inserts: Inserts,
              deletes: Deletes
            },
    columns(Original,C1),
    rows(Original,R1),
    empty_table(C2,R2,Final0),
    table_check_deletes(Deletes,Original,Delete_Conflicts,Options),
    table_add_copies(Copies,Original,Final0,Final1,Copy_Conflicts,Options),
    table_add_moves(Moves,Original,Final1,Final2,Move_Conflicts,Options),
    table_add_inserts(Inserts,Final2,Final,Insert_Conflicts,Options),
    exclude([null]>>true,Delete_Conflicts,Delete_Conflict),
    exclude([null]>>true,Copy_Conflicts,Copy_Conflict),
    exclude([null]>>true,Move_Conflicts,Move_Conflict),
    exclude([null]>>true,Insert_Conflicts,Insert_Conflict),
    (   Delete_Conflict = [],
        Copy_Conflict = [],
        Move_Conflict = [],
        Insert_Conflict = []
    ->  Conflict = null
    ;   Conflict = _{ '@op' : "ModifyTableConflict",
                      dimensions: _{ '@before' : [C1,R1],
                                     '@after' : [C2,R2]
                                   },
                      copies : Copy_Conflict,
                      moves: Move_Conflict,
                      inserts: Insert_Conflict,
                      deletes: Delete_Conflict
                    }
    ).
simple_op_diff_value('KeepTable', _Diff, Same, Same, Conflict, _Options) :-
    columns(Same,C),
    rows(Same,R),
    empty_table(C,R,Conflict).

empty_row(N, Row) :-
    length(Row, N),
    maplist([null]>>true, Row).

empty_table(_,0, []) :- !.
empty_table(Columns,Rows,[Row|Table]) :-
    New_Rows is Rows - 1,
    empty_row(Columns,Row),
    empty_table(Columns,New_Rows,Table).

table_check_delete(Delete,Original,Conflict,_Options) :-
    _{'@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
      '@value': Before} = Delete,
    table_window(X,W,Y,H,Original,Window),
    (   Before = Window
    ->  Conflict = null
    ;   Conflict = _{ '@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
                      '@expected' : Before,
                      '@found' : Window
                    }
    ).

table_check_deletes([],_Before,[],_Options).
table_check_deletes([Delete|Deletes],Before,[Conflict|Conflicts],Options) :-
    table_check_delete(Delete,Before,Conflict,Options),
    table_check_deletes(Deletes,Before,Conflicts,Options).

table_add_copy(Copy,Original,Final0,Final1,Conflict,_Options) :-
    _{'@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
      '@value': Before } = Copy,
    table_window(X,W,Y,H,Original,Window),
    (   Before = Window
    ->  replace_table_window(X,Y,Window,Final0,Final1),
        Conflict = null
    ;   replace_table_window(X,Y,Window,Final0,Final1),
        Conflict = _{ '@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
                      '@expected': Before,
                      '@found' : Window
                    }
    ).

table_add_copies([],_,After,After,[],_Options).
table_add_copies([Copy|Copies],Before,After0,AfterN,[Conflict|Conflicts],Options) :-
    table_add_copy(Copy,Before,After0,After1,Conflict,Options),
    table_add_copies(Copies,Before,After1,AfterN,Conflicts,Options).

table_add_move(Move,Original,Final0,Final1,Conflict,_Options) :-
    _{'@from' : _{'@height':H1,'@width':W1,'@x':X1,'@y':Y1},
      '@to' : _{'@height':H2,'@width':W2,'@x':X2,'@y':Y2},
      '@value': Before} = Move,
    table_window(X1,W1,Y1,H1,Original,Window),
    (   Before = Window
    ->  replace_table_window(X2,Y2,Window,Final0,Final1),
        Conflict = null
    ;   replace_table_window(X2,Y2,Window,Final0,Final1),
        Conflict = _{'@from' : _{'@height':H1,'@width':W1,'@x':X1,'@y':Y1},
                     '@to' : _{'@height':H2,'@width':W2,'@x':X2,'@y':Y2},
                     '@expected': Before,
                     '@found' : Window
                    }
    ).

table_add_moves([],_,After,After,[],_Options).
table_add_moves([Move|Moves],Before,After0,AfterN,[Conflict|Conflicts],Options) :-
    table_add_move(Move,Before,After0,After1,Conflict,Options),
    table_add_moves(Moves,Before,After1,AfterN,Conflicts,Options).

table_add_insert(Insert,Final0,Final1,Conflict,_Options) :-
    _{'@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
      '@value': After} = Insert,
    empty_table(H,W,Empty),
    table_window(X,W,Y,H,Final0,Window),
    (   Empty = Window
    ->  replace_table_window(X,Y,After,Final0,Final1),
        Conflict = null
    ;   replace_table_window(X,Y,After,Final0,Final1),
        Conflict = _{'@at' : _{'@height':H,'@width':W,'@x':X,'@y':Y},
                     '@found' : Window
                    }
    ).

table_add_inserts([],After,After,[],_Options).
table_add_inserts([Insert|Inserts],After0,AfterN,[Conflict|Conflicts]) :-
    table_add_insert(Insert,After0,After1,Conflict,Options),
    table_add_inserts(Inserts,After1,AfterN,Conflicts,Options).

diff_op(Diff, Op) :-
    is_dict(Diff),
    get_dict('@op', Diff, Operation_String),
    atom_string(Op, Operation_String).

get_dict_or_null(Key,JSON,V) :-
    (   get_dict(Key,JSON,V)
    ->  true
    ;   V = null).

simple_patch_key_value(Key,JSON,Diff,Result,Conflict,Options) :-
    % If it's in the diff, we're changing it.
    get_dict(Key,Diff,Key_Diff),
    !,
    (   is_list(Key_Diff)
    ->  get_dict_or_null(Key,JSON,V),
        simple_patch_list(Key_Diff,V,Value,Conflict_Value,Options),
        Result = Key-Value,
        Conflict = Key-Conflict_Value
    ;   diff_op(Key_Diff,Op)
    ->  get_dict_or_null(Key,JSON,V),
        simple_op_diff_value(Op,Key_Diff,V,Value,Conflict_Value,Options),
        Result = Key-Value,
        Conflict = Key-Conflict_Value
    ;   get_dict_or_null(Key,JSON,V),
        simple_patch(Key_Diff,V,Value,Conflict_Value,Options),
        Result = Key-Value,
        Conflict = Key-Conflict_Value
    ).
simple_patch_key_value(Key,JSON,_Diff,Key-Value,null,_Options) :-
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

