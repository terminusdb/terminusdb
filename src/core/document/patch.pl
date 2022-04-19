:- module('document/patch',
          [   simple_patch/4,
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

simple_patch(Diff,JSON_In,Maybe_JSON_Out,Options) :-
    is_dict(JSON_In),
    !,
    dict_keys(JSON_In,Doc_Keys),
    dict_keys(Diff,Diff_Keys),
    union(Doc_Keys,Diff_Keys,Keys),
    pairs_and_conflicts_from_keys(Keys,JSON_In,Diff,Pairs,Options),
    promote_pairs_conflict(Pairs,Maybe_JSON_Out).
simple_patch(Diff,Before,After,Options) :-
    is_list(Diff),
    !,
    simple_patch_list(Diff,Before,After,Options).
simple_patch(Diff,Before,After,Options) :-
    diff_op(Diff,Op),
    !,
    simple_op_diff_value(Op, Diff, Before, After, Options).
simple_patch(Before,Original,Final,Options) :-
    check_before_after(Original,Before,Original,Final, Options).

simple_patch_list(Diff,List_In,Result,Options) :-
    is_list(List_In),
    !,
    maplist({Options}/[Patch,Elt,Patched]>>
            simple_patch(Patch,Elt,Patched,Options),
            Diff,List_In,List_Out),
    promote_list_conflict(List_Out,Result).

promote_list_conflict(List_In,Result) :-
    (   maplist([success(Patch),Patch]>>true,List_In,List_Out)
    ->  Result = success(List_Out)
    ;   maplist([Res,Out]>>(   Res = conflict(Con)
                           ->  Out = Con
                           ;   Out = null),List_In,List_Out),
        Result = conflict(List_Out)
    ).

promote_pairs_conflict(Pairs, Result) :-
    (   maplist([Key-success(S),Key-S]>>true, Pairs, Key_Values)
    ->  dict_create(JSON, json, Key_Values),
        Result = success(JSON)
    ;   convlist([Key-conflict(C),Key-C]>>true, Pairs, Key_Values),
        dict_create(JSON, json, Key_Values),
        Result = conflict(JSON)
    ).

simple_patch_table(Diff,Table_In,Table_Out,Options) :-
    is_list(Table_In),
    !,
    rows(Diff,Diff_Row_Length),
    columns(Diff,Diff_Column_Length),
    rows(Table_In,Row_Length),
    columns(Table_In,Column_Length),
    Diff_Row_Length = Row_Length,
    Diff_Column_Length = Column_Length,
    maplist({Options}/[Diff_Row,Row_In,Row_Out]>>
            (   maplist({Options}/[Patch,Elt,Patched]>>
                        simple_patch(Patch,Elt,Patched,Options),
                        Diff_Row,Row_In,Row_Candidates),
                promote_pairs_conflict(Row_Candidates,Row_Out)
            ),
            Diff,Table_In,Candidates_Out),
    promote_pairs_conflict(Candidates_Out,Table_Out).

%%% Conflict utils
pairs_and_conflicts_from_keys([], _, _, [], _Options).
pairs_and_conflicts_from_keys([Key|Keys], JSON, Diff,
                              [Result|Values], Options) :-
    simple_patch_key_value(Key,JSON,Diff,Result,Options),
    !,
    pairs_and_conflicts_from_keys(Keys, JSON, Diff, Values, Options).

check_before_after(Original, Before, After, Final, Options) :-
    option(match_final_state(true), Options),
    !,
    (   Original = After
    ->  Final = success(After)
    ;   Original = Before
    ->  Final = success(After)
    ;   Final = conflict(json{ '@op' : 'Conflict',
                               '@expected' : Before,
                               '@found' : Original })
    ).
check_before_after(Original, Before, After, Final, _Options) :-
    (   Original = Before
    ->  Final = success(After)
    ;   Final = conflict(json{ '@op' : 'Conflict',
                               '@expected' : Before,
                               '@found' : Original })
    ).

non_conflict_list(List, Conflict) :-
    maplist([_,null]>>true, List, Conflict).

non_conflict_table(Table, Conflict) :-
    maplist([Row,Conflict_Row]>>maplist([_,null]>>true, Row, Conflict_Row),
            Table, Conflict).

check_before_after_list(Original,Before,After,Final,Options) :-
    check_before_after_list_(Original,Before,After,Result,Options),
    promote_list_conflict(Result,Final).

check_before_after_list_(Original, Original, [], [], _Options) :-
    !.
check_before_after_list_([], [], Final, Final, _Options).
check_before_after_list_([Original|Original_List],
                        [Before|Before_List],
                        [After|After_List],
                        [Final|Final_List], Options) :-
    check_before_after(Original,Before,After,Final,Options),
    check_before_after_list_(Original_List,Before_List,After_List,Final_List,Options).

check_before_after_table(Original,Before,After,Final,Options) :-
    check_before_after_table_(Original,Before,After,Result,Options),
    promote_list_conflict(Result,Final).

check_before_after_table_(Original, Original, [], [], _Options) :-
    !.
check_before_after_table_([], [], Final, Final, _Options).
check_before_after_table_([Original|Original_List],
                          [Before|Before_List],
                          [After|After_List],
                          [Final|Final_List], Options) :-
    check_before_after_list(Original,Before,After,Final,Options),
    check_before_after_table_(Original_List,Before_List,After_List,Final_List,Options).

append_results(success(Result1),success(Result2),success(Concatenated)) :-
    append(Result1,Result2,Concatenated).
append_results(conflict(Result1),success(Success),conflict(Concatenated)) :-
    non_conflict_list(Success,Result2),
    append(Result1,Result2,Concatenated).
append_results(success(Success),conflict(Result2),conflict(Concatenated)) :-
    non_conflict_list(Success,Result1),
    append(Result1,Result2,Concatenated).
append_results(conflict(Result1),conflict(Result2),conflict(Concatenated)) :-
    append(Result1,Result2,Concatenated).

unsplit_table(success(Total),To_Row,To_Column,success(TL),success(TR),success(BL),success(BR)) :-
    split_table(Total,To_Row,To_Column,TL,TR,BL,BR),
    !.
unsplit_table(conflict(Total),To_Row,To_Column,TL_Res,TR_Res,BL_Res,BR_Res) :-
    maplist([Result,Lifted]>>
            (   Result = conflict(Conflict)
            ->  Lifted = Conflict
            ;   Result = success(Success),
                non_conflict_table(Success,Lifted)
            ),[TL_Res,TR_Res,BL_Res,BR_Res], [TL,TR,BL,BR]),
    split_table(Total,To_Row,To_Column,TL,TR,BL,BR).

%%% Patch operations
simple_op_diff_value('ForceValue', Diff, _, success(After), _Options) :-
    get_dict('@after', Diff, After).
simple_op_diff_value('SwapValue', Diff, Original, Final, Options) :-
    get_dict('@before', Diff, Before),
    get_dict('@after', Diff, After),
    check_before_after(Original, Before, After, Final, Options).
simple_op_diff_value('CopyList', Diff, Original, Final, Options) :-
    get_dict('@to', Diff, To),
    get_dict('@rest', Diff, Next_Diff),
    nth_list_prefix_suffix(To,Original,Original_Prefix,Original_Suffix),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op, Next_Diff, Original_Suffix, Rest, Options),
    append_results(success(Original_Prefix), Rest, Final).
simple_op_diff_value('SwapList', Diff, Original, Final, Options) :-
    get_dict('@before', Diff, Before_Prefix),
    get_dict('@after', Diff, After_Prefix),
    get_dict('@rest', Diff, Next_Diff),
    length(Before_Prefix, Before_Length),
    nth_list_prefix_suffix(Before_Length,Original,Original_Prefix,Original_Suffix),
    check_before_after_list(Original_Prefix,Before_Prefix,After_Prefix,Final_Prefix,
                            Options),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op, Next_Diff, Original_Suffix, Final_Suffix, Options),
    append_results(Final_Prefix,Final_Suffix,Final).
simple_op_diff_value('PatchList', Diff, Original, Final, Options) :-
    %% This should recover when the patch list length is smaller instead of failing.
    get_dict('@patch', Diff, Patch),
    get_dict('@rest', Diff, Rest),
    length(Patch, To),
    nth_list_prefix_suffix(To,Original,Original_Prefix,Original_Suffix),
    simple_patch_list(Patch,Original_Prefix,Final_Prefix,Options),
    diff_op(Rest,Op),
    simple_op_diff_value(Op,Rest,Original_Suffix,Final_Suffix,Options),
    append_results(Final_Prefix,Final_Suffix,Final).
simple_op_diff_value('KeepList', _Diff, Same, success(Same), _Options).
simple_op_diff_value('CopyTable', Diff, Original, Final, Options) :-
    get_dict('@to_row', Diff, To_Row),
    get_dict('@to_column', Diff, To_Column),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    split_table(Original,To_Row,To_Column,Top_Left,Top_Right,Bottom_Left,Bottom_Right),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New,Options),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New,Options),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New,Options),
    unsplit_table(Final, To_Row, To_Column, success(Top_Left), TR_New, BL_New, BR_New).
simple_op_diff_value('SwapTable', Diff, Original, Final, Options) :-
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
    check_before_after_table(TL_Original,TL_Before,
                             TL_After,TL_Final,
                             Options),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,BL_Original,BL_Final,Options),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,TR_Original,TR_Final,Options),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,BR_Original,BR_Final,Options),
    rows(TL_After,To_Row_After),
    columns(TL_After,To_Column_After),
    unsplit_table(Final, To_Row_After, To_Column_After, TL_Final, TR_Final, BL_Final, BR_Final).
simple_op_diff_value('PatchTable', Diff, Original, Final, Options) :-
    get_dict('@top_left', Diff, TL_Diff),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    rows(TL_Diff,To_Row),
    columns(TL_Diff,To_Column),
    split_table(Original,To_Row,To_Column,
                TL_Original,TR_Original,BL_Original,BR_Original),
    simple_patch_table(TL_Diff,TL_Original, TL_Final, Options),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,BL_Original,BL_Final,Options),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,TR_Original,TR_Final,Options),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,BR_Original,BR_Final,Options),
    rows(Top_Left_After,To_Row_After),
    columns(Top_Left_After,To_Column_After),
    unsplit_table(Final, To_Row_After, To_Column_After,
                TL_Final, TR_Final, BL_Final, BR_Final).
simple_op_diff_value('ModifyTable', Diff, Original, Result, Options) :-
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
    ->  Result = success(Final)
    ;   Result = conflict(_{ '@op' : "ModifyTableConflict",
                             dimensions: _{ '@before' : [C1,R1],
                                            '@after' : [C2,R2]
                                          },
                             copies : Copy_Conflict,
                             moves: Move_Conflict,
                             inserts: Insert_Conflict,
                             deletes: Delete_Conflict
                           })
    ).
simple_op_diff_value('KeepTable', _Diff, Same, success(Same), _Options).

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

simple_patch_key_value(Key,JSON,Diff,Result,Options) :-
    % If it's in the diff, we're changing it.
    get_dict(Key,Diff,Key_Diff),
    !,
    (   is_list(Key_Diff)
    ->  get_dict_or_null(Key,JSON,V),
        simple_patch_list(Key_Diff,V,Value,Options),
        Result = Key-Value
    ;   diff_op(Key_Diff,Op)
    ->  get_dict_or_null(Key,JSON,V),
        simple_op_diff_value(Op,Key_Diff,V,Value,Options),
        Result = Key-Value
    ;   get_dict_or_null(Key,JSON,V),
        simple_patch(Key_Diff,V,Value,Options),
        Result = Key-Value
    ).
simple_patch_key_value(Key,JSON,_Diff,Key-success(Value),_Options) :-
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

    simple_patch(Patch,Before,success(After),[]).

test(flat_patch_conflict, []) :-
    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ name : _{ '@op' : "SwapValue",
                         '@before' : "joe",
                         '@after' : "james" }
             },

    simple_patch(Patch,Before,conflict(Conflict),[]),

    Conflict = json{ name:json{ '@expected':"joe",
					            '@found':"jim",
					            '@op':'Conflict'
					          }
			       }.

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
    simple_patch(Patch,Before,conflict(Conflict), []),

    Conflict = json{name:json{'@expected':"jake",
                              '@found':"jim",
                              '@op':'Conflict'}}.

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

    simple_patch(Patch,Before,success(After),[]).

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

    simple_patch(Patch,Before,success(After),[]).


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

    simple_patch(Patch,Before,success(After),[]).

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

    simple_patch(Patch,Before,conflict(Conflict), []),

    Conflict =
    json{
        address:
        json{
            address1:json{ '@expected':"Probusgasse 7",
				           '@found':"Probusgasse 6",
				           '@op':'Conflict'
				         }
		}
    }.

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

    simple_patch(Patch,Before,success(After),[]),
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

    simple_patch(Patch,Before,success(After),[]),
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

    simple_patch(Patch,Before,conflict(Conflict), []),

    Conflict = json{ table:[ [null,null,null],
	                         [null,null,null],
	                         [null,null,json{'@expected':8,
                                             '@found':9,
                                             '@op':'Conflict'}]
	                       ]
                   }.

test(deep_list_patch_conflict, []) :-

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

    simple_patch(Patch,Before,conflict(Conflict), []),

    Conflict = json{ addresses:[ null,
		                         json{ address1:json{ '@expected':"Probusgasse 6",
				                                      '@found':"Probusgasse 7",
				                                      '@op':'Conflict'
				                                    }
		                             }
		                       ]
                   }.

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
    simple_patch(Patch,Before,success(After), []),

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
    simple_patch(Patch,Before,success(After), []),
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

    simple_patch(Patch,Before,conflict(Conflict), []),

    Conflict = json{address:json{city:json{'@found':"Vienna",
                                           '@expected':"Timbuktu",
                                           '@op':'Conflict'}}}.

test(after_matches_patch, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig"
              },
    Patch = _{ '@id' : "Person/Ludwig",
               name : _{ '@op' : "SwapValue",
                         '@before' : "Ludo",
                         '@after' : "Ludwig" }
             },
    simple_patch(Patch,Before,success(After), [match_final_state(true)]),

    After = _{ '@id' : "Person/Ludwig",
               '@type' : "Person",
               name : "Ludwig"
             }.


:- end_tests(simple_patch).

