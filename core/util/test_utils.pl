:- module(test_utils,[
              try/1,
              status_200/1,
              curl_json/2,
              report_curl_command/1,
              admin_pass/1,
              setup_temp_store/1,
              teardown_temp_store/1,
              with_temp_store/1,
              ensure_label/1,
              layer_schema_context_from_label_descriptor/2,
              layer_schema_context_from_label_descriptor/3,
              ref_schema_context_from_label_descriptor/2,
              ref_schema_context_from_label_descriptor/3,
              repo_schema_context_from_label_descriptor/2,
              repo_schema_context_from_label_descriptor/3,
              create_db_with_test_schema/2,
              create_db_without_schema/3,
              print_all_triples/1,
              print_all_triples/2
          ]).

/** <module> Test Utilities
 *
 * Utils to assist in testing.
 *
 * Printing during tests goes through two pipelines.
 *
 * Actual test output is sent to print_message with a **Kind** of
 * `testing`. Use test_format/3 for most things.
 *
 * progress of testing is reported to `debug/3` with a topic of
 * `terminus(testing_progress(Msg))`, where `Msg` in
 * `[run, error, fail]`
 *
 * Debug output should go through `debug/3`
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */


:- use_module(utils).
:- use_module(file_utils).

:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core(api)).

:- use_module(library(terminus_store)).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- meta_predicate test_format(:, +, +).

%!  test_format(+Goal:callable, +Format:text, +Args:list) is det
%
%   print the message formed as in [[format/2]] for message from
%   test Goal.
%
%   @arg Goal a callable term, the argument to [[try/1]], name of our
%   test
%   @arg Format string (an atom) as in [[format/2]]
%   @arg Args list of arguments for the format string
%
test_format(Goal, Format, Args) :-
    print_message(testing, test_format(Goal, Format, Args)).

:- multifile prolog:message//1.

prolog:message(test_format(Goal, Format, Args)) -->
    [
           '~Ntest ~q:'-[Goal],
           Format-Args
       ].

:- meta_predicate try(0).

%!  try(+Goal:callable) is semidet
%
%   calls `Goal` as once, writing debug information,
%
try(Goal) :-
    test_format(Goal, '~N* Running test ~q', [Goal]),
    debug(terminus(testing_progress(run)), 'running ~q', [Goal]),
    (   catch(Goal, Error, true)
    ->  (   var(Error)
        ->  true
        ;   test_format(Goal, '~N+ ERROR! Could not successfully run ~q: ~q',[Goal,Error]),
            debug(terminus(testing_progress(error)), 'ERROR! Could not successfully run ~q: ~q',[Goal,Error]),
            fail
        )
    ;
        test_format(Goal, '~N+ FAIL! Could not successfully run ~q',[Goal]),
        debug(terminus(testing_progress(fail)), 'FAIL! Could not successfully run ~q',[Goal]),
        fail
    ).

write_arg(Arg) :-
    string(Arg),
    !,
    writeq(Arg).
write_arg(Arg) :-
    re_match('.*:.*', Arg),
    !,
    writeq(Arg).
write_arg(Arg) :-
    re_match('[^ ]+ ', Arg),
    !,
    format('"~s"',Arg).
write_arg(Arg) :-
    atom(Arg),
    !,
    write(Arg).

write_args(Args) :-
    intersperse(' ', Args,Spaced),
    !,
    maplist(write_arg,Spaced),
    format('~n',[]).

report_curl_command(Args) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, predicate_indicator, PredIndicator),
    with_output_to(string(ArgStr), write_args(Args)),
    test_format(PredIndicator, '~NRunning command: curl ~w',[ArgStr]).

status_200(URL) :-
    http_open(URL, _, [status_code(200)]).


/*
 * curl_json(+Args,-JSON) is semidet.
 */
curl_json(Args,JSON) :-
    terminus_path(Path),
    process_create(path(curl), Args,
                   [ stdout(pipe(Out)),
                     stderr(std),
                     process(PID),
                     cwd(Path)
                   ]),

    process_wait(PID,Status),

    (   Status=killed(Signal)
    ->  interpolate(["curl killed with signal ",Signal], M),
        format('~n~s~n', M),
        fail
    ;   true),

    catch(json_read_dict(Out, JSON),
          _,
          JSON = _{'terminus:status' : 'terminus:failure'}),

    close(Out).

/*
 * admin_pass(+Pass) is det.
 *
 * Get the administrator password for testing from the environment,
 * or try the default ('root')
 */
admin_pass(Pass) :-
    (   getenv('TERMINUSDB_ADMIN_PASSWD', Pass)
    ->  true
    ;   Pass='root').

setup_temp_store(State) :-
    State=Store-Dir,
    tmp_file(temporary_terminus_store, Dir),
    make_directory(Dir),
    open_directory_store(Dir, Store),
    initialize_database_with_store('http://localhost:1234', 'root', Store),
    local_triple_store(Store).

teardown_temp_store(State) :-
    State=Store-Dir,
    retract_local_triple_store(Store),
    delete_directory_and_contents(Dir).

:- meta_predicate with_temp_store(:).
with_temp_store(Goal) :-
    setup_call_cleanup(setup_temp_store(State),
                       Goal,
                       teardown_temp_store(State)).

ensure_label(Label) :-
    triple_store(Store),
    ignore(create_named_graph(Store, Label, _Graph)).

layer_schema_context_from_label_descriptor(Label_Descriptor, Context) :-
    Commit_Info = commit_info{author:"test",message:"test"},
    ref_schema_context_from_label_descriptor(Label_Descriptor, Commit_Info, Context).
layer_schema_context_from_label_descriptor(Label_Descriptor, Commit_Info, Context) :-
    layer_ontology(Layer_Label),
    Layer_Descriptor = labelled_graph{label:Layer_Label,type:schema,name:"layer"},
    open_read_write_obj(Layer_Descriptor, Layer_Read_Write_Object),

    open_descriptor(Label_Descriptor, Commit_Info, Incomplete_Transaction_Object),
    Transaction_Object = Incomplete_Transaction_Object.put(schema_objects,
                                                           [Layer_Read_Write_Object]),

    create_context(Transaction_Object, Commit_Info, Context).

ref_schema_context_from_label_descriptor(Label_Descriptor, Context) :-
    Commit_Info = commit_info{author:"test",message:"test"},
    ref_schema_context_from_label_descriptor(Label_Descriptor, Commit_Info, Context).
ref_schema_context_from_label_descriptor(Label_Descriptor, Commit_Info, Context) :-
    layer_ontology(Layer_Label),
    ref_ontology(Ref_Label),
    Layer_Descriptor = labelled_graph{label:Layer_Label,type:schema,name:"layer"},
    Ref_Descriptor = labelled_graph{label:Ref_Label,type:schema,name:"ref"},
    open_read_write_obj(Layer_Descriptor, Layer_Read_Write_Object),
    open_read_write_obj(Ref_Descriptor, Ref_Read_Write_Object),

    open_descriptor(Label_Descriptor, Commit_Info, Incomplete_Transaction_Object),
    Transaction_Object = Incomplete_Transaction_Object.put(schema_objects,
                                                           [Layer_Read_Write_Object,
                                                            Ref_Read_Write_Object]),

    create_context(Transaction_Object, Commit_Info, Context).

repo_schema_context_from_label_descriptor(Label_Descriptor, Context) :-
    Commit_Info = commit_info{author:"test",message:"test"},
    repo_schema_context_from_label_descriptor(Label_Descriptor, Commit_Info, Context).
repo_schema_context_from_label_descriptor(Label_Descriptor, Commit_Info, Context) :-
    layer_ontology(Layer_Label),
    repository_ontology(Repo_Label),
    Layer_Descriptor = labelled_graph{label:Layer_Label,type:schema,name:"layer"},
    Repo_Descriptor = labelled_graph{label:Repo_Label,type:schema,name:"ref"},
    open_read_write_obj(Layer_Descriptor, Layer_Read_Write_Object),
    open_read_write_obj(Repo_Descriptor, Repo_Read_Write_Object),

    open_descriptor(Label_Descriptor, Commit_Info, Incomplete_Transaction_Object),
    Transaction_Object = Incomplete_Transaction_Object.put(schema_objects,
                                                           [Layer_Read_Write_Object,
                                                            Repo_Read_Write_Object]),

    create_context(Transaction_Object, Commit_Info, Context).


create_db_with_test_schema(User, Db_Name) :-
    user_database_name(User, Db_Name, Full_Name),
    Prefixes = _{ doc  : 'terminus://worldOnt/document/',
                  scm : 'http://example.com/data/worldOntology#'},

    create_db(Full_Name, "test", "a test db", Prefixes),
    resolve_absolute_descriptor([User, Db_Name], Branch_Descriptor),

    create_graph(Branch_Descriptor,
                 commit_info{ author : "test",
                              message: "Create an empty schema graph"},
                 schema,
                 "main",
                 _),

    create_context(Branch_Descriptor, commit_info{author: "test", message: "add test schema"}, Context),
    terminus_path(Path),
    interpolate([Path, '/test/worldOnt.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),
    update_turtle_graph(Context, schema, "main", TTL).

create_db_without_schema(User, Db_Name) :-
    user_database_name(User, Db_Name, Full_Name),
    Prefixes = _{ doc : 'http://somewhere.for.now/document',
                  scm : 'http://somewhere.for.now/schema' },
    create_db(Full_Name, "test", "a test db", Prefixes).

create_db_without_schema(DB_ID, DB_Name, Comment) :-
    Prefixes = _{ doc : 'http://somewhere.for.now/document',
                  scm : 'http://somewhere.for.now/schema' },
    create_db(DB_ID, DB_Name, Comment, Prefixes).

:- begin_tests(db_test_schema_util).
test(create_db_and_insert_invalid_data,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("user", "test"))),
      cleanup(teardown_temp_store(State)),
      throws(error(schema_check_failure(_)))])
:-
    resolve_absolute_string_descriptor("user/test", Descriptor),
    create_context(Descriptor, commit_info{author:"test",message:"this should never commit"}, Context),

    with_transaction(Context,
                     ask(Context,
                         insert(a,b,c)),
                     _).

:- end_tests(db_test_schema_util).

print_all_triples(Askable) :-
    findall(t(S,P,O),
            ask(Askable, t(S,P,O)),
            Triples),
    forall(member(Triple,Triples),
           (   writeq(Triple), nl)).

print_all_triples(Askable, Selector) :-
    findall(t(S,P,O),
            ask(Askable, t(S,P,O, Selector)),
            Triples),
    forall(member(Triple,Triples),
           (   writeq(Triple), nl)).
