:- module(todo, [
              epic/1,
              story/1,
              epic_story/2,
              story_task/2,
              story_props/2
          ]).
/** <module> Todo - our kanban board
 *
 */

:- dynamic
    epic/1,
    story/1,
    epic_story/2,
    story_task/2,
    props/2.

:-discontiguous
    epic/1,
    story/1,
    epic_story/2,
    props/2.

%!  epic(-EpicName:atom) is nondet
%
%   @arg EpicName the name of an epic
%
%   iterates over the epics on backtracking

%!  story(-StoryName:atom) is nondet
%
%   @arg StoryName the name of a story
%
%   iterates over the stories on backtracking

%!  epic_story(?Epic:atom, ?Story:atom) is nondet
%
%   @arg Epic the name of an epic
%   @arg Story the name of a story in that epic
%
%   succeeds when Story is a story within Epic.

%!  story_task(?Story:atom, ?Task:atom) is nondet
%
%   @arg Story a user story
%   @arg Task  a task needed for that story to work
%
%   succeeds when Task is a task within Story


%!  props(?Entity:atom, ?Props:list) is nondet
%
%   @arg Entity  an epic, story, or task
%   @arg Props   a list of terms that are props of this entity
%
%   This is for convenience typing in. When thihngs calm down I'll
%   make some arity 2 preds to add properties on their own.
%

		 /*******************************
		 *   A-box  Actual content      *
		 *******************************/

story(logging).
props(logging,
            [ desc('developers are able to look at logs from running server'),
              long_desc(
'Logging from server and from store, with all logging redirected through print_message/3 and the print_message pipeline.\n\
if log_listener_install not called, rust falls back to writing to stdout.\n'),
              tasks([
                  'provide convenience method to set message_hook to redirect to arbitrary location',
                  'add logging to lots of locations in terminus_store',
                  'move Gavin\'s format calls to debug/3',
                  'add debug/3 to lots of locations in terminus_server',
                  'add a log_listener_install api'
              ])
            ]).


		 /*******************************
		 *  Convenience Utils           *
		 *******************************/

story_props(Name, Props) :-
    props(Name, Props).   % for now

