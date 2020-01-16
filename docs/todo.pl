:- module(todo, [
              epic/1,
              story/1,
              epic_story/2,
              story_task/2,
              story_props/2
          ]).


:- dynamic
    epic/1,
    story/1,
    epic_story/2,
    story_task/2.


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

