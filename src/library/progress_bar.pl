:- use_module(progress_bar,
              [
                  cli_progress_bar/5
              ]).

:- use_module(library(progress_bar/progress_bar)).

running_time(TS0) --> {get_time(TS),RunningTime is TS - TS0},[" ~2f seconds" - [RunningTime]].

cli_progress_bar(IntroText,Progress,TS0) -->
	{
		Progress = Index/Total,
		OutroText = pb(running_time(TS0))
	},
	default_progress_bar(Index,Total,IntroText,OutroText).
