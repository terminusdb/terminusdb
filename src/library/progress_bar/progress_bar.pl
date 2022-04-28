:- module('progress_bar/progress_bar', [
	simple_spinner//1,			% simple_spinner(+Progress:int) is det.
	default_spinner//2,			% default_spinner(+Progress:int,TextLeft:text) is det.
	fancy_spinner//3,			% fancy_spinner(+Progress:int, +TextLeft:text, +TextRight:text) is det.
	spinner//10,				% spinner(+Progress:int,
								%	+SpinLeftLeft:atom,+TextLeft:text,SpinLeftRight:atom,
								%	+SpinCenterLeft:atom,+TextCenter:text,+SpinCenterRight:atom,
								%	+SpinRightLeft:atom,+TextRight:text,+SpinRightRight:atom)
	spinner_end//0,
	simple_progress_bar//2,		% simple_progress_bar(+Index:int, +Total:int) is det.
	default_progress_bar//4,	% default_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text)
	fancy_progress_bar//7,		% fancy_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text,StartText,TodoText:text) is det.
	progress_bar//12			% progress_bar(+Index:int,+Total:int,+IntroText:text,+OutroText:text,
								%	+StartMarker:char,+StartText:text,+DoneChar:char,+DoneText:text,
								%	+TodoText:text,+TodoChar:char,+EndText:text,+EndMarker:char) is det.
	]).

/** <module> DCG rules that renders a text-based progress bar, or spinner

 	progress_bar makes available a DCG rule (e.g. simple_progress_bar, simple_spinner) that is to be used as part of the
 	prolog  messaging system to print the progress of a used defined process (see
 	https://www.swi-prolog.org/pldoc/man?section=printmsg). Note that the term 'process' denotes an abstract task a user
 	wishes to complete (i.e. it doens't refer to a separate thread)

 	The message is meant to be called repeatedly while the user- process progresses (i.e. it is part of the user
 	processing loop). To this end the DCG rule (e.g. simple_progress_bar) should be provided: the current state (=Index),
 	with respect to the total denoting completion (=Total). Additional information may be provided to allow informing the
 	user about the processing (see example below). Note the spinner//10 doesn't define a Total (it does define Index) and
 	therefore it's progress with respect to completion is unknown. When the user-process finishes, the user should call
 	spinner_end//0 to end the spinner.

 	Under the hood, the DCG rules formats the message according to a specific layout (see progress_bar//12, and
 	spinner//10 for details). simple_progress_bar//2, default_progress_bar//4, fancy_progress_bar//7 are specialisations
 	of progress_bar//12 defaulting some of its values (similarily for spinner). While the user-process progresses, the DCG
 	rule is called repeatedly (i.e. as part of the user defined loop). Every message  is prefixed with a 'carriage-return
 	only' character ('\r'), which effectively resets the last printed message, and creates the animation effect.

 	## Example

 	The example below illustrates the use of simple_progress_bar//2 within =user_defined_predicate= to visually portray
 	advancement whilte iterating elements within the forall/2 loop. Within the forall-loop print_message/2 calls the DGC
 	rule my_application_does_stuff//2 to format to pre-process the message passed to default_progress_bar//4.

	==
	user_defined_predicate(...) :-
 		...
 		length(Items,Total),
 		forall(nth1(Index,Items,Item), (
 			print_message(informational,my_application_does_stuff(Index,Total)
 			%  do something with Item
			),
		...

 	my_application_does_stuff(Index,Total) -->
 		Percentage is (Index/Total) * 100,
 		format(string(OutroText),"[~0f%]",[Percentage]),
 		default_progress_bar(Index,Total,"Processing",OutroText).
	==

	@author Joost Geurts
	@license MIT License

*/

:- use_module(library(dcg/basics)).
:- use_module(library(error)).

:- use_module(library(debug)).
:- debug(progress_bar).
:- debug(progress_bar,"progress_bar pack loaded",[]).

:- multifile prolog:message//1.
prolog:message(pb(Msg)) --> Msg.


%!	spinner(?Id:atom, ?Frames:list(atom)) is nondet.
%	defines the spinners
spinner(none,['']).
spinner(classic,['|','/','—','\\']).
spinner(mini,['x','+']).
spinner(dots1,['.  ','.. ','...',' ..','  .']).
spinner(dots2,['.  ','.. ','...']).
spinner(bar,['▁','▃','▄','▅','▆','▇','█','▇','▆','▅','▄','▃']).
spinner(dqpb,['d', 'q', 'p', 'b']).


%!	simple_spinner(+Progress:int)// is det.
%	renders a spinner message on the center of the screen-line
simple_spinner(Progress) -->
	{
		SpinLeftLeft = none,
		TextLeft = "",
		SpinLeftRight = none,
		SpinCenterLeft = dots1,
		TextCenter = " Processing ",
		SpinCenterRight = dots1,
		SpinRightLeft = none,
		TextRight = "",
		SpinRightRight = none
	},
	spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight).

%!	default_spinner(+Progress:int,TextLeft:text)// is det.
%	renders a text message followed by a spinner
default_spinner(Progress,TextLeft) -->
	{
		SpinLeftLeft = none,
		SpinLeftRight = classic,
		SpinCenterLeft = none,
		TextCenter = "",
		SpinCenterRight = none,
		SpinRightLeft = none,
		TextRight = "",
		SpinRightRight = none
	},
	spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight).

%!	fancy_spinner(+Progress:int, +TextLeft:text, +TextRight:text)// is det.
%	fancy_spinner renders a (dynamic) message on the left of the screen (including a spinner) and rights-aligned message
fancy_spinner(Progress,TextLeft,TextRight) -->
	{
		SpinLeftLeft = bar,
		SpinLeftRight = none,
		SpinCenterLeft = none,
		TextCenter = "",
		SpinCenterRight = none,
		SpinRightLeft = none,
		SpinRightRight = none
	},
	spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight).

%!	spinner(+Progress:int,
%!		+SpinLeftLeft:atom,+TextLeft:text,SpinLeftRight:atom,
%!		+SpinCenterLeft:atom,+TextCenter:text,+SpinCenterRight:atom,
%!		+SpinRightLeft:atom,+TextRight:text,+SpinRightRight:atom)// is det.
%
%	Generates a spinner message that is meant to be called repeatedly by a task to indicate its progress
%	(while its completion cannot be determined ahead of timer, otherwise a progess bar would be more appropriate)
%
%	The layout of the spinner message is setup according to the following schema:
%	==
%	SLL TL SLR 			SCL TC SCR 			SRL TR SRR
%	==
%
%	=SLL= stands for Spinner-Left-Left, =TL= stands for Text-Left, =SLR= stands for Spinner-Left-Right
%	The other abreviation follow the same schema
%
%	* 	=Progress=
%		is an possitive integer that represents advancement of a task (it is assumed to grow 1 with every call)
%	*	=TL=
%		is a left-aligned text or message,
%	*	=TC=
%		is a center-aligned text or message,
% 	*	=TR=
%		is a right-aligned text or message
%	* 	=SLL=, =SLR=, =SCL=,=SCR=,=SRL= and =SRR=
%		are atoms represent a spinner Id (e.g. 'classic', 'dots', .. ). Noting 'none' denotes absence of a spinner
%
%	As the termination of a spinner us unnknown a newline should be emmited when done, this can be done using end_spinner.
spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight) -->
	{
		findall(Id,spinner(Id,_),Ids),
		must_be(nonneg, Progress),
		must_be(oneof(Ids),SpinLeftLeft),
		must_be(any,TextLeft), (atomic(TextLeft) -> TextLeftStr = TextLeft ; message_to_string(TextLeft,TextLeftStr)),
		must_be(oneof(Ids),SpinLeftRight),
		must_be(oneof(Ids),SpinCenterLeft),
		must_be(any,TextCenter), (atomic(TextCenter) -> TextCenterStr = TextCenter ; message_to_string(TextCenter,TextCenterStr)),
		must_be(oneof(Ids),SpinCenterRight),
		must_be(oneof(Ids),SpinRightLeft),
		must_be(any,TextRight), (atomic(TextRight) -> TextRightStr = TextRight ; message_to_string(TextRight,TextRightStr)),
		must_be(oneof(Ids),SpinRightRight)
	},
	remove_line_content,
	prefix_line,
	full_width_spinner(Progress,SpinLeftLeft,TextLeftStr,SpinLeftRight,SpinCenterLeft,TextCenterStr,SpinCenterRight,SpinRightLeft,TextRightStr,SpinRightRight),
	[flush].

%!	spinner_end// is det.
%	spinner_end ends the the spinner
spinner_end --> [nl].

full_width_spinner(Progress,SLL,TL,SLR,SCL,TC,SCR,SRL,TR,SRR) -->
   {
    	tty_size(_,W0),
    	Position = 0,
        Width is W0 - 3 % '% ' prefix + cursor
   },
   render_spinner(Progress,Position,Width,SLL,TL,SLR,SCL,TC,SCR,SRL,TR,SRR).

% PosL				   PosC 			PosR
% SLL TL SLR 		SCL TC SCR 	  SRL TR SRR
render_spinner(Progress,Position,Width,SLL,TL,SLR,SCL,TC,SCR,SRL,TR,SRR) -->
	{
		must_be(nonneg,Position),
		must_be(nonneg,Width),
		PosL = Position,
		spinner(SCL,[Frame_SCL|_]),atom_length(Frame_SCL,Len_SCL),
		string_length(TC,Len_TC),
		spinner(SCR,[Frame_SCR|_]),
		spinner(SCR,[Frame_SCR|_]),atom_length(Frame_SCR,Len_SCR),
		PosC is PosL + round(Width/2) - round((Len_SCL + Len_TC + Len_SCR) / 2) - 1,
		spinner(SRL,[Frame_SRL|_]),atom_length(Frame_SRL,Len_SRL),
		string_length(TR,Len_TR),
		spinner(SRR,[Frame_SRR|_]),atom_length(Frame_SRR,Len_SRR),
		PosR is Position + Width - Len_SRL - Len_TR - Len_SRR
	},
	do_render_spinner(Progress,PosL,SLL,TL,SLR,PosC,SCL,TC,SCR,PosR,SRL,TR,SRR).

do_render_spinner(Progress,PosL,SLL,TL,SLR,PosC,SCL,TC,SCR,PosR,SRL,TR,SRR) -->
	{
		spinner(SLL,Frames_SLL),length(Frames_SLL,N_SLL), I_SLL is Progress mod N_SLL, nth0(I_SLL,Frames_SLL,Frame_SLL),
		spinner(SLR,Frames_SLR),length(Frames_SLR,N_SLR), I_SLR is Progress mod N_SLR, nth0(I_SLR,Frames_SLR,Frame_SLR),
		spinner(SCL,Frames_SCL),length(Frames_SCL,N_SCL), I_SCL is Progress mod N_SCL, nth0(I_SCL,Frames_SCL,Frame_SCL),
		spinner(SCR,Frames_SCR),length(Frames_SCR,N_SCR), I_SCR is Progress mod N_SCR, nth0(I_SCR,Frames_SCR,Frame_SCR),
		spinner(SRL,Frames_SRL),length(Frames_SRL,N_SRL), I_SRL is Progress mod N_SRL, nth0(I_SRL,Frames_SRL,Frame_SRL),
		spinner(SRR,Frames_SRR),length(Frames_SRR,N_SRR), I_SRR is Progress mod N_SRR, nth0(I_SRR,Frames_SRR,Frame_SRR),

		MetaTabSpec = "~~~w|~~w~~w~~w~~~w|~~w~~w~~w~~~w|~~w~~w~~w",
		format(atom(TabSpec),MetaTabSpec,[PosL,PosC,PosR])
	},
	[TabSpec-[Frame_SLL,TL,Frame_SLR,Frame_SCL,TC,Frame_SCR,Frame_SRL,TR,Frame_SRR]].


%! simple_progress_bar(+Index:int, +Total:int)// is det.
%	simple_progress_bar renders a progress bar, and the percentage completed
simple_progress_bar(Index,Total) -->
	{
		IntroText = '',
		Percentage is (Index/Total) * 100, format(string(OutroText),"[~0f%]",[Percentage]),
		StartMarker = '[',
		StartText = '',
		DoneChar = '*',
		DoneText = '',
		TodoText = '',
		TodoChar = ' ',
		EndText = '',
		EndMarker = ']'
	},
	progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).

%! default_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text)// is det.
%	default_progress_bar renders a progress bar, including an IntroText, OutroText (that may be dynamically updated)
default_progress_bar(Index,Total,IntroText,OutroText) -->
	{
		StartMarker = '[',
		StartText = '',
		DoneChar = '=',
		DoneText = '>',
		TodoText = '',
		TodoChar = ' ',
		EndText = '',
		EndMarker = ']'
	},
	progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).

%! fancy_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text,StartText,TodoText:text, EndText:text)// is det.
%	fancy_progress_bar renders a progress bar, including an IntroText, OutroText, StartText, TodoText and EndText (that may be dynamically updated)
fancy_progress_bar(Index,Total,IntroText,OutroText,StartText,TodoText,EndText) -->
	{
		StartMarker = '\u2503',		% ┃
		DoneChar = '\u25A0',		% ■
		DoneText = '\u25BA',		% ►
		TodoChar = '\u25A1',		% □
		EndMarker = '\u2503'		% ┃
	},
	progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).

%! progress_bar(+Index:int,+Total:int,+IntroText:text,+OutroText:text,
%!		+StartMarker:char,+StartText:text,+DoneChar:char,+DoneText:text,
%!		+TodoText:text,+TodoChar:char,+EndText:text,+EndMarker:char)// is det.
%	progress_bar renders the progress of a process using the full width of the terminal
%	where Index represent the current advancement, and Total represent completion. (i.e. Index =< Total)
%
%	The layout of the bar is setup according to the following schema:
%	==
%	Intro [Start+++++++++++++++><---------End] Outro
%	==
%
%	* 	=IntroText= (=|'Intro'|= in the schema)
%		represents the text or message that is printed before the bar
%	*	=StartMarker= (=|'['|=)
%		defined the character used to render the left boundary of the bar
%	*	=StartText= (=|'Start'|=)
%		is a text or message printed at the right side of the StartMarker, provided there is sufficient space
%	*	=DoneChar= (=|'+'|=)
%		is the charcater used to render the advancement completed
%	*	=DoneText= (=|'>'|=)
%		is a text or message that is printed (provided there is space) at the left side ('i.e done') of the current advancement
%	*	=TodoText= (=|'<'|=)
%		is a text or message that is printed (provided there is space) at the right side (i.e. todo') of the current advancement
%	*	=TodoChar= (=|'-'|=)
%		is the character used to render the advacement that remains to be made
%	*	=EndText= (=|'End'|=)
%		is a text or message printed at the left side of the EndMarker, provided there is sufficient space
%	*	=EndMarker= (=|']'|=)
%		defined the character used to render the right boundary of the bar
%	*	=OutroText= (=|'Outro'|=)
%		represents the text or message that is printed after the bar

% @fixme: IntroText, OutroText,StartText etc should be messages (or strings)
progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker) -->
	{
		must_be(nonneg,Index),
		must_be(nonneg,Total),
        must_be(any,IntroText), (atomic(IntroText) -> IntroTextStr = IntroText ; message_to_string(IntroText,IntroTextStr)),
        must_be(any,OutroText), (atomic(OutroText) -> OutroTextStr = OutroText ; message_to_string(OutroText,OutroTextStr)),
		must_be(char,StartMarker),
        must_be(any,StartText), (atomic(StartText) -> StartTextStr = StartText ; message_to_string(StartText,StartTextStr)),
        must_be(char,DoneChar),
        must_be(any,DoneText), (atomic(DoneText) -> DoneTextStr = DoneText ; message_to_string(DoneText,DoneTextStr)),
        must_be(any,TodoText), (atomic(TodoText) -> TodoTextStr = TodoText ; message_to_string(TodoText,TodoTextStr)),
        must_be(char,TodoChar),
        must_be(any,EndText), (atomic(EndText) -> EndTextStr = EndText ; message_to_string(EndText,EndTextStr)),
        must_be(char,EndMarker)
	},
	remove_line_content,
	prefix_line,
	full_width_bar(Index,Total,IntroTextStr,OutroTextStr,StartMarker,StartTextStr,DoneChar,DoneTextStr,TodoTextStr,TodoChar,EndTextStr,EndMarker),
	finished(Index,Total).

remove_line_content --> [at_same_line,'\r'].
prefix_line --> ['% '].
finished(Total,Total) --> !,[nl].
finished(_,_) --> [flush].


% Renders an instance of the progress_bar using the full width of the terminal
full_width_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker) -->
    {
    	tty_size(_,W0),
        Width is W0 - 2, % '% ' prefix
        StartPosition = 0
    },
    render_bar(StartPosition,Width,Index,Total,IntroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker,OutroText).

% Renders a profress_bar of Width starting at StartPosition
render_bar(StartPosition,Width,Index,Total,IntroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker,OutroText) -->
    {
    	must_be(nonneg,StartPosition),
        must_be(nonneg,Width),
        string_length(IntroText,LIntroText),
        string_length(StartMarker,LStartMarker),
        string_length(EndMarker,LEndMarker),
        string_length(OutroText,LOutroText),

        BarWidth is Width - LIntroText - LStartMarker - LEndMarker - LOutroText,
        (Index == Total -> % for rounding errors
        	(DoneWidth = BarWidth, TodoWidth = 0)
        	;
        	(
        		DoneWidth is floor(Index * (BarWidth / Total)),
        		TodoWidth is BarWidth - DoneWidth
        	)
        )
        %debug(progress_bar,"progress:~w, Width:~w, BarWidth:~w, DoneWidth:~w, TodoWidth:~w",[Index/Total,Width,BarWidth,DoneWidth,TodoWidth])
    },
    do_render_bar(StartPosition,DoneWidth,TodoWidth,IntroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker,OutroText).


%! do_render_bar(
%!		+StartPosition:int,+DoneWidth:int,+ToDoWidth:int,
%!		+StartMarker,+StartText:text,+DoneChar:char,+DoneText:text,
%!		+TodoText:text,+TodoChar:char,+EndText:text, +EndMarker:char)// is det.
%
%	render_bar does the actual rendering: It generates Template for format/3, and processes the arguments
%	StartPosition represents the position to start the rendering
%	DoneWidth represents the length (in characters) of the Done area
%	TodoWidth represents the length (in characters) of the Todo area
do_render_bar(StartPosition,DoneWidth,TodoWidth,IntroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker,OutroText) -->
    {
        must_be(nonneg,DoneWidth),
        must_be(nonneg,TodoWidth),
        progress_text(StartText,DoneWidth,DoneChar,DoneText,TodoText,TodoChar,TodoWidth,EndText,ProgressText),
        MetaTabSpec = '~~~w|~w~w~w~w~w',
        format(atom(TabSpec),MetaTabSpec,[StartPosition,IntroText,StartMarker,ProgressText,EndMarker,OutroText])
   },
    [TabSpec].

repeat_string(_,0,"") :- !.
repeat_string(Str,1,Str) :- !.
repeat_string(Str,N,Result) :-
	NN is N - 1,
	repeat_string(Str,NN,Result0),
	string_concat(Str,Result0,Result).

reverse_string(Str,StrR) :-
	string_codes(Str,Cs),
	reverse(Cs,CsR),
	string_codes(StrR,CsR).

super_impose_left(Base,Super,Result) :-
	super_impose(Base,Super,Result).

super_impose_right(Base,Super,Result) :-
	reverse_string(Base,BaseR),
	reverse_string(Super,SuperR),
	super_impose(BaseR,SuperR,ResultR),
	reverse_string(ResultR,Result).

super_impose(Base,Super,Result) :-
	normalise_text(Base,BaseN),
	normalise_text(Super,SuperN),
	string_codes(BaseN,BaseCs),
	string_codes(SuperN,SuperCs),
	super_impose_codes(BaseCs,SuperCs,ResultCs),
	string_codes(Result,ResultCs).

super_impose_codes([],[],[]) :- !.
super_impose_codes(BaseCs,[],BaseCs) :- !.
super_impose_codes([],SuperCs,SuperCs) :- !.

super_impose_codes([BaseC|BaseCs],[SuperC|SuperCs],[BaseC|ResultCs]) :-
	code_type(SuperC,white),!,	% @FIXME
	super_impose_codes(BaseCs,SuperCs,ResultCs).

super_impose_codes([_|BaseCs],[SuperC|SuperCs],[SuperC|ResultCs]) :-
	super_impose_codes(BaseCs,SuperCs,ResultCs).

normalise_text(Str,StrN) :-
	atom_codes(Str, Cs),
   	phrase(normalise_text(StrN), Cs, []).

 %% normalise input
normalise_text(Text) --> normalise_chars(Cs),{atom_codes(Text,Cs)}.
normalise_chars(NCs) --> normalise_char(C), normalise_chars(Cs),!,{append(C,Cs,NCs)}.
normalise_chars([]) --> !.

normalise_char(Cs) --> "\t",{atom_codes("   ",Cs)},!.
normalise_char(Cs) --> "\r",{atom_codes("",Cs)},!.
normalise_char(Cs) --> "\f",{atom_codes("",Cs)},!.
normalise_char(Cs) --> "\240",{atom_codes(" ",Cs)},!. % non-breaking space

normalise_char([C]) --> [C],!.

progress_text(StartText,DoneWidth,DoneChar,DoneText,TodoText,TodoChar,TodoWidth,EndText,ProgressText) :-
	done_bar(DoneWidth,DoneChar,DoneText,DoneBar),
	todo_bar(TodoWidth,TodoChar,TodoText,TodoBar),
	string_concat(DoneBar,TodoBar,Bar0),
	super_impose(Bar0,StartText,Bar1),
	super_impose_right(Bar1,EndText,ProgressText).

todo_bar(TodoWidth,TodoChar,TodoText,TodoBar) :-
	reverse_string(TodoText,RTodoText),
	done_bar(TodoWidth,TodoChar,RTodoText,RTodoBar),
	reverse_string(RTodoBar,TodoBar).

done_bar(DoneWidth,_DoneChar,DoneText,DoneBar) :-
	string_length(DoneText,N),
	DoneWidth =< N,!,
	sub_string(DoneText, _, DoneWidth, 0, DoneBar).

done_bar(DoneWidth,DoneChar,DoneText,DoneBar) :-
	string_length(DoneText,N),
	DoneN is DoneWidth - N,
	repeat_string(DoneChar,DoneN,DoneChars),
	string_concat(DoneChars,DoneText,DoneBar).
