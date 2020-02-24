:- module(time_travel,[
              show_history/1,
              history/2,
              revert/2
          ]).

/** <module> Time Travel
 *
 * Some convenience tools for time travel
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
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

:- use_module(utils).
:- use_module(file_utils).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

show_history(Tag) :-
    history(Tag,History),
    maplist([layer(Elt,TimeStamp)]>>(
                current_output(Out),
                format_time(Out, '%FT%T%:z', TimeStamp),
                format(Out, ' ~s~n', [Elt])
            ), History).

read_label_id(Path,Layer) :-
    setup_call_cleanup(
        open(Path, read, Stream),
        read_string(Stream,_,Size_Layer),
        close(Stream)
    ),
    split_string(Size_Layer, "\n", "", [_Size,Layer|_]).

read_layer_id(Path,Layer) :-
    setup_call_cleanup(
        open(Path, read, Stream),
        read_string(Stream,_,Layer),
        close(Stream)
    ).

history(Tag,History) :-
    db_path(Path),
    sanitise_file_name(Tag,Safe_Tag),
    interpolate([Path, '/', Safe_Tag, '.label'], Label),
    read_label_id(Label,Layer),
    layer_history(Layer,History).

layer_history(Layer,[layer(Layer,Time)|History]) :-
    db_path(Path),
    sub_string(Layer,0,3,_Length,Sub_Dir),
    interpolate([Path, '/', Sub_Dir, '/', Layer], Layer_File),
    time_file(Layer_File, Time),
    interpolate([Layer_File, '/parent.hex'], Parent),
    (   exists_file(Parent)
    ->  read_layer_id(Parent, New_Layer),
        layer_history(New_Layer, History)
    ;   History = []
    ).

revert(Tag, Layer) :-
    db_path(Path),
    sanitise_file_name(Tag,Safe_Tag),
    interpolate([Path, '/', Safe_Tag, '.label'], Label),
    layer_history(Layer, Hist),
    length(Hist, N),

    setup_call_cleanup(
        open(Label, write, Stream),
        (   write(Stream, N),
            nl(Stream),
            write(Stream,Layer),
            nl(Stream)
        ),
        close(Stream)
    ).

