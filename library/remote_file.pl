:- module(remote_file,[
              copy_remote/4
          ]).

:- use_module(library(http/http_open)).

/** <module> Remote File
 *
 * Remote file manipulation
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(file_utils).


/*
 * copy_remote(+Remote, +Name, -File) is det.
 *
 */
copy_remote(Remote, Name, File, Options) :-
    (   get_time(Time),
        sanitise_file_name(Name,Safe),
        temp_path(Dir),
        format(atom(File), "~w/~w-~w", [Dir,Safe,Time]),
        (   memberchk(user(User),Options),
            memberchk(password(Pass),Options),
            http_open(Remote, In, [authorization(basic(User,Pass))])
        ->  true
            % or try with no pass..
        ;   http_open(Remote, In, []))
    ->  open(File, write, FileBuffer),
        copy_stream_data(In, FileBuffer),
        close(FileBuffer)
    ;   format(atom(M), 'Unable to retrieve blob id ~w from remote location ~w', [Name,Remote]),
        throw(error(M))
    ).
