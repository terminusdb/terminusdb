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
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the  the Apache License, Version 2.0           *
 *  (the "License");                                                     *
 *  you may not use this file except in compliance with the License.     *
 *  You may obtain a copy of the License at                              *
 *                                                                       *
 *  http://www.apache.org/licenses/LICENSE-2.0                           *
 *                                                                       *
 *  Unless required by applicable law or agreed to in writing, software  *
 *  distributed under the License is distributed on an "AS IS" BASIS,    *
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      *
 *  implied.                                                             *
 *  See the License for the specific language governing permissions and  *
 *  limitations under the License.                                       *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(file_utils).

/*
 * copy_remote(+Remote, +Name, -File) is det.
 *
 */
copy_remote(Remote, Name, File, Options) :-
    (
        (   memberchk(user(User),Options),
            memberchk(password(Pass),Options),
            http_open(Remote, In, [authorization(basic(User,Pass))])
        ->  true
            % or try with no pass..
        ;   http_open(Remote, In, []))
    ->  tmp_file_stream(binary, File, Stream),
        copy_stream_data(In, Stream),
        close(Stream)
    ;   format(atom(M), 'Unable to retrieve blob id ~w from remote location ~w', [Name,Remote]),
        throw(error(M))
    ).
