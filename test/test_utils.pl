:- module(test_utils,[
              try/1
          ]).
                 
/** <module> Test Utilities
 * 
 * Utils to assist in testing.
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

:- meta_predicate try(0).
try(Goal) :- 
    (   format('~n*****************************************************',[]),
        format('~n* Running test ~q', [Goal]),
        format('~n*****************************************************~n',[]),
        call(Goal)
    ->  true
    ;   format('~n+++++++++++++++++++++++++++++++++++++++++++++++++++++~n', []),
        format('~n+ FAIL! Could not successfully run ~q',[Goal]),
        format('~n+++++++++++++++++++++++++++++++++++++++++++++++++++++~n', []),
        fail
    ).
