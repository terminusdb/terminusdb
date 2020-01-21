:- module(tests,[
              run_tests/0
          ]).

/** <module> Run all tests
 *
 * This file contains the testing regime
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(test(api_test)).
:- use_module(test(jsonld_test)).

%!  run_tests is semidet
%
%   This is run by Travis CI, or when we want to manually
%   invoke the integration tests.
%
%   Travis does something like
%
%   ----
%   swipl -g run_tests -g halt
%   ----
%
%   which will exit swipl with 0 if run_tests succeeds, and 1 if it
%   fails. Travis looks at this error code.
%
%   If any of the individual tests fail, run_tests will fail.
%
run_tests :-
    run_api_tests,
    run_jsonld_tests.

