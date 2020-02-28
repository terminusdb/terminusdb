:- module(sdk,[
              connect/2,
              ask/2,
              ask/1]).

/** <module> Prolog SDK
 *
 * Package to make interaction with WOQL and the TerminusDB easy and
 * efficient.
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
:- use_module(database).
:- use_module(woql_compile).

:- reexport(woql_term).

pre_term_to_term_and_bindings(Pre_Term,Term,Bindings_In,Bindings_Out) :-
    (   var(Pre_Term)
    ->  (   member(V=X,Bindings_In),
            Pre_Term == X
        ->  Bindings_In = Bindings_Out,
            Term = v(V)
        ;   gensym('Var',G),
            Bindings_Out = [G=Pre_Term|Bindings_In],
            Term = v(G)
        )
    ;   is_dict(Pre_Term)
    ->  Term = Pre_Term,
        Bindings_In=Bindings_Out
    ;   Pre_Term =.. [F|Args],
        mapm(sdk:pre_term_to_term_and_bindings,Args,New_Args,Bindings_In,Bindings_Out),
        Term =.. [F|New_Args]
    ).

ask(Server,Pre_Term) :-
    pre_term_to_term_and_bindings(Pre_Term,Term,[],Bindings_Out),
    select(bindings=_,Server,
           bindings=Bindings_Out,New_Ctx),
    compile_query(Term,Prog,New_Ctx,_),
    debug(terminus(sdk),'Program: ~q~n', [Prog]),
    woql_compile:Prog.

ask(Pre_Term) :-
    empty_ctx(Ctx),
    ask(Ctx,Pre_Term).
