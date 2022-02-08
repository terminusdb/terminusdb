:- module(http_utils,[
              basic_authorization/3
          ]).

:- use_module(library(base64)).

basic_authorization(User,Pass,Authorization) :-
    format(string(S), "~s:~s", [User, Pass]),
    base64(S, Base64_Destination_Auth),
    format(string(Authorization), "Basic ~s", [Base64_Destination_Auth]).
