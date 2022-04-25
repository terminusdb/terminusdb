:- module(http_utils,[
              basic_authorization/3,
              bearer_authorization/2,
              token_authorization/2
          ]).

:- use_module(library(base64)).

basic_authorization(User,Pass,Authorization) :-
    format(string(S), "~s:~s", [User, Pass]),
    base64(S, Base64_Destination_Auth),
    format(string(Authorization), "Basic ~s", [Base64_Destination_Auth]).

bearer_authorization(Token,Authorization) :-
    format(string(Authorization), "Bearer ~s", [Token]).

token_authorization(Token,Authorization) :-
    format(string(Authorization), "Token ~s", [Token]).
