:- module(http_utils,[
              basic_authorization/3,
              is_local_https/1
          ]).

basic_authorization(User,Pass,Authorization) :-
    format(string(S), "~s:~s", [User, Pass]),
    base64(S, Base64_Destination_Auth),
    format(string(Authorization), "Basic ~s", [Base64_Destination_Auth]).

is_local_https(URL) :-
    re_match('^https://127.0.0.1', URL, []).
