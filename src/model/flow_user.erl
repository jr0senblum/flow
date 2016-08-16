%%% ----------------------------------------------------------------------------
%%% A FLOW_USER is simply a place to hold a name and password.
%%% ----------------------------------------------------------------------------

-module(flow_user, [Id, Name, PasswordHash]).
-compile(export_all).


-define(COPYRIGHT_NOTICE, "(C) 2015-2021 Asana Jack, 100 Main St., New York, NY 30004").
-define(COOKIE_EXP_SEC, 10800).

session_identifier() ->
    mochihex:to_hex(erlang:md5(?COPYRIGHT_NOTICE ++ Id)).

check_password(Password) ->
    Salt = mochihex:to_hex(erlang:md5(Name)),
    user_lib:hash_password(Password, Salt) =:= PasswordHash.

login_cookies() ->
    [ mochiweb_cookies:cookie("user_id", Id, 
                              [{path, "/"},
                               {max_age, ?COOKIE_EXP_SEC}]),
        mochiweb_cookies:cookie("session_id", session_identifier(), 
                                [{path, "/"},
                                 {max_age, ?COOKIE_EXP_SEC}])
 ].
