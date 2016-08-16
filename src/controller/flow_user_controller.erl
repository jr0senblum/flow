%%%-----------------------------------------------------------------------------
%%% Controller used for logging in/out a user.
%%%-----------------------------------------------------------------------------

-module(flow_user_controller, [Req]).
-compile(export_all).


login('GET', []) ->
    {ok, [{redirect, Req:header(referer)}]};

login('POST', []) ->
    Name = Req:post_param("name"),
    case boss_db:find(flow_user, [{name, Name}], [{limit,1}]) of
        [FlowUser] ->
            case FlowUser:check_password(Req:post_param("password")) of
                true ->
                    {redirect, proplists:get_value("redirect", Req:post_params(), "/"), 
                     FlowUser:login_cookies()};
                false ->
                    {ok, [{error, "Bad name/password combination"}]}
            end;
        [] ->
            {ok, [{error, "No user named " ++ Name}]}
    end.


logout('GET', []) ->
    {redirect, "/user/login",
        [ mochiweb_cookies:cookie("user_id", "", [{path, "/"}]),
          mochiweb_cookies:cookie("session_id", "", [{path, "/"}]) ]}.
