%%%-----------------------------------------------------------------------------
%%% Controller handling /landing urls
%%%-----------------------------------------------------------------------------

-module(flow_landing_controller, [Req]).
-compile(export_all).


before_(_) ->
    user_lib:require_login(Req).


% /landing/landing
landing('GET', [], FlowUser) ->
    {ok, [{user, FlowUser}]}.

% /landing/flow GET JSON to the requester consisting of all flows.
flows('GET', [], _FlowUser) ->
    Flows = boss_db:find(flow,[],[{order_by,name}]),
    {json, [{flows, Flows}]}.

% /landing/delete POSTING id of flow to delte.
delete('POST',[], _FlowUser) ->
    Id = binary_to_list(Req:post_param(<<"id">>)),
    boss_db:delete(Id),
    {205, "reset content", []}.

% /landing/edit POSTING id, name and description to edit/create.
edit('POST',[], _FlowUser) ->
    Id = binary_to_list(Req:post_param(<<"id">>)),
    Name = binary_to_list(Req:post_param(<<"name">>)),
    Description = binary_to_list(Req:post_param(<<"description">>)),

    case Id of
        "new" -> 
            F = boss_record:new(flow, [{name, Name},
                                       {description, Description},
                                       {asanas,""}]),
            F:save(),
            {201, "create", []};
        _Flow_id ->
            Old = boss_db:find(Id),
            New = Old:set([{name, Name}, 
                           {description, Description}]),
            New:save(),
            {205, "reset content", []}
    end.
