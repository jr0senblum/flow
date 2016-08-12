%%%-----------------------------------------------------------------------------
%%% Controller handling /landing urls
%%%-----------------------------------------------------------------------------

-module(flow_landing_controller, [Req]).
-compile(export_all).



% /landing/landing
landing('GET', []) ->
    {ok, []}.

% /landing/flow GET JSON to the requester consisting of all flows.
flows('GET', []) ->
    Flows = boss_db:find(flow,[],[{order_by,name}]),
    {json, [{flows, Flows}]}.

% /landing/delete POSTING id of flow to delte.
delete('POST',[]) ->
    Id = binary_to_list(Req:post_param(<<"id">>)),
    boss_db:delete(Id),
    {205, "reset content", []}.

% /landing/edit POSTING id, name and description to edit/create.
edit('POST',[]) ->
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
