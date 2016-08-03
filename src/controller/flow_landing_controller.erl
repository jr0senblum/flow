-module(flow_landing_controller, [Req]).
-compile(export_all).


% for view/admin/asana.html: return all asanas, update an asana.
landing('GET', []) ->
    Flows = boss_db:find(flow,[],[{order_by,name}]),
    {ok, [{flows, Flows}]}.

flows('GET', []) ->
    Flows = boss_db:find(flow,[],[{order_by,name}]),
    {json, [{flows, Flows}]}.

delete('POST',[]) ->
    Id = binary_to_list(Req:post_param(<<"id">>)),
    boss_db:delete(Id),
    {205, "reset content", []}.

edit('POST',[]) ->
    Id = binary_to_list(Req:post_param(<<"id">>)),
    Name = binary_to_list(Req:post_param(<<"name">>)),
    Description = binary_to_list(Req:post_param(<<"description">>)),
    io:format("it is ~p~n",[{Id, Name, Description}]),
    case Id of
        "new" -> 
            F = flow:new(id, Name, Description, ""),
            F:save(),
            {201, "create", []};
        _Flow_id ->
            Old = boss_db:find(Id),
            New = Old:set([{name, Name},{description, Description}]),
            New:save(),
            {205, "reset content", []}
    end.
