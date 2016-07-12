-module(flow_admin_controller, [Req]).
-compile(export_all).


hello('GET', []) ->
    {ok, [{message, "Hello, world!"}]}.


muscle_group_list('GET', []) ->
    MuscleGroups = boss_db:find(muscle_group, []),
    {ok, [{musclegroups, MuscleGroups}]}.

asana('GET', []) ->
    {ok, []};
asana('GET', ["all"]) ->
    Asanas = boss_db:find(asana, []),
    {json, [{asanas, Asanas}]};
asana('POST', ["update"]) ->
    JSON = Req:post_param(<<"new_asana">>),
    PList = json_to_plist(JSON),
    Id = proplists:get_value(id, PList),
    Old = boss_db:find(Id),
    New = Old:set(PList),
    New:save(),
    {205, "reset content", []}.


details('GET', [AsanaId]) ->
    Asana = boss_db:find(AsanaId),
    Mg = Asana:muscle_group_objects(),
    Rom = Asana:range_objects(),
    Enters = Asana:enters_from(),
    Exits =  Asana:exits_to(),
    {json, [{mg, Mg},
            {rom, Rom},
            {enters, Enters},
            {exits, Exits}]}.


json_to_plist(JSON) ->
    PList = jsx:decode(JSON),
    F = fun({K,V}) when is_binary(V) -> {binary_to_atom(K, utf8), binary_to_list(V)};
           ({K,V}) ->{binary_to_atom(K, utf8), V}
        end,
    lists:map(F, PList).


