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
    {json, [{asanas, Asanas}]}.

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
