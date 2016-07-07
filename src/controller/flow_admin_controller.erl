-module(flow_admin_controller, [Req]).
-compile(export_all).


hello('GET', []) ->
    {ok, [{message, "Hello, world!"}]}.


muscle_group_list('GET', []) ->
    MuscleGroups = boss_db:find(muscle_group, []),
    {ok, [{musclegroups, MuscleGroups}]}.

asana('GET', []) ->
    Asana = boss_db:find_first(asana, []),
    MgId = asana_mg:muscle_group_id(Asana:first_asana_mg()),
    MuscleGroup = boss_db:find_first(muscle_group, [{id, equals, MgId}]),
    {ok, [{asana, Asana}, {mg, MuscleGroup}]}.
