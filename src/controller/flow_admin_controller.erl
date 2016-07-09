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

musclegroups('GET', [AsanaId, "musclegroups"]) ->
    Asana = boss_db:find(AsanaId),
    {json, [{mg, Asana:muscle_group_objects()}]}.

