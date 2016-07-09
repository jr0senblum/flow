


-module(flow_construct_controller, [Req]).
-compile(export_all).


hello('GET', []) ->
    {ok, [{message, "Hello, world!"}]}.


muscle_group_list('GET', []) ->
    MuscleGroups = boss_db:find(muscle_group, []),
    {ok, [{musclegroups, MuscleGroups}]}.

flow('GET', []) ->
    Asana = boss_db:find_first(asana, []),
    {ok, [{asana, Asana}, 
          {mg, Asana:muscle_group_objects()},
          {rom, Asana:range_objects()},
          {path, "/static/tittibhasana.jpg"}]}.



