-module(asana, [Id, 
                Name::string(),
                Sanskrit::string(),
                Strength::boolean(),
                Flexibility::boolean(),
                Ballance::boolean()
               ]).

-compile(export_all).
-has({asana_mg, many}).
-has({asana_range, many}).
-has({enter_from, many}).




muscle_group_objects() ->
    [AsanaMg:muscle_group() || AsanaMg <- THIS:asana_mg()].

range_objects() ->
    [AsanaRange:range_of_motion() || AsanaRange <- THIS:asana_range()].

enters_from() ->
    [boss_db:find(EntersFrom:from_asana_id()) || EntersFrom <- THIS:enter_from()].

exits_to() ->
    EF = boss_db:find(enter_from, [{from_asana_id, equals, THIS:id()}]),
    [boss_db:find(EntersFrom:asana_id()) || EntersFrom <- EF].
    
