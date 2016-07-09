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




muscle_group_objects() ->
    [AsanaMg:muscle_group() || AsanaMg <- THIS:asana_mg()].

range_objects() ->
    [AsanaRange:range_of_motion() || AsanaRange <- THIS:asana_range()].


    
