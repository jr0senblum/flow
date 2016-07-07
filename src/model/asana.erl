-module(asana, [Id, 
                Name::string(),
                Sanskrit::string(),
                Strength::boolean(),
                Flexibility::boolean(),
                Ballance::boolean()
               ]).

-compile(export_all).
-has({asana_mg, many}).
-has({asana_ranges, many}).
