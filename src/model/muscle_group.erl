%%% ----------------------------------------------------------------------------
%%% MUSCLE_GROUP applied to one or more asana via asana_mg.
%%% ----------------------------------------------------------------------------

-module(muscle_group, [Id, Name::string()]).
-compile(export_all).

-has({asana_mg, many}).
