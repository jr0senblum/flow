%%% ----------------------------------------------------------------------------
%%% ASANA_MG map a muscle_group to an asana.
%%% ----------------------------------------------------------------------------

-module(asana_mg, [Id, AsanaId, MuscleGroupId]).
-compile(export_all).

-belongs_to(asana).
-belongs_to(muscle_group).
