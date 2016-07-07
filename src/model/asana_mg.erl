-module(asana_mg, [Id, AsanaId, MuscleGroupId]).
-compile(export_all).

-belongs_to(asana).
-belongs_to(muscle_group).
