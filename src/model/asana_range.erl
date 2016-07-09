-module(asana_range, [Id, AsanaId, RangeOfMotionId]).
-compile(export_all).

-belongs_to(asana).
-belongs_to(range_of_motion).
