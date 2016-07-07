-module(asana_ranges, [Id, AsanaId, RangeOfMotionId]).
-compile(export_all).

-belongs_to(asana).
-belongs_to(range_of_motion).
