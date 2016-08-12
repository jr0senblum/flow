%%% ----------------------------------------------------------------------------
%%% RANGE_OF_MOTION applied to one or more asana via asana_ranges
%%% ----------------------------------------------------------------------------

-module(range_of_motion, [Id, Name::string()]).
-compile(export_all).

-has({asana_ranges, many}).
