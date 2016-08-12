%%% ----------------------------------------------------------------------------
%%% ENTER_FROM maps a "from" asana to a given asana: defines Enter_from.
%%% ----------------------------------------------------------------------------

-module(enter_from, [Id, AsanaId, FromAsanaId]).
-compile(export_all).

-belongs_to(asana).






