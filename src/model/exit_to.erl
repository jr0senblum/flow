%%% ----------------------------------------------------------------------------
%%% EXIT_TO maps a "to" asana to a given asana: defines Exit_to.
%%% ----------------------------------------------------------------------------

-module(exit_to, [Id, AsanaId, ToAsanaId]).
-compile(export_all).

-belongs_to(asana).






