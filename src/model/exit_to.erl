%%% ----------------------------------------------------------------------------
%%% EXIT_TO maps a "to" asana to a given asana: defines Exit_to.
%%% ----------------------------------------------------------------------------

-module(exit_to, [Id, AsanaId, ToAsanaId]).
-compile(export_all).

-belongs_to(asana).


% Ignore blank asanas in any sort of automatic linkage.
before_create() ->
    Asana = boss_db:find(AsanaId),
    From =  boss_db:find(ToAsanaId),
    case (Asana:is_special() orelse From:is_special()) of
        true ->
            {error, "Ignoring Blank Asanas"};
        false ->
            ok
    end.






