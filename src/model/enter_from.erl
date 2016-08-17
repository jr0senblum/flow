%%% ----------------------------------------------------------------------------
%%% ENTER_FROM maps a "from" asana to a given asana: defines Enter_from.
%%% ----------------------------------------------------------------------------

-module(enter_from, [Id, AsanaId, FromAsanaId]).
-compile(export_all).

-belongs_to(asana).

% Ignore blank asanas in any sort of automatic linkage.
before_create() ->
    Asana = boss_db:find(AsanaId),
    From =  boss_db:find(FromAsanaId),
    case (Asana:is_blank() orelse From:is_blank()) of
        true ->
            {error, "Ignoring Blank Asanas"};
        false ->
            ok
    end.


    






