%%%-----------------------------------------------------------------------------
%%% Controller used for configure /configure urls
%%%-----------------------------------------------------------------------------

-module(flow_configure_controller, [Req]).
-compile(export_all).

before_(_) ->
    user_lib:require_login(Req).


% configure/asana GET return nothing to asana.html
asana('GET', [], _FlowUser) ->
    {ok, []};

%configure/asana/all GET all asanas
asana('GET', ["all"], _FlowUser) ->
    Asanas = boss_db:find(asana, [], [{order_by, name}]),
    {json, [{asanas, Asanas}]};

%configure/asana/update POST update the given asana.
asana('POST', ["update"], _FlowUser) ->
    UpdatedAsana = update_asana(Req:post_param(<<"new_asana">>)),

    update_froms(UpdatedAsana, Req:post_param(<<"from_asanas">>)),
    update_tos(UpdatedAsana, Req:post_param(<<"to_asanas">>)),

    update_mmgs(UpdatedAsana, Req:post_param(<<"mmgs">>)),
    update_roms(UpdatedAsana, Req:post_param(<<"roms">>)),
    {205, "reset content", []}.

% configure/related/asanaId return the associated entities of a given asana. Also 
% return all of the muscle_grou and range_of_motions. These are not related
% to the AsanaId, per se, but they might have changed via another screen
% so it's good to get the latest.
related('GET', [AsanaId], _FlowUser) ->
    Asana = boss_db:find(AsanaId),
    Mg = Asana:muscle_group_objects(),
    Rom = Asana:range_objects(),
    Enters = Asana:enters_from(),
    Exits =  Asana:exits_to(),
    AllMg = boss_db:find(muscle_group, [], [{order_by, name}]),
    AllRom = boss_db:find(range_of_motion,[], [{order_by, name}]),
    {json, [{mg, Mg},
            {allmg, AllMg},
            {rom, Rom},
            {allrom, AllRom},
            {enters, Enters},
            {exits, Exits}]}.



%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------


% Given a JSON representation of the asana record, update the persisted version.
% json_to_plist returns [{prop, value}...] which is what the set/1 takes.
update_asana(JSON) ->
    PList = json_to_plist(JSON),
    Id = proplists:get_value(id, PList),
    Old = boss_db:find(Id),
    New = Old:set(PList),
    {ok, New} = New:save(),
    New.


% Given a JSON representation of enter_from records to be set for the given
% asana, do so.
update_froms(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_enters_from(PList).


% Given a JSON representation of exit_to records to be set for the given
% asana, do so.
update_tos(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_exits_to(PList).


% Given a JSON representation of major muscle group records to be set for the 
% given asana, do so.
update_mmgs(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_mmgs(PList).

% Given a JSON representation of ranges of motion records to be set for the 
% given asana, do so.
update_roms(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_roms(PList).



% Converts a JSON string to property lists with binary-string properties 
% converted to atoms and, for values, strings (to match Boss' entity record 
% structure).
json_to_plist(JSON) ->
    PList = jsx:decode(JSON),
    F = fun({K,V}) when is_binary(V) -> {binary_to_atom(K, utf8), binary_to_list(V)};
           ({K,V}) ->{binary_to_atom(K, utf8), V};
           (E) when is_binary(E)  ->
                binary_to_list(E)
        end,
    lists:map(F, PList).


