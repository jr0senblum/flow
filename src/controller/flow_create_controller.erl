-module(flow_create_controller, [Req]).
-compile(export_all).


create('GET', []) ->
    {ok, []};
create('GET', [FlowId]) ->
    [Flow] = boss_db:find(flow, [id, equals, FlowId]),
    Asanas = [string:strip(A) || A <- string:tokens(Flow:asanas(), ",")],
    {ok, [{flow, Asanas},
          {id, FlowId},
          {name, Flow:name()}]}.


% for view/admin/asana.html: return all asanas, update an asana.
asana('GET', []) ->
    {ok, []};
asana('GET', ["all"]) ->
    Asanas = boss_db:find(asana, [], [{order_by, name}]),
    {json, [{asanas, Asanas}]}.


% return the associated objects of a given asana
related('GET', [AsanaId]) ->
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

replacements('GET', [AsanaIdLeft, AsanaIdRight]) when AsanaIdLeft == "undefined"->
    Works = (boss_db:find(AsanaIdRight)):enters_from(),
    {json, [{replacements, Works}]};

replacements('GET', [AsanaIdLeft, AsanaIdRight]) ->
    AsanaLeftExits = sets:from_list((boss_db:find(AsanaIdLeft)):exits_to()),
    AsanaRightEnters = sets:from_list((boss_db:find(AsanaIdRight)):enters_from()),
    Works = 
        sets:to_list(sets:intersection(AsanaLeftExits, AsanaRightEnters)),
    
    {json, [{replacements, Works}]}.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions

% Parse the JSON description of the passed-in asana and update the persisted
% version. JSON -> ASANA
%
update_asana(JSON) ->
    PList = json_to_plist(JSON),
    Id = proplists:get_value(id, PList),
    Old = boss_db:find(Id),
    New = Old:set(PList),
    {ok, New} = New:save(),
    New.

update_froms(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_enters_from(PList).

update_tos(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_exits_to(PList).

update_mmgs(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_mmgs(PList).

update_roms(UpdatedAsana, JSON) ->
    PList = json_to_plist(JSON),
    UpdatedAsana:replace_roms(PList).



% Converts a JSON string to property lists with binary-string properties converted 
% to atoms and, for values, strings (to match Boss' enetiry record structure)
%.
json_to_plist(JSON) ->
    PList = jsx:decode(JSON),
    F = fun({K,V}) when is_binary(V) -> {binary_to_atom(K, utf8), binary_to_list(V)};
           ({K,V}) ->{binary_to_atom(K, utf8), V};
           (E) when is_binary(E)  ->
                binary_to_list(E)
        end,
    lists:map(F, PList).


