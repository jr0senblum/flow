%%%-----------------------------------------------------------------------------
%%% Controller used for creating a flow /create urls
%%%-----------------------------------------------------------------------------

-module(flow_create_controller, [Req]).
-compile(export_all).


before_(_) ->
    user_lib:require_login(Req).


create('GET', [FlowId], _FlowUser) ->
    [VinyasaX] = boss_db:find(asana, [{name, equals, "Vinyasa x"}]),
    {ok, [{id, FlowId},
          {vinyasax, VinyasaX:id()}]}.

flow('GET', [FlowId], _FlowUser) ->
    [Flow] = boss_db:find(flow, [id, equals, FlowId]),
    Asanas = [list_to_binary(string:strip(A)) || A <- string:tokens(Flow:asanas(), ",")],
    {json, [{flow, Asanas}]}.

save('POST', [FlowId], _FlowUser) ->
    JsonIds = Req:post_param(<<"updated_flow">>),
    IdsList = json_to_plist(JsonIds),
    IdsString = string:join(IdsList, ","),

    F = boss_db:find(FlowId),
    G = F:set(asanas, IdsString),
    G:save(),
    linker:link_flow(IdsList),
    {205, "reset content", []}.

% for view/admin/asana.html: return all asanas, update an asana.
asana('GET', [], _FlowUser) ->
    {ok, []};
asana('GET', ["all"], _FlowUser) ->
    Asanas = boss_db:find(asana, [], [{order_by, name}]),
    {json, [{asanas, Asanas}]}.


% return the associated exits of a given asana.
exits('GET', [AsanaId], _FlowUser) ->
    Asana = boss_db:find(AsanaId),
    Exits =  Asana:exits_to(),
    {json, [{exits, Exits}]}.


% Given ALeft ARight and A, what Asana have gone from ALeft to ARight.
replacements('GET', [AsanaIdLeft, AsanaIdRight], _FlowUser) when AsanaIdLeft == "undefined"->
    Works = (boss_db:find(AsanaIdRight)):enters_from(),
    {json, [{replacements, Works}]};

replacements('GET', [AsanaIdLeft, AsanaIdRight], _FlowUser) when AsanaIdRight == "undefined"->
    Works = (boss_db:find(AsanaIdLeft)):exits_to(),
    {json, [{replacements, Works}]};

replacements('GET', [AsanaIdLeft, AsanaIdRight], _FlowUser) ->
    AsanaLeftExits = sets:from_list((boss_db:find(AsanaIdLeft)):exits_to()),
    AsanaRightEnters = sets:from_list((boss_db:find(AsanaIdRight)):enters_from()),
    Works = 
        sets:to_list(sets:intersection(AsanaLeftExits, AsanaRightEnters)),
    {json, [{replacements, Works}]}.




%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------


% Parse the JSON into a PLIST suitible for using as the parameter to various 
% update fns.

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



% Converts a JSON string to property lists with binary-string properties 
% converted to atoms and, for values, strings (to match Boss' enetity record
% structure).
json_to_plist(JSON) ->
    PList = jsx:decode(JSON),
    F = fun({K,V}) when is_binary(V) -> {binary_to_atom(K, utf8), binary_to_list(V)};
           ({K,V}) ->{binary_to_atom(K, utf8), V};
           (E) when is_binary(E)  ->
                binary_to_list(E)
        end,
    lists:map(F, PList).


