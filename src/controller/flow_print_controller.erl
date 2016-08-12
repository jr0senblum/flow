%%%-----------------------------------------------------------------------------
%%% Controller used to create the to-be-printed page /print urls
%%%-----------------------------------------------------------------------------

-module(flow_print_controller, [Req]).
-compile(export_all).


% /print/print/flow-id GETs flow-id and flow-name for the print.html
print('GET', [FlowId]) ->
    Flow = boss_db:find(FlowId),
    {ok, [{flow_id, FlowId},
          {flow_name, Flow:name()}]}.

% /print/details/flow-id GETS the asana names and image names for the print.html
details('GET', [FlowId]) -> 
    Flow        = boss_db:find(FlowId),
    AsanaIds    = get_ids_from_flow(Flow),
    AsanaNames  = get_names_from_ids(AsanaIds),
    ImageNames  = construct_img_names(AsanaNames),
    BinaryNames = [list_to_binary(Name) || Name <- AsanaNames],
    {json, [{asana_names, BinaryNames},
            {img_strings, ImageNames}]}.



%% -----------------------------------------------------------------------------
%% Helper fns
%% -----------------------------------------------------------------------------

get_ids_from_flow(Flow) ->
    [string:strip(A) || A <- string:tokens(Flow:asanas(), ",")].
    
get_names_from_ids(AsanaIds) ->
    [name_from_id(Id) || Id <- AsanaIds].

construct_img_names(AsanaNames) ->
    Prefix = "/static/stickfigures/",
    Ext = ".jpg",
    lists:map(fun(N) -> list_to_binary([Prefix, string:to_lower(N), Ext]) end,
              AsanaNames).

name_from_id(AsanaId) ->
    A = boss_db:find(AsanaId),
    A:name().

