-module(flow_print_controller, [Req]).
-compile(export_all).


print('GET', []) ->
    {ok, []};
print('GET', [FlowId]) ->
    Flow = boss_db:find(FlowId),
    {ok, [{flow_id, FlowId},
          {flow_name, Flow:name()}]}.

details('GET', [FlowId]) -> 
    Flow = boss_db:find(FlowId),
    AsanaIds = [string:strip(A) || A <- string:tokens(Flow:asanas(), ",")],
    Names = [list_to_binary(name_from_id(AsanaId)) || AsanaId <- AsanaIds],
    Imgs = [list_to_binary("/static/stickfigures/" ++ string:to_lower(binary_to_list(N)) ++ ".jpg") || N <- Names],

    {json, [{asana_names, Names},
          {img_strings, Imgs}]}.


name_from_id(AsanaId) ->
    A = boss_db:find(AsanaId),
    A:name().

