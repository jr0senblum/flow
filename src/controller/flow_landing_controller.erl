


-module(flow_landing_controller, [Req]).
-compile(export_all).


% for view/admin/asana.html: return all asanas, update an asana.
landing('GET', []) ->
    Flows = boss_db:find(flow,[],[{order_by,name}]),
    {ok, [{flows, Flows}]}.


