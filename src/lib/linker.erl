%%%-----------------------------------------------------------------------------
%%% Linker is used to process a flow (ordered list of <<"asana ids">>) creating
%%% new enters_from and exits_to as necessary.
%%%-----------------------------------------------------------------------------
-module (linker).

-export([link_flow/1]).


% Given a list of asana ids in order, set the enters_to and exits_from accordingly.
link_flow(Flow) ->
    link_asana(Flow).


% Step through each pair of asana, updating exit_to.
link_asana([]) ->
    ok;
link_asana([_A|[]]) ->
    ok;
link_asana([A, B|Tl]) ->
    AsanaA = boss_db:find(A),
    AsanaB = boss_db:find(B),
    update_exit_to(AsanaA, AsanaB),
    link_asana([B|Tl]).


% If A already exits to B, do nothing; otherwise create the correct exit_to and
% enter_from relationships.
update_exit_to(A, B) ->
    case exists(A, exits_to, B) of
        true ->
            ok;
        false ->
            create(A, exits_to, B),
            create(A, enters_from, B)
    end.
    
                
exists(A, exits_to, B) ->
    case A:exit_to([{to_asana_id, equals, B:id()}]) of
        [] ->
            false;
        _Asana ->
            true
    end.


create (A, exits_to, B) ->
    New = boss_record:new(exit_to, [{asana_id, A:id()}, {to_asana_id, B:id()}]),
    New:save();

create(A, enters_from, B) ->
    New = boss_record:new(enter_from, [{asana_id, B:id()}, 
                                       {from_asana_id, A:id()}]),
    New:save().

