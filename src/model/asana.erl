-module(asana, [Id, 
                Name::string(),
                Sanskrit::string(),
                Strength::boolean(),
                Flexibility::boolean(),
                Ballance::boolean()
               ]).

-compile(export_all).
-has({asana_mg, many}).
-has({asana_range, many}).
-has({enter_from, many}).
-has({exit_to, many}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return lists of related objects.

muscle_group_objects() ->
    lists:sort(fun sort_fun/2, [AsanaMg:muscle_group() || AsanaMg <- THIS:asana_mg()]).

range_objects() ->
    lists:sort(fun sort_fun/2, [AsanaRange:range_of_motion() || AsanaRange <- THIS:asana_range()]).

enters_from() ->
    lists:sort(fun sort_fun/2, [boss_db:find(EntersFrom:from_asana_id()) || EntersFrom <- THIS:enter_from()]).

exits_to() ->
    lists:sort(fun sort_fun/2, [boss_db:find(ExitsTo:to_asana_id()) || ExitsTo <- THIS:exit_to()]).


sort_fun(A, B) ->
    A:name() =< B:name().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Replace related objects. Assumes that we are given the entire list of
% related object (i.e., enter_from / exit_to) ids, so always delete first.
%

replace_enters_from(AsanaList) ->
    delete_enters_from(),
    [(enter_from:new(id, THIS:id(), AsanaId)):save() || AsanaId <- AsanaList],
    ok.

replace_exits_to(AsanaList) ->
    delete_exits_to(),
    [(exit_to:new(id, THIS:id(), AsanaId)):save() || AsanaId <- AsanaList],
    ok.

replace_mmgs(MgList) ->
    delete_asana_mmgs(),
    [(asana_mg:new(id, THIS:id(), MgId)):save() || MgId <- MgList],
    ok.

replace_roms(RangeList) ->
    delete_asana_roms(),
    [(asana_range:new(id, THIS:id(), RangeId)):save() || RangeId <- RangeList],
    ok.




delete_enters_from() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:enter_from()]. 

delete_exits_to() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:exit_to()]. 

delete_asana_mmgs() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:asana_mg()]. 

delete_asana_roms() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:asana_range()]. 


                
