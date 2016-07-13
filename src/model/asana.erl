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
    [AsanaMg:muscle_group() || AsanaMg <- THIS:asana_mg()].

range_objects() ->
    [AsanaRange:range_of_motion() || AsanaRange <- THIS:asana_range()].

enters_from() ->
    [boss_db:find(EntersFrom:from_asana_id()) || EntersFrom <- THIS:enter_from()].

exits_to() ->
    [boss_db:find(ExitsTo:to_asana_id()) || ExitsTo <- THIS:exit_to()].



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

delete_enters_from() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:enter_from()]. 

delete_exits_to() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:exit_to()]. 


                
