%%% ----------------------------------------------------------------------------
%%% ASANA cover both poses (asanas) and transitions (vinyasas).
%%% They have english names, sanskirt names, and a boolean indicating whether
%%% they are a pose or a vinyasa.
%%% ----------------------------------------------------------------------------

-module(asana, [Id, 
                Name::string(),
                Sanskrit::string(),
                IsVinyasa ::boolean(),
                Balance::boolean(),
                Flexibility::boolean(),
                Strength::boolean(),
                VType::string(),
                IsBlank::boolean()
               ]).

-compile(export_all).


% Many ranges and muscle groups and they can be entered-from multiple asanas
% and can exit it multiple asanas.
-has({asana_mg, many}).
-has({asana_range, many}).
-has({enter_from, many}).
-has({exit_to, many}).


% Vinyasa types are either the all-caps, strings: up, down, side or none
validation_tests()->
    [{fun() -> lists:member(string:to_upper(VType), ["UP","DOWN","SIDE","NONE"]) end,
      {error, "Vinyasa type must be \"up\", \"down\", \"side\" or \"none\"."}}
    ].


% Vinyasa type should always be to_upper.
before_create() ->
    Modified = set([{v_type, string:to_upper(VType)}]),
    {ok, Modified}.



%% -----------------------------------------------------------------------------
%% Additional Asana methods
%% -----------------------------------------------------------------------------


% Retrieves a sorted list of muscle group (mg) objects associated with this 
% asana. Sorts on name.
muscle_group_objects() ->
    lists:sort(fun sort_fun/2, 
               [AsanaMg:muscle_group() || AsanaMg <- THIS:asana_mg()]).


% Retrieves a sorted list of range-of-motion (range) records associated with 
% this asana. Sorts on name.
range_objects() ->
    lists:sort(fun sort_fun/2, 
               [ARange:range_of_motion() || ARange <- THIS:asana_range()]).


% Retrieves a sorted list of asana records which enter from this asana. 
% Sorts on name.
enters_from() ->
    lists:sort(fun sort_fun/2, 
               [boss_db:find(EntersFrom:from_asana_id()) 
                || EntersFrom <- THIS:enter_from()]).


% Retrieves a sorted list of asana records which exit to this asana. 
% Sorts on name.
exits_to() ->
    lists:sort(fun sort_fun/2, 
               [boss_db:find(ExitsTo:to_asana_id())
                || ExitsTo <- THIS:exit_to()]).

sort_fun(A, B) ->
    A:name() =< B:name().


% Replace the associated enters_from asanas with those having the supplied list
% of asana ids. Create the corresponding exit_to entities.
replace_enters_from(AsanaList) ->
    delete_enters_from(),
    [(enter_from:new(id, THIS:id(), AsanaId)):save() || AsanaId <- AsanaList],
    [(exit_to:new(id, AsanaId, THIS:id())):save() || AsanaId <- AsanaList],
    ok.

% Replace the associated exit_to asanas with those having the supplied list 
% of asana ids. Create the corresponding enter_from entities.
replace_exits_to(AsanaList) ->
    delete_exits_to(),
    [(exit_to:new(id, THIS:id(), AsanaId)):save() || AsanaId <- AsanaList],
    [(enter_from:new(id, AsanaId, THIS:id())):save() || AsanaId <- AsanaList],
    ok.

% Replace the associated major muscle group records with those having the ids in
% the supplied list.
replace_mmgs(MgList) ->
    delete_asana_mmgs(),
    [(asana_mg:new(id, THIS:id(), MgId)):save() || MgId <- MgList],
    ok.

% Replace the associated ranges-of-motion records with those having the ids in
% the supplied list.
replace_roms(RangeList) ->
    delete_asana_roms(),
    [(asana_range:new(id, THIS:id(), RangeId)):save() || RangeId <- RangeList],
    ok.



% delete functions
delete_enters_from() ->
    _ = [delete_enters_from(EnterFrom) || EnterFrom <- THIS:enter_from()]. 

delete_exits_to() ->
    _ = [delete_exits_to(ExitTo) || ExitTo <- THIS:exit_to()]. 

delete_asana_mmgs() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:asana_mg()]. 

delete_asana_roms() ->
    _ = [boss_db:delete(Rec:id()) || Rec <- THIS:asana_range()]. 


% Delete the enter_from entitity and the corresponding exit_to entity.         
delete_enters_from(AnEnterFrom) ->       
    FromId = AnEnterFrom:from_asana_id(),
    boss_db:delete(AnEnterFrom:id()),
    case boss_db:find(exit_to,[{to_asana_id, equals, THIS:id()},
                               {asana_id, equals, FromId}]) of
        [] ->
            ok;
        [ExitTo] ->
            boss_db:delete(ExitTo:id())
    end.

% Delete the exit_to entitity and the corresponding enter_from entity.         
delete_exits_to(AnExitTo) ->       
    ToId = AnExitTo:to_asana_id(),
    boss_db:delete(AnExitTo:id()),
    case boss_db:find(enter_from,[{from_asana_id, equals, THIS:id()},
                               {asana_id, equals, ToId}]) of
        [] ->
            ok;
        [EnterFrom] ->
            boss_db:delete(EnterFrom:id())
    end.
    
