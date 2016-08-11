-module (flow_util).
-export ([init/0, seed/0, load_asana/0, kill_asana/0]).
-compile([{parse_transform, lager_transform}]).
-define (APPNAME, flow).



init() ->
  init_db (),
  ok.

init_db () ->
  init_db ([node ()]).


init_db (Nodes) ->
  mnesia:create_schema (Nodes),
  mnesia:change_table_copy_type (schema, node(), disc_copies), % only for local node?
  mnesia:start (),
  ModelList = [ list_to_atom (M) || M <- boss_files:model_list (?APPNAME) ],
  ExistingTables = mnesia:system_info(tables),
  Tables = (ModelList ++ ['_ids_']) -- ExistingTables,
  create_model_tables (Nodes, Tables).

% create all the tables
create_model_tables (_, []) -> ok;
create_model_tables (Nodes, [Model | Models]) ->
  [create_model_table (Nodes, Model)] ++
   create_model_tables (Nodes, Models).

% specific tables (not generated from model)
create_model_table (Nodes, '_ids_') ->
  create_table (Nodes, '_ids_', [type, id]);

% tables generated from model
create_model_table (Nodes, Model) ->
  Record = boss_record_lib:dummy_record (Model),
  { Model, create_table (Nodes, Model, Record:attribute_names ()) }.

% single table creator wrapper
create_table (Nodes, Table, Attribs) ->
  mnesia:create_table (Table,
    [ { disc_copies, Nodes   },
      { attributes,  Attribs } ]).



% seeding

-define (MUSCLE_GROUPS, lists:sort(["Quardriceps", "Hamstrings", "Calves", 
                                    "Chest", "Back", "Shoulders", "Triceps", 
                                    "Biceps", "Forearms", "Trapezius", "Abs",
                                    "Groin"])).

-define (JOINTS, lists:sort(["Neck", "Intervertabral", "Shoulder", "Elbow", 
                               "Wrist", "Sacroiliac", "Hip", "Knee", "Ankle"])).

-define (MOTION, lists:sort(["Longitudinal Forward", "Longitudinal Backward",
                             "Transversal Open", "Transversal Closed",
                             "Twist Left", "Twist Right"])).

-define (ORIENTATIONS, lists:sort(["Head-Up", "Head-Down", "Belly-Up",
                                   "Belly-down", "Left-Side Down", 
                                   "Right-Side Down"])).
-define (LEVELS, lists:sort(["High", "Middle", "Low"])).

seed() ->
    seed_group(muscle_group, ?MUSCLE_GROUPS),
    seed_group(joint, ?JOINTS),
    seed_group(motion_type, ?MOTION),
    seed_group(orientation, ?ORIENTATIONS),
    seed_group(level, ?LEVELS),
    seed_group(range_of_motion, [J:name() ++ ": " ++ R:name() || 
                                    J <- boss_db:find(joint,[]),
                                    R <- boss_db:find(motion_type,[])]),
    load_asana().

kill_asana() ->
    Tables =  [asana, asana_mg, asana_range, enter_from, exit_to],
    [reset_group_id(T) || T <- Tables],
    [delete_all_existing(T, boss_db:find_first(T, [])) || T <- Tables].

seed_asana() ->
    Tables =  [asana, asana_mg, asana_range, enter_from, exit_to],
    [reset_group_id(T) || T <- Tables],
    [delete_all_existing(T, boss_db:find_first(T, [])) || T <- Tables],

    

    A = asana:new(id, "Firefly", "Tittibhasana", true, true, true),
    {ok, Asana} = A:save(),

    B = asana:new(id, "Chair", "Utkatasana", true, false, true),
    {ok, Basana} = B:save(),

    C = asana:new(id, "Triangle", "Trikonasana", false, true, false),
    {ok, Casana} = C:save(),

    E = asana:new(id, "Mountain", "Tadasana", false, false, false),
    E:save(),



    EF = enter_from:new(id, Basana:id(), Asana:id()),
    EF:save(),

    FG = enter_from:new(id, Casana:id(), Basana:id()),
    FG:save(),

    ET0 = exit_to:new(id, Asana:id(), Basana:id()),
    ET0:save(),

    ET1 = exit_to:new(id, Basana:id(), Casana:id()),
    ET1:save(),

    MgIds = [(boss_db:find_first(muscle_group, [{name, equals, MuscleGroup}])):id() ||
                MuscleGroup <- ["Hamstrings", "Chest", "Forearms", "Back", "Abs"]],
    Ranges = [(boss_db:find_first(range_of_motion, [{name, equals, Range}])):id() ||
                Range <- ["Hip: Transversal Open"]],


    [(asana_mg:new(id, Asana:id(), M)):save() || M <- MgIds],
    [(asana_range:new(id, Asana:id(), R)):save() || R <- Ranges].        



seed_group(Table, Data) ->
    lager:info("seeding ~p.", [Table]),
    delete_all_existing(Table, boss_db:find_first(Table, [])),
    reset_group_id(Table),
    load_data(Table, Data),
    lager:info("seeding ~p complete.", [Table]),
    ok.

delete_all_existing(_Table, undefined) ->
    ok;
delete_all_existing(Table, Item) ->
    boss_db:delete(Item:id()),
    delete_all_existing(Table, boss_db:find_first(Table, [])).

reset_group_id(Table) ->
    mnesia:dirty_write('_ids_', {'_ids_', Table, 0}).

load_data(Table, Data) ->
    lists:map(fun(Item) -> save_item(Table:new(id, Item)) end, Data).

save_item(Item) ->
    case Item:save() of
        {ok, _} ->
            lager:info("inserted ~p.", [Item]);
        {error, Message} ->
            lager:warning("failed to insert ~p with ~p.", 
                          [Item, Message])
    end.


load_asana() ->
    {ok, Terms} = file:consult("priv/asanas"),
    [process(X) || X <- lists:sort(Terms)].

process({Name, Sanskrit}) ->
    insert({Name, Sanskrit, false, false, false, false, "none"});

process({Name, Sanskrit, VinyasaType}) ->
    insert({Name, Sanskrit, true, false, false, false, VinyasaType});

process({Name, Sanskrit, Balance, Flexibility, Strength}) ->
    insert({Name, Sanskrit, false, Balance, Flexibility, Strength, "none"}).


insert({Name, Sanskrit, IsV, B, F, S,VType}) ->
    Name2 = string:strip(Name),
    Sanskrit2 = string:strip(Sanskrit),
    VinyasaType = string:strip(string:to_upper(VType)),
    PropList = [{sanskrit, Sanskrit2}, 
                {strength, S}, 
                {flexibility, F}, 
                {balance, B},
                {is_vinyasa, IsV},
                {v_type, VinyasaType}],

    case boss_db:find_first(asana,[{name, equals, Name2}]) of
        undefined ->
            A = boss_record:new(asana, [{name, Name2} | PropList]), 
            A:save();
        A ->
            (A:set(PropList)):save()
    end.

    
    
