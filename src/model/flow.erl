%%% ----------------------------------------------------------------------------
%%% FLOW is a sequence of asanas maintained as a string of comma separated
%%% asana ids.
%%% ----------------------------------------------------------------------------

-module(flow, [Id, 
               Name::string(), 
               Description::string(), 
               Asanas::string()]).
-compile(export_all).
