%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to utils.


-module(utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ms_to_next_hour/0,ms_to_midnight/0]).


ms_to_next_hour() ->
	{_,Time} = calendar:local_time(),
	3600000 - next_hour(Time).

ms_to_midnight() ->
	{_,Time} = calendar:local_time(),
	S = calendar:time_to_seconds(Time),
	(86400 - S) * 1000.
 
%% ====================================================================
%% Internal functions
%% ====================================================================

next_hour({_,59,S}) -> S * 1000;
next_hour({_,M,S}) -> M * 60000 + S * 1000.
