%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to loc_share.


-module(test).
-vsn("1.0.001").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,stop/0,status/0,test/0]).

start() ->
	%%c:cd("/Dev/Ump/Erl/logger/src"),
	case init:get_argument(logfile) of 
		{ok,[[File]]} -> error_logger:logfile({open,File});
		_ -> error_logger:tty(true)
	end,
	error_logger:info_msg("STARTING ~p\n", [self()]),
	application:start(logger).

stop() ->
	error_logger:info_msg("STOPPED ~p\n", [self()]),
	application:stop(logger).

status() ->
	application:which_applications().

test() ->
	%X = "abc",
    %FName = lists:flatten(io_lib:format("~p/logger.log.~s",[X,X])),
	X = string:join(lists:map(fun(X) -> integer_to_list(X) end,tuple_to_list({128,209,307,48})),"_"),
	P = 80,
	Y = lists:flatten(io_lib:format("~s-~B", [X,P])),
	io:format("Test : ~p~nt", [Y]).
