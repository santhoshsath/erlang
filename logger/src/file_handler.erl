%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to file_handler.


-module(file_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

init([]) ->
	timer:send_after(utils:ms_to_midnight(), reset_error_logger),
    {ok, #state{}}.

handle_call({open_file,Dir,FileName,FileCtr}, _From, State) ->
    Reply = open_file(Dir,FileName,FileCtr),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reset_error_logger, State) ->
	case error_logger:logfile(filename) of
		{error, no_log_file} -> ok;
		Filename -> 
			OldFilename = Filename ++ ".old",
			file:delete(OldFilename),
			error_logger:logfile(close),
			file:rename(Filename, OldFilename),
			error_logger:logfile({open,Filename})
	end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

open_file(Dir,FileName,FileCtr) ->
	F = mk_filename(Dir,FileName,FileCtr),
	case file:read_file_info(F) of 
		{error, enoent} -> %% Open and close the file, in raw mode only the process that opened the file can use it
			D = open_file(F), file:close(D), {F,FileCtr};
		{ok, _} -> open_file(Dir,FileName,FileCtr + 1)
	end.

open_file(FullFileName) ->
	{ok,Device} = file:open(FullFileName,[write,raw]),
	Device.

mk_filename(Dir,FileName,FileCtr) ->
	lists:flatten(io_lib:format("~s/~s-~s~~~w.log",[Dir,FileName,timestamp(),FileCtr])).

timestamp(Now) ->
    {{YY, MM, DD}, {Hour, _Min, _Sec}} = calendar:now_to_local_time(Now),
    io_lib:format("~4..0w-~2..0w-~2..0w-~2..0w",[YY, MM, DD, Hour]).
timestamp() ->
  timestamp(erlang:timestamp()).
