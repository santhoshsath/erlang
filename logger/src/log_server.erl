%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to log_server.

%% http://stackoverflow.com/questions/1840717/achieving-code-swapping-in-erlangs-gen-server
%% http://stackoverflow.com/questions/15464606/erlang-kill-all-processes-running-in-background

-module(log_server).
-vsn("1.0.001").
-behaviour(gen_server).
-include_lib("kernel/include/inet.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,set_socket/2]).


start_link(LogsDir) ->
	gen_server:start_link(?MODULE, [LogsDir], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
	%% Instruct the new worker that it owns the socket.
	gen_tcp:controlling_process(Socket, Pid),
	gen_server:cast(Pid, {connected, Socket}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {socket,device,logsdir,filename,remote_host,remote_port,file_ctr=1,rotate=0}).

init([LogsDir]) ->
	process_flag(trap_exit, true),
	%%error_logger:info_msg("Spawned client : ~p with logs dir ~p~n", [self(),LogsDir]),
    {ok, #state{logsdir=LogsDir}}.

handle_call(Msg, _From, State) ->
	error_logger:info_msg("Received unhandled call message ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast({connected,Socket}, State) ->
	{RemoteAddr,RemotePort} = get_peer_info(inet:peername(Socket)),
	error_logger:info_msg("New client ~s:~B will be handled by pid ~p~n", [RemoteAddr,RemotePort,self()]),
	%%error_logger:info_msg("Socket option ~p~n", [prim_inet:getopts(Socket, [active])]),
	{Dir,FileName,Rotate,{Device,Ctr}} = open_log(Socket,State#state.logsdir,RemoteAddr,State#state.file_ctr),
    socket_opts(Socket),
    {	
	 	noreply, 
		State#state{
			socket=Socket,filename=FileName,device=Device,
			logsdir=Dir,file_ctr=Ctr,remote_host=RemoteAddr,
			remote_port=RemotePort, rotate=Rotate
		}
	};
handle_cast(Msg, State) ->
	error_logger:info_msg("Received unhandled cast message ~p~n", [Msg]),
    {noreply, State}.

handle_info({tcp,_Port,Msg}, State) ->
	%%error_logger:info_msg("Received tcp info message ~p~n", [Msg]),
	log(State,Msg),
	socket_active(State),
    {noreply, State};
handle_info({tcp_error,Port,Error}, State) ->
	error_logger:info_msg("Worker ~p on port ~p received TCP Error ~p~n", [self(),Port,Error]),
    {noreply, State};
handle_info({tcp_closed,_Port}, State) ->
    {stop, tcp_closed, State};
handle_info(reset, State) ->
	%%error_logger:info_msg("Received reset from timer~n"),
    {noreply, reset(State)};
handle_info(Info, State) ->
	error_logger:info_msg("Received unhandled info message ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
	error_logger:info_msg("worker ~p terminated with reason ~p~n", [self(),Reason]),
	file:close(State#state.device),
	socket_close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, reset(State)}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_peer_info({ok, {IP, Port}}) ->
	%%error_logger:info_msg("New client : ~p:~p will be handled by pid ~p~n", [IP,Port,self()]),
	case inet:gethostbyaddr(IP) of
		{ok,Host} -> {Host#hostent.h_name,Port};
		_ -> {mk_host_name(IP),Port} 
	end;
get_peer_info(_) ->
    {undefined,undefined}.

mk_host_name(Addr) when is_tuple(Addr) ->
	string:join(lists:map(fun(X) -> integer_to_list(X) end,tuple_to_list(Addr)),"_").

socket_active(#state{socket=Socket}) ->
	inet:setopts(Socket, [{active, once}]).

socket_opts(Socket) ->
	inet:setopts(Socket, [{active, once}, {packet, raw}, binary]).

socket_close(#state{socket=Socket})  ->
	gen_tcp:close(Socket).

reset(State) ->
	file:close(State#state.device),
	{Device,Ctr} = init_log(State#state.logsdir,State#state.filename,State#state.file_ctr,State#state.rotate),
	State#state{device=Device,file_ctr=Ctr}.

open_log(Socket,LogsDir,RemoteAddr,FileCtr) ->
	{ok,<<Len:8>>} = gen_tcp:recv(Socket,1),
	{ok,FileName} = gen_tcp:recv(Socket,Len),
	{ok,<<Rotate:8>>} = gen_tcp:recv(Socket,1),
	Dir = mk_dir(LogsDir,RemoteAddr),
	{Dir,FileName,Rotate,init_log(Dir,FileName,FileCtr,Rotate)}.

init_log(Dir,FileName,FileCtr,Rotate) ->
	{Device,Ctr,FullFileName} = open_file(Dir,FileName,FileCtr),
	error_logger:info_msg("Log file ~s opened for ~p~n", [FullFileName,self()]),
	reset_timer(Rotate),
	{Device,Ctr}.

open_file(Dir,FileName,FileCtr) ->
	{FullFileName, CurrentCtr} = gen_server:call(file_handler, {open_file,Dir,FileName,FileCtr}),
	{ok,Device} = file:open(FullFileName,[write,raw]),
	{Device,CurrentCtr,FullFileName}.

mk_dir(LogsDir,RemoteAddr) ->
    Dir = lists:flatten(io_lib:format("~s/~s",[LogsDir,RemoteAddr])),
	%%error_logger:info_msg("Creating dir ~s for host ~s~n", [Dir,RemoteAddr]),
	file:make_dir(Dir),
	Dir.

reset_timer(0) ->
	timer:send_after(utils:ms_to_midnight(), reset);

reset_timer(N) ->
	timer:send_after(utils:ms_to_next_hour() + ((N-1) * 3600000), reset).

log(#state{device=D},Msg) ->
	log(D,Msg);
log(D,Msg) ->
	case file:write(D, Msg) of 
		ok -> ok;
		Err -> error_logger:error_msg("Error writing message ~p to log file ~p: ~p~n", [Msg,D,Err])
	end.

%% code:add_path("../ebin").