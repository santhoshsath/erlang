%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to tcp_listener.


-module(tcp_listener).
-vsn("1.0.001").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).


%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor (tcp_server_sup) to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).
 
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
	listener,       % Listening socket
	acceptor,       % Asynchronous acceptor's internal reference
	module          % FSM handling module
}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% ====================================================================
start_listen({ok,Socket},State) ->
	%%Create first accepting process
	{ok,State#state{listener = Socket, acceptor = async_accept(Socket)}};
start_listen({error,Reason},_State) ->
	{stop, Reason}.

init([Port,Module]) ->
 	process_flag(trap_exit, true),
	Opts = [binary, {packet, raw}, {reuseaddr, true},{keepalive, true}, {backlog, 1000}, {active, false}],
	error_logger:info_msg("UMP Log server listening on port ~p\n", [Port]),
	%%error_logger:info_msg("Handler module is ~p\n", [Module]),
	start_listen(gen_tcp:listen(Port, Opts),#state{module = Module}).
 
%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% ====================================================================
handle_call(Request, From, State) ->
    {stop, {unknown_call, From, Request}, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%% Messages sent to this process
%% prim_inet:async_accept will send messages when a connection is accepted
%% ====================================================================
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener=ListSock, acceptor=Ref, module=Module} = State) ->
	try
		case set_sockopt(ListSock, CliSocket) of
			ok	-> ok;
			{error, Reason} -> exit({set_sockopt, Reason})
        end,
        %% New client connected - spawn a new process using the simple_one_for_one supervisor.
        {ok, Pid} = logger:start_client(),
		error_logger:info_msg("New client : ~p connected~n", [inet:peername(CliSocket)]),
		%% Make the new Pid owner of the Socket; All messages will be delivered to that Pid  
        ok = Module:set_socket(Pid, CliSocket),
        {noreply, State#state{acceptor=async_accept(ListSock)}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
		{stop, Why, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, #state{listener = Socket}) ->
	gen_tcp:close(Socket),
	error_logger:error_msg("Closed Listener ~p.\n", [Socket]),
	init:stop().


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

async_accept(Socket) ->
	%% Signal the network driver that we are ready to accept another connection
	case prim_inet:async_accept(Socket, -1) of 
		{ok, Ref} -> Ref;
		{error, Error} -> exit({async_accept, inet:format_error(Error)})
	end.

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok	-> ok;
			Error -> gen_tcp:close(CliSocket), Error
        end;
	Error ->
		gen_tcp:close(CliSocket), Error
    end.