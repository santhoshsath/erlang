%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to chat.

%% Start in docker
%% https://blog.scottlogic.com/2016/01/25/playing-with-docker-compose-and-erlang.html
%% erl -pa ebin conf -logfile /dev/logs/log-server.log -run logger start_server
%% STOP a running deamon server 
%% One way to achieve this is to start another erlang console, attach from it to first one and do all necessary things to terminate it properly.
%% 
%%     You need to know name of the target node. From your example node is started without any name, you can give it by adding flag -name -sname like this: erl -sname node_1 -s system start -detached
%% 
%%     Start another node with different name: erl -sname node_2
%% 
%%     Press ^G (control G) on the terminal with node_2
%% 
%%     Press r and type name of the first node: node_1@localhost (or whatever name it have)
%% 
%%     Press c
%% 
%% 
%% Eshell V5.10.1  (abort with ^G)
%% 
%% (node_2@localhost)1>
%% 
%% User switch command
%% 
%%  --> r 'node_1@localhost'
%% 
%%  --> c
%% 
%% Eshell V5.10.1  (abort with ^G)
%% 
%% (node_1@localhost)1>
%% 
%% You shell see new prompt with name of the first node. Now all your commands will be executed on the first node. To terminate first node you could type erlang:halt().
%% Java NetAppender in C:\Dev\sandbox\CustomLogger

%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles
%% http://erlangcentral.org/frame/?href=http%3A%2F%2Fwww.sics.se%2F~joe%2Ftutorials%2Frobust_server%2Frobust_server.html#.Va_rt7V2aM8
%% Main supervisor
%% Spawns a tcp listener process and a tcp client supervisor process using one for one strategy
%%
%% To Run in Docker
%% https://www.digitalocean.com/community/tutorials/how-to-share-data-between-the-docker-container-and-the-host
%%
-module(logger).
-vsn("1.0.001").
-behaviour(application).
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEFAULT_PORT,9089).
-define(LOGS_DIR, "/dev/logs").
-define(CLIENT_SUPERVISOR_ID,tcp_client_sup).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_client/0,start_server/0,stop_server/0]).

start_client() ->
	supervisor:start_child(?CLIENT_SUPERVISOR_ID, []).

start_server() ->
	case init:get_argument(logfile) of 
		{ok,[[File]]} -> 
			error_logger:info_msg("Loggging to ~s\n", [File]),
			error_logger:logfile({open,File});
		_ -> 
			error_logger:tty(true)
	end,
	error_logger:info_msg("STARTING ~p\n", [self()]),
	application:start(logger).

stop_server() ->
	error_logger:info_msg("STOPPED ~p\n", [self()]),
	application:stop(logger).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	inets:start(),
	timer:start(),
	ListenerPort	= get_app_env(listener_port,?DEFAULT_PORT),
	LogsDir			= get_app_env(logs_dir,?LOGS_DIR),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenerPort, log_server,LogsDir]).
	

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
	X=fun() ->
    	error_logger:error_msg("Starting Inets stop\n", []),
		inets:stop(),
    	error_logger:error_msg("Finished Inets stop\n", [])
	end,
	spawn(X),
    ok.

%% Supervisor call back for tcp listener
%% http://www.erlang.org/doc/man/supervisor.html
%% init([ListenerPort, ws_chat])
init([ListenerPort, Module,LogsDir]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
		 	%% The child processes this supervior spawns/monitors
            [
              % File Handler
              {	file_handler_sup,                        % Id       = internal id
                  {file_handler,start_link,[]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [file_handler]                           % Modules  = [Module] | dynamic
              },
              % TCP Listener
              {	tcp_server_sup,                        % Id       = internal id
                  {tcp_listener,start_link,[ListenerPort,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [tcp_listener]                           % Modules  = [Module] | dynamic
              },
              %% Client instance supervisor
			  %% Any children this supervisor supervises will have to started
			  %% manually using supervisor:start_child
              {   ?CLIENT_SUPERVISOR_ID,
                  {supervisor,start_link,[{local, ?CLIENT_SUPERVISOR_ID}, ?CLIENT_SUPERVISOR_ID, [Module,LogsDir]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

 get_app_env(Opt,Def) ->
	case application:get_env(?MODULE, Opt) of 
		{ok, Val} -> Val;
		_ -> case init:get_argument(Opt) of 
				[[Val | _ ]] -> Val;
				_ -> Def
			end
	end.
