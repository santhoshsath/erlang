%% @author Santhosh Sathiyaselan
%% @doc @todo Add description to tcp_client_sup.


-module(tcp_client_sup).
-vsn("1.0.001").
-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([Module,LogsDir]) ->
	{ok,
        {	{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, %% Superviosr flags
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[LogsDir]},           % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary, A temporary child process is never restarted
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [Module]                                 % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
%% ====================================================================
%% Internal functions
%% ====================================================================


