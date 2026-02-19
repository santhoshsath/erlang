{application, logger,
 [
  {description, "Log server"},
  {vsn, "1.0"},
  {id, "logger"},
  {modules,      [tcp_listener, tcp_client_sup, logger]},
  {registered,   [logger, tcp_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {logger, []}},
  {env,
        [
            {listener_port,9089},
			{logs_dir,"c:\\dev\\logs"}
        ]
	}
 ]
}.
