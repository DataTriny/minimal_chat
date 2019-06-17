%%% Starting the Minimal chat application.
%%% The port is defined in the app's env.
-module(minimal_chat).
-behavior(application).

-export([start/2, stop/1]).

start(normal, []) ->
	{ok, _} = mc_server_sup:start_link(),
	{ok, Pid} = mc_clients_sup:start_link(),
	{ok, Pid}.

stop(_) -> ok.