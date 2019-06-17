-module(mc_server_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ParentSpec = {mc_server,
		{mc_server, start_link, []},
		permanent, 1000, worker, [mc_server]},
	ChildSpecs = [ParentSpec],
	ok = supervisor:check_childspecs(ChildSpecs),
	{ok, {{one_for_one, 60, 3600}, ChildSpecs}}.