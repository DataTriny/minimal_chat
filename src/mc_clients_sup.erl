%%% The supervisor in charge of all the socket acceptors.
-module(mc_clients_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, Port} = application:get_env(port),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
	spawn_link(fun() -> listen(ListenSocket) end),
	{ok, {{simple_one_for_one, 60, 3600},
		[{mc_client,
		{mc_client, start_link, []},
		temporary, 5000, worker, [mc_client]}
		]}}.

listen(ListenSocket) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	{ok, ChildPid} = supervisor:start_child(?MODULE, [AcceptSocket]),
	ok = gen_tcp:controlling_process(AcceptSocket, ChildPid),
	mc_client:ask_for_username(ChildPid),
	listen(ListenSocket).