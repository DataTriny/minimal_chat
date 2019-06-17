%%% The supervisor in charge of all the socket acceptors.
-module(mc_clients_sup).
-behavior(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, Port} = application:get_env(port),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
	spawn_link(fun empty_listeners/0),
	{ok, {{simple_one_for_one, 60, 3600},
		[{mc_client,
		{mc_client, start_link, [ListenSocket]},
		temporary, 5000, worker, [mc_client]}
		]}}.

start_socket() ->
	supervisor:start_child(?MODULE, []).

%% Start with 10 listeners so that multiple incomming connections can be started at once.
empty_listeners() ->
	[start_socket() || _ <- lists:seq(1, 10)],
	ok.