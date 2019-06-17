-module(mc_server).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, dict:new()}.

handle_call({try_log_in, Username, Socket}, _From, Users) ->
	{Reply, NewUsers} = case dict:find(Username, Users) of
		{ok, _} -> {already_taken, Users};
		error ->
			gen_server:cast(self(), {connected, Username}),
			{ok, dict:store(Username, Socket, Users)}
		end,
	{reply, Reply, NewUsers};
handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({connected, Username}, Users) ->
	broadcast(Username, Username ++ " joined the chat.", Users),
	{noreply, Users};
handle_cast({disconnected, ""}, Users) ->
	{noreply, Users};
handle_cast({disconnected, Username}, Users) ->
	broadcast(Username, Username ++ " left the chat.", Users),
	{noreply, dict:erase(Username, Users)};
handle_cast({say, Username, Msg}, Users) ->
	broadcast(Username, Username ++ ": " ++ Msg, Users),
	{noreply, Users}.

handle_info(_E, S) ->
	{noreply, S}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(Reason, _State) ->
	io:format("Terminate reason: ~p~n", [Reason]).

%% Sends a message to the client.
broadcast(SenderUsername, Msg, Users) ->
	Receivers = dict:fold(fun (Username, Socket, Acc) ->
		case Username of
			SenderUsername -> Acc;
			_ -> [Socket|Acc]
		end
		end, [], Users),
	lists:foreach(fun(Socket) -> mc_client:send(Socket, Msg) end, Receivers).