-module(mc_server).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, []}.

handle_call({connect, Socket}, _From, Users) ->
	{reply, ok, [Socket|Users]};
handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({disconnect, Socket}, Users) ->
	broadcast(Socket, "Someone left the chat.", Users),
	{noreply, Users -- [Socket]};
handle_cast({say, Socket, Msg}, Users) ->
	broadcast(Socket, Msg, Users),
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
broadcast(Sender, Msg, Users) ->
	Receivers = [Socket || Socket <- Users, Socket =/= Sender],
	lists:foreach(fun(Socket) -> mc_client:send(Socket, Msg) end, Receivers).