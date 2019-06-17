-module(mc_server).
-behavior(gen_server).

-record(state, {users, % A dict containing connected users.
	messages}). % A list of all messages sent so far.

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([disconnect/1, say/2, try_log_in/2]).

%%% Public API

%% Notify the server that a client has disconnected.
disconnect(Username) ->
	gen_server:cast(mc_server, {disconnected, Username}).

%% Notify the server that a client sent a message.
say(Username, Msg) ->
	gen_server:cast(mc_server, {say, Username, Msg}).

%% Try to register a client with his/her username, ensuring that it has not already been taken.
try_log_in(Username, Socket) ->
	gen_server:call(mc_server, {try_log_in, Username, Socket}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, #state{users = dict:new(), messages = []}}.

handle_call({try_log_in, Username, Socket}, _From, #state{users = Users, messages = Messages}) ->
	{Reply, NewUsers} = case dict:find(Username, Users) of
		{ok, _} -> {already_taken, Users};
		error ->
			gen_server:cast(self(), {connected, Username}),
			{{ok, lists:reverse(Messages)}, dict:store(Username, Socket, Users)}
		end,
	{reply, Reply, #state{users = NewUsers, messages = Messages}};
handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({connected, Username}, #state{users = Users, messages = Messages}) ->
	Msg = Username ++ " joined the chat.",
	broadcast(Username, Msg, Users),
	{noreply, #state{users = Users, messages = [Msg|Messages]}};
handle_cast({disconnected, ""}, S) ->
	{noreply, S};
handle_cast({disconnected, Username}, #state{users = Users, messages = Messages}) ->
	Msg = Username ++ " left the chat.",
	broadcast(Username, Msg, Users),
	{noreply, #state{users = dict:erase(Username, Users), messages = [Msg|Messages]}};
handle_cast({say, Username, Msg}, #state{users = Users, messages = Messages}) ->
	SentMsg = Username ++ ": " ++ Msg,
	broadcast(Username, SentMsg, Users),
	{noreply, #state{users = Users, messages = [SentMsg|Messages]}}.

handle_info(_E, S) ->
	{noreply, S}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(Reason, _State) ->
	io:format("Terminate reason: ~p~n", [Reason]).

%%% Helper functions

%% Sends a message to all connected clients, except the sender itself.
broadcast(SenderUsername, Msg, Users) ->
	Receivers = dict:fold(fun (Username, Socket, Acc) ->
		case Username of
			SenderUsername -> Acc;
			_ -> [Socket|Acc]
		end
		end, [], Users),
	lists:foreach(fun(Socket) -> mc_client:send(Socket, Msg) end, Receivers).