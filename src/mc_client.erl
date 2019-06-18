-module(mc_client).
-behavior(gen_server).

-record(state, {username = "",
	logged_in = false,
	socket}). % The remote socket.
 
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([ask_for_username/1, disconnect/1, send/2]).

%%% Public API

%% Starts the login process by sending the welcome message to the newly connected user.
ask_for_username(Pid) ->
	gen_server:cast(Pid, ask_for_username).

%% Disconnects an user.
disconnect(#state{username = Username, socket = Socket}) ->
	mc_server:disconnect(Username),
	gen_tcp:close(Socket),
	ok.

%% Sends a message to the client.
send(Socket, Msg) ->
	ok = gen_tcp:send(Socket, Msg ++ "\r\n"),
	ok = inet:setopts(Socket, [{active, once}]),
	ok.

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	process_flag(trap_exit, true),
	{ok, #state{socket = Socket}}.

handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(ask_for_username, S = #state{socket = Socket}) ->
	send(Socket, "Welcome! Please choose an username."),
	{noreply, S}.

handle_info({tcp, Socket, Str}, S = #state{logged_in = false}) ->
	Username = line(Str),
	NewState = case mc_server:try_log_in(Username, Socket) of
		{ok, PreviousMessages} ->
			lists:foreach(fun(Msg) -> send(Socket, Msg) end, PreviousMessages),
			send(Socket, "Hello " ++ Username ++ "!"),
			#state{username = Username, logged_in = true, socket = Socket};
		already_taken ->
			send(Socket, "This username is already used by someone else. Please pick another one."),
			S
		end,
	{noreply, NewState};
handle_info({tcp, Socket, Str}, S = #state{username = Username, logged_in = true}) ->
	Msg = line(Str),
	send(Socket, "\r"),
	mc_server:say(Username, Msg),
	{noreply, S};
handle_info({tcp_closed, _Socket}, S) ->
	{stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
	{stop, normal, S};
handle_info(E, S) ->
	io:format("Unexpected: ~p~n", [E]),
	{noreply, S}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, State) ->
	disconnect(State);
terminate(Reason, State) ->
	disconnect(State),
	io:format("Terminate reason: ~p~n", [Reason]).

%%% Helper functions

%% Remove unwanted trailing characters from a string.
line(Str) ->
	string:chomp(Str).