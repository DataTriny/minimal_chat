-module(mc_client).
-behavior(gen_server).

-record(state, {socket}). % The current socket (either the listening socket or the remote one).
 
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([send/2]).

start_link(ListenSocket) ->
	gen_server:start_link(?MODULE, ListenSocket, []).

init(ListenSocket) ->
	process_flag(trap_exit, true),
	gen_server:cast(self(), accept),
	{ok, #state{socket = ListenSocket}}.

handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(accept, S = #state{socket = ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	mc_clients_sup:start_socket(),
	gen_server:call(mc_server, {connect, AcceptSocket}),
	{noreply, S#state{socket = AcceptSocket}}.

handle_info({tcp, Socket, Str}, S = #state{}) ->
	Msg = line(Str),
	refresh_socket(Socket),
	gen_server:cast(mc_server, {say, Socket, Msg}),
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

terminate(normal, _S = #state{socket = Socket}) ->
	gen_server:cast(mc_server, {disconnect, Socket}),
	gen_tcp:close(Socket),
	ok;
terminate(Reason, _S = #state{socket = Socket}) ->
	gen_server:cast(mc_server, {disconnect, Socket}),
	gen_tcp:close(Socket),
	io:format("Terminate reason: ~p~n", [Reason]).

%% Utility function to deal with Telnet input.
line(Str) ->
	hd(string:tokens(Str, "\r\n")).

refresh_socket(Socket) ->
	ok = inet:setopts(Socket, [{active, once}]).

%% Sends a message to the client.
send(Socket, Msg) ->
	ok = gen_tcp:send(Socket, Msg ++ "\r\n"),
	refresh_socket(Socket),
	ok.