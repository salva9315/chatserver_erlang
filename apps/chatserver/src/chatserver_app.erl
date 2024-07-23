%% chat_server.erl
-module(chatserver_app).
-export([start/0, start/1, accept/2, loop/3, broadcast_message/3, client_manager/1]).

-record(state, {clients = []}).

%% Start chat server on the default port 8080.
start() ->
    start(8080).

%% Start chat server on the specified port.
start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Chat server started on port ~p~n", [Port]),
    ClientManagerPid = spawn(fun() -> client_manager(#state{}) end),
    accept(ListenSocket, ClientManagerPid).

%% Accept connections and spawn new process for handling each client.
accept(ListenSocket, ClientManagerPid) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Client connected.~n"),
    gen_tcp:send(Socket, <<"Enter your username: ">>),
    spawn(fun() -> loop(Socket, undefined, ClientManagerPid) end),
    accept(ListenSocket, ClientManagerPid).

%% Main loop for handling client communication.
%% First loop: Handle receving of username input before receiving any other message
loop(Socket, undefined, ClientManagerPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Username = binary:part(Data, 0, byte_size(Data) - 1),  % Remove newline character
            io:format("Username received: ~p~n", [Username]),
            gen_tcp:send(Socket, <<"Welcome, ", Username/binary, "!\n">>),
            ClientManagerPid ! {new_client, self(), Socket, Username},
            loop(Socket, Username, ClientManagerPid);
        {error, closed} ->
            io:format("Client disconnected during username input.~n"),
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            ok
    end;

%% Main loop for handling client communication.
%% Second loop: Handle messaging
loop(Socket, Username, ClientManagerPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Message received from ~p: ~p~n", [Username, Data]),
            ClientManagerPid ! {message, Username, Data},
            loop(Socket, Username, ClientManagerPid);
        {error, closed} ->
            io:format("Client ~p disconnected.~n", [Username]),
            ClientManagerPid ! {remove_client, self(), Username, Socket},
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            ok
    end.

%% Used for manaing list  of connected clients, in this way the client list is updated on each spawned process
%% TODO: could be a bottleneck and give concurrency errors? to be improved
client_manager(State) ->
    io:format("Managing clients. Current state: ~p~n", [State#state.clients]),
    receive
        {new_client, Pid, Socket, Username} ->
            NewState = State#state{clients = [{Pid, Socket, Username} | State#state.clients]},
            io:format("New client ~p connected. Total clients: ~p~n", [Username, length(NewState#state.clients)]),
            client_manager(NewState);
        {remove_client, Pid, Username, _} ->
            NewClients = lists:filter(fun({ClientPid, _, _}) -> ClientPid =/= Pid end, State#state.clients),
            io:format("Client ~p removed. Total clients: ~p~n", [Username, length(NewClients)]),
            broadcast_message(<<"Server">>, <<Username/binary, " has left the chat.\n">>, NewClients),
            client_manager(State#state{clients = NewClients});
        {message, From, Message} ->
            io:format("Broadcasting message from ~p: ~p~n", [From, Message]),
            broadcast_message(From, Message, State#state.clients),
            client_manager(State)
    end.

%% Broadcasts message to all connected clients.
broadcast_message(From, Message, Clients) ->
    lists:foreach(fun({_, Socket, _}) ->
        io:format("Sending message from ~p to ~p~n", [From, Socket]),
        gen_tcp:send(Socket, <<From/binary, ": ", Message/binary>>)
    end, Clients).