%% chat_client.erl
-module(chatclient_app).
-export([start/0, start/2, handle_initial_message/1, set_username/1, send_loop/1, recv_loop/1, send_messages/1]).

%% Starts the chat client and connects to the default server (localhost:8080).
start() ->
    start("localhost", 8080).

%% Starts the chat client and connects to the specified server and port.
start(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    io:format("Connected to ~p:~p~n", [Host, Port]),
    handle_initial_message(Socket).

%% Handles the initial message from the server.
handle_initial_message(Socket) ->
    io:format("Waiting for initial message from server...~n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Initial message received: ~s~n", [Data]),
            set_username(Socket),
            spawn(fun() -> recv_loop(Socket) end),  % Start receiving loop
            send_loop(Socket);  % Start sending loop
        {error, closed} ->
            io:format("Connection closed.~n")
    end.

%% Prompts the user to enter a username and sends it to the server.
set_username(Socket) ->
    Username = io:get_line("Enter your username: "),
    io:format("Sending username: ~s", [Username]),
    gen_tcp:send(Socket, erlang:list_to_binary(Username ++ "\n")).

%% Loop for receiving messages from the server.
recv_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received: ~s~n", [Data]),
            recv_loop(Socket);
        {error, closed} ->
            io:format("Connection closed.~n")
    end.

%% Loop for sending messages to the server.
send_loop(Socket) ->
    io:format("Enter messages below:~n"),
    send_messages(Socket).

%% Continuously prompts the user for messages and sends them to the server.
send_messages(Socket) ->
    Msg = io:get_line(""),
    io:format("Sending message: ~s", [Msg]),
    gen_tcp:send(Socket, erlang:list_to_binary(Msg ++ "\n")),
    send_messages(Socket).