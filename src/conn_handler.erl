-module(conn_handler).
-include("constants.hrl").
-compile(export_all).
-behavior(supervisor).
-export([start/0,start_link/0,init/1]).

% This module is responsible for listening for incoming
% network connections, parsing the requests, and interfacing
% with the backend

% Starts the connection handler in a separate process
start() -> spawn(fun() -> start_link() end).

% Starts the connection handler in the current process
start_link() -> {ok, _Pid} = supervisor:start_link({local,?MODULE},?MODULE,[]).

% Setup the supervisor to create a network listener which is
% automatically restarted on failure
init([]) ->
    ListenerSpec = {net_listener, {?MODULE, start_listener, []}, permanent, 10000, worker, [?MODULE]},
    {ok, {{one_for_one, 3, 1}, [ListenerSpec]}}.


% Listens on a socket and waits for incoming clients
start_listener() ->
    {ok, Listen} = gen_tcp:listen(?DEFAULT_PORT, [binary,{reuseaddr,true},{active,false},{backlog,50},{packet,raw}]),
    error_logger:info_report("Listening for connections"),
    accept_loop(Listen).

% Loops accepting new clients
accept_loop(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} -> spawn_handler(Socket), 
                        error_logger:info_report(["Accepted new client."]),
                        accept_loop(Listen);

        {error, Err} -> error_logger:error_report(["Error accepting client.",Err]),
                        accept_loop(Listen)
    end.

% Spawns a new handler for an incoming request
spawn_handler(Socket) ->
    Pid = spawn(conn_handler, client_handler, []),
    gen_tcp:controlling_process(Socket, Pid),
    Pid ! {socket, Socket}.


% Handles a single client connection
client_handler() ->
    receive 
        {socket, Socket} -> 
            % Enable receiving data from the client
            inet:setopts(Socket, [{active,true}]),
            case receive_loop(Socket, [], infinity) of
                {data, IoList} ->
                    % Check the first byte to determine if
                    % we are using the ASCII or binary protocol
                    % If we see the magic request byte, assume binary
                    Data = iolist_to_binary(IoList),
                    case Data of 
                        <<?REQUEST_MAGIC,_>> -> binary_handler(Socket, Data);
                        _ -> ascii_handler(Socket, Data)
                    end
            end
    end.


% Receives data from a single client connection
% @spec receive_loop(socket(), iolist(), atom() | number()) -> {data, iolist()} | closed | error | timeout
receive_loop(Socket, Data, Timeout) ->
    receive
        {tcp, Socket, In} -> {data, [Data, In]};
        {tcp_closed, Socket} -> closed;
        {tcp_error, Socket, Reason} -> error_logger:error_report(["Error receiving data from client.", Reason]),
                                       error
    after 
        Timeout -> error_logger:error_report(["Timed out receiving data from client."]), 
                 timeout
    end.

% Like receive loop, but continues running until there
% is a certain amount of data available.
receive_loop(Socket, Data, Timeout, Min) ->
    DataLength = iolist_size(Data),
    if
        DataLength >= Min -> {data, Data};
        true -> 
            case receive_loop(Socket, Data, Timeout) of
                {data, More} -> receive_loop(Socket, More, Timeout, Min);
                Err -> Err
            end
    end.


% Handles a binary connection
% @spec binary_handler(Socket(), iolist()) -> void()
binary_handler(_Socket, _Data) -> true.

% Handles an ASCII connection
% @spec ascii_handler(Socket(), iolist()) -> void()
ascii_handler(Socket, Data) -> 
    Remaining = try 
        case ascii_handler:match_request(Data) of
            {true, Captured} ->
                ascii_handler:handle_request(Socket, Data, Captured);

            false ->
                {data, DataPlus} = receive_loop(Socket, Data, infinity),
                DataPlus;

            impossible ->
                ascii_handler:handle_unknown(Socket, Data, []), []
        end
    catch
         Class:Exception ->
            io:format("Caught exception! ~p ~p~n",[Class,Exception]),
            erlang:display(erlang:get_stacktrace()),
            ascii_handler:handler_server_error(Socket, Data, []),
            []
    end,

    % Recurse with the remaining data
    ascii_handler(Socket, Remaining).
       


