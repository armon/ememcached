-module(ascii_handler).
-compile(export_all).
-include("entries.hrl").
-include("constants.hrl").
-include("config.hrl").
-export([match_request/1]).

% Handles the details of the ASCII protocol

% Checks if we have enough input to potentially parse a request
% Returns impossible if the given input could never become a request
% @spec match_request(IoList) -> {true, Captured} | false | impossible
match_request(IoList) ->
    case re:run(IoList, "^(?:[^\s\r\n]+\s*)+(\r\n)?") of 
        {match, Captured} -> 
            case Captured of
                [Group,_] -> {true, Group};
                [_] -> false
            end;
        nomatch -> 
            case iolist_size(IoList) of
                0 -> false;
                _ -> impossible
            end
    end.

% Handles a request once match_request returns {true, Captured}
% Takes the client socket, the received data, and the opaque handle from match_request
% Returns an iolist() of the data which was not used in the handling of this request.
% @spec handle_request(socket(), iolist(), term()) -> iolist()
handle_request(Socket, Data, Captured) ->
    Bin = iolist_to_binary(Data),
    
    % Get the request line
    {_Start,Length} = Captured,
    ExcludeNewLine = Length-2,
    <<RequestLine:ExcludeNewLine/binary,_:2/binary,Rest/binary>> = Bin,
    io:format("Request: ~p Rest: ~p~n",[RequestLine,Rest]),
    case RequestLine of
        <<"get",Args/binary>> -> handle_get(Socket, Args, Rest);
        <<"gets",Args/binary>> -> handle_gets(Socket, Args, Rest);
        <<"set",Args/binary>> -> handle_set(Socket, Args, Rest);
        <<"add",Args/binary>> -> handle_add(Socket, Args, Rest);
        <<"replace",Args/binary>> -> handle_replace(Socket, Args, Rest);
        <<"append",Args/binary>> -> handle_append(Socket, Args, Rest);
        <<"prepend",Args/binary>> -> handle_prepend(Socket, Args, Rest);
        <<"cas",Args/binary>> -> handle_cas(Socket, Args, Rest);
        <<"delete",Args/binary>> -> handle_delete(Socket, Args, Rest);
        <<"incr",Args/binary>> -> handle_incr(Socket, Args, Rest);
        <<"decr",Args/binary>> -> handle_decr(Socket, Args, Rest);
        <<"stats",Args/binary>> -> handle_stats(Socket, Args, Rest);
        <<"flush_all",Args/binary>> -> handle_flush_all(Socket, Args, Rest);
        <<"version",Args/binary>> -> handle_version(Socket, Args, Rest);
        <<"quit",Args/binary>> -> handle_quit(Socket, Args, Rest);
        _ -> handle_unknown(Socket, RequestLine, Rest)
    end.

% Request Handlers:
% Takes the socket, arguments to the request, data after the request
% and returns any remaining unhandled data.

% Handles a get command
% @spec handle_get(socket(), binary(), binary()) -> iolist().
handle_get(_, _, _) -> true.

% Handles a gets command
% @spec handle_gets(socket(), binary(), binary()) -> iolist().
handle_gets(_, _, _) -> true.



% Handles a set command
% @spec handle_get(socket(), binary(), binary()) -> iolist().
handle_set(Socket, Args, Rest) ->
    case store_helper(parse_cmd(Args)) of
        {Key,Flags,_Exp,Byte,_Reply} ->
            % Read the data
            {<<Data:Byte/binary,"\r\n">>, AfterData} = get_data(Socket, Rest, Byte+2),

            % Set this through the storage layer
            storage:set(#entry{protocol = ascii,key=Key,
                value=Data,
                size=Byte,
                flags=Flags,
                expiration = infinity}),

            % Return the unread data
            AfterData;

        invalid -> handle_unknown(Socket, Args, Rest)
    end.

% Handles a add command
% @spec handle_set(socket(), binary(), binary()) -> iolist().
handle_add(_, _, _) -> true.

% Handles a replace command
% @spec handle_replace(socket(), binary(), binary()) -> iolist().
handle_replace(_, _, _) -> true.

% Handles a append command
% @spec handle_append(socket(), binary(), binary()) -> iolist().
handle_append(_, _, _) -> true.

% Handles a prepend command
% @spec handle_prepend(socket(), binary(), binary()) -> iolist().
handle_prepend(_, _, _) -> true.

% Handles a cas command
% @spec handle_cas(socket(), binary(), binary()) -> iolist().
handle_cas(_, _, _) -> true.

% Handles a delete command
% @spec handle_delete(socket(), binary(), binary()) -> iolist().
handle_delete(_, _, _) -> true.

% Handles a incr command
% @spec handle_incr(socket(), binary(), binary()) -> iolist().
handle_incr(_, _, _) -> true.

% Handles a decr command
% @spec handle_decr(socket(), binary(), binary()) -> iolist().
handle_decr(_, _, _) -> true.

% Handles a stats command
% @spec handle_stats(socket(), binary(), binary()) -> iolist().
handle_stats(_, _, _) -> true.

% Handles a flush_all command
% @spec handle_flush_all(socket(), binary(), binary()) -> iolist().
handle_flush_all(_, _, _) -> true.

% Handles a version command
% @spec handle_version(socket(), binary(), binary()) -> iolist().
handle_version(Socket, _, Rest) -> 
    Version = io_lib:format("VERSION ~s\r\n",[?VERSION]),
    gen_tcp:send(Socket, Version),
    Rest.

% Handles a quit command
% @spec handle_quit(socket(), binary(), binary()) -> iolist().
handle_quit(Socket, _, Rest) -> 
    gen_tcp:close(Socket),
    Rest.

% Handles an unknown command
% @spec handle_get(socket(), binary(), binary()) -> iolist().
handle_unknown(Socket, RequestLine, Rest) -> 
    io:format("Unknown request: ~p ~p~n", [RequestLine, Rest]),
    gen_tcp:send(Socket, io_lib:format(?ASCII_CLIENT_ERR, ["Invalid Request Line"])),
    Rest.

% Handles an internall error where we barf
% @spec handle_server_error(socket(), iolist()) -> iolist().
handler_server_error(Socket, Data, Rest) ->
    io:format("Internal error, data: ~p~n",[Data]),
    gen_tcp:send(Socket, io_lib:format(?ASCII_SERVER_ERR, ["Internal Error"])),
    Rest.


% Splits the command on whitespace, removes 0 length elements
parse_cmd(Args) -> [X || X <- re:split(Args, "\s+"), byte_size(X) > 0].

% Ensures the inputs to a store function are valid
% Converts ExpTime, and Byte to integer values
% Reply is either true or false
% @spec store_helper(list()) -> invalid | {Key,Flags,ExpTime,Byte,Reply}
store_helper([Key, Flags, ExpTime, Byte]) ->
    try {Key, 
        list_to_integer(binary_to_list(Flags),10),
        list_to_integer(binary_to_list(ExpTime), 10),
        list_to_integer(binary_to_list(Byte), 10),
        true}
    catch _ -> invalid end;

store_helper([Key, Flags, ExpTime, Byte, <<"noreply">>]) ->
    case store_helper([Key, Flags, ExpTime, Byte]) of
        invalid -> invalid;
        {K,F,E,B,true} -> {K,F,E,B,false}
    end;

store_helper(_) -> invalid.

% Reads the data for a set command
% Takes the socket, the data we have, and
% the bytes needed
% @spec get_data(socket(), iolist(), int()) -> {<<binary>>,<<binary>>}
get_data(Socket, Data, Bytes) ->
    DataLength = iolist_size(Data),
    if
        DataLength >= Bytes -> split_binary(iolist_to_binary(Data), Bytes); 
        true ->
            {data, More} = conn_handler:receive_loop(Socket, Data, ?ASCII_RECV_TIMEOUT_MILLI, Bytes),
            get_data(Socket, More, Bytes)
    end.

% Converts the expiration time to an actual unix time
% @spec expiration_to_time(integer()) -> infinity | integer()
expiration_to_time(0) -> infinity;
expiration_to_time(Exp) when Exp =< 3600*24*30 -> 
    {Big,Small,_Micro} = erlang:now(),
    Big*1000000+Small+Exp;
expiration_to_time(Exp) -> Exp.

