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
    % Match some number of non-spaces followed by a space,
    % finally ending in a new line
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
        <<"gets",Args/binary>> -> handle_gets(Socket, Args, Rest);
        <<"get",Args/binary>> -> handle_get(Socket, Args, Rest);
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

%%%%%%% Retrieval Commands

% Handles a get command
% @spec handle_get(socket(), binary(), binary()) -> iolist().
handle_get(Socket, Args, Rest) -> 
  % Get the individual keys
  Keys = parse_cmd(Args),

  % Handle one at a time
  handle_get_key(Socket, false, Keys),

  % Return the remaining bytes
  Rest.


% Handles a gets command
% @spec handle_gets(socket(), binary(), binary()) -> iolist().
handle_gets(Socket, Args, Rest) -> 
  % Get the individual keys
  Keys = parse_cmd(Args),

  % Handle one at a time, enable CAS output
  handle_get_key(Socket, true, Keys),

  % Return the remaining bytes
  Rest.

% Handles a GET for a key, optionally emits the CAS key
% @spec handle_get_key(socket(), boolean(), list()) -> void
handle_get_key(Socket, _UseCas, []) -> gen_tcp:send(Socket,?ASCII_END), void;
handle_get_key(Socket, UseCas, [Key|Remain]) ->
  % Get the entry
  case storage:get(Key) of
    notfound -> pass;
    expired -> pass;
    Entry ->
      % Generate the response line
      ResponseLine = if
        UseCas -> io_lib:format(?ASCII_GET_CAS, [Entry#entry.key, Entry#entry.flags, Entry#entry.size, Entry#entry.version]);
        true -> io_lib:format(?ASCII_GET_NO_CAS, [Entry#entry.key, Entry#entry.flags, Entry#entry.size])
      end,

      % Send the response line and the data
      gen_tcp:send(Socket, ResponseLine),
      gen_tcp:send(Socket, [Entry#entry.value,"\r\n"])
  end,

  % Recurse
  handle_get_key(Socket, UseCas, Remain).

%%%%% Store update commands

% Implements a generic storage command
% We parse the request line for the components
% and then trigger the appropriate backend function
% @spec generic_stor(Socket(), binary(), binary(), atom) -> binary()
generic_store(Socket, Args, Rest, Func) ->
    case store_helper(parse_cmd(Args)) of
        {Key,Flags,Exp,Byte,Cas,Reply} ->
            % Read the data
            {<<Data:Byte/binary,"\r\n">>, AfterData} = get_data(Socket, Rest, Byte+2),

            % Set this through the storage layer
            Entry = #entry{protocol = ascii,key=Key,
                value=Data,
                size=Byte,
                flags=Flags,
                version=Cas,
                expiration = expiration_to_time(Exp)},

            % Call our application function, provide the entry
            Result = apply(storage, Func, [Entry]),

            % Respond to the client unless "noreply" was sent
            if 
              Reply -> 
                case Result of
                  stored -> gen_tcp:send(Socket, ?ASCII_STORED);
                  exists -> gen_tcp:send(Socket, ?ASCII_NOT_STORED);
                  modified -> gen_tcp:send(Socket, ?ASCII_MODIFIED);
                  notexist when Func == cas -> gen_tcp:send(Socket, ?ASCII_NOT_FOUND);
                  notexist -> gen_tcp:send(Socket, ?ASCII_NOT_STORED)
                end
            end,

            % Return the unread data
            AfterData;

        invalid -> handle_unknown(Socket, Args, Rest)
    end.

% Handles a set command
% @spec handle_get(socket(), binary(), binary()) -> iolist().
handle_set(Socket, Args, Rest) -> generic_store(Socket, Args, Rest, set).

% Handles a add command
% @spec handle_set(socket(), binary(), binary()) -> iolist().
handle_add(Socket, Args, Rest) -> generic_store(Socket, Args, Rest, add).

% Handles a replace command
% @spec handle_replace(socket(), binary(), binary()) -> iolist().
handle_replace(Socket, Args, Rest) -> generic_store(Socket, Args, Rest, replace).

% Handles a append command
% @spec handle_append(socket(), binary(), binary()) -> iolist().
handle_append(Socket, Args, Rest) -> generic_store(Socket, Args, Rest, append).

% Handles a prepend command
% @spec handle_prepend(socket(), binary(), binary()) -> iolist().
handle_prepend(Socket, Args, Rest) -> generic_store(Socket, Args, Rest, prepend).

% Handles a cas command
% @spec handle_cas(socket(), binary(), binary()) -> iolist().
handle_cas(Socket, Args, Rest) -> generic_store(Socket, Args, Rest, cas).

%%%%% Handle Delete commands

% Handles a delete command
% @spec handle_delete(socket(), binary(), binary()) -> iolist().
handle_delete(Socket, Args, Rest) ->
  case del_helper(parse_cmd(Args)) of
    {Key, Exp, Reply} ->
      % Create a modification
      Mod = #modification{key=Key,value=expiration_to_time(Exp),operation=delete},

      % Apply the delete
      Result = apply(storage, delete, [Mod]),

      % Respond to the client, unless "noreply"
      if 
        Reply ->
          case Result of
            deleted -> gen_tcp:send(Socket, ?ASCII_DELETED);
            notexit -> gen_tcp:send(Socket, ?ASCII_NOT_FOUND)
          end
      end,

      % Return the unread data
      Rest;

    invalid -> handle_unknown(Socket, Args, Rest)
  end.

% Handles a flush_all command
% @spec handle_flush_all(socket(), binary(), binary()) -> iolist().
handle_flush_all(_, _, _) -> true.

%%%%%% Handle Modification commands

generic_mod(Socket, Args, Rest, Operation) ->
  case mod_helper(parse_cmd(Args)) of
    {Key,Value,Reply} ->
      % Create a modificatoin, send to the storage layer
      Mod = #modification{key=Key,value=Value,operation=Operation},

      % Call our application function, provide the entry
      Result = apply(storage, modify, [Mod]),

      % Respond to the client unless "noreply" was sent
      if 
        Reply -> 
          case Result of
            {updated, NewValue} -> gen_tcp:send(Socket, io_lib:format(?ASCII_UPDATED, [NewValue]));
            notexist -> gen_tcp:send(Socket, ?ASCII_NOT_FOUND);
            notnum -> gen_tcp:send(Socket, io_lib:format(?ASCII_CLIENT_ERR, ["cannot increment or decrement non-numeric value"]))
          end
      end,

      % Return the unread data
      Rest;

    invalid -> handle_unknown(Socket, Args, Rest)
  end.


% Handles a incr command
% @spec handle_incr(socket(), binary(), binary()) -> iolist().
handle_incr(Socket, Args, Rest) -> generic_mod(Socket, Args, Rest, incr).

% Handles a decr command
% @spec handle_decr(socket(), binary(), binary()) -> iolist().
handle_decr(Socket, Args, Rest) -> generic_mod(Socket, Args, Rest, decr).


%%%%%%%% Special commands

% Handles a stats command
% @spec handle_stats(socket(), binary(), binary()) -> iolist().
handle_stats(_, _, _) -> true.


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


%%%%%%%%%%%%%%%%%%%%
% Utility Helpers  %
%%%%%%%%%%%%%%%%%%%% 

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
        undefined, % No CAS value provided
        true}
    catch _ -> invalid end;

store_helper([Key, Flags, ExpTime, Byte, <<"noreply">>]) ->
    case store_helper([Key, Flags, ExpTime, Byte]) of
        invalid -> invalid;
        {K,F,E,B,C,true} -> {K,F,E,B,C,false}
    end;

store_helper([Key, Flags, ExpTime, Byte, Cas]) ->
    try {Key, 
        list_to_integer(binary_to_list(Flags),10),
        list_to_integer(binary_to_list(ExpTime), 10),
        list_to_integer(binary_to_list(Byte), 10),
        list_to_integer(binary_to_list(Cas), 10),
        true}
    catch _ -> invalid end;

store_helper([Key, Flags, ExpTime, Byte, Cas, <<"noreply">>]) ->
    case store_helper([Key, Flags, ExpTime, Byte, Cas]) of
        invalid -> invalid;
        {K,F,E,B,C,true} -> {K,F,E,B,C,false}
    end;

store_helper(_) -> invalid.


% Ensures the inputs to a modification function (incr / decr)
% are valid. Converts the value to integer value.
% Reply is either true or false
% @spec mod_helper([binary(), binary()]) -> invalid | {Key, Value, Reply}
mod_helper([Key, Value]) ->
    try {Key, 
        list_to_integer(binary_to_list(Value), 10),
        true}
    catch _ -> invalid end;

mod_helper([Key, Value, <<"noreply">>]) ->
    case mod_helper([Key, Value]) of
        invalid -> invalid;
        {K,V,true} -> {K,V,false}
    end.

% Ensures the inputs to a delete function are valid.
% Converts the time to an integer or now if not provided.
% Reply is either true or false
% @spec del_helper([binary()..]) -> invalid | {Key, Time, Reply}
del_helper([Key]) -> {Key, now, true};
del_helper([Key, <<"noreply">>]) -> {Key, now, false};

del_helper([Key, Time]) ->
  try {Key,
      list_to_integer(binary_to_list(Time), 10),
      true}
  catch _ -> invalid end;

del_helper([Key, Time, <<"noreply">>]) ->
  case del_helper([Key, Time]) of 
      invalid -> invalid;
      {K,E,true} -> {K,E,false}
  end;

del_helper(_) -> invalid.


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
% @spec expiration_to_time(integer()) -> infinity | now | integer()
expiration_to_time(0) -> infinity;
expiration_to_time(now) -> now;
expiration_to_time(Exp) when Exp =< 3600*24*30 -> 
    {Big,Small,_Micro} = erlang:now(),
    Big*1000000+Small+Exp;
expiration_to_time(Exp) -> Exp.

