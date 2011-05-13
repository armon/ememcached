-module(storage).
-compile(export_all).
-include("entries.hrl").
-include("config.hrl").

% This module interfaces around the implementing backend,
% which could be ETS, DETS, or anything else.

% Starts the backend (if necessary)
% @spec start() -> void().
start() -> apply(?STORAGE_BACKEND, start, []).

% Gets an item from the backend
% @spec get(binary()) -> entry() | notfound | expired
get(Key) -> 
  io:format("Called get ~p~n", [Key]), 

  % Lookup the key
  Result = apply(?STORAGE_BACKEND, get, [Key]),

  % Get the current time
  {Big,Small,_Micro} = erlang:now(),
  Now = Big*1000000+Small,

  % Check if the key is not found or expired
  case Result of
    notfound -> notfound;
    Result when Result#entry.expiration < Now -> expired;
    _ -> io:format("Result ~p~n", [Result]), Result
  end.


% Sets an item in the backend
% @spec set(entry()) -> stored
set(Entry) -> io:format("Called set ~p~n", [Entry#entry.key]), apply(?STORAGE_BACKEND, set, [Entry]).

