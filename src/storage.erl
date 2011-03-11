-module(storage).
-compile(export_all).
-include("entries.hrl").
-include("config.hrl").

% This module interfaces around the implementing backend,
% which could be ETS, DETS, or anything else.

% Starts the backend (if necessary)
% @spec start() -> void().
start() -> apply(?STORAGE_BACKEND, start, []).

% Sets an item in the backend
% @spec set(entry()) -> stored
set(Entry) -> io:format("Called set ~p~n", [Entry#entry.key]), apply(?STORAGE_BACKEND, set, [Entry]).

