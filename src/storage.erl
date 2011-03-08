-module(storage).
-compile(export_all).
-define(STORAGE_BACKEND, storage_ets).
-include("entries.hrl").

% This module interfaces around the implementing backend,
% which could be ETS, DETS, or anything else.

% Starts the backend (if necessary)
% @spec start() -> void().
start() -> apply(?STORAGE_BACKEND, start, 0).

