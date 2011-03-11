-module(ememcached).
-compile(export_all).
-export([start/0]).

% This is the main entry point for the system and
% boostraps the rest of the system.

start() ->
  % Start the storage backend
  storage:start(),

  % Start the connection handler
  conn_handler:start().


