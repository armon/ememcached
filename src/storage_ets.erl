-module(storage_ets).
-compile(export_all).
-include("entries.hrl").
-export([start/0]).

% Initializes the table store
start() ->
    % Create our ETS table
    ets:new(storage_ets_table, [set,public,named_table]).


