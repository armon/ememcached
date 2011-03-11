-module(storage_ets).
-compile(export_all).
-include("entries.hrl").
-export([start/0]).

% Initializes the table store
start() ->
    % Create our ETS table, use a new thread so that the table will continue to exist.
    spawn(fun() -> 
        ets:new(storage_ets_table, [set,public,named_table, {write_concurrency,true}, {read_concurrency, true}]),
        receive _ -> void end
    end).

% Takes an entry, returns a tuple we can use
entry_to_tuple(Entry) -> {Entry#entry.key, Entry}.

% Called with an entry to set, no pre/post conditions
% @spec set(entry()) -> stored
set(Entry) -> ets:insert(storage_ets_table, entry_to_tuple(Entry)), stored.




