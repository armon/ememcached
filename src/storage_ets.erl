-module(storage_ets).
-compile(export_all).
-include("entries.hrl").
-include("config.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([start_link/0,init/1]).

% Takes an entry, returns a tuple we can use
entry_to_tuple(Entry) -> {Entry#entry.key, Entry}.

% Called with an entry to set, no pre/post conditions
% Must increment or set a new version
% @spec set(entry()) -> stored
set(Entry) -> 
  Versioned = Entry#entry{version = random:uniform(1 bsl 64)},
  ets:insert(storage_ets_table, entry_to_tuple(Versioned)), stored.

% Called with a key to lookup, no conditions.
% @spec get(binary()) -> entry() | notfound | expired
get(Key) -> 
  case ets:lookup(storage_ets_table, Key) of
    [{Key, Entry}] -> Entry;
    [] -> notfound
  end.

% Called with a key to delete
% @spec delete(binary()) -> true
delete(Key) -> ets:delete(storage_ets_table, Key).

% Called to flush the entire backend
% @spec flush() -> true
flush() -> ets:delete_all_objects(storage_ets_table).

% Starts the server
% @spec start_link() -> {ok, Pid()} | ignore | {error, Error()}
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Initializes this backend.
% Creates the ets table.
init([]) -> 
  EtsTable = {storage_ets_table, {?MODULE, start_ets, []}, 
                permanent, 3000, worker, [?MODULE]},
  Reaper = {storage_ets_reaper, {?MODULE, start_reaper, []}, 
                permanent, 30000, worker, [?MODULE]},
  {ok, {{one_for_one, 3, 60}, [EtsTable,Reaper]}}.

% Starts a new ETS table, waits for any EXIT calls
% and properly closes the ETS table.
start_ets() ->
    {ok, spawn_link(fun() ->
      io:format("ETS table started~n"),
      process_flag(trap_exit, true),
      Table = ets:new(storage_ets_table, [set,public,named_table, {write_concurrency,true}, {read_concurrency, true}]),
      receive 
          {'EXIT', _From, Reason} -> ets:delete(Table), exit(Reason)
      end
    end)}.

% Starts a new reaper process which scans for any
% expired keys on an interval
start_reaper() -> {ok, spawn_link(?MODULE, reaper, [])}.

% Periodically scans for expired keys, deletes them.
reaper() ->
    timer:sleep(?ETS_REAP_INTERVAL_MILLI),
    scan_keys(),
    reaper().

scan_keys() ->
    % Get the current time
    {Big,Small,_Micro} = erlang:now(),
    Now = Big*1000000+Small,
        
    % Generate a match spec to find expired keys
    MatchSpec = ets:fun2ms(fun({_Key, #entry{expiration=E}}) when E =< Now -> true end),

    % Reap the table
    NumReaped = ets:select_delete(storage_ets_table, MatchSpec),
    io:format("Reaped ~p entries!~n", [NumReaped]).


