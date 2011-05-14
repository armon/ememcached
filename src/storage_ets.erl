-module(storage_ets).
-compile(export_all).
-include("entries.hrl").
-behaviour(gen_server).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


% Takes an entry, returns a tuple we can use
entry_to_tuple(Entry) -> {Entry#entry.key, Entry}.

% Called with an entry to set, no pre/post conditions
% @spec set(entry()) -> stored
set(Entry) -> ets:insert(storage_ets_table, entry_to_tuple(Entry)), stored.

% Called with a key to lookup, no conditions.
% @spec get(binary()) -> entry() | notfound | expired
get(Key) -> 
  case ets:lookup(storage_ets_table, Key) of
    [{Key, Entry}] -> Entry;
    [] -> notfound
  end.


%%%%% gen_server callbacks

% Our state object
-record(state, {ets_table}).

% Starts the server
% @spec start_link() -> {ok, Pid()} | ignore | {error, Error()}
start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Initializes this backend.
% Creates the ets table.
init([]) -> 
  Table = ets:new(storage_ets_table, [set,public,named_table, {write_concurrency,true}, {read_concurrency, true}]),
  {ok, #state{ets_table=Table}}.

% Handles an incoming request. These are not expected
handle_call(_Request, _From, State) -> {reply, ok, State}.

% Handles incoming casts
% These are also unexpected.
handle_cast(_Msg, State) -> {noreply, State}.

% Handles spontaneous info messages
% We don't expect any
handle_info(_Info, State) -> {noreply, State}.

% Called upon our termination
% Deletes the table, exits
terminate(_Reason, State) -> ets:delete(State#state.ets_table), ok.

% Handles code changes on the fly
code_change(_OldVsn, State, _Extra) -> {ok, State}.

