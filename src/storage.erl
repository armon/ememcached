-module(storage).
-compile(export_all).
-export([start/0]).
-include("entries.hrl").
-include("config.hrl").

% This module interfaces around the implementing backend,
% which could be ETS, DETS, or anything else.

% Initializes the supervision tree. This starts the backend
% as well as our own worker children.
% @spec start() -> void().
start() -> spawn(fun() -> start_link() end).

% Starts the connection handler in the current process
start_link() -> 
  case supervisor:start_link({local,?MODULE},?MODULE,[]) of
    {ok, Pid} ->
      create_workers(Pid, ?STORAGE_WORKERS), {ok, Pid};
    State -> io:format("Failed to start storage: ~p~n",[State]), State
  end.

% Setup the supervisor to create a network listener which is
% automatically restarted on failure
init([]) -> 
  BackendSpec = {storage_backend_impl, {?STORAGE_BACKEND, start_link, []}, 
                permanent, 10000, worker, [?STORAGE_BACKEND]},
  {ok, {{one_for_one, 3, 1}, [BackendSpec]}}.

% Creates our storage workers
create_workers(_Pid, 0) -> ok;
create_workers(Pid, WorkerNum) ->
    WorkerSpec = {list_to_atom(integer_to_list(WorkerNum)), 
                  {storage_worker, start_link, [WorkerNum-1]}, 
                  permanent, 1000, worker, [storage_worker]},
    {ok, _Child} = supervisor:start_child(Pid, WorkerSpec),
    create_workers(Pid, WorkerNum-1).


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

