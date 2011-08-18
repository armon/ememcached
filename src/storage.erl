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
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).

% Setup the supervisor to create a network listener which is
% automatically restarted on failure
init([]) -> 
  BackendSpec = {storage_backend_impl, {?STORAGE_BACKEND, start_link, []}, 
                permanent, 10000, supervisor, [?STORAGE_BACKEND]},
  WorkerSpec = create_workers(?STORAGE_WORKERS, []),
  {ok, {{one_for_one, 3, 1}, WorkerSpec++[BackendSpec]}}.

% Creates our storage workers
create_workers(0, Workers) -> Workers;
create_workers(WorkerNum, Workers) ->
    WorkerSpec = {list_to_atom("worker_" ++ integer_to_list(WorkerNum)), 
                  {storage_worker, start_link, [WorkerNum-1]}, 
                  permanent, 1000, worker, [storage_worker]},
    create_workers(WorkerNum-1,[WorkerSpec|Workers]).

% Gets an item from the backend. Always hit the backend
% directly. There are possible races, involving a pending
% write, but our semantics allow for this.
% @spec get(binary()) -> entry() | notfound | expired | deleted
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
    Result when Result#entry.value =:= deleted -> deleted;
    Result when Result#entry.expiration < Now -> expired;
    _ -> io:format("Result ~p~n", [Result]), Result
  end.

%%%%% Update operations
% Since these operations have potential race conditions, we
% need to serialize them through our worker pool.

% Sets an item in the backend
% @spec set(entry()) -> stored
set(Entry) -> 
  Worker = get_worker(Entry#entry.key),
  io:format("Called set ~p ~p~n", [Entry#entry.key, Worker]),
  gen_server:call(Worker, {set, Entry}).


% Adds an item in the backend, if it does not exist.
% @spec add(Entry()) -> stored | exists | deleted
add(Entry) ->
  Worker = get_worker(Entry#entry.key),
  io:format("Called add ~p ~p ~n", [Entry#entry.key, Worker]),
  gen_server:call(Worker, {add, Entry}).


% Replaces an item in the backend, if it exist.
% @spec add(Entry()) -> stored | notexist | deleted
replace(Entry) ->
  Worker = get_worker(Entry#entry.key),
  io:format("Called replace ~p ~p ~n", [Entry#entry.key, Worker]),
  gen_server:call(Worker, {replace, Entry}).


% Appends data to an item in the backend, if it exist.
% @spec add(Entry()) -> stored | notexist | deleted
append(Entry) ->
  Worker = get_worker(Entry#entry.key),
  io:format("Called append ~p ~p ~n", [Entry#entry.key, Worker]),
  gen_server:call(Worker, {append, Entry}).


% Prepends data to an item in the backend, if it exist.
% @spec add(Entry()) -> stored | notexist | deleted
prepend(Entry) ->
  Worker = get_worker(Entry#entry.key),
  io:format("Called prepend ~p ~p ~n", [Entry#entry.key, Worker]),
  gen_server:call(Worker, {prepend, Entry}).


% Replaces an item in the backend, if it exist and the version matches.
% This is basically a "check and set" operation.
% @spec add(Entry()) -> stored | notexist | modified | deleted
cas(Entry) ->
  Worker = get_worker(Entry#entry.key),
  io:format("Called cas ~p ~p ~n", [Entry#entry.key, Worker]),
  gen_server:call(Worker, {cas, Entry}).


% Increments or decrements the value of an item in the backend if it exists
% @spec incr(Modification()) -> {updated, NewValue} | notexist | notnum
modify(Modification) ->
  Worker = get_worker(Modification#modification.key),
  io:format("Called modify ~p ~p ~n", [Modification#modification.key, Worker]),
  gen_server:call(Worker, {mod, Modification}).

% Deletes a given item in the bacakend if it exists
% @spec delete(Modification()) -> deleted | notexist
delete(Modification) ->
  Worker = get_worker(Modification#modification.key),
  io:format("Called delete ~p ~p ~n", [Modification#modification.key, Worker]),
  gen_server:call(Worker, {delete, Modification}).

% Flushes the storage backend
% @spec flush(Modification()) -> deleted
flush(Modification) ->
    case Modification#modification.value of
        now -> apply(?STORAGE_BACKEND, flush, []);
        Time ->
          spawn(fun() ->
              {Big,Small,_Micro} = erlang:now(),
              Now = Big*1000000+Small,
              Delay = max(Time - Now,0),
              io:format("Delay ~p ~n", [Delay]),
              timer:sleep(Delay*1000), % Delay execution
              io:format("Wake~n"),
              apply(?STORAGE_BACKEND, flush, [])
          end)
    end,
    deleted.


% Returns the worker responsible for handling a given key
% @spec get_worker(Entry()) -> {global, Name}
get_worker(Key) ->
  % Hash the key, into one of the buckets for our worker
  HashValue = erlang:phash2(Key, ?STORAGE_WORKERS),

  % Get the workers name
  Name = list_to_atom("storage_worker_" ++ integer_to_list(HashValue)),

  % Return the global handle
  {global, Name}.

