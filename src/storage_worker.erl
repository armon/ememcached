-module(storage_worker).
-behaviour(gen_server).
-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("config.hrl").
-include("entries.hrl").

% This module is implements a gen_server worker that is
% used under a supervision tree created by storage.
% These workers are used to serialize requests that have
% potential race conditions based on a hash of the key value

% Our state object
-record(state, {worker_num}).

% Starts the server
% @spec start_link() -> {ok, Pid()} | ignore | {error, Error()}
start_link(WorkerNum) -> 
  Name = list_to_atom("storage_worker_" ++ integer_to_list(WorkerNum)),
  gen_server:start_link({global, Name}, ?MODULE, [WorkerNum], []).

% Initializes this worker
init([WorkerNum]) -> {ok, #state{worker_num=WorkerNum}}.

% Handles an incoming request.
handle_call({set, Entry}, _From, State) ->
  {reply, case storage:get(Entry#entry.key) of
    deleted -> stored;
    _ -> apply(?STORAGE_BACKEND, set, [Entry])
  end, State};


handle_call({add, Entry}, _From, State) -> 
  {reply, case storage:get(Entry#entry.key) of
    #entry{} -> exists;
    deleted -> deleted;
    _ -> apply(?STORAGE_BACKEND, set, [Entry])
  end, State};


handle_call({replace, Entry}, _From, State) -> 
  {reply, case storage:get(Entry#entry.key) of
    #entry{} -> apply(?STORAGE_BACKEND, set, [Entry]);
    deleted -> deleted;
    _ -> notexist
  end, State};


handle_call({append, Entry}, _From, State) -> 
  {reply, case storage:get(Entry#entry.key) of
    #entry{} = Existing ->
      % Update the value and size
      NewEntry = Existing#entry{value=[Existing#entry.value, Entry#entry.value],
                                size=Existing#entry.size + Entry#entry.size},
      apply(?STORAGE_BACKEND, set, [NewEntry]);
    deleted -> deleted;
    _ -> notexist
  end, State};


handle_call({prepend, Entry}, _From, State) -> 
  {reply, case storage:get(Entry#entry.key) of
    #entry{} = Existing ->
      % Update the value and size
      NewEntry = Existing#entry{value=[Entry#entry.value, Existing#entry.value],
                                size=Existing#entry.size + Entry#entry.size},
      apply(?STORAGE_BACKEND, set, [NewEntry]);
    deleted -> deleted;
    _ -> notexist
  end, State};


handle_call({cas, Entry}, _From, State) -> 
  {reply, case storage:get(Entry#entry.key) of
    Existing when Existing#entry.version =:= Entry#entry.version -> apply(?STORAGE_BACKEND, set, [Entry]);
    #entry{} -> modified;
    deleted -> deleted;
    _ -> notexist
  end, State};


handle_call({mod, Mod}, _From, State) -> 
  {reply, case storage:get(Mod#modification.key) of
    #entry{} = Existing ->
      try
        CurrentVal = list_to_integer(binary_to_list(iolist_to_binary(Existing#entry.value)),10),
        Modified = case Mod#modification.operation of
          incr -> CurrentVal + Mod#modification.value;
          decr -> CurrentVal - Mod#modification.value
        end,
        NewVal = max(0, Modified) band 18446744073709551615, % Wrap at 64bits
        ValStr = list_to_binary(integer_to_list(NewVal)),
        NewEntry = Existing#entry{value = ValStr},
        apply(?STORAGE_BACKEND, set, [NewEntry]),
        {updated, ValStr}
      catch
        _:_ -> notnum
      end;

    _ -> notexist
  end, State};


handle_call({delete, Mod}, _From, State) -> 
  {reply, case storage:get(Mod#modification.key) of
    #entry{} = Entry->
      io:format("Time: ~p~n",[Mod#modification.value]),
      % Check if we delete now or in the future
      case Mod#modification.value of
        Time when is_integer(Time), Time > 0 ->
          % Replace entry with one with no data, mark as deleted
          apply(?STORAGE_BACKEND, set, [Entry#entry{value=deleted,size=0}]),

          % Spawn a function to run in the future
          WorkerPid = self(),
          spawn(fun() ->
              {Big,Small,_Micro} = erlang:now(),
              Now = Big*1000000+Small,
              Delay = max(Time - Now,0),
              io:format("Delay ~p ~n", [Delay]),
              timer:sleep(Delay*1000), % Delay execution
              io:format("Wake~n"),

              % Dispatch again to force delete
              gen_server:call(WorkerPid, {force_delete, Mod})
          end);
        
        % No delay, delete now
        _ -> apply(?STORAGE_BACKEND, delete, [Mod#modification.key]),
             io:format("Deleted")
      end,

      % Report as deleted
      deleted;

    _ -> notexist
  end, State};


handle_call({force_delete, Mod}, _From, State) -> 
  apply(?STORAGE_BACKEND, delete, [Mod#modification.key]),
  io:format("Force Deleted"),
  {reply, deleted, State};


handle_call(_Request, _From, State) -> {reply, error, State}.

% Handles incoming casts
% These are also unexpected.
handle_cast(_Msg, State) -> {noreply, State}.

% Handles spontaneous info messages
% We don't expect any
handle_info(_Info, State) -> {noreply, State}.

% Called upon our termination
terminate(_Reason, _State) -> ok.

% Handles code changes on the fly
code_change(_OldVsn, State, _Extra) -> {ok, State}.



