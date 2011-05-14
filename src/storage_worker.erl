-module(storage_worker).
-behaviour(gen_server).
-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

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

% Handles an incoming request. These are expected frequently.
handle_call(_Request, _From, State) -> 
  Reply = ok,
  {reply, Reply, State}.

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



