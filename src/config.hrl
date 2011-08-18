% This file is used to define our configuration

% The time we wait for the data block to come after a command is issued
-define(ASCII_RECV_TIMEOUT_MILLI, 10000).

% This is the storage backend that we use
-define(STORAGE_BACKEND, storage_ets).

% The number of storage workers
-define(STORAGE_WORKERS, 256).

% This is the length of time in milliseconds
%  between ETS expiration reaping
-define(ETS_REAP_INTERVAL_MILLI, 5000).
 

