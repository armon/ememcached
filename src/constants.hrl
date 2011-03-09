% Defines constants used in the memcached protocol

% For our app
-define(VERSION,"ememcached 0.1a").

% Default ports
-define(DEFAULT_PORT, 11211).
 
% Magic Bytes
-define(REQUEST_MAGIC, 16#80).
-define(RESPONSE_MAGIC, 16#81).

% Response statuses
-define(STATUS_OK, 16#0).
-define(STATUS_KEY_NOT_FOUND, 16#1).
-define(STATUS_KEY_EXISTS, 16#2).
-define(STATUS_VALUE_TOO_LARGE, 16#3).
-define(STATUS_INVALID_ARGS, 16#4).
-define(STATUS_ITEM_NOT_STORED, 16#5).
-define(STATUS_INCR_DECR_NON_NUM, 16#6).
-define(STATUS_UNKN_CMD, 16#81).
-define(STATUS_OUT_OF_MEM, 16#82).

% Opcodes
% The "Q" extention means 'quiet', or to
% not respond with uninteresting data.
-define(OP_GET, 16#0).
-define(OP_SET, 16#1).
-define(OP_ADD, 16#2).
-define(OP_REPLACE, 16#3).
-define(OP_DELETE, 16#4).
-define(OP_INCR, 16#5).
-define(OP_DECR, 16#6).
-define(OP_QUIT, 16#7).
-define(OP_FLUSH, 16#8).
-define(OP_GETQ, 16#9).
-define(OP_NOOP, 16#A).
-define(OP_VERSION, 16#B).
-define(OP_GETK, 16#C).
-define(OP_GETKQ, 16#D).
-define(OP_APPEND, 16#E).
-define(OP_PREPEND, 16#F).
-define(OP_STAT, 16#10).
-define(OP_SETQ, 16#11).
-define(OP_ADDQ, 16#12).
-define(OP_REPLACEQ, 16#13).
-define(OP_DELETEQ, 16#14).
-define(OP_INCRQ, 16#15).
-define(OP_DECRQ, 16#16).
-define(OP_QUITQ, 16#17).
-define(OP_FLUSHQ, 16#18).
-define(OP_APPENDQ, 16#19).
-define(OP_PREPENDQ, 16#1A).

% Types
-define(TYPE_RAW, 16#0).

% ASCII Protocol
-define(ASCII_CLIENT_ERR, "CLIENT_ERROR ~s\r\n").
-define(ASCII_SERVER_ERR, "SERVER_ERROR ~s\r\n").
