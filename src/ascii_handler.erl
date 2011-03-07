-module(ascii_handler).
-compile(export_all).
-export([match_request/1]).

% Handles the details of the ASCII protocol

% Checks if we have enough input to potentially parse a request
% Returns impossible if the given input could never become a request
% @spec match_request(IoList) -> {true, Captured} | false | impossible
match_request(IoList) ->
    case re:run(IoList, "^(?:[^\s\r\n]+\s*)+(\r\n)?") of 
        {match, Captured} -> 
            case Captured of
                [Group,_] -> {true, Group};
                [_] -> false
            end;
        nomatch -> impossible
    end.

