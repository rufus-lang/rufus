%% rufus_error converts data from compilation errors into human-readable
%% strings.
-module(rufus_error).

-include_lib("rufus_type.hrl").

%% API exports

-export([message/2]).

%% API

-spec message(unmatched_return_type_error(), map()) -> binary().
message(unmatched_return_type, Data) ->
    Actual = maps:get(actual, Data),
    Expected = maps:get(expected, Data),
    Message = io_lib:format("mismatched return type, got ~p but want ~p.", [Actual, Expected]),
    list_to_binary(Message).
