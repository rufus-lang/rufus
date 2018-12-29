-module(rufus_error_test).

-include_lib("eunit/include/eunit.hrl").

message_for_unmatched_return_type_test() ->
    Data = #{actual => int, expected => float},
    Message = rufus_error:message(unmatched_return_type, Data),
    ?assertEqual(<<"mismatched return type, got int but want float.">>, Message).
