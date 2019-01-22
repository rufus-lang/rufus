-module(rufus_check_types_test).

-include_lib("eunit/include/eunit.hrl").

forms_with_empty_module_test() ->
    RufusText = "module empty",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_check_types:forms(Forms)).

forms_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_check_types:forms(Forms)).

forms_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() int { 42.0 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {error, unmatched_return_type, Data} = rufus_check_types:forms(Forms),
    ?assertEqual(#{actual => float, expected => int}, Data).

forms_with_function_returning_a_variable_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    io:format("Forms => ~n~n~p~n", [Forms]),
    ?assertEqual({ok, Forms}, rufus_check_types:forms(Forms)).
