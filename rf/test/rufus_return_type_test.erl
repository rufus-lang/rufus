-module(rufus_return_type_test).

-include_lib("eunit/include/eunit.hrl").

check_empty_package_test() ->
    RufusText = "package empty",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_return_type:check(Forms)).

check_test() ->
    RufusText = "
    package example

    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_return_type:check(Forms)).

check_function_with_unmatched_return_type_test() ->
    RufusText = "
    package example

    func Number() int { 42.0 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {error, unmatched_return_type, _Data} = rufus_return_type:check(Forms).
