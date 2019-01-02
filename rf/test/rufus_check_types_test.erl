-module(rufus_check_types_test).

-include_lib("eunit/include/eunit.hrl").

forms_with_empty_package_test() ->
    RufusText = "package empty",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_check_types:forms(Forms)).

forms_test() ->
    RufusText = "
    package example

    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_check_types:forms(Forms)).

forms_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    package example

    func Number() int { 42.0 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {error, unmatched_return_type, _Data} = rufus_check_types:forms(Forms).
