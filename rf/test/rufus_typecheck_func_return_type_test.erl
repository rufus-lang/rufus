-module(rufus_typecheck_func_return_type_test).

-include_lib("eunit/include/eunit.hrl").

forms_with_empty_module_test() ->
    RufusText = "module empty",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_typecheck_func_return_type:forms(Forms)).

forms_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_typecheck_func_return_type:forms(Forms)).

forms_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() int { 42.0 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {error, unmatched_return_type, Data} = rufus_typecheck_func_return_type:forms(Forms),
    ?assertEqual(#{actual => float, expected => int}, Data).
