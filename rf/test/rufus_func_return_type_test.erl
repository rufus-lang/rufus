-module(rufus_func_return_type_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_with_empty_module_test() ->
    RufusText = "module empty",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_func_return_type:typecheck(Forms)).

typecheck_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_func_return_type:typecheck(Forms)).

typecheck_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() int { 42.0 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {error, unmatched_return_type, Data} = rufus_func_return_type:typecheck(Forms),
    ?assertEqual(#{actual => float, expected => int}, Data).
