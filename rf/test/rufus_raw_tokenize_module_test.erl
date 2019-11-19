-module(rufus_raw_tokenize_module_test).

-include_lib("eunit/include/eunit.hrl").

string_with_module_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("module example"),
    ?assertEqual([
        {module, 1},
        {identifier, 1, "example"}
    ], Tokens).
