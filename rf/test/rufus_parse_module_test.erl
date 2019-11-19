-module(rufus_parse_module_test).

-include_lib("eunit/include/eunit.hrl").

parse_module_test() ->
    {ok, Tokens} = rufus_tokenize:string("module example"),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 1,
                   spec => example}}
    ], Forms).
