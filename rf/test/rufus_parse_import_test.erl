-module(rufus_parse_import_test).

-include_lib("eunit/include/eunit.hrl").

parse_import_test() ->
    RufusText = "import \"bar\"",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {import, #{
            line => 1,
            spec => "bar"
        }}
    ],
    ?assertEqual(Expected, Forms).
