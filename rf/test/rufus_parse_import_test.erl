-module(rufus_parse_import_test).

-include_lib("eunit/include/eunit.hrl").

parse_import_test() ->
    RufusText = "
    import \"bar\"
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {import, #{line => 2,
                   spec => "bar"}}
    ], Forms).
