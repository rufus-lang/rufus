-module(rufus_parse_try_catch_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_try_catch_block_with_single_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [],
    ?assertEqual(Expected, Forms).
