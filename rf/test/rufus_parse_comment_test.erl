-module(rufus_parse_comment_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_module_level_comment_test() ->
    RufusText =
        "// echo module contains a server that returns the messages it receives\n"
        "module echo\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2, spec => echo}}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_module_level_end_of_line_comment_test() ->
    RufusText =
        "module echo // echo module exposes an echo server\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    io:format("tokens => ~p~n", [Tokens]),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 1, spec => echo}}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_comment_in_function_block_test() ->
    RufusText =
        "func Echo(value atom) atom {\n"
        "    // Echo returns the value it receives\n"
        "    value\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [{identifier, #{line => 3, spec => value}}],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type => {type, #{line => 1, spec => atom}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).
