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
    Expected = [
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {atom_lit, #{
                                    line => 4,
                                    spec => error,
                                    type =>
                                        {type, #{line => 4, spec => atom}}
                                }}
                        }}
                    ],
                    line => 2,
                    try_exprs => [
                        {atom_lit, #{
                            line => 3,
                            spec => ok,
                            type => {type, #{line => 3, spec => atom}}
                        }}
                    ]
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => example
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_try_after_block_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [
                        {call, #{args => [], line => 5, spec => cleanup}}
                    ],
                    catch_clauses => [],
                    line => 2,
                    try_exprs => [
                        {atom_lit, #{
                            line => 3,
                            spec => ok,
                            type => {type, #{line => 3, spec => atom}}
                        }}
                    ]
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => example
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_try_catch_block_with_single_clause_and_after_block_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [
                        {call, #{args => [], line => 7, spec => cleanup}}
                    ],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {atom_lit, #{
                                    line => 4,
                                    spec => error,
                                    type =>
                                        {type, #{line => 4, spec => atom}}
                                }}
                        }}
                    ],
                    line => 2,
                    try_exprs => [
                        {atom_lit, #{
                            line => 3,
                            spec => ok,
                            type => {type, #{line => 3, spec => atom}}
                        }}
                    ]
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => example
        }}
    ],
    ?assertEqual(Expected, Forms).
