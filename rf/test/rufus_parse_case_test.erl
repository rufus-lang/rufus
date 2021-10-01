-module(rufus_parse_case_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_case_block_with_single_atom_clause_test() ->
    RufusText =
        "func MaybeConvert(value atom) string {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs =>
                [
                    {'case', #{
                        clauses =>
                            [
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {string_lit, #{
                                                line => 4,
                                                spec => <<"true">>,
                                                type =>
                                                    {type, #{line => 4, spec => string}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {atom_lit, #{
                                            line => 3,
                                            spec => true,
                                            type =>
                                                {type, #{line => 3, spec => atom}}
                                        }}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{line => 2, spec => value}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type => {type, #{line => 1, spec => atom}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => string}},
            spec => 'MaybeConvert'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_case_block_with_single_bool_clause_test() ->
    RufusText =
        "func MaybeConvert(value bool) string {\n"
        "    case value {\n"
        "    match true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs =>
                [
                    {'case', #{
                        clauses =>
                            [
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {string_lit, #{
                                                line => 4,
                                                spec => <<"true">>,
                                                type =>
                                                    {type, #{line => 4, spec => string}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {bool_lit, #{
                                            line => 3,
                                            spec => true,
                                            type =>
                                                {type, #{line => 3, spec => bool}}
                                        }}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{line => 2, spec => value}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type => {type, #{line => 1, spec => bool}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => string}},
            spec => 'MaybeConvert'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_case_block_with_single_float_clause_test() ->
    RufusText =
        "func MaybeConvert(value float) string {\n"
        "    case value {\n"
        "    match 1.0 ->\n"
        "        \"1\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs =>
                [
                    {'case', #{
                        clauses =>
                            [
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {string_lit, #{
                                                line => 4,
                                                spec => <<"1">>,
                                                type =>
                                                    {type, #{line => 4, spec => string}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {float_lit, #{
                                            line => 3,
                                            spec => 1.0,
                                            type =>
                                                {type, #{line => 3, spec => float}}
                                        }}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{line => 2, spec => value}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type => {type, #{line => 1, spec => float}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => string}},
            spec => 'MaybeConvert'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_case_block_with_single_int_clause_test() ->
    RufusText =
        "func MaybeConvert(value int) string {\n"
        "    case value {\n"
        "    match 1 ->\n"
        "        \"1\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs =>
                [
                    {'case', #{
                        clauses =>
                            [
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {string_lit, #{
                                                line => 4,
                                                spec => <<"1">>,
                                                type =>
                                                    {type, #{line => 4, spec => string}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {int_lit, #{
                                            line => 3,
                                            spec => 1,
                                            type =>
                                                {type, #{line => 3, spec => int}}
                                        }}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{line => 2, spec => value}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type => {type, #{line => 1, spec => int}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => string}},
            spec => 'MaybeConvert'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_case_block_with_single_string_clause_test() ->
    RufusText =
        "func MaybeConvert(value string) string {\n"
        "    case value {\n"
        "    match \"1\" ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs =>
                [
                    {'case', #{
                        clauses =>
                            [
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {string_lit, #{
                                                line => 4,
                                                spec => <<"true">>,
                                                type =>
                                                    {type, #{line => 4, spec => string}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {string_lit, #{
                                            line => 3,
                                            spec => <<"1">>,
                                            type =>
                                                {type, #{line => 3, spec => string}}
                                        }}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{line => 2, spec => value}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type => {type, #{line => 1, spec => string}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => string}},
            spec => 'MaybeConvert'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_case_block_with_single_cons_clause_test() ->
    RufusText =
        "func MaybeConvert(value list[string]) atom {\n"
        "    case value {\n"
        "    match list[string]{head|tail} ->\n"
        "        :ok\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs =>
                [
                    {'case', #{
                        clauses =>
                            [
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {atom_lit, #{
                                                line => 4,
                                                spec => ok,
                                                type =>
                                                    {type, #{line => 4, spec => atom}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {cons, #{
                                            head =>
                                                {identifier, #{line => 3, spec => head}},
                                            line => 3,
                                            tail =>
                                                {identifier, #{line => 3, spec => tail}},
                                            type =>
                                                {type, #{
                                                    element_type =>
                                                        {type, #{line => 3, spec => string}},
                                                    kind => list,
                                                    line => 3,
                                                    spec => 'list[string]'
                                                }}
                                        }}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{line => 2, spec => value}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => value,
                        type =>
                            {type, #{
                                element_type =>
                                    {type, #{line => 1, spec => string}},
                                kind => list,
                                line => 1,
                                spec => 'list[string]'
                            }}
                    }}
                ],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'MaybeConvert'
        }}
    ],
    ?assertEqual(Expected, Forms).
