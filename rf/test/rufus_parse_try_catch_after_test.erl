-module(rufus_parse_try_catch_after_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_bare_catch_block_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4
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

parse_function_with_try_catch_block_with_single_atom_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
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

parse_function_with_try_catch_block_with_single_bool_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch true {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {bool_lit, #{
                                    line => 4,
                                    spec => true,
                                    type =>
                                        {type, #{line => 4, spec => bool}}
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

parse_function_with_try_catch_block_with_single_float_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch 42.0 {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {float_lit, #{
                                    line => 4,
                                    spec => 42.0,
                                    type =>
                                        {type, #{line => 4, spec => float}}
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

parse_function_with_try_catch_block_with_single_int_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch 42 {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 42,
                                    type =>
                                        {type, #{line => 4, spec => int}}
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

parse_function_with_try_catch_block_with_single_string_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch \"error\" {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {string_lit, #{
                                    line => 4,
                                    spec => <<"error">>,
                                    type =>
                                        {type, #{line => 4, spec => string}}
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

parse_function_with_try_catch_block_with_single_cons_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch list[atom]{:error|tail} {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {cons, #{
                                    head =>
                                        {atom_lit, #{
                                            line => 4,
                                            spec => error,
                                            type =>
                                                {type, #{line => 4, spec => atom}}
                                        }},
                                    line => 4,
                                    tail =>
                                        {identifier, #{line => 4, spec => tail}},
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{line => 4, spec => atom}},
                                            kind => list,
                                            line => 4,
                                            spec => 'list[atom]'
                                        }}
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

parse_function_with_try_catch_block_with_single_identifier_and_type_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch errorCode atom {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {param, #{
                                    line => 4,
                                    spec => errorCode,
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

parse_function_with_try_catch_block_with_single_match_op_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error = errorCode atom {\n"
        "        log(\"oh no\")\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 5,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 5, spec => string}}
                                        }}
                                    ],
                                    line => 5,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 4,
                            match_expr =>
                                {match_op, #{
                                    left =>
                                        {atom_lit, #{
                                            line => 4,
                                            spec => error,
                                            type =>
                                                {type, #{line => 4, spec => atom}}
                                        }},
                                    line => 4,
                                    right =>
                                        {param, #{
                                            line => 4,
                                            spec => errorCode,
                                            type =>
                                                {type, #{line => 4, spec => atom}}
                                        }}
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

parse_function_with_try_catch_block_with_single_atom_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
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

parse_function_with_try_catch_block_with_single_bool_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match false ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {bool_lit, #{
                                    line => 5,
                                    spec => false,
                                    type =>
                                        {type, #{line => 5, spec => bool}}
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

parse_function_with_try_catch_block_with_single_float_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match 42.0 ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {float_lit, #{
                                    line => 5,
                                    spec => 42.0,
                                    type =>
                                        {type, #{line => 5, spec => float}}
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

parse_function_with_try_catch_block_with_single_int_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match 42 ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 42,
                                    type =>
                                        {type, #{line => 5, spec => int}}
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

parse_function_with_try_catch_block_with_single_string_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match \"error\" ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {string_lit, #{
                                    line => 5,
                                    spec => <<"error">>,
                                    type =>
                                        {type, #{line => 5, spec => string}}
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

parse_function_with_try_catch_block_with_single_cons_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match list[atom]{:error|tail} ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {cons, #{
                                    head =>
                                        {atom_lit, #{
                                            line => 5,
                                            spec => error,
                                            type =>
                                                {type, #{line => 5, spec => atom}}
                                        }},
                                    line => 5,
                                    tail =>
                                        {identifier, #{line => 5, spec => tail}},
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{line => 5, spec => atom}},
                                            kind => list,
                                            line => 5,
                                            spec => 'list[atom]'
                                        }}
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

parse_function_with_try_catch_block_with_single_identifier_and_type_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match errorCode atom ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {param, #{
                                    line => 5,
                                    spec => errorCode,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
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

parse_function_with_try_catch_block_with_single_match_op_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error = errorCode atom ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {match_op, #{
                                    left =>
                                        {atom_lit, #{
                                            line => 5,
                                            spec => error,
                                            type =>
                                                {type, #{line => 5, spec => atom}}
                                        }},
                                    line => 5,
                                    right =>
                                        {param, #{
                                            line => 5,
                                            spec => errorCode,
                                            type =>
                                                {type, #{line => 5, spec => atom}}
                                        }}
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

parse_function_with_try_catch_block_with_multiple_clauses_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
        "        log(\"oh no\")\n"
        "        :error\n"
        "    match :failure ->\n"
        "        :failure\n"
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
                                {call, #{
                                    args => [
                                        {string_lit, #{
                                            line => 6,
                                            spec => <<"oh no">>,
                                            type =>
                                                {type, #{line => 6, spec => string}}
                                        }}
                                    ],
                                    line => 6,
                                    spec => log
                                }},
                                {atom_lit, #{
                                    line => 7,
                                    spec => error,
                                    type =>
                                        {type, #{line => 7, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                        }},
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 9,
                                    spec => failure,
                                    type =>
                                        {type, #{line => 9, spec => atom}}
                                }}
                            ],
                            line => 8,
                            match_expr =>
                                {atom_lit, #{
                                    line => 8,
                                    spec => failure,
                                    type =>
                                        {type, #{line => 8, spec => atom}}
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

parse_function_with_try_catch_block_with_single_match_clause_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
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
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
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
        "        doSomething()\n"
        "        :ok\n"
        "    } after {\n"
        "        firstCleanup()\n"
        "        secondCleanup()\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [
                        {call, #{args => [], line => 6, spec => firstCleanup}},
                        {call, #{args => [], line => 7, spec => secondCleanup}}
                    ],
                    catch_clauses => [],
                    line => 2,
                    try_exprs => [
                        {call, #{args => [], line => 3, spec => doSomething}},
                        {atom_lit, #{
                            line => 4,
                            spec => ok,
                            type => {type, #{line => 4, spec => atom}}
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

parse_function_with_try_catch_block_with_single_match_clause_and_after_block_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
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
                    after_exprs => [{call, #{args => [], line => 8, spec => cleanup}}],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
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

parse_function_with_try_catch_block_with_multiple_clauses_and_after_block_test() ->
    RufusText =
        "func example() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
        "        :error\n"
        "    match :error ->\n"
        "        :failure\n"
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
                    after_exprs => [{call, #{args => [], line => 10, spec => cleanup}}],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                        }},
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 8,
                                    spec => failure,
                                    type =>
                                        {type, #{line => 8, spec => atom}}
                                }}
                            ],
                            line => 7,
                            match_expr =>
                                {atom_lit, #{
                                    line => 7,
                                    spec => error,
                                    type =>
                                        {type, #{line => 7, spec => atom}}
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
