-module(rufus_expr_try_catch_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_with_try_and_catch_blocks_both_returning_an_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
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
                                    type => {type, #{line => 5, spec => atom}}
                                }},
                            type => {type, #{line => 6, spec => atom}}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {atom_lit, #{
                            line => 4,
                            spec => ok,
                            type => {type, #{line => 4, spec => atom}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => atom}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => atom}},
                    spec => 'func() atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_try_and_catch_blocks_both_returning_a_bool_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() bool {\n"
        "    try {\n"
        "        true\n"
        "    } catch :error {\n"
        "        false\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {bool_lit, #{
                                    line => 6,
                                    spec => false,
                                    type =>
                                        {type, #{line => 6, spec => bool}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type => {type, #{line => 5, spec => atom}}
                                }},
                            type => {type, #{line => 6, spec => bool}}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {bool_lit, #{
                            line => 4,
                            spec => true,
                            type => {type, #{line => 4, spec => bool}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => bool}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => bool}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => bool}},
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_try_and_catch_blocks_both_returning_a_float_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() float {\n"
        "    try {\n"
        "        42.0\n"
        "    } catch :error {\n"
        "        13.8\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {float_lit, #{
                                    line => 6,
                                    spec => 13.8,
                                    type =>
                                        {type, #{line => 6, spec => float}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type => {type, #{line => 5, spec => atom}}
                                }},
                            type => {type, #{line => 6, spec => float}}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {float_lit, #{
                            line => 4,
                            spec => 42.0,
                            type => {type, #{line => 4, spec => float}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => float}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => float}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => float}},
                    spec => 'func() float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_try_and_catch_blocks_both_returning_an_int_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() int {\n"
        "    try {\n"
        "        42\n"
        "    } catch :error {\n"
        "        13\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {int_lit, #{
                                    line => 6,
                                    spec => 13,
                                    type => {type, #{line => 6, spec => int}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type => {type, #{line => 5, spec => atom}}
                                }},
                            type => {type, #{line => 6, spec => int}}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {int_lit, #{
                            line => 4,
                            spec => 42,
                            type => {type, #{line => 4, spec => int}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => int}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => int}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => int}},
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_try_and_catch_blocks_both_returning_a_string_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() string {\n"
        "    try {\n"
        "        \"ok\"\n"
        "    } catch :error {\n"
        "        \"error\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {string_lit, #{
                                    line => 6,
                                    spec => <<"error">>,
                                    type =>
                                        {type, #{line => 6, spec => string}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type => {type, #{line => 5, spec => atom}}
                                }},
                            type => {type, #{line => 6, spec => string}}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {string_lit, #{
                            line => 4,
                            spec => <<"ok">>,
                            type => {type, #{line => 4, spec => string}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => string}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => string}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => string}},
                    spec => 'func() string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_try_and_catch_blocks_with_mismatched_catch_clause_return_type_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        42\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        actual => int,
        catch_clause =>
            {catch_clause, #{
                exprs => [
                    {int_lit, #{
                        line => 6,
                        spec => 42,
                        type => {type, #{line => 6, spec => int}}
                    }}
                ],
                line => 5,
                match_expr =>
                    {atom_lit, #{
                        line => 5,
                        spec => error,
                        type => {type, #{line => 5, spec => atom}}
                    }},
                type => {type, #{line => 6, spec => int}}
            }},
        expected => atom,
        globals => #{
            'Maybe' => [
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type =>
                        {type, #{line => 2, spec => atom}},
                    spec => 'func() atom'
                }}
            ]
        },
        try_exprs => [
            {atom_lit, #{
                line => 4,
                spec => ok,
                type => {type, #{line => 4, spec => atom}}
            }}
        ]
    },
    ?assertEqual(
        {error, mismatched_try_catch_return_type, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).

typecheck_and_annotate_with_try_and_catch_blocks_with_mismatched_try_block_return_type_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        42\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        actual => atom,
        catch_clause =>
            {catch_clause, #{
                exprs => [
                    {atom_lit, #{
                        line => 6,
                        spec => error,
                        type => {type, #{line => 6, spec => atom}}
                    }}
                ],
                line => 5,
                match_expr =>
                    {atom_lit, #{
                        line => 5,
                        spec => error,
                        type => {type, #{line => 5, spec => atom}}
                    }},
                type => {type, #{line => 6, spec => atom}}
            }},
        expected => int,
        globals => #{
            'Maybe' => [
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => atom}},
                    spec => 'func() atom'
                }}
            ]
        },
        try_exprs => [
            {int_lit, #{
                line => 4,
                spec => 42,
                type => {type, #{line => 4, spec => int}}
            }}
        ]
    },
    ?assertEqual(
        {error, mismatched_try_catch_return_type, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).
