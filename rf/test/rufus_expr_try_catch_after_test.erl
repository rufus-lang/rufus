-module(rufus_expr_try_catch_after_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_function_with_bare_catch_block_test() ->
    RufusText =
        "module example\n"
        "func MaybeDivideBy(n int) atom {\n"
        "    try {\n"
        "        1 / n\n"
        "        :ok\n"
        "    } catch {\n"
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
                                    line => 7,
                                    spec => error,
                                    type =>
                                        {type, #{line => 7, spec => atom}}
                                }}
                            ],
                            line => 6,
                            match_expr => undefined,
                            type => {type, #{line => 7, spec => atom}}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 1,
                                    type => {type, #{line => 4, spec => int}}
                                }},
                            line => 4,
                            op => '/',
                            right =>
                                {identifier, #{
                                    line => 4,
                                    spec => n,
                                    type => {type, #{line => 2, spec => int}}
                                }},
                            type => {type, #{line => 4, spec => int}}
                        }},
                        {atom_lit, #{
                            line => 5,
                            spec => ok,
                            type => {type, #{line => 5, spec => atom}}
                        }}
                    ],
                    type => {type, #{line => 5, spec => atom}}
                }}
            ],
            line => 2,
            params => [
                {param, #{
                    line => 2,
                    spec => n,
                    type => {type, #{line => 2, spec => int}}
                }}
            ],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'MaybeDivideBy',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [{type, #{line => 2, spec => int}}],
                    return_type => {type, #{line => 2, spec => atom}},
                    spec => 'func(int) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_try_and_catch_blocks_both_returning_an_atom_literal_test() ->
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

typecheck_and_annotate_function_with_try_and_catch_blocks_both_returning_a_bool_literal_test() ->
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

typecheck_and_annotate_function_with_try_and_catch_blocks_both_returning_a_float_literal_test() ->
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

typecheck_and_annotate_function_with_try_and_catch_blocks_both_returning_an_int_literal_test() ->
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

typecheck_and_annotate_function_with_try_and_catch_blocks_both_returning_a_string_literal_test() ->
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

typecheck_and_annotate_function_with_catch_block_matching_variable_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch error atom {\n"
        "        error\n"
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
                                {identifier, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {param, #{
                                    line => 5,
                                    spec => error,
                                    type => {type, #{line => 5, spec => atom}}
                                }},
                            type => {type, #{line => 5, spec => atom}}
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

typecheck_and_annotate_function_with_catch_block_matching_cons_expression_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch list[atom]{head|tail} {\n"
        "        head\n"
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
                                {identifier, #{
                                    line => 6,
                                    spec => head,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {cons, #{
                                    head =>
                                        {identifier, #{
                                            line => 5,
                                            locals => #{},
                                            spec => head,
                                            type =>
                                                {type, #{line => 5, spec => atom}}
                                        }},
                                    line => 5,
                                    tail =>
                                        {identifier, #{
                                            line => 5,
                                            locals => #{
                                                head => [{type, #{line => 5, spec => atom}}]
                                            },
                                            spec => tail,
                                            type =>
                                                {type, #{
                                                    element_type =>
                                                        {type, #{line => 5, spec => atom}},
                                                    kind => list,
                                                    line => 5,
                                                    spec => 'list[atom]'
                                                }}
                                        }},
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{line => 5, spec => atom}},
                                            kind => list,
                                            line => 5,
                                            spec => 'list[atom]'
                                        }}
                                }},
                            type => {type, #{line => 5, spec => atom}}
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

typecheck_and_annotate_function_with_catch_block_with_match_op_expression_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch list[atom]{head|tail} = items list[atom] {\n"
        "        head\n"
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
                                {identifier, #{
                                    line => 6,
                                    spec => head,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {match_op, #{
                                    left =>
                                        {cons, #{
                                            head =>
                                                {identifier, #{
                                                    line => 5,
                                                    locals => #{
                                                        items => [
                                                            {type, #{
                                                                element_type =>
                                                                    {type, #{
                                                                        line => 5,
                                                                        spec => atom
                                                                    }},
                                                                kind => list,
                                                                line => 5,
                                                                spec => 'list[atom]'
                                                            }}
                                                        ]
                                                    },
                                                    spec => head,
                                                    type =>
                                                        {type, #{line => 5, spec => atom}}
                                                }},
                                            line => 5,
                                            tail =>
                                                {identifier, #{
                                                    line => 5,
                                                    locals => #{
                                                        head => [
                                                            {type, #{line => 5, spec => atom}}
                                                        ],
                                                        items => [
                                                            {type, #{
                                                                element_type =>
                                                                    {type, #{
                                                                        line => 5,
                                                                        spec => atom
                                                                    }},
                                                                kind => list,
                                                                line => 5,
                                                                spec => 'list[atom]'
                                                            }}
                                                        ]
                                                    },
                                                    spec => tail,
                                                    type =>
                                                        {type, #{
                                                            element_type =>
                                                                {type, #{line => 5, spec => atom}},
                                                            kind => list,
                                                            line => 5,
                                                            spec => 'list[atom]'
                                                        }}
                                                }},
                                            type =>
                                                {type, #{
                                                    element_type =>
                                                        {type, #{line => 5, spec => atom}},
                                                    kind => list,
                                                    line => 5,
                                                    spec => 'list[atom]'
                                                }}
                                        }},
                                    line => 5,
                                    right =>
                                        {param, #{
                                            line => 5,
                                            spec => items,
                                            type =>
                                                {type, #{
                                                    element_type =>
                                                        {type, #{line => 5, spec => atom}},
                                                    kind => list,
                                                    line => 5,
                                                    spec => 'list[atom]'
                                                }}
                                        }},
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{line => 5, spec => atom}},
                                            kind => list,
                                            line => 5,
                                            spec => 'list[atom]'
                                        }}
                                }},
                            type => {type, #{line => 5, spec => atom}}
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

typecheck_and_annotate_function_with_try_and_multiple_catch_blocks_returning_an_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
        "        :error\n"
        "    match :failure ->\n"
        "        :failure\n"
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
                                    line => 7,
                                    spec => error,
                                    type =>
                                        {type, #{line => 7, spec => atom}}
                                }}
                            ],
                            line => 6,
                            match_expr =>
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type => {type, #{line => 6, spec => atom}}
                                }},
                            type => {type, #{line => 7, spec => atom}}
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
                                    type => {type, #{line => 8, spec => atom}}
                                }},
                            type => {type, #{line => 9, spec => atom}}
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

typecheck_and_annotate_function_with_try_block_in_match_op_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    result = try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "    result\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            locals => #{},
                            spec => result,
                            type => {type, #{line => 4, spec => atom}}
                        }},
                    line => 3,
                    right =>
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
                        }},
                    type => {type, #{line => 4, spec => atom}}
                }},
                {identifier, #{
                    line => 8,
                    spec => result,
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

typecheck_and_annotate_function_with_bare_catch_block_and_an_after_block_test() ->
    RufusText =
        "module example\n"
        "func cleanup() atom { :cleanup }\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        :error\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {atom_lit, #{
                    line => 2,
                    spec => cleanup,
                    type => {type, #{line => 2, spec => atom}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => cleanup,
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => atom}},
                    spec => 'func() atom'
                }}
        }},
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [
                        {call, #{
                            args => [],
                            line => 9,
                            spec => cleanup,
                            type => {type, #{line => 2, spec => atom}}
                        }}
                    ],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 7,
                                    spec => error,
                                    type =>
                                        {type, #{line => 7, spec => atom}}
                                }}
                            ],
                            line => 6,
                            match_expr => undefined,
                            type => {type, #{line => 7, spec => atom}}
                        }}
                    ],
                    line => 4,
                    try_exprs => [
                        {atom_lit, #{
                            line => 5,
                            spec => ok,
                            type => {type, #{line => 5, spec => atom}}
                        }}
                    ],
                    type => {type, #{line => 5, spec => atom}}
                }}
            ],
            line => 3,
            params => [],
            return_type => {type, #{line => 3, spec => atom}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type => {type, #{line => 3, spec => atom}},
                    spec => 'func() atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% typecheck_and_annotate scope tests

typecheck_and_annotate_function_with_try_catch_and_after_blocks_accessing_variables_from_outer_scope_test() ->
    RufusText =
        "module example\n"
        "func cleanup(value atom) atom { value }\n"
        "func Maybe() atom {\n"
        "    ok = :ok\n"
        "    error = :error\n"
        "    value = :cleanup\n"
        "    try {\n"
        "        ok\n"
        "    } catch :error {\n"
        "        error\n"
        "    } after {\n"
        "        cleanup(value)\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 2,
                    spec => value,
                    type => {type, #{line => 2, spec => atom}}
                }}
            ],
            line => 2,
            params => [
                {param, #{
                    line => 2,
                    spec => value,
                    type => {type, #{line => 2, spec => atom}}
                }}
            ],
            return_type => {type, #{line => 2, spec => atom}},
            spec => cleanup,
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [{type, #{line => 2, spec => atom}}],
                    return_type => {type, #{line => 2, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => ok,
                            type => {type, #{line => 4, spec => atom}}
                        }},
                    line => 4,
                    right =>
                        {atom_lit, #{
                            line => 4,
                            spec => ok,
                            type => {type, #{line => 4, spec => atom}}
                        }},
                    type => {type, #{line => 4, spec => atom}}
                }},
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 5,
                            locals => #{ok => [{type, #{line => 4, spec => atom}}]},
                            spec => error,
                            type => {type, #{line => 5, spec => atom}}
                        }},
                    line => 5,
                    right =>
                        {atom_lit, #{
                            line => 5,
                            spec => error,
                            type => {type, #{line => 5, spec => atom}}
                        }},
                    type => {type, #{line => 5, spec => atom}}
                }},
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 6,
                            locals => #{
                                error => [{type, #{line => 5, spec => atom}}],
                                ok => [{type, #{line => 4, spec => atom}}]
                            },
                            spec => value,
                            type => {type, #{line => 6, spec => atom}}
                        }},
                    line => 6,
                    right =>
                        {atom_lit, #{
                            line => 6,
                            spec => cleanup,
                            type => {type, #{line => 6, spec => atom}}
                        }},
                    type => {type, #{line => 6, spec => atom}}
                }},
                {try_catch_after, #{
                    after_exprs => [
                        {call, #{
                            args => [
                                {identifier, #{
                                    line => 12,
                                    spec => value,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 12,
                            spec => cleanup,
                            type => {type, #{line => 2, spec => atom}}
                        }}
                    ],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {identifier, #{
                                    line => 10,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                            ],
                            line => 9,
                            match_expr =>
                                {atom_lit, #{
                                    line => 9,
                                    spec => error,
                                    type => {type, #{line => 9, spec => atom}}
                                }},
                            type => {type, #{line => 5, spec => atom}}
                        }}
                    ],
                    line => 7,
                    try_exprs => [
                        {identifier, #{
                            line => 8,
                            spec => ok,
                            type => {type, #{line => 4, spec => atom}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => atom}}
                }}
            ],
            line => 3,
            params => [],
            return_type => {type, #{line => 3, spec => atom}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type => {type, #{line => 3, spec => atom}},
                    spec => 'func() atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_try_variable_accessed_outside_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        ok = :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "    ok\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form => {identifier, #{line => 8, locals => #{}, spec => ok}},
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 2}},
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
                        line => 3,
                        try_exprs => [
                            {match_op, #{
                                left =>
                                    {identifier, #{line => 4, spec => ok}},
                                line => 4,
                                right =>
                                    {atom_lit, #{
                                        line => 4,
                                        spec => ok,
                                        type =>
                                            {type, #{line => 4, spec => atom}}
                                    }}
                            }}
                        ]
                    }},
                    {identifier, #{line => 8, spec => ok}}
                ],
                line => 2,
                locals => #{},
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
        ]
    },
    ?assertEqual(
        {error, unknown_identifier, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).

typecheck_and_annotate_function_with_catch_variable_accessed_outside_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        error = :error\n"
        "    }\n"
        "    error\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{line => 8, locals => #{}, spec => error}},
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 2}},
            {func, #{
                exprs => [
                    {try_catch_after, #{
                        after_exprs => [],
                        catch_clauses => [
                            {catch_clause, #{
                                exprs => [
                                    {match_op, #{
                                        left =>
                                            {identifier, #{line => 6, spec => error}},
                                        line => 6,
                                        right =>
                                            {atom_lit, #{
                                                line => 6,
                                                spec => error,
                                                type =>
                                                    {type, #{line => 6, spec => atom}}
                                            }}
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
                        line => 3,
                        try_exprs => [
                            {atom_lit, #{
                                line => 4,
                                spec => ok,
                                type =>
                                    {type, #{line => 4, spec => atom}}
                            }}
                        ]
                    }},
                    {identifier, #{line => 8, spec => error}}
                ],
                line => 2,
                locals => #{},
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
        ]
    },
    ?assertEqual(
        {error, unknown_identifier, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).

typecheck_and_annotate_function_with_after_variable_accessed_outside_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } after {\n"
        "        cleanup = :cleanup\n"
        "    }\n"
        "    cleanup\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{line => 8, locals => #{}, spec => cleanup}},
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 2}},
            {func, #{
                exprs => [
                    {try_catch_after, #{
                        after_exprs => [
                            {match_op, #{
                                left =>
                                    {identifier, #{line => 6, spec => cleanup}},
                                line => 6,
                                right =>
                                    {atom_lit, #{
                                        line => 6,
                                        spec => cleanup,
                                        type =>
                                            {type, #{line => 6, spec => atom}}
                                    }}
                            }}
                        ],
                        catch_clauses => [],
                        line => 3,
                        try_exprs => [
                            {atom_lit, #{
                                line => 4,
                                spec => ok,
                                type =>
                                    {type, #{line => 4, spec => atom}}
                            }}
                        ]
                    }},
                    {identifier, #{line => 8, spec => cleanup}}
                ],
                line => 2,
                locals => #{},
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
        ]
    },
    ?assertEqual(
        {error, unknown_identifier, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).

typecheck_and_annotate_function_with_try_variable_accessed_in_catch_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        ok = :ok\n"
        "    } catch :error {\n"
        "        ok\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form => {identifier, #{line => 6, locals => #{}, spec => ok}},
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 2}},
            {func, #{
                exprs => [
                    {try_catch_after, #{
                        after_exprs => [],
                        catch_clauses => [
                            {catch_clause, #{
                                exprs => [{identifier, #{line => 6, spec => ok}}],
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
                        line => 3,
                        try_exprs => [
                            {match_op, #{
                                left =>
                                    {identifier, #{line => 4, spec => ok}},
                                line => 4,
                                right =>
                                    {atom_lit, #{
                                        line => 4,
                                        spec => ok,
                                        type =>
                                            {type, #{line => 4, spec => atom}}
                                    }}
                            }}
                        ]
                    }}
                ],
                line => 2,
                locals => #{},
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
        ]
    },
    ?assertEqual(
        {error, unknown_identifier, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).

typecheck_and_annotate_function_with_try_variable_accessed_in_after_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        ok = :ok\n"
        "    } after {\n"
        "        ok\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form => {identifier, #{line => 6, locals => #{}, spec => ok}},
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 2}},
            {func, #{
                exprs => [
                    {try_catch_after, #{
                        after_exprs => [{identifier, #{line => 6, spec => ok}}],
                        catch_clauses => [],
                        line => 3,
                        try_exprs => [
                            {match_op, #{
                                left =>
                                    {identifier, #{line => 4, spec => ok}},
                                line => 4,
                                right =>
                                    {atom_lit, #{
                                        line => 4,
                                        spec => ok,
                                        type =>
                                            {type, #{line => 4, spec => atom}}
                                    }}
                            }}
                        ]
                    }}
                ],
                line => 2,
                locals => #{},
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
        ]
    },
    ?assertEqual(
        {error, unknown_identifier, Data},
        rufus_expr:typecheck_and_annotate(Forms)
    ).

%% typecheck_and_annotate failure mode tests

typecheck_and_annotate_function_with_try_and_catch_blocks_with_mismatched_try_block_return_type_test() ->
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

typecheck_and_annotate_function_with_try_and_catch_blocks_with_mismatched_catch_clause_return_type_test() ->
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

typecheck_and_annotate_function_with_try_and_multiple_catch_blocks_with_a_mismatched_catch_clause_return_type_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
        "        :error\n"
        "    match :failure ->\n"
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
                        line => 9,
                        spec => 42,
                        type => {type, #{line => 9, spec => int}}
                    }}
                ],
                line => 8,
                match_expr =>
                    {atom_lit, #{
                        line => 8,
                        spec => failure,
                        type => {type, #{line => 8, spec => atom}}
                    }},
                type => {type, #{line => 9, spec => int}}
            }},
        expected => atom,
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
