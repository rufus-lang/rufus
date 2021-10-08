-module(rufus_expr_case_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_function_with_case_block_with_single_atom_clause_test() ->
    RufusText =
        "func MaybeConvert(value atom) string {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            type => {type, #{line => 3, spec => atom}}
                                        }},
                                    type => {type, #{line => 4, spec => string}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => atom}}
                            }},
                        type => {type, #{line => 4, spec => string}}
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
            spec => 'MaybeConvert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => atom}}],
                    return_type => {type, #{line => 1, spec => string}},
                    spec => 'func(atom) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_bool_clause_test() ->
    RufusText =
        "func MaybeConvert(value bool) string {\n"
        "    case value {\n"
        "    match true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            type => {type, #{line => 3, spec => bool}}
                                        }},
                                    type => {type, #{line => 4, spec => string}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => bool}}
                            }},
                        type => {type, #{line => 4, spec => string}}
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
            spec => 'MaybeConvert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => bool}}],
                    return_type => {type, #{line => 1, spec => string}},
                    spec => 'func(bool) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_float_clause_test() ->
    RufusText =
        "func MaybeConvert(value float) string {\n"
        "    case value {\n"
        "    match 1.0 ->\n"
        "        \"1\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            type => {type, #{line => 3, spec => float}}
                                        }},
                                    type => {type, #{line => 4, spec => string}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => float}}
                            }},
                        type => {type, #{line => 4, spec => string}}
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
            spec => 'MaybeConvert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => float}}],
                    return_type => {type, #{line => 1, spec => string}},
                    spec => 'func(float) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_int_clause_test() ->
    RufusText =
        "func MaybeConvert(value int) string {\n"
        "    case value {\n"
        "    match 1 ->\n"
        "        \"1\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            type => {type, #{line => 3, spec => int}}
                                        }},
                                    type => {type, #{line => 4, spec => string}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => int}}
                            }},
                        type => {type, #{line => 4, spec => string}}
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
            spec => 'MaybeConvert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => int}}],
                    return_type => {type, #{line => 1, spec => string}},
                    spec => 'func(int) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_string_clause_test() ->
    RufusText =
        "func MaybeConvert(value string) atom {\n"
        "    case value {\n"
        "    match \"ok\" ->\n"
        "        :ok\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                        {string_lit, #{
                                            line => 3,
                                            spec => <<"ok">>,
                                            type =>
                                                {type, #{line => 3, spec => string}}
                                        }},
                                    type => {type, #{line => 4, spec => atom}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => string}}
                            }},
                        type => {type, #{line => 4, spec => atom}}
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
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'MaybeConvert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => string}}],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'func(string) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_identifier_clause_test() ->
    RufusText =
        "func Echo(value atom) atom {\n"
        "    case value {\n"
        "    match v ->\n"
        "        v\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            {identifier, #{
                                                line => 4,
                                                spec => v,
                                                type =>
                                                    {type, #{line => 1, spec => atom}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {identifier, #{
                                            line => 3,
                                            locals =>
                                                #{
                                                    value =>
                                                        [{type, #{line => 1, spec => atom}}]
                                                },
                                            spec => v,
                                            type => {type, #{line => 1, spec => atom}}
                                        }},
                                    type => {type, #{line => 1, spec => atom}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => atom}}
                            }},
                        type => {type, #{line => 1, spec => atom}}
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
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => atom}}],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_identifier_and_type_clause_test() ->
    RufusText =
        "func Echo(value atom) atom {\n"
        "    case value {\n"
        "    match v atom ->\n"
        "        v\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            {identifier, #{
                                                line => 4,
                                                spec => v,
                                                type =>
                                                    {type, #{line => 3, spec => atom}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {param, #{
                                            line => 3,
                                            spec => v,
                                            type => {type, #{line => 3, spec => atom}}
                                        }},
                                    type => {type, #{line => 3, spec => atom}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => atom}}
                            }},
                        type => {type, #{line => 3, spec => atom}}
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
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => atom}}],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_single_identifier_and_incorrect_type_clause_test() ->
    RufusText =
        "func Echo(value atom) atom {\n"
        "    case value {\n"
        "    match v int ->\n"
        "        v\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        actual => int,
        expected => atom,
        match_expr =>
            {param, #{
                line => 3,
                spec => v,
                type => {type, #{line => 3, spec => int}}
            }},
        stack =>
            [
                {'case', #{
                    clauses =>
                        [
                            {case_clause, #{
                                exprs => [{identifier, #{line => 4, spec => v}}],
                                line => 3,
                                match_expr =>
                                    {param, #{
                                        line => 3,
                                        spec => v,
                                        type => {type, #{line => 3, spec => int}}
                                    }}
                            }}
                        ],
                    line => 2,
                    match_expr =>
                        {identifier, #{
                            line => 2,
                            spec => value,
                            type => {type, #{line => 1, spec => atom}}
                        }}
                }},
                {func_exprs, #{line => 1}},
                {func, #{
                    exprs =>
                        [
                            {'case', #{
                                clauses =>
                                    [
                                        {case_clause, #{
                                            exprs =>
                                                [{identifier, #{line => 4, spec => v}}],
                                            line => 3,
                                            match_expr =>
                                                {param, #{
                                                    line => 3,
                                                    spec => v,
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
                    locals =>
                        #{value => [{type, #{line => 1, spec => atom}}]},
                    params =>
                        [
                            {param, #{
                                line => 1,
                                spec => value,
                                type => {type, #{line => 1, spec => atom}}
                            }}
                        ],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'Echo',
                    type =>
                        {type, #{
                            kind => func,
                            line => 1,
                            param_types => [{type, #{line => 1, spec => atom}}],
                            return_type => {type, #{line => 1, spec => atom}},
                            spec => 'func(atom) atom'
                        }}
                }}
            ]
    },
    ?assertEqual(
        {error, unmatched_case_clause_type, Data}, rufus_expr:typecheck_and_annotate(Forms)
    ).

typecheck_and_annotate_function_with_case_block_with_multiple_clauses_test() ->
    RufusText =
        "func Convert(value bool) atom {\n"
        "    case value {\n"
        "    match false ->\n"
        "        :error\n"
        "    match true ->\n"
        "        :ok\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                                spec => error,
                                                type =>
                                                    {type, #{line => 4, spec => atom}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {bool_lit, #{
                                            line => 3,
                                            spec => false,
                                            type => {type, #{line => 3, spec => bool}}
                                        }},
                                    type => {type, #{line => 4, spec => atom}}
                                }},
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {atom_lit, #{
                                                line => 6,
                                                spec => ok,
                                                type =>
                                                    {type, #{line => 6, spec => atom}}
                                            }}
                                        ],
                                    line => 5,
                                    match_expr =>
                                        {bool_lit, #{
                                            line => 5,
                                            spec => true,
                                            type => {type, #{line => 5, spec => bool}}
                                        }},
                                    type => {type, #{line => 6, spec => atom}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => bool}}
                            }},
                        type => {type, #{line => 6, spec => atom}}
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
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Convert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => bool}}],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'func(bool) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_case_block_with_mismatched_clause_return_types_test() ->
    RufusText =
        "func MaybeConvert(value bool) atom {\n"
        "    case value {\n"
        "    match false ->\n"
        "        \"false\"\n"
        "    match true ->\n"
        "        :ok\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{
        actual => atom,
        expected => string,
        form =>
            {case_clause, #{
                exprs =>
                    [
                        {atom_lit, #{
                            line => 6,
                            spec => ok,
                            type => {type, #{line => 6, spec => atom}}
                        }}
                    ],
                line => 5,
                match_expr =>
                    {bool_lit, #{
                        line => 5,
                        spec => true,
                        type => {type, #{line => 5, spec => bool}}
                    }},
                type => {type, #{line => 6, spec => atom}}
            }}
    },
    ?assertEqual({error, mismatched_case_clause_return_type, Data}, Result).

typecheck_and_annotate_function_with_case_block_with_catch_all_clause_test() ->
    RufusText =
        "func Convert(value atom) string {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        \"true\"\n"
        "    match _ ->\n"
        "        \"false\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
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
                                            type => {type, #{line => 3, spec => atom}}
                                        }},
                                    type => {type, #{line => 4, spec => string}}
                                }},
                                {case_clause, #{
                                    exprs =>
                                        [
                                            {string_lit, #{
                                                line => 6,
                                                spec => <<"false">>,
                                                type =>
                                                    {type, #{line => 6, spec => string}}
                                            }}
                                        ],
                                    line => 5,
                                    type => {type, #{line => 6, spec => string}}
                                }}
                            ],
                        line => 2,
                        match_expr =>
                            {identifier, #{
                                line => 2,
                                spec => value,
                                type => {type, #{line => 1, spec => atom}}
                            }},
                        type => {type, #{line => 6, spec => string}}
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
            spec => 'Convert',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => atom}}],
                    return_type => {type, #{line => 1, spec => string}},
                    spec => 'func(atom) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
