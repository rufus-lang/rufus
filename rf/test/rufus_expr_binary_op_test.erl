-module(rufus_expr_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

%% Mathematical operators

typecheck_and_annotate_mathematical_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 19 + 23 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 19,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '+',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 23,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'FortyTwo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_mathematical_operator_with_floats_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float { 1.0 + 2.14159265359 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {float_lit, #{
                            line => 3,
                            spec => 1.0,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => float
                                }}
                        }},
                    line => 3,
                    op => '+',
                    right =>
                        {float_lit, #{
                            line => 3,
                            spec => 2.14159265359,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => float
                                }}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => float}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => float}},
            spec => 'Pi',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => float}},
                    source => rufus_text,
                    spec => 'func() float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_mathematical_operator_with_float_and_int_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 19.0 + 23 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {float_lit, #{
                        line => 3,
                        spec => 19.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '+',
                right =>
                    {int_lit, #{
                        line => 3,
                        spec => 23,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_mathematical_operator_with_float_and_float_and_int_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 13.0 + 6.0 + 23 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {binary_op, #{
                        left =>
                            {float_lit, #{
                                line => 3,
                                spec => 13.0,
                                type =>
                                    {type, #{
                                        line => 3,
                                        source => inferred,
                                        spec => float
                                    }}
                            }},
                        line => 3,
                        op => '+',
                        right =>
                            {float_lit, #{
                                line => 3,
                                spec => 6.0,
                                type =>
                                    {type, #{
                                        line => 3,
                                        source => inferred,
                                        spec => float
                                    }}
                            }},
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '+',
                right =>
                    {int_lit, #{
                        line => 3,
                        spec => 23,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_mathematical_operator_with_bools_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Concat() bool { true + false }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {bool_lit, #{
                        line => 3,
                        spec => true,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => bool
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '+',
                right =>
                    {bool_lit, #{
                        line => 3,
                        spec => false,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => bool
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_mathematical_operator_with_strings_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Concat() string { \"port\" + \"manteau\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {string_lit, #{
                        line => 3,
                        spec => <<"port">>,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => string
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '+',
                right =>
                    {string_lit, #{
                        line => 3,
                        spec => <<"manteau">>,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => string
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_remainder_mathematical_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Six() int { 27 % 7 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 27,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '%',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 7,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'Six',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_remainder_mathematical_operator_with_floats_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Six() int { 27.0 % 7.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {float_lit, #{
                        line => 3,
                        spec => 27.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '%',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 7.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% Conditional operators

typecheck_and_annotate_conditional_operator_with_bools_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { true and false }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {bool_lit, #{
                            line => 3,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    line => 3,
                    op => 'and',
                    right =>
                        {bool_lit, #{
                            line => 3,
                            spec => false,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_conditional_operator_with_nested_bools_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { false or true and true }\n"
        "    func Truthy() bool { true and true or false }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {bool_lit, #{
                            line => 3,
                            spec => false,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    line => 3,
                    op => 'or',
                    right =>
                        {binary_op, #{
                            left =>
                                {bool_lit, #{
                                    line => 3,
                                    spec => true,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            source => inferred,
                                            spec => bool
                                        }}
                                }},
                            line => 3,
                            op => 'and',
                            right =>
                                {bool_lit, #{
                                    line => 3,
                                    spec => true,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            source => inferred,
                                            spec => bool
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {binary_op, #{
                            left =>
                                {bool_lit, #{
                                    line => 4,
                                    spec => true,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => bool
                                        }}
                                }},
                            line => 4,
                            op => 'and',
                            right =>
                                {bool_lit, #{
                                    line => 4,
                                    spec => true,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => bool
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    line => 4,
                    op => 'or',
                    right =>
                        {bool_lit, #{
                            line => 4,
                            spec => false,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    type =>
                        {type, #{line => 4, source => inferred, spec => bool}}
                }}
            ],
            line => 4,
            params => [],
            return_type =>
                {type, #{line => 4, source => rufus_text, spec => bool}},
            spec => 'Truthy',
            type =>
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_conditional_operator_with_bool_and_int_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { true and 0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {bool_lit, #{
                        line => 3,
                        spec => true,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => bool
                            }}
                    }},
                line => 3,
                locals => #{},
                op => 'and',
                right =>
                    {int_lit, #{
                        line => 3,
                        spec => 0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% Comparison operators

typecheck_and_annotate_equality_comparison_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { 1 == 2 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '==',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_equality_comparison_operator_with_mismatched_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { 1 == 2.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {int_lit, #{
                        line => 3,
                        spec => 1,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '==',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 2.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_inequality_comparison_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { 1 != 1 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '!=',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_inequality_comparison_operator_with_mismatched_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { :two != 2.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {atom_lit, #{
                        line => 3,
                        spec => two,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '!=',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 2.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_less_than_comparison_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { 2 < 1 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '<',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_less_than_comparison_operator_with_mismatched_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { 2 < 1.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {int_lit, #{
                        line => 3,
                        spec => 2,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '<',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 1.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_less_than_comparison_operator_with_unsupported_operand_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { :two < :one }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {atom_lit, #{
                        line => 3,
                        spec => two,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '<',
                right =>
                    {atom_lit, #{
                        line => 3,
                        spec => one,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_less_than_or_equal_comparison_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { 2 <= 1 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '<=',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_less_than_or_equal_comparison_operator_with_mismatched_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { 2 <= 1.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {int_lit, #{
                        line => 3,
                        spec => 2,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '<=',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 1.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_less_than_or_equal_comparison_operator_with_unsupported_operand_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { :two <= :one }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {atom_lit, #{
                        line => 3,
                        spec => two,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '<=',
                right =>
                    {atom_lit, #{
                        line => 3,
                        spec => one,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_greater_than_comparison_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { 1 > 2 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '>',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_greater_than_comparison_operator_with_mismatched_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { 2 > 1.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {int_lit, #{
                        line => 3,
                        spec => 2,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '>',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 1.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_greater_than_comparison_operator_with_unsupported_operand_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { :two > :one }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {atom_lit, #{
                        line => 3,
                        spec => two,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '>',
                right =>
                    {atom_lit, #{
                        line => 3,
                        spec => one,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_greater_than_or_equal_comparison_operator_with_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { 1 >= 2 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    line => 3,
                    op => '>=',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Falsy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_greater_than_or_equal_comparison_operator_with_mismatched_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { 2 >= 1.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {int_lit, #{
                        line => 3,
                        spec => 2,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => int
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '>=',
                right =>
                    {float_lit, #{
                        line => 3,
                        spec => 1.0,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => float
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unmatched_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_greater_than_or_equal_comparison_operator_with_unsupported_operand_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() bool { :two >= :one }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {binary_op, #{
                left =>
                    {atom_lit, #{
                        line => 3,
                        spec => two,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }},
                line => 3,
                locals => #{},
                op => '>=',
                right =>
                    {atom_lit, #{
                        line => 3,
                        spec => one,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => atom
                            }}
                    }}
            }}
    },
    ?assertEqual({error, unsupported_operand_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).
