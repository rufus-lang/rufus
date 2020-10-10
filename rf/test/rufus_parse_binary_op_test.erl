-module(rufus_parse_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

%% Mathematical operators

parse_function_adding_two_ints_test() ->
    RufusText = "func Three() int { 1 + 2 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    line => 1,
                    op => '+',
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 1,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Three'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_adding_three_ints_test() ->
    RufusText = "func Six() int { 1 + 2 + 3 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    left =>
                        {binary_op, #{
                            op => '+',
                            left =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            right =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            line => 1
                        }},
                    line => 1,
                    op => '+',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 3,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Six'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_subtracting_two_ints_test() ->
    RufusText = "func One() int { 2 - 1 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    line => 1,
                    op => '-',
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 1,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'One'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_subtracting_three_ints_test() ->
    RufusText = "func MinusNine() int { 3 - 5 - 7 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    left =>
                        {binary_op, #{
                            op => '-',
                            left =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            right =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 5,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            line => 1
                        }},
                    line => 1,
                    op => '-',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 7,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'MinusNine'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_multiplying_two_ints_test() ->
    RufusText = "func FortyTwo() int { 2 * 21 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    line => 1,
                    op => '*',
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 21,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'FortyTwo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_multiplying_three_ints_test() ->
    RufusText = "func OneTwenty() int { 4 * 5 * 6 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    left =>
                        {binary_op, #{
                            op => '*',
                            left =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 4,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            right =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 5,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            line => 1
                        }},
                    line => 1,
                    op => '*',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 6,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'OneTwenty'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_dividing_two_ints_test() ->
    RufusText = "func FortyTwo() int { 84 / 2 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    line => 1,
                    op => '/',
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 84,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'FortyTwo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_dividing_three_ints_test() ->
    RufusText = "func Five() int { 100 / 10 / 2 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    left =>
                        {binary_op, #{
                            op => '/',
                            left =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 100,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            right =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 10,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            line => 1
                        }},
                    line => 1,
                    op => '/',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Five'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_remaindering_two_ints_test() ->
    RufusText = "func Six() int { 27 % 7 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    line => 1,
                    op => '%',
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 27,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 7,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Six'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_remaindering_three_ints_test() ->
    RufusText = "func Four() int { 100 % 13 % 5 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {binary_op, #{
                    left =>
                        {binary_op, #{
                            op => '%',
                            left =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 100,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            right =>
                                {int_lit, #{
                                    line => 1,
                                    spec => 13,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            spec => int
                                        }}
                                }},
                            line => 1
                        }},
                    line => 1,
                    op => '%',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 5,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Four'
        }}
    ],
    ?assertEqual(Expected, Forms).

%% Conditional operators

parse_function_anding_two_bools_test() ->
    RufusText = "func Falsy() bool { true and false }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {bool_lit, #{
                            line => 1,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }},
                    line => 1,
                    op => 'and',
                    right =>
                        {bool_lit, #{
                            line => 1,
                            spec => false,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_oring_two_bools_test() ->
    RufusText = "func Truthy() bool { true or false }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {bool_lit, #{
                            line => 1,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }},
                    line => 1,
                    op => 'or',
                    right =>
                        {bool_lit, #{
                            line => 1,
                            spec => false,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Truthy'
        }}
    ],
    ?assertEqual(Expected, Forms).

%% Comparison operators

parse_function_comparing_two_bools_for_equality_test() ->
    RufusText = "func Falsy() bool { true == false }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {bool_lit, #{
                            line => 1,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }},
                    line => 1,
                    op => '==',
                    right =>
                        {bool_lit, #{
                            line => 1,
                            spec => false,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_comparing_two_bools_for_inequality_test() ->
    RufusText = "func Falsy() bool { true != true }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {bool_lit, #{
                            line => 1,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }},
                    line => 1,
                    op => '!=',
                    right =>
                        {bool_lit, #{
                            line => 1,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => bool
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_comparing_two_ints_to_determine_which_is_lesser_test() ->
    RufusText = "func Falsy() bool { 5 < 3 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 5,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    line => 1,
                    op => '<',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 3,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_comparing_two_ints_to_determine_which_is_lesser_or_equal_test() ->
    RufusText = "func Falsy() bool { 5 <= 3 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 5,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    line => 1,
                    op => '<=',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 3,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_comparing_two_ints_to_determine_which_is_greater_test() ->
    RufusText = "func Falsy() bool { 3 > 5 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 3,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    line => 1,
                    op => '>',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 5,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_comparing_two_ints_to_determine_which_is_greater_or_equal_test() ->
    RufusText = "func Falsy() bool { 3 >= 5 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {int_lit, #{
                            line => 1,
                            spec => 3,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    line => 1,
                    op => '>=',
                    right =>
                        {int_lit, #{
                            line => 1,
                            spec => 5,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Falsy'
        }}
    ],
    ?assertEqual(Expected, Forms).
