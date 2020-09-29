-module(rufus_parse_list_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_returning_empty_list_of_ints_test() ->
    RufusText = "func EmptyNumbers() list[int] { list[int]{} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [],
                    line => 1,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'EmptyNumbers'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_list_of_int_with_one_element_test() ->
    RufusText = "func OneNumber() list[int] { list[int]{10} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {int_lit, #{
                            line => 1,
                            spec => 10,
                            type =>
                                {type, #{
                                    line => 1,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 1,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'OneNumber'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_list_of_many_ints_test() ->
    RufusText = "func ManyNumbers() list[int] { list[int]{10, 2} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {int_lit, #{
                            line => 1,
                            spec => 10,
                            type =>
                                {type, #{
                                    line => 1,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                        {int_lit, #{
                            line => 1,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 1,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 1,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'ManyNumbers'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_nested_list_of_list_of_ints_test() ->
    RufusText =
        "func NestedNumbers() list[list[int]] { list[list[int]]{list[int]{1}, list[int]{2, 3}} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 1,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 1,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 1,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 1,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }},
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 1,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 1,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 1,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 1,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 1,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }}
                    ],
                    line => 1,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 1,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 1,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[list[int]]'
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[list[int]]'
                }},
            spec => 'NestedNumbers'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_and_returning_list_of_ints_test() ->
    RufusText = "func Echo(numbers list[int]) list[int] { numbers }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {identifier, #{
                    line => 1,
                    spec => numbers
                }}
            ],
            line => 1,
            params => [
                {param, #{
                    line => 1,
                    spec => numbers,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_cons_expression_test() ->
    RufusText = "func Numbers() list[int] { list[int]{1|{2}} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {cons, #{
                    head =>
                        {int_lit, #{
                            line => 1,
                            spec => 1,
                            type =>
                                {type, #{
                                    line => 1,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    line => 1,
                    tail =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 1,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 1,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 1,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 1,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 1,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Numbers'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_creates_cons_expression_from_a_variable_and_a_list_literal_test() ->
    RufusText = "func Prepend(number int) list[int] { list[int]{number|{2, 3, 4}} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 1,
                            spec => number
                        }},
                    line => 1,
                    tail =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 1,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 1,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 1,
                                    spec => 4,
                                    type =>
                                        {type, #{
                                            line => 1,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 1,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 1,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 1,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 1,
            params => [
                {param, #{
                    line => 1,
                    spec => number,
                    type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 1,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Prepend'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_creates_cons_expression_from_head_and_tail_variables_test() ->
    RufusText =
        "\n"
        "    func Numbers() list[int] {\n"
        "        head = 1\n"
        "        tail = list[int]{2, 3, 4}\n"
        "        list[int]{head|tail}\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => head
                        }},
                    line => 3,
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                }},
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            spec => tail
                        }},
                    line => 4,
                    right =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 4,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 4,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 4,
                                    spec => 4,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 4,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 4,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }}
                }},
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 5,
                            spec => head
                        }},
                    line => 5,
                    tail =>
                        {identifier, #{
                            line => 5,
                            spec => tail
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 5,
                                    source => rufus_text,
                                    spec => int
                                }},
                            line => 5,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{
                            line => 2,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 2,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Numbers'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_cons_pattern_test() ->
    RufusText = "func Echo(list[int]{head|tail}) list[int] { list[int]{head|tail} }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {cons, #{
                    head => {identifier, #{line => 1, spec => head}},
                    line => 1,
                    tail => {identifier, #{line => 1, spec => tail}},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 1, source => rufus_text, spec => int}},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 1,
            params => [
                {cons, #{
                    head => {identifier, #{line => 1, spec => head}},
                    line => 1,
                    tail => {identifier, #{line => 1, spec => tail}},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 1, source => rufus_text, spec => int}},
                            line => 1,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 1, source => rufus_text, spec => int}},
                    line => 1,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_list_pattern_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Reverse(list[int]{a, b, c}) list[int] {\n"
        "        list[int]{c, b, a}\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {identifier, #{line => 4, spec => c}},
                        {identifier, #{line => 4, spec => b}},
                        {identifier, #{line => 4, spec => a}}
                    ],
                    line => 4,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 4, source => rufus_text, spec => int}},
                            line => 4,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [
                {list_lit, #{
                    elements => [
                        {identifier, #{line => 3, spec => a}},
                        {identifier, #{line => 3, spec => b}},
                        {identifier, #{line => 3, spec => c}}
                    ],
                    line => 3,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    line => 3,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Reverse'
        }}
    ],
    ?assertEqual(Expected, Forms).
