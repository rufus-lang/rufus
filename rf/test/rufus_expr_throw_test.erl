-module(rufus_expr_throw_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_function_with_throw_atom_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw :kaboom\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {atom_lit, #{
                            line => 3,
                            spec => kaboom,
                            type => {type, #{line => 3, spec => atom}}
                        }},
                    line => 3,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 3,
                            spec => 'throw atom'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_bool_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw true\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {bool_lit, #{
                            line => 3,
                            spec => true,
                            type => {type, #{line => 3, spec => bool}}
                        }},
                    line => 3,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 3,
                            spec => 'throw bool'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_float_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 42.0\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {float_lit, #{
                            line => 3,
                            spec => 42.0,
                            type => {type, #{line => 3, spec => float}}
                        }},
                    line => 3,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 3,
                            spec => 'throw float'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_int_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 42\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {int_lit, #{
                            line => 3,
                            spec => 42,
                            type => {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    type =>
                        {type, #{kind => throw, line => 3, spec => 'throw int'}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_string_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw \"kaboom\"\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {string_lit, #{
                            line => 3,
                            spec => <<"kaboom">>,
                            type => {type, #{line => 3, spec => string}}
                        }},
                    line => 3,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 3,
                            spec => 'throw string'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_variable_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    a = :kaboom\n"
        "    throw a\n"
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
                            spec => a,
                            type => {type, #{line => 3, spec => atom}}
                        }},
                    line => 3,
                    right =>
                        {atom_lit, #{
                            line => 3,
                            spec => kaboom,
                            type => {type, #{line => 3, spec => atom}}
                        }},
                    type => {type, #{line => 3, spec => atom}}
                }},
                {throw, #{
                    expr =>
                        {identifier, #{
                            line => 4,
                            spec => a,
                            type => {type, #{line => 3, spec => atom}}
                        }},
                    line => 4,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 4,
                            spec => 'throw atom'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_cons_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    head = 1\n"
        "    tail = list[int]{2, 3, 4}\n"
        "    throw list[int]{head|tail}\n"
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
                            spec => head,
                            type => {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type => {type, #{line => 3, spec => int}}
                        }},
                    type => {type, #{line => 3, spec => int}}
                }},
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{head => [{type, #{line => 3, spec => int}}]},
                            spec => tail,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 4, spec => int}},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 4,
                                    spec => 2,
                                    type => {type, #{line => 4, spec => int}}
                                }},
                                {int_lit, #{
                                    line => 4,
                                    spec => 3,
                                    type => {type, #{line => 4, spec => int}}
                                }},
                                {int_lit, #{
                                    line => 4,
                                    spec => 4,
                                    type => {type, #{line => 4, spec => int}}
                                }}
                            ],
                            line => 4,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 4, spec => int}},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            element_type => {type, #{line => 4, spec => int}},
                            kind => list,
                            line => 4,
                            spec => 'list[int]'
                        }}
                }},
                {throw, #{
                    expr =>
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 5,
                                    spec => head,
                                    type => {type, #{line => 3, spec => int}}
                                }},
                            line => 5,
                            tail =>
                                {identifier, #{
                                    line => 5,
                                    spec => tail,
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{line => 4, spec => int}},
                                            kind => list,
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 5, spec => int}},
                                    kind => list,
                                    line => 5,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 5,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 5,
                            spec => 'throw list[int]'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

typecheck_and_annotate_function_with_throw_match_op_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 1 = 1\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {match_op, #{
                            left =>
                                {int_lit, #{
                                    line => 3,
                                    spec => 1,
                                    type => {type, #{line => 3, spec => int}}
                                }},
                            line => 3,
                            right =>
                                {int_lit, #{
                                    line => 3,
                                    spec => 1,
                                    type => {type, #{line => 3, spec => int}}
                                }},
                            type => {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    type =>
                        {type, #{kind => throw, line => 3, spec => 'throw int'}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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
