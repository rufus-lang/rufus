-module(rufus_parse_throw_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_that_throws_an_atom_literal_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw :kaboom\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {atom_lit, #{
                            line => 2,
                            spec => kaboom,
                            type => {type, #{line => 2, spec => atom}}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_a_bool_literal_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw true\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {bool_lit, #{
                            line => 2,
                            spec => true,
                            type => {type, #{line => 2, spec => bool}}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_a_float_literal_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw 42.0\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {float_lit, #{
                            line => 2,
                            spec => 42.0,
                            type => {type, #{line => 2, spec => float}}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_an_int_literal_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw 42\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {int_lit, #{
                            line => 2,
                            spec => 42,
                            type => {type, #{line => 2, spec => int}}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_a_string_literal_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw \"kaboom\"\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {string_lit, #{
                            line => 2,
                            spec => <<"kaboom">>,
                            type => {type, #{line => 2, spec => string}}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_a_cons_expression_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw list[int]{2|tail}\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {cons, #{
                            head =>
                                {int_lit, #{
                                    line => 2,
                                    spec => 2,
                                    type => {type, #{line => 2, spec => int}}
                                }},
                            line => 2,
                            tail => {identifier, #{line => 2, spec => tail}},
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 2, spec => int}},
                                    kind => list,
                                    line => 2,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_an_identifier_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw kaboom\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr => {identifier, #{line => 2, spec => kaboom}},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_that_throws_a_match_op_expression_test() ->
    RufusText =
        "func Explode() atom {\n"
        "    throw a = b\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {match_op, #{
                            left => {identifier, #{line => 2, spec => a}},
                            line => 2,
                            right => {identifier, #{line => 2, spec => b}}
                        }},
                    line => 2
                }}
            ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Explode'
        }}
    ],
    ?assertEqual(Expected, Forms).
