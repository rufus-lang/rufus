-module(rufus_raw_tokenize_func_test).

-include_lib("eunit/include/eunit.hrl").

string_with_function_returning_a_bool_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func False() bool { false }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "False"},
            {'(', 1},
            {')', 1},
            {bool, 1},
            {'{', 1},
            {bool_lit, 1, false},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number() float { 42.0 }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "number"},
            {'(', 1},
            {')', 1},
            {float, 1},
            {'{', 1},
            {float_lit, 1, 42.0},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number() int { 42 }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "number"},
            {'(', 1},
            {')', 1},
            {int, 1},
            {'{', 1},
            {int_lit, 1, 42},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func text() string { \"42\" }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "text"},
            {'(', 1},
            {')', 1},
            {string, 1},
            {'{', 1},
            {string_lit, 1, "42"},
            {'}', 1}
        ],
        Tokens
    ).

string_with_multiline_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "\n"
        "func text() string {\n"
        "    \"42\"\n"
        "}\n"
    ),
    ?assertEqual(
        [
            {eol, 1},
            {func, 2},
            {identifier, 2, "text"},
            {'(', 2},
            {')', 2},
            {string, 2},
            {'{', 2},
            {eol, 2},
            {string_lit, 3, "42"},
            {eol, 3},
            {'}', 4},
            {eol, 4}
        ],
        Tokens
    ).

string_with_multiline_multiple_expression_function_returning_an_atom_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "\n"
        "func number() atom {\n"
        "    \"42\"\n"
        "    :fortytwo\n"
        "}\n"
    ),
    ?assertEqual(
        [
            {eol, 1},
            {func, 2},
            {identifier, 2, "number"},
            {'(', 2},
            {')', 2},
            {atom, 2},
            {'{', 2},
            {eol, 2},
            {string_lit, 3, "42"},
            {eol, 3},
            {atom_lit, 4, fortytwo},
            {eol, 4},
            {'}', 5},
            {eol, 5}
        ],
        Tokens
    ).

string_with_semicolon_multiple_expression_function_returning_an_atom_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number() atom { \"42\"; :fortytwo }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "number"},
            {'(', 1},
            {')', 1},
            {atom, 1},
            {'{', 1},
            {string_lit, 1, "42"},
            {';', 1},
            {atom_lit, 1, fortytwo},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_takes_an_int_and_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func echo(n int) int { n }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "echo"},
            {'(', 1},
            {identifier, 1, "n"},
            {int, 1},
            {')', 1},
            {int, 1},
            {'{', 1},
            {identifier, 1, "n"},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_takes_an_int_and_a_string_and_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number(n int, s string) float { 42.0 }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "number"},
            {'(', 1},
            {identifier, 1, "n"},
            {int, 1},
            {',', 1},
            {identifier, 1, "s"},
            {string, 1},
            {')', 1},
            {float, 1},
            {'{', 1},
            {float_lit, 1, 42.0},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_takes_an_unused_argument_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func unused(_ int) int { 0 }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "unused"},
            {'(', 1},
            {identifier, 1, "_"},
            {int, 1},
            {')', 1},
            {int, 1},
            {'{', 1},
            {int_lit, 1, 0},
            {'}', 1}
        ],
        Tokens
    ).

string_with_function_takes_an_unused_named_argument_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func unused(_num int) int { 0 }"),
    ?assertEqual(
        [
            {func, 1},
            {identifier, 1, "unused"},
            {'(', 1},
            {identifier, 1, "_num"},
            {int, 1},
            {')', 1},
            {int, 1},
            {'{', 1},
            {int_lit, 1, 0},
            {'}', 1}
        ],
        Tokens
    ).
