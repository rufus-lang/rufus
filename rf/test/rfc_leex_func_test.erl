-module(rfc_leex_func_test).

-include_lib("eunit/include/eunit.hrl").

arity0_function_returns_a_float_test() ->
    {ok, Tokens, _} = rfc_leex:string("func number42() float { 42.0 }"),
    [
     {func, 1, "func"}, {identifier, 1, "number42"},
     {paren_begin, 1, "("}, {paren_end, 1, ")"}, {float, 1, "float"},
     {block_begin, 1, "{"}, {float_lit, 1, "42.0"}, {block_end, 1, "}"}
    ] = Tokens.

arity0_function_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_leex:string("func number42() int { 42 }"),
    [
     {func, 1, "func"}, {identifier, 1, "number42"},
     {paren_begin, 1, "("}, {paren_end, 1, ")"}, {int, 1, "int"},
     {block_begin, 1, "{"}, {int_lit, 1, "42"}, {block_end, 1, "}"}
    ] = Tokens.

arity0_function_returns_a_string_test() ->
    {ok, Tokens, _} = rfc_leex:string("func int42() string { \"hello\" }"),
    [
     {func, 1, "func"}, {identifier, 1, "int42"},
     {paren_begin, 1, "("}, {paren_end, 1, ")"}, {string, 1, "string"},
     {block_begin, 1, "{"}, {string_lit, 1, "hello"}, {block_end, 1, "}"}
    ] = Tokens.

arity0_multiline_function_returns_a_string_test() ->
    {ok, Tokens, _} = rfc_leex:string("
func int42() string {
    \"hello\"
}
"),
    [
     {func, 2, "func"}, {identifier, 2, "int42"},
     {paren_begin, 2, "("}, {paren_end, 2, ")"}, {string, 2, "string"},
     {block_begin, 2, "{"}, {string_lit, 3, "hello"}, {block_end, 4, "}"}
    ] = Tokens.

arity1_function_takes_an_int_and_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_leex:string("func echo(n int) int { n }"),
    [
     {func, 1, "func"}, {identifier, 1, "echo"},
     {paren_begin, 1, "("}, {identifier, 1, "n"}, {int, 1, "int"}, {paren_end, 1, ")"}, {int, 1, "int"},
     {block_begin, 1, "{"}, {identifier, 1, "n"}, {block_end, 1, "}"}
    ] = Tokens.

arity2_function_tokes_an_int_and_a_string_and_returns_a_float_test() ->
    {ok, Tokens, _} = rfc_leex:string("func float42(n int, s string) float { 42.0 }"),
    [
     {func, 1, "func"}, {identifier, 1, "float42"},
     {paren_begin, 1, "("},
     {identifier, 1, "n"}, {int, 1, "int"}, {comma, 1, ","},
     {identifier, 1, "s"}, {string, 1, "string"},
     {paren_end, 1, ")"}, {float, 1, "float"},
     {block_begin, 1, "{"}, {float_lit, 1, "42.0"}, {block_end, 1, "}"}
    ] = Tokens.
