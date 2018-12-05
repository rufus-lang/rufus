-module(rfc_scan_func_test).

-include_lib("eunit/include/eunit.hrl").

function_returns_a_float_test() ->
    {ok, Tokens, _} = rfc_scan:string("func number42() float { 42.0 }"),
    [
     {func, 1},
     {identifier, 1, "number42"},
     {'(', 1},
     {')', 1},
     {float, 1},
     {'{', 1},
     {float_lit, 1, 42.0},
     {'}', 1}
    ] = Tokens.

function_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_scan:string("func number42() int { 42 }"),
    [
     {func, 1},
     {identifier, 1, "number42"},
     {'(', 1},
     {')', 1},
     {int, 1},
     {'{', 1},
     {int_lit, 1, 42},
     {'}', 1}
    ] = Tokens.

function_returns_a_string_test() ->
    {ok, Tokens, _} = rfc_scan:string("func text42() string { \"42\" }"),
    [
     {func, 1},
     {identifier, 1, "text42"},
     {'(', 1},
     {')', 1},
     {string, 1},
     {'{', 1},
     {string_lit, 1, "42"},
     {'}', 1}
    ] = Tokens.

multiline_function_returns_a_string_test() ->
    {ok, Tokens, _} = rfc_scan:string("
func text42() string {
    \"42\"
}
"),
    [
     {func, 2},
     {identifier, 2, "text42"},
     {'(', 2},
     {')', 2},
     {string, 2},
     {'{', 2},
     {string_lit, 3, "42"},
     {'}', 4}
    ] = Tokens.

function_takes_an_int_and_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_scan:string("func echo(n int) int { n }"),
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
    ] = Tokens.

function_tokes_an_int_and_a_string_and_returns_a_float_test() ->
    {ok, Tokens, _} = rfc_scan:string("func float42(n int, s string) float { 42.0 }"),
    [
     {func, 1},
     {identifier, 1, "float42"},
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
    ] = Tokens.

%% function_takes_an_unused_argument_test() ->
%%     {ok, Tokens, _} = rfc_scan:string("func unused(_ int) int { 0 }")

%% function_takes_an_unused_named_argument_test() ->
%%     {ok, Tokens, _} = rfc_scan:string("func unused(_num int) int { 0 }")
