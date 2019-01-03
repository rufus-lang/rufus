-module(rufus_parse_test).

-include_lib("eunit/include/eunit.hrl").

parse_empty_package_test() ->
    {ok, Tokens, _} = rufus_scan:string("package empty"),
    {ok, AST} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 1, "empty"}
    ], AST).

parse_function_returning_a_bool_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package example
     func True() bool { true }
    "),
    {ok, AST} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "True", [], bool, [{expr, 3, {bool, true}}]}
    ], AST).

parse_function_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package math
     func Pi() float { 3.14159265359 }
    "),
    {ok, AST} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "math"},
     {func, 3, "Pi", [], float, [{expr, 3, {float, 3.14159265359}}]}
    ], AST).

parse_function_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package rand
     func Number() int { 42 }
    "),
    {ok, AST} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "rand"},
     {func, 3, "Number", [], int, [{expr, 3, {int, 42}}]}
    ], AST).

parse_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package example
     func Greeting() string { \"Hello\" }
    "),
    {ok, AST} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "Greeting", [], string, [{expr, 3, {string, "Hello"}}]}
    ], AST).

parse_import_test() ->
    {ok, Tokens, _} = rufus_scan:string("
package foo
import \"bar\"
"),
    {ok, AST} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "foo"},
     {import, 3, "bar"}
    ], AST).
