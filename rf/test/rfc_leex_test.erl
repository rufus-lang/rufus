-module(rfc_leex_test).

-include_lib("eunit/include/eunit.hrl").

empty_package_test() ->
    {ok, Tokens, _} = rfc_leex:string("package empty"),
    [{package, 1, "package"}, {identifier, 1, "empty"}] = Tokens.

arity_0_function_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity0
func number42() int { 42 }
"),
    [
     {package,  2, "package"}, {identifier, 2, "arity0"},
     {func, 3, "func"}, {identifier, 3, "number42"},
     {paren_begin, 3, "("}, {paren_end, 3, ")"}, {int, 3, "int"},
     {block_begin, 3, "{"}, {int, 3, "42"}, {block_end, 3, "}"}
    ] = Tokens.

arity_1_function_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity1
func mirror(n int) int { n } "),
    [
     {package,  2, "package"}, {identifier, 2, "arity1"},
     {func, 3, "func"}, {identifier, 3, "mirror"},
     {paren_begin, 3, "("}, {identifier, 3, "n"}, {int, 3, "int"}, {paren_end, 3, ")"}, {int, 3, "int"},
     {block_begin, 3, "{"}, {identifier, 3, "n"}, {block_end, 3, "}"}
    ] = Tokens.
