-module(rufus_parse_test).

-include_lib("eunit/include/eunit.hrl").

parse_empty_package_test() ->
    {ok, Tokens, _} = rufus_scan:string("package empty"),
    {ok, AST} = rufus_parse:parse(Tokens),
    [
     {package, 1, "empty"}
    ] = AST.

parse_function_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package rand
     func Int() int { 42 }
    "),
    {ok, AST} = rufus_parse:parse(Tokens),
    [
     {package, 2, "rand"},
     {func, 3, "Int", [], int, [{expr,3,{int,42}}]}
    ] = AST.

parse_import_test() ->
    {ok, Tokens, _} = rufus_scan:string("
package foo
import \"bar\"
"),
    {ok, AST} = rufus_parse:parse(Tokens),
    [
     {package, 2, "foo"},
     {import, 3, "bar"}
    ] = AST.
