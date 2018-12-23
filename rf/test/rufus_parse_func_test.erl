-module(rufus_parse_func_test).

-include_lib("eunit/include/eunit.hrl").

function_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package rand
     func Int() int { 42 }
    "),
    {ok, AST} = rufus_parse:parse(Tokens),
    [
     {package, 2, "rand"},
     {func, 3, "Int", [], int, [{expr,3,{int,42}}]}
    ] = AST.
