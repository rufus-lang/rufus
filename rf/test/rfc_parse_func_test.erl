-module(rfc_parse_func_test).

-include_lib("eunit/include/eunit.hrl").

function_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_scan:string("
     package rand
     func Int() int { 42 }
    "),
    {ok, AST} = rfc_parse:parse(Tokens),
    [
     {package, 2, "rand"},
     {func, 3, "Int", [], int, [{expr,3,{int,42}}]}
    ] = AST.
