-module('rfc_parse_test').

-include_lib("eunit/include/eunit.hrl").

empty_package_test() ->
    {ok, Tokens, _} = rfc_scan:string("\npackage empty"),
    {ok, AST} = rfc_parse:parse(Tokens),
    {package, "empty"} = AST.
