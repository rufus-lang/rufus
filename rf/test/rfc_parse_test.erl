-module('rfc_parse_test').

-include_lib("eunit/include/eunit.hrl").

empty_package_test() ->
    {ok, Tokens, _} = rfc_scan:string("package empty"),
    {ok, AST} = rfc_parse:parse(Tokens),
    {package, "empty"} = AST.

%% import_test() ->
%%     {ok, Tokens, _} = rfc_scan:string("
%% package foo
%% import \"bar\"
%% "),
%%     {ok, AST} = rfc_parse:parse(Tokens),
%%     {package, "empty"} = AST.
